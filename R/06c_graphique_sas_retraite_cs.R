# ==============================================================================
# 06c_graphique_sas_retraite_cs.R — Sortie d'emploi DÉCOMPOSÉE : sas vs retraite
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (04) ; param δ, AGE_CONJ_TOUS_REGIMES_2023 (00) ;
#             paramètres de restitution (00)
# PRODUIT   : objet `repartition_sas` ; sorties/sas_vs_retraite_par_cs.png
#
# POURQUOI (correction "lourde") : μ = âge de SORTIE DE L'EMPLOI, souvent
# inférieur à l'âge légal de LIQUIDATION (surtout chez les ouvriers, μ~59 ans).
# Entre les deux, l'individu est "parti de l'emploi mais pas encore en retraite"
# = SAS (chômage senior, inactivité, invalidité). Le 06/06b regroupent sas +
# retraite sous une seule couleur ; ce graphique les DISTINGUE, pour montrer que
# les ouvriers traversent une longue zone grise avant de pouvoir liquider.
#
# MÉTHODE : parmi les sortis de l'emploi (part calendaire p_cal_central), on
# répartit entre RETRAITE (a atteint l'âge légal en 2030) et SAS (sorti mais
# encore sous l'âge légal en 2030). L'âge légal est APPROCHÉ par un seuil unique
# AGE_LEGAL_2030 (cf. note ci-dessous) — approximation assumée, cohérente avec
# le modèle light qui ne gère pas l'âge légal génération par génération.
# LIMITE : ce découpage est INDICATIF (répartition d'une espérance), il n'entre
# pas dans les décomptes officiels du 05.
# ==============================================================================
if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Seuil d'âge légal moyen atteint sur l'horizon 2030 (approximation). L'ancrage
# tous régimes fin 2023 (62,75) + montée résiduelle : on prend ~63,5 ans, ordre
# de grandeur de l'âge légal effectif des générations concernées d'ici 2030.
AGE_LEGAL_2030 <- 63.5

if (file.exists("R/00b_fonts.R")) source("R/00b_fonts.R") else {FONT_TITRE<-"sans";FONT_CORPS<-"sans"}

decalage <- ANNEE_REF_GRAPHIQUE - 2024
niveaux <- c("Maintien en emploi", "En retraite (liquidée)",
             "En sas (parti, pas encore liquidé)", "Sortie invalidité", "Décès")

repartition_sas <- bts_projete |>
  mutate(
    age_affiche = age_2024 + decalage,
    tranche = cut(age_affiche, breaks = BREAKS_TRANCHES, labels = LABELS_TRANCHES),
    # part totale de sortie par le calendrier (sas + retraite), pondérée risques
    sortie_cal = p_cal_central * (1 - p_inval) * (1 - p_deces),
    # clé du découpage : l'individu aura-t-il atteint l'âge légal en 2030 ?
    # si oui -> sa sortie d'emploi débouche sur une RETRAITE liquidée ;
    # si non (sorti à μ < âge légal) -> il est encore dans le SAS en 2030.
    atteint_legal_2030 = age_2030 >= AGE_LEGAL_2030,
    `En retraite (liquidée)`              = if_else(atteint_legal_2030, sortie_cal, 0),
    `En sas (parti, pas encore liquidé)`  = if_else(atteint_legal_2030, 0, sortie_cal),
    `Sortie invalidité` = p_inval * (1 - p_cal_central) * (1 - p_deces),
    `Décès`             = p_deces * (1 - p_cal_central) * (1 - p_inval),
    `Maintien en emploi` = 1 - (sortie_cal + `Sortie invalidité` + `Décès`)
  ) |>
  group_by(cs1, tranche) |>
  summarise(across(all_of(niveaux), mean), .groups = "drop") |>
  pivot_longer(-c(cs1, tranche), names_to = "classe", values_to = "part") |>
  mutate(classe = factor(classe, levels = niveaux),
         cs1 = factor(cs1, levels = c("Cadres", "Prof. intermediaires",
                                      "Employes", "Ouvriers")))

# Palette : retraite = bleu franc (aboutissement) ; sas = bleu-gris délavé
# (zone grise, transitoire) ; le contraste sas/retraite est le message.
couleurs <- c("En retraite (liquidée)"             = "#3E6E8E",  # bleu ardoise
              "En sas (parti, pas encore liquidé)" = "#B8C9D4",  # bleu-gris pâle
              "Sortie invalidité"                  = "#E0A458",  # ambre
              "Décès"                              = "#8E7196",  # prune
              "Maintien en emploi"                 = "#8FB996")  # sauge

CREME <- "#FBF7EF"; ENCRE <- "#2B2622"; ENCRE2 <- "#6E655C"

g <- repartition_sas |>
  ggplot(aes(x = tranche, y = part, fill = classe)) +
  geom_col(position = "fill", width = 0.72, color = CREME, linewidth = 0.6) +
  geom_text(data = ~ filter(.x, part >= 0.05),
            aes(label = percent(part, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            color = "white", size = 3, family = FONT_CORPS) +
  facet_wrap(~ cs1, nrow = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = couleurs) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE)) +
  labs(
    title = "Retraite ou sas ? La zone grise des fins de carrière",
    subtitle = sprintf("Devenir en 2030 des salariés de 43 ans et + — sas = sorti de l'emploi mais sous l'âge légal (~%.1f ans)",
                       AGE_LEGAL_2030),
    caption = "Le sas (bleu pâle) distingue ceux qui ont quitté l'emploi sans pouvoir encore liquider leur retraite.\nDécoupage indicatif (âge légal approché) ; n'entre pas dans les décomptes. Sources : DREES, EACR, Insee — calculs propres · données : table test",
    x = sprintf("Âge en %d", ANNEE_REF_GRAPHIQUE), y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text            = element_text(family = FONT_CORPS, color = ENCRE),
    plot.title      = element_text(family = FONT_TITRE, size = 28, hjust = 0, color = ENCRE, margin = margin(b = 2)),
    plot.subtitle   = element_text(family = FONT_CORPS, size = 10.5, color = ENCRE2, margin = margin(b = 14)),
    plot.caption    = element_text(family = FONT_CORPS, size = 7.5, color = ENCRE2, hjust = 0, margin = margin(t = 14)),
    strip.text      = element_text(family = FONT_TITRE, size = 18, color = ENCRE, margin = margin(b = 6)),
    axis.text.x     = element_text(family = FONT_CORPS, size = 8.5, color = ENCRE2, angle = 30, hjust = 1),
    axis.text.y     = element_text(family = FONT_CORPS, size = 9, color = ENCRE2),
    axis.title.x    = element_text(family = FONT_CORPS, size = 10, color = ENCRE2, margin = margin(t = 8)),
    legend.position = "top", legend.justification = "left",
    legend.text     = element_text(family = FONT_CORPS, size = 9.5),
    legend.margin   = margin(b = 6),
    panel.grid.major.y = element_line(color = "#E8E0D4", linewidth = 0.4),
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    panel.spacing   = unit(1.1, "lines"),
    plot.background  = element_rect(fill = CREME, color = NA),
    panel.background = element_rect(fill = CREME, color = NA),
    plot.margin     = margin(18, 20, 12, 18)
  )

ggsave(file.path(DIR_SORTIES, "sas_vs_retraite_par_cs.png"),
       g, width = 11, height = 5.8, dpi = 300, device = ragg::agg_png, background = CREME)
message("06c OK -> sorties/sas_vs_retraite_par_cs.png")
