# ==============================================================================
# 06b_graphique_repartition_cs.R — Répartition par CS × tranche d'âge (design)
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (04) ; paramètres de restitution (00)
# PRODUIT   : objet `repartition_cs` ; sorties/repartition_par_cs.png
# LECTURE   : petits multiples — une facette par CS, barres empilées à 100 %
#             des parts ESPÉRÉES par cause (mêmes 4 postes que le 06, mêmes
#             maths de risques concurrents). Révèle le GRADIENT SOCIAL : plus
#             on va vers les ouvriers, plus la sortie est précoce et fréquente.
# DESIGN    : palette « atelier » douce, typographie manuscrite (Caveat/Kalam,
#             style Excalidraw), fond crème, rendu ragg. Rigueur dataviz :
#             barres à somme 100 %, couleurs séquentielles lisibles, étiquettes
#             seulement sur segments >= 4 %, ordre des CS = gradient social.
# ==============================================================================
if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
library(dplyr); library(tidyr); library(ggplot2); library(scales)

# --- Typographie manuscrite (style Excalidraw) via ragg ----------------------
# ragg lit les polices système ; on déclare des familles nommées si présentes.
if (file.exists("R/00b_fonts.R")) source("R/00b_fonts.R") else {FONT_TITRE<-"sans";FONT_CORPS<-"sans"}

# --- Données : parts espérées par cause, PAR CS et tranche -------------------
decalage <- ANNEE_REF_GRAPHIQUE - 2024
niveaux <- c("Maintien en emploi", "Sortie de l'emploi (retraite ou sas)",
             "Sortie invalidité", "Décès")

repartition_cs <- bts_projete |>
  mutate(age_affiche = age_2024 + decalage,
         tranche = cut(age_affiche, breaks = BREAKS_TRANCHES, labels = LABELS_TRANCHES),
         # risques concurrents (identique au 06) : parts additives, somme = 1
         `Maintien en emploi` = 1 - p_central,   # vraie non-sortie (cf. heatmap)
         # 3 causes réparties À L'INTÉRIEUR de p_central (somme == p_central) :
         .brut_ret = p_cal_central * (1 - p_inval) * (1 - p_deces),
         .brut_inv = p_inval * (1 - p_cal_central) * (1 - p_deces),
         .brut_dec = p_deces * (1 - p_cal_central) * (1 - p_inval),
         .som = pmax(.brut_ret + .brut_inv + .brut_dec, 1e-12),
         `Sortie de l'emploi (retraite ou sas)` = p_central * .brut_ret / .som,
         `Sortie invalidité`                    = p_central * .brut_inv / .som,
         `Décès`                                = p_central * .brut_dec / .som) |>
  group_by(cs1, tranche) |>
  summarise(across(all_of(niveaux), mean), .groups = "drop") |>
  pivot_longer(-c(cs1, tranche), names_to = "classe", values_to = "part") |>
  mutate(classe = factor(classe, levels = niveaux),
         # ordre des facettes = gradient social (cadres -> ouvriers)
         cs1 = factor(cs1, levels = c("Cadres", "Prof. intermediaires",
                                      "Employes", "Ouvriers")))

# --- Palette « atelier » : douce, chaude, lisible ---------------------------
# maintien = sauge apaisée ; retraite = bleu encre ; invalidité = ambre ;
# décès = prune sourde. Contraste suffisant, pas criard, daltonien-friendly.
couleurs <- c("Sortie de l'emploi (retraite ou sas)" = "#3E6E8E",  # bleu ardoise
              "Sortie invalidité"                 = "#E0A458",  # ambre doux
              "Décès"                             = "#8E7196",  # prune grisée
              "Maintien en emploi"                = "#8FB996")  # sauge

CREME   <- "#FBF7EF"   # fond papier
ENCRE   <- "#2B2622"   # texte principal (brun-noir doux)
ENCRE2  <- "#6E655C"   # texte secondaire

g <- repartition_cs |>
  ggplot(aes(x = tranche, y = part, fill = classe)) +
  geom_col(position = "fill", width = 0.72, color = CREME, linewidth = 0.6) +
  geom_text(data = ~ filter(.x, part >= 0.04),
            aes(label = percent(part, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            color = "white", size = 3.1, family = FONT_CORPS) +
  facet_wrap(~ cs1, nrow = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = couleurs) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE)) +
  labs(
    title = "Qui sera encore là en 2030 ?",
    subtitle = sprintf("Devenir des salariés de 43 ans et + selon la catégorie sociale — scénario central (δ = %.2f an)",
                       delta_central),
    caption = "Lecture : à 55-60 ans, 23 % des cadres ont quitté l'emploi contre 60 % des ouvriers.\nSources : DREES (departretraite_parcsp, Insee enquête Emploi), EACR invalidité, mortalité Insee — calculs propres · données : table test",
    x = sprintf("Âge en %d", ANNEE_REF_GRAPHIQUE), y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text            = element_text(family = FONT_CORPS, color = ENCRE),
    plot.title      = element_text(family = FONT_TITRE, size = 30, hjust = 0,
                                   color = ENCRE, margin = margin(b = 2)),
    plot.subtitle   = element_text(family = FONT_CORPS, size = 11, color = ENCRE2,
                                   margin = margin(b = 14)),
    plot.caption    = element_text(family = FONT_CORPS, size = 7.5, color = ENCRE2,
                                   hjust = 0, margin = margin(t = 14)),
    strip.text      = element_text(family = FONT_TITRE, size = 18, color = ENCRE,
                                   margin = margin(b = 6)),
    axis.text.x     = element_text(family = FONT_CORPS, size = 8.5, color = ENCRE2,
                                   angle = 30, hjust = 1),
    axis.text.y     = element_text(family = FONT_CORPS, size = 9, color = ENCRE2),
    axis.title.x    = element_text(family = FONT_CORPS, size = 10, color = ENCRE2,
                                   margin = margin(t = 8)),
    legend.position = "top", legend.justification = "left",
    legend.text     = element_text(family = FONT_CORPS, size = 10),
    legend.margin   = margin(b = 6),
    panel.grid.major.y = element_line(color = "#E8E0D4", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.spacing   = unit(1.1, "lines"),
    plot.background  = element_rect(fill = CREME, color = NA),
    panel.background = element_rect(fill = CREME, color = NA),
    plot.margin     = margin(18, 20, 12, 18)
  )

ggsave(file.path(DIR_SORTIES, "repartition_par_cs.png"),
       g, width = 11, height = 5.6, dpi = 300, device = ragg::agg_png,
       background = CREME)
message("06b OK -> sorties/repartition_par_cs.png")