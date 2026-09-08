# ==============================================================================
# 06e_graphique_repartition_age_regroupe.R — Variantes du 06d PAR ÂGE :
#   (1) toutes CS confondues, par tranche d'âge ;
#   (2) restreint aux 55 ans et +, en détail âge par âge.
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (04) ; paramètres de restitution (00),
#             AGE_SENIOR (00)
# PRODUIT   : objets `repartition_age_regroupe`, `repartition_55plus_age` ;
#             sorties/repartition_par_age_regroupe.png
#             sorties/repartition_55plus_regroupe.png
# LECTURE   : mêmes trois postes que le 06d (maintien / sortie retraite-sas /
#             invalidité ou décès), mêmes maths de risques concurrents, même
#             note de lecture. Le graphique 55+ passe à la granularité ANNÉE
#             D'ÂGE : c'est entre 55 et 61 ans que la bascule se joue, la
#             tranche la masquait.
# ==============================================================================
if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
library(dplyr); library(tidyr); library(ggplot2); library(scales)

# --- Typographie manuscrite (style Excalidraw) via ragg, comme 06b/06d -------
if (file.exists("R/00b_fonts.R")) source("R/00b_fonts.R") else {FONT_TITRE<-"sans";FONT_CORPS<-"sans"}

# --- Base commune : les 3 postes du 06d, calculés une fois par individu ------
decalage <- ANNEE_REF_GRAPHIQUE - 2024
niveaux <- c("Maintien en emploi", "Sortie de l'emploi (retraite ou sas)",
             "Sortie invalidité ou décès")

base_age <- bts_projete |>
  mutate(age_affiche = age_2024 + decalage,
         tranche = cut(age_affiche, breaks = BREAKS_TRANCHES, labels = LABELS_TRANCHES),
         `Maintien en emploi` = 1 - p_central,
         # répartition additive de p_central entre les causes (somme == p_central)
         .brut_ret = p_cal_central * (1 - p_inval) * (1 - p_deces),
         .brut_inv = p_inval * (1 - p_cal_central) * (1 - p_deces),
         .brut_dec = p_deces * (1 - p_cal_central) * (1 - p_inval),
         .som = pmax(.brut_ret + .brut_inv + .brut_dec, 1e-12),
         `Sortie de l'emploi (retraite ou sas)` = p_central * .brut_ret / .som,
         `Sortie invalidité ou décès` = p_central * (.brut_inv + .brut_dec) / .som)

# --- Palette « atelier » et note de lecture, identiques au 06d ---------------
couleurs <- c("Sortie de l'emploi (retraite ou sas)" = "#3E6E8E",  # bleu ardoise
              "Sortie invalidité ou décès"           = "#E0A458",  # ambre doux
              "Maintien en emploi"                   = "#8FB996")  # sauge
CREME  <- "#FBF7EF"; ENCRE <- "#2B2622"; ENCRE2 <- "#6E655C"

note_lecture <- paste(
  "Note de lecture — un seul devenir par salarié : les trois états sont des risques concurrents, chacun n'est compté que dans le PREMIER qui le fait sortir de l'emploi.",
  "Passé 61 ans, le départ en retraite est quasi certain et absorbe les autres causes : la part « invalidité ou décès » devient invisible non parce que ces événements",
  "cessent d'exister, mais parce qu'ils surviendraient pour l'essentiel après la sortie de l'emploi — hors du champ du modèle, qui compte des sorties d'emploi, pas des décès.",
  sep = "\n")

theme_atelier <- theme_minimal(base_size = 13) +
  theme(
    text            = element_text(family = FONT_CORPS, color = ENCRE),
    plot.title      = element_text(family = FONT_TITRE, size = 30, hjust = 0,
                                   color = ENCRE, margin = margin(b = 2)),
    plot.subtitle   = element_text(family = FONT_CORPS, size = 11, color = ENCRE2,
                                   margin = margin(b = 14)),
    # la note de lecture EST le message : corps 9.5, encre pleine
    plot.caption    = element_text(family = FONT_CORPS, size = 9.5, color = ENCRE,
                                   hjust = 0, lineheight = 1.25,
                                   margin = margin(t = 14)),
    axis.text.x     = element_text(family = FONT_CORPS, size = 9.5, color = ENCRE2),
    axis.text.y     = element_text(family = FONT_CORPS, size = 9, color = ENCRE2),
    axis.title.x    = element_text(family = FONT_CORPS, size = 10, color = ENCRE2,
                                   margin = margin(t = 8)),
    legend.position = "top", legend.justification = "left",
    legend.text     = element_text(family = FONT_CORPS, size = 10),
    legend.margin   = margin(b = 6),
    panel.grid.major.y = element_line(color = "#E8E0D4", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background  = element_rect(fill = CREME, color = NA),
    panel.background = element_rect(fill = CREME, color = NA),
    plot.margin     = margin(18, 20, 12, 18)
  )

# ============= (1) Toutes CS confondues, par tranche d'âge ====================
repartition_age_regroupe <- base_age |>
  group_by(tranche) |>
  summarise(across(all_of(niveaux), mean), .groups = "drop") |>
  pivot_longer(-tranche, names_to = "classe", values_to = "part") |>
  mutate(classe = factor(classe, levels = niveaux))

g1 <- repartition_age_regroupe |>
  ggplot(aes(x = tranche, y = part, fill = classe)) +
  geom_col(position = "fill", width = 0.62, color = CREME, linewidth = 0.6) +
  geom_text(data = ~ filter(.x, part >= 0.04),
            aes(label = percent(part, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            color = "white", size = 3.4, family = FONT_CORPS) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = couleurs) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  labs(
    title = "Qui sera encore là en 2030 ?",
    subtitle = sprintf("Devenir des salariés de 43 ans et + par tranche d'âge, toutes catégories sociales — scénario central (δ = %.2f an) ; invalidité et décès regroupés",
                       delta_central),
    caption = paste0(note_lecture, "\n",
                     "Sources : DREES (departretraite_parcsp, Insee enquête Emploi), EACR invalidité, mortalité Insee — calculs propres · données : table test"),
    x = sprintf("Âge en %d", ANNEE_REF_GRAPHIQUE), y = NULL, fill = NULL
  ) +
  theme_atelier

# largeur 11 : la note de lecture et le sous-titre sont calibrés pour cette
# largeur (comme 06d) — plus étroit, ils débordent du cadre
ggsave(file.path(DIR_SORTIES, "repartition_par_age_regroupe.png"),
       g1, width = 11, height = 6.4, dpi = 300, device = ragg::agg_png,
       background = CREME)

# ============= (2) Les 55 ans et + seulement, âge par âge =====================
repartition_55plus_age <- base_age |>
  filter(age_affiche >= AGE_SENIOR) |>
  group_by(age_affiche) |>
  summarise(across(all_of(niveaux), mean), effectif = n(), .groups = "drop") |>
  pivot_longer(-c(age_affiche, effectif), names_to = "classe", values_to = "part") |>
  mutate(classe = factor(classe, levels = niveaux))

g2 <- repartition_55plus_age |>
  ggplot(aes(x = factor(age_affiche), y = part, fill = classe)) +
  geom_col(position = "fill", width = 0.72, color = CREME, linewidth = 0.6) +
  geom_text(data = ~ filter(.x, part >= 0.04),
            aes(label = percent(part, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            color = "white", size = 2.9, family = FONT_CORPS) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = couleurs) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  labs(
    title = sprintf("Les %d ans et + : la bascule, âge par âge", AGE_SENIOR),
    subtitle = sprintf("Devenir d'ici 2030 des salariés de %d ans et +, toutes catégories sociales — scénario central (δ = %.2f an) ; invalidité et décès regroupés",
                       AGE_SENIOR, delta_central),
    caption = paste0(note_lecture, "\n",
                     "Sources : DREES (departretraite_parcsp, Insee enquête Emploi), EACR invalidité, mortalité Insee — calculs propres · données : table test"),
    x = sprintf("Âge en %d", ANNEE_REF_GRAPHIQUE), y = NULL, fill = NULL
  ) +
  theme_atelier

ggsave(file.path(DIR_SORTIES, "repartition_55plus_regroupe.png"),
       g2, width = 10.5, height = 6.4, dpi = 300, device = ragg::agg_png,
       background = CREME)
message("06e OK -> sorties/repartition_par_age_regroupe.png + repartition_55plus_regroupe.png")
