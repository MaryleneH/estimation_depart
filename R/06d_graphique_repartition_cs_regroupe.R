# ==============================================================================
# 06d_graphique_repartition_cs_regroupe.R — Variante du 06b : invalidité et
#                                           décès REGROUPÉS en une seule part
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (04) ; paramètres de restitution (00)
# PRODUIT   : objet `repartition_cs_regroupe` ;
#             sorties/repartition_par_cs_regroupe.png
# LECTURE   : mêmes petits multiples que le 06b (une facette par CS, barres à
#             100 % des parts espérées), mais en TROIS postes seulement :
#             maintien / sortie retraite-sas / invalidité ou décès. Version de
#             présentation : moins de couleurs, message identique.
# POURQUOI LA NOTE DE LECTURE : les trois devenirs sont des risques
#             CONCURRENTS — chaque salarié n'est compté que dans le PREMIER
#             état qui le fait sortir de l'emploi. Passé ~61 ans, la sortie
#             retraite est quasi certaine et absorbe presque tout : la part
#             « invalidité ou décès » tend vers zéro NON parce que ces
#             événements cessent d'exister, mais parce qu'ils surviendraient
#             pour l'essentiel APRÈS la sortie de l'emploi, hors du champ du
#             modèle (qui compte des sorties d'emploi, pas des décès dans la
#             population générale). D'où la note portée sur le graphique.
# ==============================================================================
if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
library(dplyr); library(tidyr); library(ggplot2); library(scales)

# --- Typographie manuscrite (style Excalidraw) via ragg, comme 06b -----------
if (file.exists("R/00b_fonts.R")) source("R/00b_fonts.R") else {FONT_TITRE<-"sans";FONT_CORPS<-"sans"}

# --- Données : mêmes maths de risques concurrents que le 06b, puis on SOMME
#     les deux causes « événements de vie » en un seul poste ------------------
decalage <- ANNEE_REF_GRAPHIQUE - 2024
niveaux <- c("Maintien en emploi", "Sortie de l'emploi (retraite ou sas)",
             "Sortie invalidité ou décès")

repartition_cs_regroupe <- bts_projete |>
  mutate(age_affiche = age_2024 + decalage,
         tranche = cut(age_affiche, breaks = BREAKS_TRANCHES, labels = LABELS_TRANCHES),
         `Maintien en emploi` = 1 - p_central,
         # répartition additive de p_central entre les 3 causes (somme == p_central)
         .brut_ret = p_cal_central * (1 - p_inval) * (1 - p_deces),
         .brut_inv = p_inval * (1 - p_cal_central) * (1 - p_deces),
         .brut_dec = p_deces * (1 - p_cal_central) * (1 - p_inval),
         .som = pmax(.brut_ret + .brut_inv + .brut_dec, 1e-12),
         `Sortie de l'emploi (retraite ou sas)` = p_central * .brut_ret / .som,
         # REGROUPEMENT : invalidité + décès en un seul poste
         `Sortie invalidité ou décès` = p_central * (.brut_inv + .brut_dec) / .som) |>
  group_by(cs1, tranche) |>
  summarise(across(all_of(niveaux), mean), .groups = "drop") |>
  pivot_longer(-c(cs1, tranche), names_to = "classe", values_to = "part") |>
  mutate(classe = factor(classe, levels = niveaux),
         cs1 = factor(cs1, levels = c("Cadres", "Prof. intermediaires",
                                      "Employes", "Ouvriers")))

# --- Palette « atelier » (mêmes teintes que 06b, l'ambre porte le regroupé) --
couleurs <- c("Sortie de l'emploi (retraite ou sas)" = "#3E6E8E",  # bleu ardoise
              "Sortie invalidité ou décès"           = "#E0A458",  # ambre doux
              "Maintien en emploi"                   = "#8FB996")  # sauge

CREME   <- "#FBF7EF"   # fond papier
ENCRE   <- "#2B2622"   # texte principal
ENCRE2  <- "#6E655C"   # texte secondaire

note_lecture <- paste(
  "Note de lecture — un seul devenir par salarié : les trois états sont des risques concurrents, chacun n'est compté que dans le PREMIER qui le fait sortir de l'emploi.",
  "Passé 61 ans, le départ en retraite est quasi certain et absorbe les autres causes : la part « invalidité ou décès » devient invisible non parce que ces événements",
  "cessent d'exister, mais parce qu'ils surviendraient pour l'essentiel après la sortie de l'emploi — hors du champ du modèle, qui compte des sorties d'emploi, pas des décès.",
  sep = "\n")

g <- repartition_cs_regroupe |>
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
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  labs(
    title = "Qui sera encore là en 2030 ?",
    subtitle = sprintf("Devenir des salariés de 43 ans et + selon la catégorie sociale — scénario central (δ = %.2f an) ; invalidité et décès regroupés",
                       delta_central),
    caption = paste0(note_lecture, "\n",
                     "Sources : DREES (departretraite_parcsp, Insee enquête Emploi), EACR invalidité, mortalité Insee — calculs propres · données : table test"),
    x = sprintf("Âge en %d", ANNEE_REF_GRAPHIQUE), y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text            = element_text(family = FONT_CORPS, color = ENCRE),
    plot.title      = element_text(family = FONT_TITRE, size = 30, hjust = 0,
                                   color = ENCRE, margin = margin(b = 2)),
    plot.subtitle   = element_text(family = FONT_CORPS, size = 11, color = ENCRE2,
                                   margin = margin(b = 14)),
    # la note de lecture EST le message : corps plus grand qu'une caption
    # ordinaire (9.5), encre pleine, pour qu'elle soit lue, pas décorative
    plot.caption    = element_text(family = FONT_CORPS, size = 9.5, color = ENCRE,
                                   hjust = 0, lineheight = 1.25,
                                   margin = margin(t = 14)),
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

ggsave(file.path(DIR_SORTIES, "repartition_par_cs_regroupe.png"),
       g, width = 11, height = 6.1, dpi = 300, device = ragg::agg_png,
       background = CREME)
message("06d OK -> sorties/repartition_par_cs_regroupe.png")
