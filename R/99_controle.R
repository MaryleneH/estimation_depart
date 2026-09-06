# ==============================================================================
# 99_controle.R — Tableaux de CONTRÔLE (diagnostic, non publiés par défaut)
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts` (01/01b) et, pour le volet C, `bts_projete` (04).
#             Paramètres de restitution (00 : BREAKS_TRANCHES, LABELS_TRANCHES,
#             ANNEE_REF_GRAPHIQUE).
# PRODUIT   : impressions console + CSV dans sorties/ :
#             A. Répartition observée par tranche d'âge des 43+ (BTS 2024)
#             B. Âge maximum observé par CS (fins de carrière réelles)
#             C. Antisèche des taux projetés (μ, sortie d'emploi, causes) par
#                CS × tranche — pour comprendre/défendre le graphique 06/06b.
# Rôle : ce script n'entre PAS dans la production des graphiques ni des
#        décomptes ; c'est un outil de compréhension et de validation.
#        À lancer à la demande (pas nécessairement dans main.R).
# ==============================================================================
if (!exists("bts")) stop("Objet 'bts' introuvable : exécutez R/01 (+01b) ou main.R.")

library(dplyr)
library(tidyr)

# Tranches d'affichage cohérentes avec le graphique (âge en ANNEE_REF_GRAPHIQUE)
decalage <- ANNEE_REF_GRAPHIQUE - 2024
bts_tr <- bts |>
  mutate(age_affiche = age_2024 + decalage,
         tranche = cut(age_affiche, breaks = BREAKS_TRANCHES, labels = LABELS_TRANCHES))

# ---- A. Répartition OBSERVÉE par tranche d'âge des 43+ (photo BTS 2024) ------
cat("\n=== A. Répartition observée des 43+ par tranche d'âge (BTS", 2024, ") ===\n")
repartition_age <- bts_tr |>
  count(tranche, name = "effectif") |>
  mutate(part_pct = round(100 * effectif / sum(effectif), 1))
print(as.data.frame(repartition_age), row.names = FALSE)

# Variante croisée CS × tranche (structure d'âge de chaque CS)
cat("\n--- A bis. Effectifs par CS × tranche d'âge ---\n")
repartition_cs_age <- bts_tr |>
  count(cs1, tranche, name = "effectif") |>
  pivot_wider(names_from = tranche, values_from = effectif, values_fill = 0)
print(as.data.frame(repartition_cs_age), row.names = FALSE)

# ---- B. Âge MAXIMUM observé par CS (fins de carrière réelles) ----------------
# Lecture : l'âge du salarié le plus âgé encore en emploi dans chaque CS en 2024.
# Un âge max élevé chez les cadres (vs ouvriers) corrobore un μ plus tardif.
cat("\n=== B. Âge maximum et profil de fin de carrière observés par CS (2024) ===\n")
age_max_cs <- bts |>
  group_by(cs1) |>
  summarise(effectif = n(),
            age_median = median(age_2024),
            age_moyen  = round(mean(age_2024), 1),
            age_p95    = quantile(age_2024, 0.95),   # 95e centile : "queue" haute
            age_max    = max(age_2024),
            part_60plus_pct = round(100 * mean(age_2024 >= 60), 1),
            .groups = "drop") |>
  arrange(desc(age_max))
print(as.data.frame(age_max_cs), row.names = FALSE)

# ---- C. Antisèche des taux PROJETÉS par CS × tranche (si bts_projete dispo) --
if (exists("bts_projete")) {
  cat("\n=== C. Antisèche : μ et devenir projeté 2030 par CS × tranche ===\n")
  # μ corrigé par CS (l'âge de sortie d'emploi qui pilote tout)
  cat("\n--- C1. Âge de sortie d'emploi (μ corrigé) par CS ---\n")
  print(as.data.frame(bts_projete |> group_by(cs1) |>
    summarise(mu_corrige = round(mean(mu_corrige), 1),
              .groups = "drop")), row.names = FALSE)
  # devenir moyen par CS × tranche : sortie d'emploi vs maintien, + causes
  cat("\n--- C2. Devenir projeté (%, moyennes) par CS × tranche ---\n")
  antiseche <- bts_projete |>
    mutate(age_affiche = age_2024 + decalage,
           tranche = cut(age_affiche, breaks = BREAKS_TRANCHES, labels = LABELS_TRANCHES)) |>
    group_by(cs1, tranche) |>
    summarise(sortie_emploi = round(100 * mean(p_central), 1),
              maintien      = round(100 * mean(1 - p_central), 1),
              dont_invalidite = round(100 * mean(p_inval * (1 - p_cal_central)), 1),
              dont_deces      = round(100 * mean(p_deces * (1 - p_cal_central)), 1),
              .groups = "drop")
  print(as.data.frame(antiseche), row.names = FALSE)
  write.csv2(antiseche, file.path(DIR_SORTIES, "controle_antiseche_cs_tranche.csv"),
             row.names = FALSE)
} else {
  cat("\n(C. antisèche des taux projetés : lancez R/04 puis relancez pour l'obtenir)\n")
}

# ---- Exports des volets observés --------------------------------------------
write.csv2(repartition_age, file.path(DIR_SORTIES, "controle_repartition_age.csv"), row.names = FALSE)
write.csv2(age_max_cs,      file.path(DIR_SORTIES, "controle_age_max_par_cs.csv"),  row.names = FALSE)
# ==============================================================================
# DATAVIZ DIAGNOSTIQUES (3 graphiques de compréhension, style "atelier")
# ==============================================================================
suppressWarnings(suppressMessages({library(ggplot2); library(scales)}))
if (file.exists("R/00b_fonts.R")) source("R/00b_fonts.R") else {FONT_TITRE<-"sans";FONT_CORPS<-"sans"}
FONT_T <- FONT_TITRE; FONT_C <- FONT_CORPS
CREME <- "#FBF7EF"; ENCRE <- "#2B2622"; ENCRE2 <- "#6E655C"
PAL_CS <- c("Cadres" = "#3E6E8E", "Prof. intermediaires" = "#8FB996",
            "Employes" = "#E0A458", "Ouvriers" = "#8E7196")
dev_ok <- requireNamespace("ragg", quietly = TRUE)
save_g <- function(g, f, w, h) {
  if (dev_ok) ggsave(file.path(DIR_SORTIES, f), g, width = w, height = h,
                     dpi = 300, device = ragg::agg_png, background = CREME)
  else ggsave(file.path(DIR_SORTIES, f), g, width = w, height = h, dpi = 300)
}
theme_atelier <- function() theme_minimal(base_size = 13) + theme(
  text = element_text(family = FONT_C, color = ENCRE),
  plot.title = element_text(family = FONT_T, size = 24, hjust = 0, color = ENCRE),
  plot.subtitle = element_text(family = FONT_C, size = 10.5, color = ENCRE2, margin = margin(b = 10)),
  plot.caption = element_text(family = FONT_C, size = 7.5, color = ENCRE2, hjust = 0, margin = margin(t = 10)),
  axis.text = element_text(family = FONT_C, size = 9.5, color = ENCRE2),
  axis.title = element_text(family = FONT_C, size = 10, color = ENCRE2),
  legend.text = element_text(family = FONT_C, size = 9.5),
  legend.title = element_text(family = FONT_C, size = 10),
  strip.text = element_text(family = FONT_T, size = 16, color = ENCRE),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = CREME, color = NA),
  panel.background = element_rect(fill = CREME, color = NA),
  plot.margin = margin(16, 18, 12, 16))

# ---- Viz A : pyramide des âges observée par CS (barres horizontales) ---------
gA <- bts_tr |> count(cs1, tranche) |> group_by(cs1) |>
  mutate(part = n / sum(n)) |> ungroup() |>
  mutate(cs1 = factor(cs1, levels = c("Cadres","Prof. intermediaires","Employes","Ouvriers"))) |>
  ggplot(aes(x = part, y = tranche, fill = cs1)) +
  geom_col(width = 0.72, color = CREME, linewidth = 0.5) +
  facet_wrap(~ cs1, nrow = 1) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .05))) +
  scale_fill_manual(values = PAL_CS, guide = "none") +
  labs(title = "Structure d'âge observée des 43 ans et +",
       subtitle = paste0("Répartition par tranche au sein de chaque CS (BTS ", 2024, ")"),
       caption = "Source : BTS 2024 — données : table test", x = NULL, y = NULL) +
  theme_atelier()
save_g(gA, "controle_viz_pyramide_ages.png", 11, 4.6)

# ---- Viz B : âges de fin de carrière par CS (dot plot médian / p95 / max) ----
gB <- age_max_cs |>
  select(cs1, age_median, age_p95, age_max) |>
  tidyr::pivot_longer(-cs1, names_to = "indic", values_to = "age") |>
  mutate(indic = recode(indic, age_median = "Médian", age_p95 = "95e centile", age_max = "Maximum"),
         indic = factor(indic, levels = c("Médian","95e centile","Maximum")),
         cs1 = factor(cs1, levels = rev(c("Cadres","Prof. intermediaires","Employes","Ouvriers")))) |>
  ggplot(aes(x = age, y = cs1)) +
  geom_line(aes(group = cs1), color = "#CBB99C", linewidth = 1.2) +
  geom_point(aes(color = indic), size = 4.5) +
  scale_color_manual(values = c("Médian" = "#8FB996", "95e centile" = "#E0A458", "Maximum" = "#8E7196")) +
  labs(title = "Jusqu'à quel âge travaille-t-on, par CS ?",
       subtitle = "Âge observé des salariés en poste en 2024 : médian, 95e centile, maximum",
       caption = "Un maximum élevé et une queue haute (95e centile) signalent des fins de carrière tardives.
Source : BTS 2024 — données : table test",
       x = "Âge en 2024", y = NULL, color = NULL) +
  theme_atelier() + theme(legend.position = "top", legend.justification = "left")
save_g(gB, "controle_viz_age_fin_carriere.png", 9.5, 4.4)

# ---- Viz C : heatmap du taux de sortie d'emploi projeté (CS × tranche) -------
if (exists("bts_projete")) {
  hc <- bts_projete |>
    mutate(age_affiche = age_2024 + decalage,
           tranche = cut(age_affiche, breaks = BREAKS_TRANCHES, labels = LABELS_TRANCHES)) |>
    group_by(cs1, tranche) |>
    summarise(sortie = 100 * mean(p_central), .groups = "drop") |>
    mutate(cs1 = factor(cs1, levels = rev(c("Cadres","Prof. intermediaires","Employes","Ouvriers"))))
  gC <- hc |>
    ggplot(aes(x = tranche, y = cs1, fill = sortie)) +
    geom_tile(color = CREME, linewidth = 2) +
    geom_text(aes(label = paste0(round(sortie), "%")), family = FONT_C, size = 3.6,
              color = ifelse(hc$sortie > 55, "white", ENCRE)) +
    scale_fill_gradient(low = "#EDE6D6", high = "#3E6E8E", labels = percent_format(scale = 1)) +
    labs(title = "Taux de sortie d'emploi projeté d'ici 2030",
         subtitle = "Part des salariés ayant quitté l'emploi, par CS et tranche d'âge — scénario central",
         caption = "Lecture du gradient : plus foncé = sortie plus fréquente. Source : projection — données : table test",
         x = paste0("Âge en ", ANNEE_REF_GRAPHIQUE), y = NULL, fill = "Sortie") +
    theme_atelier() + theme(panel.grid = element_blank())
  save_g(gC, "controle_viz_heatmap_sortie.png", 9.5, 4.2)
}

message("99 OK -> tableaux de contrôle imprimés + CSV dans ", DIR_SORTIES, "/")
