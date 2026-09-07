# ==============================================================================
# 08_analyse_55plus_ze.R — Vulnérabilité RH des zones d'emploi : les 55 ans et +
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` avec colonne `ze` (scripts 01-04) ;
#             paramètres AGE_SENIOR, SEUIL_DIFFUSION (00_config.R)
# PRODUIT   : objets `synthese_ze`, `criticite_ze_cs` ;
#             sorties/analyse_55plus_par_ze.csv       (synthèse par ZE)
#             sorties/criticite_55plus_ze_cs.csv      (détail ZE x CS, masqué)
#             sorties/quadrant_55plus_ze.png          (matrice de vulnérabilité)
# LECTURE   : deux dimensions par zone d'emploi — le STOCK (part des 55+ dans
#             l'effectif 43+ : la zone a-t-elle vieilli ?) et le FLUX (taux de
#             départ attendu des 55+ d'ici 2030 : partent-ils vite ?). Le
#             croisement des deux, en quadrant autour des médianes, classe les
#             zones ; le détail ZE x CS repère les cellules où une catégorie
#             entière s'éteint (risque de non-transmission des savoir-faire).
# SECRET    : les cellules ZE x CS sous SEUIL_DIFFUSION salariés sont masquées
#             dans l'export (convention statistique publique) — le quadrant,
#             lui, n'agrège que des zones entières.
# ==============================================================================
if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
if (!"ze" %in% names(bts_projete))
  stop("Colonne 'ze' absente de bts_projete : exécutez le R/01 à jour ",
       "(zone d'emploi ajoutée au contrat de colonnes — voir 00_config.R).")

library(dplyr); library(ggplot2); library(scales)

# --- Typographie manuscrite (style Excalidraw), comme 06b --------------------
if (file.exists("R/00b_fonts.R")) source("R/00b_fonts.R") else {FONT_TITRE<-"sans";FONT_CORPS<-"sans"}

# --- A. Base seniors : 55+ localisés, décomposition par cause ----------------
# Les NA de zone (communes hors table de passage) sont REGROUPÉS, pas perdus.
base_ze <- bts_projete |>
  mutate(ze = ifelse(is.na(ze), "ZE inconnue", as.character(ze)),
         senior = age_2024 >= AGE_SENIOR,
         # parts espérées par cause chez p_central (mêmes maths que 06/06b :
         # risques concurrents, répartition additive à somme p_central)
         .brut_ret = p_cal_central * (1 - p_inval) * (1 - p_deces),
         .brut_inv = p_inval * (1 - p_cal_central) * (1 - p_deces),
         .brut_dec = p_deces * (1 - p_cal_central) * (1 - p_inval),
         .som      = pmax(.brut_ret + .brut_inv + .brut_dec, 1e-12),
         part_ret  = p_central * .brut_ret / .som,
         part_inv  = p_central * .brut_inv / .som,
         part_dec  = p_central * .brut_dec / .som)

# --- B. Synthèse par zone d'emploi : stock de seniors x flux de départs ------
synthese_ze <- base_ze |>
  group_by(ze) |>
  summarise(
    effectif_43plus     = n(),
    effectif_55plus     = sum(senior),
    part_55plus_pct     = 100 * mean(senior),
    departs_55plus      = sum(p_central[senior]),
    departs_55plus_bas  = sum(p_bas[senior]),
    departs_55plus_haut = sum(p_haut[senior]),
    dep_55_retraite     = sum(part_ret[senior]),
    dep_55_invalidite   = sum(part_inv[senior]),
    dep_55_deces        = sum(part_dec[senior]),
    .groups = "drop") |>
  mutate(taux_depart_55plus_pct = 100 * departs_55plus      / effectif_55plus,
         taux_55plus_bas_pct    = 100 * departs_55plus_bas  / effectif_55plus,
         taux_55plus_haut_pct   = 100 * departs_55plus_haut / effectif_55plus) |>
  arrange(desc(part_55plus_pct))

# Médianes = frontières du quadrant (relatives au périmètre, pas de seuil
# absolu à justifier ; les zones se comparent ENTRE ELLES)
med_part <- median(synthese_ze$part_55plus_pct)
med_taux <- median(synthese_ze$taux_depart_55plus_pct)

cat("\n--- Synthèse 55+ par zone d'emploi (scénario central) ---\n")
print(synthese_ze |>
        select(ze, effectif_43plus, effectif_55plus, part_55plus_pct,
               departs_55plus, taux_depart_55plus_pct) |>
        mutate(across(where(is.numeric), ~ round(.x, 1))) |>
        as.data.frame(), row.names = FALSE)
cat(sprintf("Médianes du quadrant : part 55+ = %.1f %% | taux de départ 55+ = %.1f %%\n",
            med_part, med_taux))

# --- C. Détail ZE x CS : où une catégorie entière s'éteint-elle ? ------------
criticite_ze_cs <- base_ze |>
  group_by(ze, cs1) |>
  summarise(
    effectif_43plus    = n(),
    effectif_55plus    = sum(senior),
    part_55plus_pct    = 100 * mean(senior),
    departs_55plus     = sum(p_central[senior]),
    # criticité = part de l'effectif TOTAL (43+) de la cellule que représentent
    # les départs de seniors : ce que la cellule perd d'ici 2030 par sa tête
    perte_seniors_pct  = 100 * sum(p_central[senior]) / n(),
    .groups = "drop") |>
  arrange(desc(perte_seniors_pct))

cellules_critiques <- criticite_ze_cs |>
  filter(effectif_43plus >= SEUIL_DIFFUSION, perte_seniors_pct >= 25)
if (nrow(cellules_critiques) > 0) {
  cat("\n--- Cellules ZE x CS critiques (>= 25 % de l'effectif perdu via les 55+) ---\n")
  print(cellules_critiques |>
          mutate(across(where(is.numeric), ~ round(.x, 1))) |>
          as.data.frame(), row.names = FALSE)
}

# --- D. Exports tableur FR, secret statistique appliqué ----------------------
write.csv2(synthese_ze |> mutate(across(where(is.numeric), ~ round(.x, 1))),
           file.path(DIR_SORTIES, "analyse_55plus_par_ze.csv"),
           row.names = FALSE)
# Masquage : sous SEUIL_DIFFUSION salariés, les valeurs de la cellule sont
# retirées (NA) mais la LIGNE reste, flaguée — l'absence se voit, ne se devine pas.
criticite_diffusable <- criticite_ze_cs |>
  mutate(sous_seuil = effectif_43plus < SEUIL_DIFFUSION,
         across(c(effectif_55plus, part_55plus_pct, departs_55plus,
                  perte_seniors_pct),
                ~ ifelse(sous_seuil, NA_real_, round(.x, 1))),
         effectif_43plus = ifelse(sous_seuil, NA_integer_, effectif_43plus))
write.csv2(criticite_diffusable,
           file.path(DIR_SORTIES, "criticite_55plus_ze_cs.csv"),
           row.names = FALSE)
n_masquees <- sum(criticite_diffusable$sous_seuil)
if (n_masquees > 0)
  message("08 : ", n_masquees, " cellule(s) ZE x CS masquée(s) (effectif < ",
          SEUIL_DIFFUSION, ").")

# --- E. Quadrant de vulnérabilité — palette « atelier » de 06b ---------------
CREME  <- "#FBF7EF"; ENCRE <- "#2B2622"; ENCRE2 <- "#6E655C"
BLEU   <- "#3E6E8E"                      # bleu ardoise (points)
ROUGE  <- "#B5583C"                      # terracotta : cadran critique
quadrants <- tibble::tribble(
  ~x,    ~y,    ~hjust, ~vjust, ~lab,
   Inf,   Inf,  1.05,   1.6,   "Vieillies, départs imminents",
  -Inf,   Inf,  -0.05,  1.6,   "Jeunes, mais seniors sur le départ",
   Inf,  -Inf,  1.05,  -0.9,   "Vieillies, départs étalés",
  -Inf,  -Inf,  -0.05, -0.9,   "Zones préservées")

g <- synthese_ze |>
  mutate(critique = part_55plus_pct >= med_part &
                    taux_depart_55plus_pct >= med_taux) |>
  ggplot(aes(x = part_55plus_pct, y = taux_depart_55plus_pct)) +
  geom_hline(yintercept = med_taux, linetype = "22", color = ENCRE2, linewidth = 0.5) +
  geom_vline(xintercept = med_part, linetype = "22", color = ENCRE2, linewidth = 0.5) +
  geom_text(data = quadrants, aes(x = x, y = y, label = lab, hjust = hjust, vjust = vjust),
            family = FONT_TITRE, size = 4.6, color = ENCRE2, alpha = 0.85) +
  geom_errorbar(aes(ymin = taux_55plus_bas_pct, ymax = taux_55plus_haut_pct),
                width = 0, linewidth = 0.5, color = "grey55", alpha = 0.8) +
  geom_point(aes(size = effectif_43plus, fill = critique),
             shape = 21, color = CREME, stroke = 1.1, alpha = 0.92) +
  ggrepel::geom_text_repel(aes(label = ze), family = FONT_CORPS, size = 3.6,
                           color = ENCRE, seed = GRAINE, point.padding = 6,
                           min.segment.length = 0.3, segment.color = ENCRE2) +
  scale_size_area(max_size = 16, labels = label_number(big.mark = " ")) +
  scale_fill_manual(values = c(`TRUE` = ROUGE, `FALSE` = BLEU), guide = "none") +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  # accuracy 0.1 : la plage de y est étroite (les 55+ partent presque tous
  # d'ici 2030), un arrondi entier fausserait la lecture des écarts
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(
    title = "Quelles zones d'emploi vont perdre leurs seniors ?",
    subtitle = sprintf(paste0("Salariés de %d ans et + : poids dans l'effectif (x) et taux de départ d'ici 2030 (y) — ",
                              "scénario central, barres = fourchette réglementaire δ"), AGE_SENIOR),
    caption = paste0("Lignes pointillées : médianes du périmètre. Taille des points : effectif 43 ans et +.\n",
                     "Sources : DREES, EACR invalidité, mortalité Insee — calculs propres · données : table test"),
    x = sprintf("Part des %d ans et + dans l'effectif 43+ (2024)", AGE_SENIOR),
    y = sprintf("Départs attendus des %d+ d'ici 2030", AGE_SENIOR),
    size = "Effectif 43+"
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
    axis.text       = element_text(family = FONT_CORPS, size = 9.5, color = ENCRE2),
    axis.title      = element_text(family = FONT_CORPS, size = 10.5, color = ENCRE2),
    legend.position = "top", legend.justification = "left",
    legend.text     = element_text(family = FONT_CORPS, size = 9.5),
    legend.title    = element_text(family = FONT_CORPS, size = 10),
    panel.grid.major = element_line(color = "#E8E0D4", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = CREME, color = NA),
    panel.background = element_rect(fill = CREME, color = NA),
    plot.margin     = margin(18, 22, 12, 18)
  )

ggsave(file.path(DIR_SORTIES, "quadrant_55plus_ze.png"),
       g, width = 10.5, height = 8, dpi = 300, device = ragg::agg_png,
       background = CREME)
message("08 OK -> sorties/analyse_55plus_par_ze.csv, criticite_55plus_ze_cs.csv, quadrant_55plus_ze.png")
