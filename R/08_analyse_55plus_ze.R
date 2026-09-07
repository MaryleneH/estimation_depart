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

# --- E. Quadrant de vulnérabilité — restitution SOBRE (lecture d'état-major) --
# Contraintes : ~42 zones sur le vrai périmètre, un décideur non statisticien,
# 30 secondes de lecture. D'où : fond blanc, une seule couleur d'alerte, seules
# les zones NOTABLES sont nommées (cadran critique, plus gros effectifs,
# extrêmes), et chaque cadran affiche son COMPTE de zones — la répartition
# d'ensemble se lit sans lire les 42 noms.
NOIR   <- "#1A1A1A"; GRIS <- "#666666"; GRILLE <- "#E3E3E3"
MARINE <- "#3D6480"                      # zones hors cadran critique
ALERTE <- "#A63D2F"                      # cadran critique uniquement

synthese_ze <- synthese_ze |>
  mutate(critique = part_55plus_pct >= med_part & taux_depart_55plus_pct >= med_taux,
         # zones nommées sur le graphique : critiques, 6 plus gros effectifs,
         # et extrêmes des deux axes (bornes du nuage)
         notable = critique |
           rank(-effectif_43plus, ties.method = "first") <= 6 |
           part_55plus_pct == max(part_55plus_pct) |
           part_55plus_pct == min(part_55plus_pct) |
           taux_depart_55plus_pct == max(taux_depart_55plus_pct) |
           taux_depart_55plus_pct == min(taux_depart_55plus_pct))

# Compte de zones par cadran, affiché sous l'intitulé de chaque coin
n_q <- synthese_ze |>
  count(haut = taux_depart_55plus_pct >= med_taux, droite = part_55plus_pct >= med_part)
n_de <- function(h, d) { v <- n_q$n[n_q$haut == h & n_q$droite == d]
                         if (length(v) == 0) 0L else v }
quadrants <- tibble::tribble(
  ~x,   ~y,   ~hjust, ~lab,
   Inf,  Inf, 1,      sprintf("VIEILLIES, DÉPARTS IMMINENTS — %d zones", n_de(TRUE,  TRUE)),
  -Inf,  Inf, 0,      sprintf("JEUNES, DÉPARTS RAPIDES — %d zones",      n_de(TRUE,  FALSE)),
   Inf, -Inf, 1,      sprintf("VIEILLIES, DÉPARTS ÉTALÉS — %d zones",    n_de(FALSE, TRUE)),
  -Inf, -Inf, 0,      sprintf("PROFIL PRÉSERVÉ — %d zones",              n_de(FALSE, FALSE))) |>
  mutate(vjust = ifelse(y > 0, 1.8, -1.2))

g <- synthese_ze |>
  ggplot(aes(x = part_55plus_pct, y = taux_depart_55plus_pct)) +
  geom_hline(yintercept = med_taux, linetype = "42", color = GRIS, linewidth = 0.45) +
  geom_vline(xintercept = med_part, linetype = "42", color = GRIS, linewidth = 0.45) +
  geom_text(data = quadrants, aes(x = x, y = y, label = lab, hjust = hjust, vjust = vjust),
            size = 3.4, fontface = "bold",
            color = c(ALERTE, GRIS, GRIS, GRIS)) +
  geom_point(aes(size = effectif_43plus, color = critique), alpha = 0.85) +
  ggrepel::geom_text_repel(data = ~ filter(.x, notable),
                           aes(label = ze, color = critique), size = 3.2,
                           fontface = "bold", seed = GRAINE, point.padding = 4,
                           min.segment.length = 0.25, segment.color = "grey70",
                           segment.size = 0.3, max.overlaps = Inf,
                           show.legend = FALSE) +
  scale_size_area(max_size = 9, labels = label_number(big.mark = " ")) +
  scale_color_manual(values = c(`TRUE` = ALERTE, `FALSE` = MARINE), guide = "none") +
  # expansion large : les intitulés de cadrans vivent dans les coins, il leur
  # faut de l'air pour ne pas mordre sur les points extrêmes
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1),
                     expand = expansion(mult = 0.07)) +
  # accuracy 0.1 : la plage de y est étroite (les 55+ partent presque tous
  # d'ici 2030), un arrondi entier fausserait la lecture des écarts
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1),
                     expand = expansion(mult = 0.10)) +
  labs(
    title = sprintf("Seniors : %d zones d'emploi en situation critique d'ici 2030",
                    n_de(TRUE, TRUE)),
    subtitle = sprintf(paste0("Chaque point est une zone d'emploi du périmètre (%d zones). ",
                              "À droite : les plus vieillies (part des %d ans et +).\n",
                              "En haut : celles où les seniors partent le plus vite d'ici 2030. ",
                              "En rouge : les deux à la fois. Seules les zones notables sont nommées."),
                       nrow(synthese_ze), AGE_SENIOR),
    caption = sprintf(paste0("Champ : salariés de 43 ans et + en 2024, périmètre BITD. Lignes pointillées : médianes du périmètre. ",
                             "Scénario central (δ = %.2f an).\n",
                             "Sources : DREES, EACR invalidité, mortalité Insee — calculs propres · données : table test"),
                      delta_central),
    x = sprintf("Part des %d ans et + dans l'effectif 43+ (2024)", AGE_SENIOR),
    y = sprintf("Départs attendus des %d+ d'ici 2030", AGE_SENIOR),
    size = "Effectif 43+"
  ) +
  theme_minimal(base_size = 12.5) +
  theme(
    text            = element_text(color = NOIR),
    plot.title      = element_text(face = "bold", size = 17, hjust = 0,
                                   color = NOIR, margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 10, color = GRIS, lineheight = 1.15,
                                   margin = margin(b = 12)),
    plot.caption    = element_text(size = 7.5, color = GRIS, hjust = 0,
                                   margin = margin(t = 12)),
    axis.text       = element_text(size = 9, color = GRIS),
    axis.title      = element_text(size = 10, color = NOIR),
    legend.position = "top", legend.justification = "left",
    legend.text     = element_text(size = 9, color = GRIS),
    legend.title    = element_text(size = 9.5),
    legend.margin   = margin(b = 2),
    panel.grid.major = element_line(color = GRILLE, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(16, 20, 12, 16)
  )

ggsave(file.path(DIR_SORTIES, "quadrant_55plus_ze.png"),
       g, width = 11, height = 8, dpi = 300, device = ragg::agg_png,
       background = "white")
message("08 OK -> sorties/analyse_55plus_par_ze.csv, criticite_55plus_ze_cs.csv, quadrant_55plus_ze.png")
