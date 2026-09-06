# ==============================================================================
# 07_tableau_contribution.R — Tableau de contribution mis en forme (gt)
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (script 04) ; DIR_SORTIES (00)
# PRODUIT   : objet `gt_contribution` ;
#             sorties/tableau_contribution.html (toujours) ;
#             sorties/tableau_contribution.png  (si webshot2 installé)
# Rôle      : présenter la décomposition en « pelures » (législation seule ->
#             + sas -> + invalidité/décès -> total) en un tableau de comité.
# Dépendance: package gt (install.packages("gt") sur votre poste).
# ==============================================================================
if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
if (!requireNamespace("gt", quietly = TRUE))
  stop("Le package 'gt' est requis : install.packages(\"gt\").")

library(dplyr)
library(gt)

# --- Données : les 4 postes de la décomposition (valeurs centrales) -----------
tot_legis <- sum(bts_projete$pA_central)         # 1. législation seule
tot_sas   <- sum(bts_projete$pB_cal_central)     # 2. + sas (age_conj -> μ)
tot_B     <- sum(bts_projete$pB_central)         # 3. + invalidité & décès = total
effectif  <- nrow(bts_projete)

tab <- tibble::tibble(
  poste = c("1. Législation seule (âge légal)",
            "2. + Apport du sas de fin de carrière",
            "3. + Apport invalidité et décès",
            "Total (scénario B)"),
  departs = round(c(tot_legis,
                    tot_sas - tot_legis,
                    tot_B   - tot_sas,
                    tot_B)),
  lecture = c("Effet pur de l'âge légal de liquidation",
              "Cessations d'emploi avant liquidation",
              "Accidents de la vie (invalidité, décès)",
              "Ensemble des sorties définitives d'ici 2030"),
  est_total = c(FALSE, FALSE, FALSE, TRUE)
) |>
  mutate(part = departs / tot_B)

# --- Mise en forme gt ---------------------------------------------------------
gt_contribution <- tab |>
  gt() |>
  cols_hide(est_total) |>
  tab_header(
    title    = md("**Départs de salariés d'ici 2030 : décomposition par cause**"),
    subtitle = md(sprintf("Salariés de 43 ans et + en 2024 — scénario central — %s salariés",
                          format(effectif, big.mark = "\u00a0")))
  ) |>
  cols_label(poste = "Contribution",
             departs = "Départs",
             lecture = "Lecture",
             part = "Part du total") |>
  fmt_number(columns = departs, decimals = 0, sep_mark = "\u00a0") |>
  fmt_percent(columns = part, decimals = 0) |>
  # signe + devant les apports (lignes 2 et 3)
  text_transform(
    locations = cells_body(columns = departs, rows = 2:3),
    fn = function(x) paste0("+", x)
  ) |>
  cols_align(align = "left",  columns = c(poste, lecture)) |>
  cols_align(align = "right", columns = c(departs, part)) |>
  # ligne de total : gras + filet supérieur
  tab_style(
    style = list(cell_text(weight = "bold"),
                 cell_fill(color = "#eef2f7")),
    locations = cells_body(rows = est_total)
  ) |>
  tab_style(
    style = cell_borders(sides = "top", weight = px(2), color = "#2f6da4"),
    locations = cells_body(rows = est_total)
  ) |>
  # barre de fond proportionnelle sur la colonne part (lisibilité visuelle)
  data_color(columns = part, palette = c("#eaf1f8", "#2f6da4"),
             domain = c(0, 1)) |>
  tab_source_note(md(
    "Sources : DREES, jeu *departretraite_parcsp* (Insee, enquête Emploi) ; quotients de mortalité Insee — calculs propres. Données individuelles : table test."
  )) |>
  tab_options(table.font.size = px(14),
              heading.title.font.size = px(16),
              column_labels.font.weight = "bold",
              table.border.top.style = "none")

# --- Exports ------------------------------------------------------------------
gtsave(gt_contribution, file.path(DIR_SORTIES, "tableau_contribution.html"))
if (requireNamespace("webshot2", quietly = TRUE)) {
  gtsave(gt_contribution, file.path(DIR_SORTIES, "tableau_contribution.png"),
         vwidth = 900, vheight = 400)
  message("07 OK -> tableau_contribution.html + .png")
} else {
  message("07 OK -> tableau_contribution.html ",
          "(installez webshot2 pour l'export PNG : install.packages(\"webshot2\"))")
}
