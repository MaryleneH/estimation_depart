# ==============================================================================
# 08_analyse_55plus_geo.R — Vulnérabilité RH par TERRITOIRE : les 55 ans et +
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` avec le contrat geo_code / geo_nom / geo_type
#             (scripts 01-04) ; AGE_SENIOR, SEUIL_DIFFUSION, GEO_ANALYSE,
#             GEO_INTERET, GEO_ZONAGES (00) ; fonctions 00c.
# PRODUIT   : objets `synthese_geo`, `criticite_geo_cs`, `tableau_55plus_geo` ;
#             fichiers SUFFIXÉS par le zonage (GEO_ZONAGES[[GEO_ANALYSE]]$suffixe),
#             ex. pour "ze" / "departement" :
#             sorties/analyse_55plus_par_<suffixe>.csv    (synthèse par territoire)
#             sorties/criticite_55plus_<suffixe>_cs.csv   (détail territoire x CS)
#             sorties/quadrant_55plus_<suffixe>.png       (matrice de vulnérabilité)
#             sorties/tableau_departs_55plus_<suffixe>.csv / .html
#             -> les résultats de plusieurs zonages coexistent dans sorties/.
# LECTURE   : deux dimensions par territoire — le STOCK (part des 55+ dans
#             l'effectif 43+ : le territoire a-t-il vieilli ?) et le FLUX (taux
#             de départ attendu des 55+ d'ici 2030). Le croisement des deux, en
#             quadrant autour des médianes, classe les territoires ; le détail
#             territoire x CS repère les cellules où une catégorie entière
#             s'éteint (risque de non-transmission des savoir-faire).
# SECRET    : les cellules territoire x CS sous SEUIL_DIFFUSION salariés sont
#             masquées dans l'export (convention statistique publique).
# GÉNÉRIQUE : ce script ne connaît AUCUN nom de zonage ni de colonne source.
#             Identifiant = geo_code ; affichage = geo_nom, jamais geo_code.
# ==============================================================================
if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
if (!all(c("geo_code", "geo_nom", "geo_type") %in% names(bts_projete)))
  stop("Contrat géographique (geo_code, geo_nom, geo_type) absent de bts_projete : ",
       "exécutez le R/01 à jour (voir bloc GEO_* de 00_config.R).")
if (!exists("filtrer_geo_interet")) stop("Exécutez d'abord R/00c_fonctions_geo.R (ou main.R).")

library(dplyr); library(ggplot2); library(scales)

ZON <- zonage_geo(GEO_ANALYSE)     # libelle / un / pluriel / suffixe du zonage
sortie <- function(gabarit) fichier_sortie_geo(gabarit, GEO_ANALYSE)

# --- A. Base seniors : 55+ localisés, décomposition par cause ----------------
# Les territoires inconnus (code NA : hors table de passage...) sont REGROUPÉS
# sous un code « inconnu », pas perdus.
base_geo <- bts_projete |>
  mutate(geo_code = ifelse(is.na(geo_code), "inconnu", as.character(geo_code)),
         geo_nom  = ifelse(geo_code == "inconnu",
                           paste(ZON$libelle, "inconnu(e)"), as.character(geo_nom)))

# Périmètre de restitution (GEO_INTERET, 00_config) : codes retenus. Rien ne
# sort en silence (décompte, codes absents signalés, arrêt si aucun match).
base_geo <- filtrer_geo_interet(base_geo, GEO_INTERET, prefixe = "08")

base_geo <- base_geo |>
  mutate(senior = age_2024 >= AGE_SENIOR,
         # parts espérées par cause chez p_central (mêmes maths que 06/06b :
         # risques concurrents, répartition additive à somme p_central)
         .brut_ret = p_cal_central * (1 - p_inval) * (1 - p_deces),
         .brut_inv = p_inval * (1 - p_cal_central) * (1 - p_deces),
         .brut_dec = p_deces * (1 - p_cal_central) * (1 - p_inval),
         .som      = pmax(.brut_ret + .brut_inv + .brut_dec, 1e-12),
         part_ret  = p_central * .brut_ret / .som,
         part_inv  = p_central * .brut_inv / .som,
         part_dec  = p_central * .brut_dec / .som)

# --- B. Synthèse par territoire : stock de seniors x flux de départs ---------
synthese_geo <- base_geo |>
  group_by(geo_code, geo_nom) |>
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
  # taux NA (et non NaN) pour un territoire SANS senior : le cas existe sur
  # données réelles et doit traverser proprement quadrant, tableaux et exports
  mutate(taux_depart_55plus_pct = ifelse(effectif_55plus > 0,
           100 * departs_55plus      / effectif_55plus, NA_real_),
         taux_55plus_bas_pct    = ifelse(effectif_55plus > 0,
           100 * departs_55plus_bas  / effectif_55plus, NA_real_),
         taux_55plus_haut_pct   = ifelse(effectif_55plus > 0,
           100 * departs_55plus_haut / effectif_55plus, NA_real_)) |>
  arrange(desc(part_55plus_pct))

# Médianes = frontières du quadrant (relatives au périmètre, pas de seuil
# absolu à justifier ; les territoires se comparent ENTRE EUX)
med_part <- median(synthese_geo$part_55plus_pct, na.rm = TRUE)
med_taux <- median(synthese_geo$taux_depart_55plus_pct, na.rm = TRUE)

cat(sprintf("\n--- Synthèse 55+ par %s (scénario central) ---\n", tolower(ZON$libelle)))
print(synthese_geo |>
        select(geo_code, geo_nom, effectif_43plus, effectif_55plus, part_55plus_pct,
               departs_55plus, taux_depart_55plus_pct) |>
        mutate(across(where(is.numeric), ~ round(.x, 1))) |>
        as.data.frame(), row.names = FALSE)
cat(sprintf("Médianes du quadrant : part 55+ = %.1f %% | taux de départ 55+ = %.1f %%\n",
            med_part, med_taux))

# --- C. Détail territoire x CS : où une catégorie entière s'éteint-elle ? ----
criticite_geo_cs <- base_geo |>
  group_by(geo_code, geo_nom, cs1) |>
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

cellules_critiques <- criticite_geo_cs |>
  filter(effectif_43plus >= SEUIL_DIFFUSION, perte_seniors_pct >= 25)
if (nrow(cellules_critiques) > 0) {
  cat(sprintf("\n--- Cellules %s x CS les plus exposées (>= 25 %% de l'effectif perdu via les 55+) ---\n",
              tolower(ZON$libelle)))
  print(cellules_critiques |>
          mutate(across(where(is.numeric), ~ round(.x, 1))) |>
          as.data.frame(), row.names = FALSE)
}

# --- D. Exports tableur FR, secret statistique appliqué ----------------------
write.csv2(synthese_geo |> mutate(across(where(is.numeric), ~ round(.x, 1))),
           sortie("analyse_55plus_par_%s.csv"), row.names = FALSE)
# Masquage : sous SEUIL_DIFFUSION salariés, les valeurs de la cellule sont
# retirées (NA) mais la LIGNE reste, flaguée — l'absence se voit, ne se devine pas.
criticite_diffusable <- criticite_geo_cs |>
  mutate(sous_seuil = effectif_43plus < SEUIL_DIFFUSION,
         across(c(effectif_55plus, part_55plus_pct, departs_55plus,
                  perte_seniors_pct),
                ~ ifelse(sous_seuil, NA_real_, round(.x, 1))),
         effectif_43plus = ifelse(sous_seuil, NA_integer_, effectif_43plus))
write.csv2(criticite_diffusable, sortie("criticite_55plus_%s_cs.csv"), row.names = FALSE)
n_masquees <- sum(criticite_diffusable$sous_seuil)
if (n_masquees > 0)
  message("08 : ", n_masquees, " cellule(s) ", tolower(ZON$libelle),
          " x CS masquée(s) (effectif < ", SEUIL_DIFFUSION, ").")

# --- E. Quadrant de vulnérabilité — restitution SOBRE (lecture d'état-major) --
# Contraintes : plusieurs dizaines de territoires, un décideur non statisticien,
# 30 secondes de lecture. Fond blanc, une seule couleur d'alerte, TOUS les
# territoires nommés (geo_nom), et chaque cadran affiche son COMPTE. Sans
# ggrepel (absent des postes de production) : placement déterministe maison.
NOIR   <- "#1A1A1A"; GRIS <- "#666666"; GRILLE <- "#E3E3E3"
MARINE <- "#3D6480"                      # territoires hors cadran critique
ALERTE <- "#A63D2F"                      # cadran critique uniquement

# Territoires TRAÇABLES : au moins un senior (taux défini). Un territoire sans
# aucun 55+ n'a pas de position sur le quadrant ; il reste dans les tableaux.
quadrant_geo <- synthese_geo |>
  filter(is.finite(taux_depart_55plus_pct)) |>
  mutate(critique = part_55plus_pct >= med_part & taux_depart_55plus_pct >= med_taux)
n_hors_quadrant <- nrow(synthese_geo) - nrow(quadrant_geo)
if (n_hors_quadrant > 0)
  message("08 : ", n_hors_quadrant, " ", tolower(ZON$libelle), "(s) sans salarié de ",
          AGE_SENIOR, " ans et + — hors quadrant, conservé(s) dans les tableaux.")

# Placement des étiquettes SANS ggrepel : algorithme glouton déterministe.
# Chaque étiquette (boîte approchée par nchar x hauteur de police, en unités
# de données) essaie une liste ORDONNÉE de positions autour de son point
# (dessous, dessus, côtés, diagonales, puis plus loin) et prend la première
# qui ne recouvre ni une étiquette déjà posée, ni un marqueur. R pur, aucune
# dépendance, résultat identique à chaque exécution.
# Les tailles de boîtes sont RELATIVES à l'étendue des axes (fractions de la
# plage x / y) : le placement s'adapte à tout zonage, quelle que soit la
# dispersion du nuage (ZE : ~34 x 16 points ; département : autre plage).
placer_etiquettes <- function(x, y, lab,
                              cw = 0.0062 * diff(range(x)), # largeur d'un caractère
                              ch = 0.030  * diff(range(y)), # hauteur d'une étiquette
                              rx = 0.0130 * diff(range(x)), # demi-largeur d'un point
                              ry = 0.0210 * diff(range(y))) { # demi-hauteur d'un point
  n <- length(x)
  demi_l <- nchar(lab) * cw / 2 + 0.15
  # positions candidates (facteurs appliqués à la demi-largeur / hauteur)
  cand <- rbind(
    c(0, -1), c(0, 1), c(0, -2), c(0, 2),          # dessous / dessus
    c(1, 0), c(-1, 0),                             # à droite / à gauche
    c(1, -1), c(-1, -1), c(1, 1), c(-1, 1),        # diagonales
    c(0, -3), c(0, 3))                             # en dernier recours
  # obstacles initiaux : TOUS les points (les étiquettes ne doivent recouvrir
  # ni une autre étiquette, ni un marqueur)
  boites <- rbind(cbind(x - rx, x + rx, y - ry, y + ry),
                  matrix(NA_real_, nrow = n, ncol = 4))
  lx <- numeric(n); ly <- numeric(n)
  sx <- rep(NA_real_, n); ex <- rep(NA_real_, n)
  for (i in order(x)) {
    for (k in seq_len(nrow(cand))) {
      dx <- cand[k, 1] * (demi_l[i] + rx + 0.2)
      dy <- if (cand[k, 1] == 0) cand[k, 2] * (ry + 0.62 * ch) else cand[k, 2] * 0.95 * ch
      bx <- c(x[i] + dx - demi_l[i], x[i] + dx + demi_l[i],
              y[i] + dy - ch / 2,    y[i] + dy + ch / 2)
      libre <- TRUE
      # garde : n'opposer que des boîtes complètes (une coordonnée manquante
      # rendrait la comparaison NA et stopperait le script)
      for (j in which(is.finite(boites[, 1]) & is.finite(boites[, 3]))) {
        if (bx[1] < boites[j, 2] && bx[2] > boites[j, 1] &&
            bx[3] < boites[j, 4] && bx[4] > boites[j, 3]) { libre <- FALSE; break }
      }
      if (libre || k == nrow(cand)) {
        boites[n + i, ] <- bx; lx[i] <- x[i] + dx; ly[i] <- y[i] + dy
        # étiquette déportée latéralement -> trait de rappel du point au bord
        # de l'étiquette (lève l'ambiguïté d'attribution dans les amas)
        if (cand[k, 1] != 0) {
          sx[i] <- x[i] + sign(dx) * rx
          ex[i] <- lx[i] - sign(dx) * (demi_l[i] + 0.05)
        }
        break
      }
    }
  }
  list(x = lx, y = ly, seg_x = sx, seg_xend = ex)
}

pos_lab <- placer_etiquettes(quadrant_geo$part_55plus_pct,
                             quadrant_geo$taux_depart_55plus_pct,
                             quadrant_geo$geo_nom)
quadrant_geo$etiquette_x <- pos_lab$x
quadrant_geo$etiquette_y <- pos_lab$y
quadrant_geo$seg_x       <- pos_lab$seg_x
quadrant_geo$seg_xend    <- pos_lab$seg_xend

# Compte de territoires par cadran, affiché sous l'intitulé de chaque coin
n_q <- quadrant_geo |>
  count(haut = taux_depart_55plus_pct >= med_taux, droite = part_55plus_pct >= med_part)
n_de <- function(h, d) { v <- n_q$n[n_q$haut == h & n_q$droite == d]
                         if (length(v) == 0) 0L else v }
# Intitulés FACTUELS : position par rapport aux médianes du périmètre, sans
# qualificatif (« vieilli », « critique »...) — le classement est relatif,
# pas un diagnostic absolu.
quadrants <- tibble::tribble(
  ~x,   ~y,   ~hjust, ~lab,
   Inf,  Inf, 1,      sprintf("Plus de seniors, départs plus élevés — %d %s",  n_de(TRUE,  TRUE),  ZON$pluriel),
  -Inf,  Inf, 0,      sprintf("Moins de seniors, départs plus élevés — %d %s", n_de(TRUE,  FALSE), ZON$pluriel),
   Inf, -Inf, 1,      sprintf("Plus de seniors, départs plus modérés — %d %s", n_de(FALSE, TRUE),  ZON$pluriel),
  -Inf, -Inf, 0,      sprintf("Moins de seniors, départs plus modérés — %d %s", n_de(FALSE, FALSE), ZON$pluriel)) |>
  mutate(vjust = ifelse(y > 0, 1.8, -1.2))

g <- quadrant_geo |>
  ggplot(aes(x = part_55plus_pct, y = taux_depart_55plus_pct)) +
  geom_hline(yintercept = med_taux, linetype = "42", color = GRIS, linewidth = 0.45) +
  geom_vline(xintercept = med_part, linetype = "42", color = GRIS, linewidth = 0.45) +
  geom_text(data = quadrants, aes(x = x, y = y, label = lab, hjust = hjust, vjust = vjust),
            size = 3.2, fontface = "bold",
            color = c(ALERTE, GRIS, GRIS, GRIS)) +
  geom_point(aes(size = effectif_43plus, color = critique), alpha = 0.85) +
  geom_segment(data = ~ filter(.x, !is.na(seg_x)),
               aes(x = seg_x, xend = seg_xend, yend = etiquette_y),
               color = "grey65", linewidth = 0.25) +
  geom_text(aes(x = etiquette_x, y = etiquette_y, label = geo_nom, color = critique),
            size = 2.7, fontface = "bold", show.legend = FALSE) +
  scale_size_area(max_size = 7, labels = label_number(big.mark = " ")) +
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
    title = sprintf("Départs des seniors d'ici 2030 : %d %s à suivre en priorité",
                    n_de(TRUE, TRUE), ZON$pluriel),
    subtitle = sprintf(paste0("Chaque point est %s du périmètre (%d %s%s). ",
                              "À droite : part des %d ans et + supérieure à la médiane.\n",
                              "En haut : taux de départ des %d+ supérieur à la médiane. ",
                              "En rouge : les deux à la fois."),
                       ZON$un, nrow(synthese_geo), ZON$pluriel,
                       if (n_hors_quadrant > 0)
                         sprintf(", dont %d sans aucun %d+, non tracé(s)",
                                 n_hors_quadrant, AGE_SENIOR) else "",
                       AGE_SENIOR, AGE_SENIOR),
    caption = sprintf(paste0("Lecture : le classement est RELATIF aux médianes du périmètre — dans tous les territoires, la plupart des %d+ de 2024 ",
                             "seront partis d'ici 2030 (taux de %.0f à %.0f %%).\n",
                             "Champ : salariés de 43 ans et + en 2024, périmètre BITD. Scénario central (δ = %.2f an). ",
                             "Sources : DREES, EACR invalidité, mortalité Insee — calculs propres · données : table test"),
                      AGE_SENIOR,
                      min(quadrant_geo$taux_depart_55plus_pct),
                      max(quadrant_geo$taux_depart_55plus_pct),
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

# canevas élargi : plusieurs dizaines d'étiquettes toutes affichées
ggsave(sortie("quadrant_55plus_%s.png"),
       g, width = 14, height = 9.5, dpi = 300, device = ragg::agg_png,
       background = "white")

# --- F. Tableau des départs des 55+ par territoire, avec ligne de total ------
# Champ STRICT : les AGE_SENIOR ans et + (le tableau ne contient qu'eux).
# Colonne géographique : code + libellé du zonage (GEO_ZONAGES).
tableau_55plus_geo <- synthese_geo |>
  arrange(desc(departs_55plus)) |>
  transmute(code            = geo_code,
            territoire      = geo_nom,
            effectif        = effectif_55plus,
            departs         = departs_55plus,
            dont_retraite   = dep_55_retraite,
            dont_invalidite = dep_55_invalidite,
            dont_deces      = dep_55_deces,
            taux_pct        = taux_depart_55plus_pct,
            bas             = departs_55plus_bas,
            haut            = departs_55plus_haut,
            est_total       = FALSE) |>
  bind_rows(
    synthese_geo |>
      summarise(code = "", territoire = "Ensemble du périmètre",
                effectif = sum(effectif_55plus),
                departs  = sum(departs_55plus),
                dont_retraite   = sum(dep_55_retraite),
                dont_invalidite = sum(dep_55_invalidite),
                dont_deces      = sum(dep_55_deces),
                bas  = sum(departs_55plus_bas),
                haut = sum(departs_55plus_haut),
                est_total = TRUE) |>
      mutate(taux_pct = 100 * departs / effectif))

# Export tableur FR — la colonne géographique porte le libellé du zonage
write.csv2(tableau_55plus_geo |>
             select(-est_total) |>
             mutate(across(where(is.numeric), ~ round(.x, 1))) |>
             rename(!!ZON$libelle := territoire),
           sortie("tableau_departs_55plus_%s.csv"), row.names = FALSE)

# Tableau MIS EN FORME sans dépendance : HTML écrit à la main (inline CSS,
# style sobre aligné sur le quadrant). S'ouvre dans un navigateur, s'imprime,
# se colle dans un document — sans gt ni aucun package supplémentaire.
# 0 chiffre après la virgule dans le tableau HTML (demande de restitution) ;
# le CSV, lui, garde une décimale pour les reprises de calcul.
# Une valeur indéfinie (taux d'un territoire sans senior) s'affiche « – ».
fmt0 <- function(x) ifelse(is.finite(x),
                           formatC(round(x), format = "d", big.mark = " "),
                           "–")
echap <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE)
                       x <- gsub("<", "&lt;",  x, fixed = TRUE)
                       gsub(">", "&gt;", x, fixed = TRUE) }
lignes_html <- with(tableau_55plus_geo, paste0(
  "      <tr", ifelse(est_total, " class=\"total\"", ""), ">",
  "<td>", echap(territoire), "</td>",
  "<td class=\"num\">", fmt0(effectif), "</td>",
  "<td class=\"num\">", fmt0(departs), "</td>",
  "<td class=\"num\">", fmt0(dont_retraite), "</td>",
  "<td class=\"num\">", fmt0(dont_invalidite), "</td>",
  "<td class=\"num\">", fmt0(dont_deces), "</td>",
  "<td class=\"num\">", fmt0(taux_pct), "</td>",
  "<td class=\"num\">", fmt0(bas), " – ", fmt0(haut), "</td></tr>"))
page_html <- c(
  "<!DOCTYPE html>",
  "<html lang=\"fr\"><head><meta charset=\"utf-8\">",
  sprintf("<title>Départs des %d ans et + par %s</title>", AGE_SENIOR, tolower(ZON$libelle)),
  "<style>",
  "  body { font-family: -apple-system, 'Segoe UI', Roboto, Arial, sans-serif;",
  "         color: #1a1a1a; margin: 2em; }",
  "  h1 { font-size: 1.15em; margin: 0 0 .2em; }",
  "  p.sous-titre { color: #666; font-size: .85em; margin: 0 0 1.2em; }",
  "  table { border-collapse: collapse; font-size: .85em; }",
  "  th { text-align: right; font-weight: 600; padding: .35em .7em;",
  "       border-bottom: 2px solid #3d6480; }",
  "  th:first-child { text-align: left; }",
  "  td { padding: .3em .7em; border-bottom: 1px solid #e3e3e3; }",
  "  td.num { text-align: right; font-variant-numeric: tabular-nums; }",
  "  tr:nth-child(even) td { background: #f7f9fa; }",
  "  tr.total td { font-weight: 700; background: #eef2f7;",
  "                border-top: 2px solid #3d6480; border-bottom: none; }",
  "  p.note { color: #666; font-size: .75em; max-width: 60em; }",
  "</style></head><body>",
  sprintf("<h1>Départs attendus d'ici 2030 des salariés de %d ans et +, par %s</h1>",
          AGE_SENIOR, tolower(ZON$libelle)),
  sprintf("<p class=\"sous-titre\">Champ : uniquement les %d ans et + en 2024 (%s salariés) — scénario central ; fourchette = δ réglementaire. Classement par départs attendus décroissants.</p>",
          AGE_SENIOR, fmt0(sum(synthese_geo$effectif_55plus))),
  "  <table>",
  sprintf(paste0("    <thead><tr><th>%s</th><th>Effectif %d+</th><th>Départs attendus</th>",
                 "<th>dont retraite / fin de carrière</th><th>dont invalidité</th>",
                 "<th>dont décès</th><th>Taux de départ (%%)</th><th>Fourchette</th></tr></thead>"),
          echap(ZON$libelle), AGE_SENIOR),
  "    <tbody>", lignes_html, "    </tbody>",
  "  </table>",
  "<p class=\"note\">Sources : DREES, EACR invalidité, mortalité Insee — calculs propres. Données individuelles : table test.</p>",
  "</body></html>")
writeLines(page_html, sortie("tableau_departs_55plus_%s.html"), useBytes = FALSE)

cat(sprintf("\n--- Tableau %d+ par %s (tête + total ; détail : CSV) ---\n",
            AGE_SENIOR, tolower(ZON$libelle)))
print(tableau_55plus_geo |>
        filter(row_number() <= 5 | est_total) |>
        select(-est_total) |>
        mutate(across(where(is.numeric), ~ round(.x, 1))) |>
        as.data.frame(), row.names = FALSE)

message("08 OK (", GEO_ANALYSE, ") -> ",
        paste(basename(c(sortie("analyse_55plus_par_%s.csv"),
                         sortie("criticite_55plus_%s_cs.csv"),
                         sortie("quadrant_55plus_%s.png"),
                         sortie("tableau_departs_55plus_%s.csv"),
                         sortie("tableau_departs_55plus_%s.html"))),
              collapse = ", "))
