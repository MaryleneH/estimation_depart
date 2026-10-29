# ==============================================================================
# 09c_cartes_typologie_note.R — Deux cartes pour la note sur la typologie (specs/11, SPEC-ANA-009)
# ------------------------------------------------------------------------------
# ENTRÉES   : la typologie de DIFFUSION (objet typologie_departements$diffusion, ou le CSV
#             sorties/typologie_territoriale/diffusion/typologie_departements.csv) et le fond local
#             (charger_fond_carte, 00f). Relançable seul après la chaîne.
# PRODUIT   : sorties/analyse_hypotheses_depart/cartes/
#               carte_profils_note.png     les sept profils, un département = une couleur ; légende avec
#                                          le nombre de départements ; non diffusé en blanc cerné de gris
#               carte_intensite_note.png   l'intensité du renouvellement en trois classes ; les pôles majeurs
#                                          à renouveler rapidement sont cernés de rouge
# DESIGN    : épuré : fond blanc, limites départementales blanches fines, limites régionales grises,
#             outre-mer en encarts, légende à gauche, titre, sous-titre et source ; aucune valeur chiffrée
#             d'un département n'est écrite (les cartes lisent des classes).
# SECRET    : un département masqué ou absent de la table est « non diffusé (secret statistique) »,
#             indiscernable d'un autre (SPEC-PUB-007) ; un département diffusé dont le profil est retenu
#             (cellule CS masquée) est non diffusé sur la carte des profils, et garde sa classe d'intensité.
# RÈGLE     : jamais d'arrêt sur la production d'un PNG (message) ; GENERER_ANALYSE faux = ignoré.
# ==============================================================================
if (!exists("charger_fond_carte") || !exists("PROFILS_TYPOLOGIE")) {
  source("R/00_config_fonctions/00_config.R")
  for (f in c("00g_format_restitution.R", "00c_fonctions_geo.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R")) source(file.path("R/00_config_fonctions", f))
}
library(dplyr)

# --- Palette et libellés (une couleur par profil, lisibles entre eux, non diffusé à part) -------------------
PALETTE_PROFILS_NOTE <- c(enjeu = "#C0392B", emergent = "#E8A33D", majeur_modere = "#2E6FB7", concentre = "#8E6BB8",
                          stable = "#9FC5E8", diffus = "#DDE3EA", expertiser = "#F2D16B", non_diffuse = "#FFFFFF")
PALETTE_INTENSITE_NOTE <- c("Faible" = "#DCE9F5", "Modérée" = "#7FB0DB", "Élevée" = "#1F4E79", "non diffusé" = "#FFFFFF")
COULEURS_NOTE <- c(encre = "#1F2933", texte = "#52606D", gris = "#8A95A3", filet = "#C9CED6", contour = "#FFFFFF", contour_nd = "#B7C0CA", cercle = "#C0392B")
LIBELLE_NON_DIFFUSE_NOTE <- "Non diffusé (secret statistique)"

# --- Préparation : une ligne par département du fond ; masqué ou absent = non diffusé -------------------------
preparer_cartes_note <- function(typo, fond) {
  if (!"masque" %in% names(typo)) stop("cartes note : table de typologie de DIFFUSION attendue (colonne masque).")
  for (v in c("geo_code", "profil_typologie")) if (!v %in% names(typo)) stop("cartes note : colonne ", v, " absente.")
  t <- typo |> mutate(geo_code = as.character(geo_code), masque = masque %in% TRUE)
  cle <- names(PROFILS_TYPOLOGIE)[match(t$profil_typologie, PROFILS_TYPOLOGIE)]
  # profil retenu par le secret alors que le département est diffusé (cellule CS masquée, 08f) : profil non diffusé, intensité conservée si elle est diffusée
  profil_nd <- grepl("^non diffusé", tolower(trimws(as.character(t$profil_typologie))))
  if (any(!t$masque & is.na(cle) & !profil_nd)) stop("cartes note : profil inconnu : ", paste(unique(t$profil_typologie[!t$masque & is.na(cle) & !profil_nd]), collapse = ", "))
  inten <- if ("classe_intensite" %in% names(t)) as.character(t$classe_intensite) else rep(NA_character_, nrow(t))
  d <- tibble::tibble(code = names(fond)) |>
    left_join(tibble::tibble(code = t$geo_code, cle = ifelse(t$masque, NA_character_, cle), intensite = ifelse(t$masque, NA_character_, inten)), by = "code")
  d$cle[is.na(d$cle)] <- "non_diffuse"
  d$intensite[is.na(d$intensite) | !(d$intensite %in% names(PALETTE_INTENSITE_NOTE))] <- "non diffusé"
  d
}

# --- Dessin commun ---------------------------------------------------------------------------------------
.theme_carte_note <- function() {
  ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(legend.position = "left", legend.justification = c(0, 1), legend.margin = ggplot2::margin(0, 8, 0, 0),
                   legend.title = ggplot2::element_text(face = "bold", size = 10, colour = COULEURS_NOTE[["encre"]]),
                   legend.text = ggplot2::element_text(size = 9.5, colour = COULEURS_NOTE[["texte"]]), legend.key.size = grid::unit(12, "pt"), legend.spacing.y = grid::unit(4, "pt"),
                   plot.title = ggplot2::element_text(face = "bold", size = 15, colour = COULEURS_NOTE[["encre"]], margin = ggplot2::margin(b = 4)),
                   plot.subtitle = ggplot2::element_text(size = 10.5, colour = COULEURS_NOTE[["texte"]], margin = ggplot2::margin(b = 10)),
                   plot.caption = ggplot2::element_text(size = 8, colour = COULEURS_NOTE[["gris"]], hjust = 0, margin = ggplot2::margin(t = 8)),
                   plot.title.position = "plot", plot.caption.position = "plot", plot.margin = ggplot2::margin(12, 12, 10, 12), plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}
dessiner_carte_note <- function(lay, classe_par_code, palette, libelles, titre, sous_titre, legende, fichier, lay_regions = NULL, cercler = character(0), source_note = NULL, dpi = 300) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 absent")
  long <- lay$long; long$classe <- factor(unname(classe_par_code[long$code]), levels = names(palette))
  n <- table(factor(classe_par_code, levels = names(palette)))
  lab <- sprintf("%s (%d)", libelles[names(palette)], as.integer(n)); names(lab) <- names(palette)
  nd <- names(palette)[length(palette)]
  contour <- ifelse(long$classe == nd, COULEURS_NOTE[["contour_nd"]], COULEURS_NOTE[["contour"]])
  g <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = long, ggplot2::aes(x = x, y = y, group = groupe, fill = classe), colour = COULEURS_NOTE[["contour"]], linewidth = 0.22) +
    ggplot2::geom_polygon(data = long[long$classe == nd, ], ggplot2::aes(x = x, y = y, group = groupe), fill = NA, colour = COULEURS_NOTE[["contour_nd"]], linewidth = 0.25, linetype = "22") +
    ggplot2::scale_fill_manual(values = palette, labels = lab, drop = FALSE, name = legende, breaks = names(palette)) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = COULEURS_NOTE[["filet"]], linewidth = 0.3), ncol = 1, byrow = TRUE)) +
    ggplot2::coord_equal(expand = FALSE) + .theme_carte_note() +
    ggplot2::labs(title = titre, subtitle = sous_titre, caption = paste0(if (!is.null(source_note)) paste0("Source : ", source_note, ". ") else "",
                                                                        "Blanc cerné de pointillés : non diffusé (secret statistique).\nOutre-mer en encarts, chacun à sa propre échelle. Fond IGN Admin Express via france-geojson."))
  if (!is.null(lay_regions)) {
    reg <- lay_regions$long[!(lay_regions$long$code %in% names(lay_regions$encarts)), ]
    g <- g + ggplot2::geom_polygon(data = reg, ggplot2::aes(x = x, y = y, group = groupe), fill = NA, colour = COULEURS_NOTE[["gris"]], linewidth = 0.38)
  }
  if (length(cercler)) g <- g + ggplot2::geom_polygon(data = long[long$code %in% cercler, ], ggplot2::aes(x = x, y = y, group = groupe), fill = NA, colour = COULEURS_NOTE[["cercle"]], linewidth = 0.8)
  if (length(lay$encarts)) {
    enc <- bind_rows(lapply(lay$encarts, function(e) tibble::tibble(x1 = e$cadre[1], x2 = e$cadre[3], y1 = -e$cadre[2], y2 = -e$cadre[4], lib = e$nom, xl = e$x_lib, yl = -e$y_lib)))
    g <- g + ggplot2::geom_rect(data = enc, ggplot2::aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1), fill = NA, colour = COULEURS_NOTE[["filet"]], linewidth = 0.3) +
      ggplot2::geom_text(data = enc, ggplot2::aes(x = xl, y = yl, label = lib), hjust = 0, size = 3, colour = COULEURS_NOTE[["texte"]])
  }
  dir.create(dirname(fichier), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(fichier, g, width = 8.6, height = 7.6, dpi = dpi, bg = "white")
  invisible(fichier)
}

# --- Les deux cartes ---------------------------------------------------------------------------------------
cartes_note_typologie <- function(typo, fond_dep, fond_reg = NULL, dossier, champ = construire_libelle_champ(), source_note = NULL, dpi = 300) {
  d <- preparer_cartes_note(typo, fond_dep)
  lay <- projeter_fond(fond_dep); lay_reg <- if (!is.null(fond_reg)) projeter_fond(fond_reg) else NULL
  sous <- paste0("Départs attendus d’ici 2030 · ", champ$court)
  lib_profils <- c(PROFILS_TYPOLOGIE, non_diffuse = LIBELLE_NON_DIFFUSE_NOTE)[names(PALETTE_PROFILS_NOTE)]
  f1 <- dessiner_carte_note(lay, setNames(d$cle, d$code), PALETTE_PROFILS_NOTE, lib_profils,
                            "Les profils des départements", sous, "Profil", file.path(dossier, "carte_profils_note.png"), lay_reg, character(0), source_note, dpi)
  poles <- d$code[d$cle == "enjeu"]
  lib_int <- c("Faible" = "Faible", "Modérée" = "Modérée", "Élevée" = "Élevée", "non diffusé" = LIBELLE_NON_DIFFUSE_NOTE)
  f2 <- dessiner_carte_note(lay, setNames(d$intensite, d$code), PALETTE_INTENSITE_NOTE, lib_int,
                            "L’intensité du renouvellement", paste0(sous, "\nDéparts attendus rapportés à l’emploi actuel tous âges, en trois classes · cerné de rouge : pôle majeur à renouveler rapidement"), "Intensité",
                            file.path(dossier, "carte_intensite_note.png"), lay_reg, poles, source_note, dpi)
  invisible(c(profils = f1, intensite = f2))
}

# --- Exécution (chaîne ou lancement seul) ------------------------------------------------------------------
if (!isTRUE(get0("GENERER_ANALYSE", ifnotfound = TRUE))) {
  message("09c : GENERER_ANALYSE = FALSE : cartes de la note ignorées.")
} else {
  typo_09c <- if (exists("typologie_departements") && !is.null(typologie_departements$diffusion)) typologie_departements$diffusion else {
    f_09c <- file.path(DIR_SORTIES, "typologie_territoriale", "diffusion", "typologie_departements.csv")
    if (file.exists(f_09c)) utils::read.csv2(f_09c, colClasses = c(geo_code = "character"), fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE) else NULL }
  if (is.null(typo_09c)) {
    message("09c : typologie de diffusion absente (08f non exécuté, zonage sans département) : cartes de la note non produites.")
  } else if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("09c : ggplot2 absent : cartes de la note non produites.")
  } else {
    dir_09c <- file.path(DIR_SORTIES, "analyse_hypotheses_depart", "cartes")
    source_09c <- if (identical(get0("SOURCE_BTS", ifnotfound = ""), "parquet")) "BTS 2024, DREES, EACR, Insee — calculs propres" else "table test — calculs propres"
    cartes_09c <- tryCatch({
      fond_dep_09c <- charger_fond_carte("departement"); fond_reg_09c <- tryCatch(charger_fond_carte("region"), error = function(e) NULL)
      cartes_note_typologie(typo_09c, fond_dep_09c, fond_reg_09c, dir_09c, construire_libelle_champ(), source_09c)
    }, error = function(e) { message("09c : cartes de la note non produites (", conditionMessage(e), ")."); NULL })
    if (!is.null(cartes_09c)) cat(sprintf("\n09c OK -> cartes de la note : %s ; %s\n", cartes_09c[["profils"]], cartes_09c[["intensite"]]))
    suppressWarnings(rm(dir_09c, source_09c, cartes_09c, fond_dep_09c, fond_reg_09c))
  }
  suppressWarnings(rm(typo_09c, f_09c))
}
