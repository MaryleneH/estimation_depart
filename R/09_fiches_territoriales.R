# ==============================================================================
# 09_fiches_territoriales.R — Fiches « chiffres clés », une par territoire
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (04) au contrat geo_code / geo_nom / geo_type ;
#             fonctions 00c (géographie) et 00d (fiches) ; paramètres
#             GENERER_FICHES, FICHES_MODE, FICHES_SELECTION, FICHES_DIR,
#             FICHES_SEUIL_PROCHE, FICHES_ANNEXE, GEO_INTERET, SEUIL_DIFFUSION, AGE_SENIOR (00)
# PRODUIT   : sorties/fiches_<suffixe du zonage>/<code>_<nom>.html (une page
#             autonome par territoire) + index.html ; objet `journal_fiches`.
# PÉRIMÈTRE : le MÊME que le 08 (recodage des territoires inconnus, filtre
#             GEO_INTERET) — les médianes de position sont donc celles du
#             quadrant. FICHES_MODE / FICHES_SELECTION décident seulement pour
#             quels territoires une fiche est éditée.
# GÉNÉRIQUE : aucun nom de zonage ni de colonne source ici (voir 00d).
# ==============================================================================
if (!isTRUE(GENERER_FICHES)) {
  message("09 : fiches territoriales désactivées (GENERER_FICHES = FALSE).")
} else {
  if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
  if (!exists("generer_fiches")) stop("Exécutez d'abord R/00d_fonctions_fiches.R (ou main.R).")
  if (!all(c("geo_code", "geo_nom", "geo_type") %in% names(bts_projete)))
    stop("Contrat géographique absent de bts_projete : exécutez le R/01 à jour.")
  library(dplyr)

  ZON_FICHES <- zonage_geo(GEO_ANALYSE)
  dir_fiches <- if (is.null(FICHES_DIR)) file.path(DIR_SORTIES, paste0("fiches_", ZON_FICHES$suffixe)) else FICHES_DIR

  # Même base que le 08 : territoires inconnus regroupés, périmètre GEO_INTERET
  base_fiches <- bts_projete |>
    mutate(geo_code = ifelse(is.na(geo_code), "inconnu", as.character(geo_code)),
           geo_nom  = ifelse(geo_code == "inconnu", paste(ZON_FICHES$libelle, "inconnu(e)"),
                             as.character(geo_nom))) |>
    filtrer_geo_interet(GEO_INTERET, prefixe = "09")

  selection_fiches <- if (identical(FICHES_MODE, "selection") && is.null(FICHES_SELECTION))
    codes_geo_interet(GEO_INTERET) else FICHES_SELECTION

  journal_fiches <- generer_fiches(
    base_fiches, dir = dir_fiches, mode = FICHES_MODE, selection = selection_fiches,
    seuil = SEUIL_DIFFUSION, age_senior = AGE_SENIOR, zonage = ZON_FICHES,
    seuil_proche = FICHES_SEUIL_PROCHE, age_min = AGE_MIN_BTS,
    annexe = isTRUE(get0("FICHES_ANNEXE", ifnotfound = FALSE)),
    source_note = if (identical(SOURCE_BTS, "parquet")) "données individuelles : BTS 2024" else "données : table test")

  cat(sprintf("\n--- Fiches %s : journal ---\n", ZON_FICHES$pluriel))
  print(journal_fiches |>
          mutate(departs = round(departs)) |>
          select(code, nom, statut, effectif, effectif55, departs, fichier, motif) |>
          as.data.frame(), row.names = FALSE)
  message("09 OK (", GEO_ANALYSE, ") -> ", dir_fiches, "/")
}
