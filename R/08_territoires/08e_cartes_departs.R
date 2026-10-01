# ==============================================================================
# 08e_cartes_departs.R — Cartes des résultats DIFFUSABLES : départs par PCS fine
#                        et par grande CS, France entière / région / département
# ------------------------------------------------------------------------------
# PRÉREQUIS : les tables de DIFFUSION écrites par 08c et 08d :
#             DIR_SORTIES/departs_pcs/diffusion/departs_pcs_<niveau>.csv
#             DIR_SORTIES/departs_cs1/diffusion/departs_cs1_<niveau>.csv
#             (lues telles quelles, avec ou sans BOM : aucune reprojection, aucun
#             recalcul ; relançable seul après la chaîne) ; fonctions 00f ;
#             fond local data/cartographie/ ; GENERER_CARTES_DEPARTS, CARTES_PNG (00).
# PRODUIT   : sorties/cartes_departs/<dimension>/carte_<dimension>_<niveau>.html :
#             région / département = carte choroplèthe interactive (+ .png de la
#             vue initiale si CARTES_PNG) ; France = TABLEAU DE BORD national de
#             chiffres clés (aucune carte). Objet `cartes_departs` (bilan).
# CHAMP     : rappelé en tête de chaque page et dans les KPI, construit par
#             construire_libelle_champ() (00f) depuis la configuration réelle.
# SÉCURITÉ  : source absolue = tables de diffusion ; une table sans colonne
#             `masque` (interne) est refusée ; les mesures des lignes masquées
#             sont effacées avant sérialisation (00f) ; contrôle bloquant.
# GÉOGRAPHIE: jointure sur les CODES (character), jamais sur les libellés ; un
#             code sans géométrie = ARRÊT avec la liste ; « inconnu » = hors
#             carte, compté. Un fichier absent = message avec le chemin attendu,
#             carte non produite, bilan final ; aucun fichier = arrêt.
# ==============================================================================
if (!isTRUE(get0("GENERER_CARTES_DEPARTS", ifnotfound = FALSE))) {
  message("08e : cartographie des départs désactivée (GENERER_CARTES_DEPARTS = FALSE).")
} else {
  if (!exists("preparer_carte_departs")) stop("Exécutez d'abord 00f_fonctions_cartes.R (ou main.R).")
  library(dplyr)
  configs_08e <- expand.grid(dimension = c("pcs", "cs1"), niveau = c("france", "region", "departement"), stringsAsFactors = FALSE)
  dir_cartes <- file.path(DIR_SORTIES, "cartes_departs")
  fonds_08e <- list()
  champ_08e <- construire_libelle_champ()          # source unique du champ, lue dans la configuration courante
  cat("\n08e — cartes et tableaux de bord des résultats diffusables\n  ", champ_08e$titre, "\n")
  bilan_08e <- lapply(seq_len(nrow(configs_08e)), function(i) {
    dm <- configs_08e$dimension[i]; nv <- configs_08e$niveau[i]
    src <- file.path(DIR_SORTIES, paste0("departs_", dm), "diffusion", sprintf("departs_%s_%s.csv", dm, nv))
    sortie <- file.path(dir_cartes, dm, sprintf("carte_%s_%s.html", dm, nv))
    if (!file.exists(src)) {
      message("08e : table de diffusion absente, carte non produite — attendu : ", src)
      return(tibble(dimension = dm, niveau = nv, source = src, carte = NA_character_, statut = "source absente",
                    territoires_csv = NA_integer_, joints = NA_integer_, masques = NA_integer_, diffusables = NA_integer_, sans_donnee = NA_integer_))
    }
    # France entière : tableau de bord national (aucune carte, aucun fond, pas de PNG)
    fond <- NULL; fond_regions <- NULL
    if (nv != "france") {
      cle_fond <- if (nv == "region") "region" else "departement"
      if (is.null(fonds_08e[[cle_fond]])) fonds_08e[[cle_fond]] <<- charger_fond_carte(cle_fond)
      fond <- fonds_08e[[cle_fond]]
      if (nv == "departement") {                 # limites régionales tracées par-dessus les départements
        if (is.null(fonds_08e[["region"]])) fonds_08e[["region"]] <<- charger_fond_carte("region")
        fond_regions <- fonds_08e[["region"]]
      }
    }
    d <- lire_csv_diffusion(src)
    prep <- preparer_carte_departs(d, dm, nv, fond)
    n <- controler_carte_departs(prep, fond)
    generer_carte_departs(prep, fond, sortie, champ = champ_08e, fond_regions = fond_regions,
                          fichier_png = if (isTRUE(get0("CARTES_PNG", ifnotfound = TRUE)) && nv != "france") sub("\\.html$", ".png", sortie) else NULL,
                          source_note = if (identical(get0("SOURCE_BTS", ifnotfound = ""), "parquet")) "BTS 2024, DREES, EACR, Insee — calculs propres" else "table test — calculs propres")
    tibble(dimension = dm, niveau = nv, source = src, carte = sortie, statut = "ok",
           territoires_csv = n[["csv"]], joints = n[["joints"]], masques = n[["masques"]], diffusables = n[["diffusables"]], sans_donnee = n[["sans"]])
  }) |> bind_rows()
  if (!any(bilan_08e$statut == "ok"))
    stop("08e : aucune table de diffusion trouvée sous ", DIR_SORTIES, " (departs_pcs/diffusion, departs_cs1/diffusion) — exécutez 08c / 08d.")
  cartes_departs <- bilan_08e
  absents <- bilan_08e$source[bilan_08e$statut != "ok"]
  message("08e OK -> ", dir_cartes, " : ", sum(bilan_08e$statut == "ok"), " page(s) HTML (France : tableau de bord national ; région, département : cartes",
          if (isTRUE(get0("CARTES_PNG", ifnotfound = TRUE))) " + PNG)" else ")",
          if (length(absents)) paste0(" ; ", length(absents), " source(s) absente(s) : ", paste(basename(absents), collapse = ", ")) else "")
  rm(configs_08e, fonds_08e, bilan_08e, absents, champ_08e)
}
