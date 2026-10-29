# ==============================================================================
# 08i_exposition_tension.R — Exposition au recrutement difficile
# ------------------------------------------------------------------------------
# PRÉREQUIS : `departs_pcs` (08c : tables département et France, exactes et de
#             diffusion), `bts_projete` (secret), `typologie_departements` (08f :
#             profils, facultatif), fonctions 00h et 00k ; classeur PCS -> FAP
#             (FICHIER_PCS_FAP_TENSION, hors dépôt). Spécification : specs/10 §13.
# PRODUIT   : objet `exposition_tension` et, dans sorties/typologie_territoriale/
#               interne/exposition_tension_departements.csv   complet (parts, variantes)
#               interne/metiers_strategiques_tension.csv       une ligne par PCS de la liste
#               interne/parametres_exposition_tension.csv      seuils et bornes effectifs
#               diffusion/exposition_tension_departements.csv  secret appliqué (lu par 08g / 08h / 08j)
#               {interne,diffusion}/exposition_tension_regions.csv  même chose au niveau région (si le 08c le produit ; lu par 08j)
#               diffusion/metiers_strategiques_tension.csv     matrice nationale (cellules diffusables)
#               diffusion/matrice_metiers_tension.png          image de la matrice (fichier à part)
#               diffusion/fiche_exposition_tension.html        une page, sans script (matrice SVG en annexe, libellés au survol)
#               diffusion/exposition_tension.xlsx              classeur à quatre feuilles (si TENSION_CLASSEUR_EXCEL)
# RÈGLES    : la tension est une propriété NATIONALE du métier, jamais du
#             département ; aucun départ recalculé ; classeur absent = message.
# ==============================================================================
if (!isTRUE(get0("GENERER_EXPOSITION_TENSION", ifnotfound = FALSE))) {
  message("08i : exposition au recrutement difficile désactivée (GENERER_EXPOSITION_TENSION = FALSE).")
} else if (!file.exists(get0("FICHIER_PCS_FAP_TENSION", ifnotfound = ""))) {
  message("08i : classeur PCS -> FAP de tension absent, exposition non produite — attendu : ", get0("FICHIER_PCS_FAP_TENSION", ifnotfound = "(non configuré)"),
          " (gabarit fictif : data/templates/pcs_fap_tension_template.xlsx).")
} else if (!exists("departs_pcs") || !all(c("departement", "france") %in% departs_pcs$niveaux)) {
  message("08i : exposition non produite — les tables département et France x PCS du 08c sont absentes ",
          "(GENERER_DEPARTS_PCS = FALSE, DEPARTS_PCS_NIVEAUX incomplet ou zonage différent du département).")
} else {
  if (!exists("exposition_departements")) stop("Exécutez d'abord 00k_fonctions_tension.R (ou main.R).")
  if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
  library(dplyr)
  par_08i <- parametres_tension()
  src_08i <- lire_pcs_fap_tension(FICHIER_PCS_FAP_TENSION, par_08i)
  met_08i <- tension_metiers(src_08i, par_08i)
  dep_08i <- departs_pcs$analytique$departement
  met_08i$present_base <- met_08i$pcs %in% normaliser_code_pcs(dep_08i$pcs)
  if (any(!met_08i$present_base))
    message("08i : ", sum(!met_08i$present_base), texte_perimetre(" PCS de la liste absente(s) des départs de la Base (aucun salarié du champ) : "),
            paste(met_08i$pcs_source[!met_08i$present_base], collapse = ", "))
  if (!is.na(par_08i$n_suivis) && par_08i$n_suivis != nrow(met_08i))
    message("08i : TENSION_N_METIERS_SUIVIS = ", par_08i$n_suivis, " alors que le classeur compte ", nrow(met_08i), " PCS : les textes nomment ", par_08i$n_suivis, " métiers suivis (00_config.R).")
  if (any(!met_08i$tension_connue)) message("08i : ", sum(!met_08i$tension_connue), texte_liste(" PCS de tension inconnue (rang manquant), comptée(s) hors des métiers stratégiques."))
  expo_08i <- exposition_departements(dep_08i, met_08i, par_08i)
  typo_int_08i <- if (exists("typologie_departements")) typologie_departements$departements else NULL
  if (!is.null(typo_int_08i))
    expo_08i$departements$cumul_renouvellement_tension <- cumul_renouvellement_tension(typo_int_08i$profil_typologie[match(expo_08i$departements$geo_code, typo_int_08i$geo_code)],
                                                         expo_08i$departements$classe_exposition)
  expo_08i$departements$texte <- rediger_exposition_departement(expo_08i$departements, par_08i)
  mat_int_08i <- matrice_metiers_tension(met_08i, departs_pcs$analytique$france, NULL, par_08i)

  dir_typo_08i <- file.path(DIR_SORTIES, "typologie_territoriale")
  dir_int_08i <- file.path(dir_typo_08i, "interne"); dir.create(dir_int_08i, showWarnings = FALSE, recursive = TRUE)
  restituer_08i <- function(x) formater_restitution(x, personnes = c(COLONNES_PERSONNES, grep("^(departs|effectif)_|^tension_departs_", names(x), value = TRUE)))
  write.csv2(restituer_08i(expo_08i$departements), file.path(dir_int_08i, "exposition_tension_departements.csv"), row.names = FALSE)
  write.csv2(restituer_08i(mat_int_08i$metiers), file.path(dir_int_08i, "metiers_strategiques_tension.csv"), row.names = FALSE)
  param_08i <- tibble(parametre = c("fichier", "feuille", "rang_max", "seuil_rang", "seuil_confiance_applique", "methode_classes", "exposition_bas_pct", "exposition_haut_pct",
                                    "couverture_min_pct", "effectif_min", "exposition_france_pct", "couverture_france_pct", "variantes_robustesse",
                                    "n_pcs_liste", "n_pcs_presentes", "n_pcs_en_tension", "n_departements", "n_departements_retenus_bornes", "sous_niveaux_admis"),
                      valeur = as.character(c(basename(FICHIER_PCS_FAP_TENSION), par_08i$feuille, par_08i$rang_max, par_08i$seuil_rang, expo_08i$seuil_confiance, par_08i$methode,
                                              expo_08i$bornes[["bas"]], expo_08i$bornes[["haut"]], par_08i$couverture_min, par_08i$effectif_min,
                                              expo_08i$france$exposition_pct, expo_08i$france$couverture_pct, paste(expo_08i$variantes, collapse = ", "),
                                              nrow(met_08i), sum(met_08i$present_base), sum(met_08i$en_tension %in% TRUE), nrow(expo_08i$departements), expo_08i$n_retenus,
                                              if (length(par_08i$sous_niveaux)) paste(par_08i$rang_max + par_08i$sous_niveaux / 10, collapse = ", ") else "aucun")))
  write.csv2(param_08i, file.path(dir_int_08i, "parametres_exposition_tension.csv"), row.names = FALSE)

  # niveau RÉGION (SPEC-TYPO-109) : mêmes calculs sur la table région x PCS du 08c ; secret sur la base rattachée à la région
  expo_reg_08i <- NULL
  if ("region" %in% departs_pcs$niveaux && all(c("region_code", "region_nom") %in% names(departs_pcs$analytique$region))) {
    reg_08i <- departs_pcs$analytique$region |> rename(geo_code = region_code, geo_nom = region_nom) |> mutate(geo_code = as.character(geo_code))
    expo_reg_08i <- exposition_departements(reg_08i, met_08i, par_08i)
    expo_reg_08i$departements$texte <- rediger_exposition_departement(expo_reg_08i$departements, par_08i, "région")
    write.csv2(restituer_08i(expo_reg_08i$departements), file.path(dir_int_08i, "exposition_tension_regions.csv"), row.names = FALSE)
  }
  dif_08i <- NULL; fiche_08i <- NA_character_
  if (!isTRUE(get0("TYPO_SECRET", ifnotfound = TRUE)) || is.null(departs_pcs$diffusion)) {
    message("08i : pas de table de diffusion du 08c (DEPARTS_PCS_SECRET = FALSE) ou TYPO_SECRET = FALSE -> aucun fichier diffusion/.")
  } else {
    typo_dif_08i <- if (exists("typologie_departements") && !is.null(typologie_departements$diffusion)) typologie_departements$diffusion else NULL
    dif_08i <- appliquer_secret_exposition(expo_08i, met_08i, bts_projete, departs_pcs$diffusion$departement, typo_dif_08i, departs_pcs$diffusion$france, regles_secret(), par_08i,
                                           univers = univers_territoires("departement", geo_interet = NULL))   # SPEC-SEC-035 ; même périmètre que le 08c
    if (any(c("n_entreprises", "part_dominante_pct") %in% names(dif_08i)) || any(grepl("^departs_|^effectif_", names(dif_08i))))
      stop("08i : effectif ou indicateur de secret présent dans la diffusion.")
    mat_08i <- matrice_metiers_tension(met_08i, departs_pcs$analytique$france, departs_pcs$diffusion$france, par_08i)
    dir_dif_08i <- file.path(dir_typo_08i, "diffusion"); dir.create(dir_dif_08i, showWarnings = FALSE, recursive = TRUE)
    write.csv2(restituer_08i(dif_08i), file.path(dir_dif_08i, "exposition_tension_departements.csv"), row.names = FALSE)
    if (!is.null(expo_reg_08i) && !is.null(departs_pcs$diffusion$region)) {
      base_reg_08i <- ajouter_region(bts_projete, GEO_PASSAGES[["departement->region"]], prefixe = "08i") |> mutate(geo_code = as.character(region_code))
      dif_reg_08i <- appliquer_secret_exposition(expo_reg_08i, met_08i, base_reg_08i,
                                                 departs_pcs$diffusion$region |> rename(geo_code = region_code, geo_nom = region_nom) |> mutate(geo_code = as.character(geo_code)),
                                                 NULL, departs_pcs$diffusion$france, regles_secret(), par_08i, territoire = "région", univers = univers_territoires("region", geo_interet = NULL))
      if (any(grepl("^departs_|^effectif_|n_entreprises|part_dominante", names(dif_reg_08i)))) stop("08i : effectif ou indicateur de secret présent dans la diffusion région.")
      write.csv2(restituer_08i(dif_reg_08i), file.path(dir_dif_08i, "exposition_tension_regions.csv"), row.names = FALSE)
      rm(base_reg_08i)
    }
    write.csv2(restituer_08i(mat_08i$metiers |> select(pcs = pcs_source, pcs_libelle, fap_code, fap_libelle, rang_tension, rang_tension_fin, en_tension, composante_dominante = composante_libelle,
                                                      levier, faible_confiance, statut, taux_depart_central_pct, departs_central, quadrant)),
               file.path(dir_dif_08i, "metiers_strategiques_tension.csv"), row.names = FALSE)
    png_08i <- file.path(dir_dif_08i, "matrice_metiers_tension.png")
    png_08i <- tryCatch(png_matrice_metiers_tension(mat_08i, png_08i),
                        error = function(e) { message("08i : PNG de la matrice non produit (", conditionMessage(e), ") — les CSV restent la référence."); NULL })
    typo_pour_msg <- if (!is.null(typo_dif_08i)) typo_dif_08i else tibble(geo_code = character(0), masque = logical(0), departs_central = numeric(0))
    msg_08i <- messages_fiche_tension(dif_08i, typo_pour_msg, mat_08i, par_08i)
    fiche_08i <- file.path(dir_dif_08i, "fiche_exposition_tension.html")
    source_08i <- if (identical(get0("SOURCE_BTS", ifnotfound = ""), "parquet")) "BTS 2024, DREES, EACR, Insee — calculs propres" else "table test — calculs propres"
    generer_fiche_exposition_tension(msg_08i, fiche_08i, png_08i, construire_libelle_champ(), par_08i, expo_08i$bornes,
                                     sum(dif_08i$tension_classe %in% "Forte"), source_08i, mat = mat_08i)
    # sortie obsolète (SPEC-TYPO-109) : la fiche des territoires, produite ici avant le 08j, est retirée si elle subsiste d'une exécution antérieure
    obsolete_08i <- file.path(dir_dif_08i, "fiche_territoires_tension.html")
    if (file.exists(obsolete_08i)) { unlink(obsolete_08i); message("08i : fichier obsolète retiré : ", obsolete_08i, " (remplacé par diffusion/fiche_metiers_liste.html, 08j).") }
    # classeur Excel (SPEC-TYPO-108) : mêmes tables de diffusion ; jamais d'arrêt
    if (isTRUE(par_08i$classeur_excel)) {
      xlsx_08i <- tryCatch(ecrire_classeur_tension(tables_classeur_tension(dif_08i, mat_08i, msg_08i, param_08i, par_08i, expo_08i$bornes, sum(dif_08i$tension_classe %in% "Forte"),
                                                                           construire_libelle_champ(), source_08i), file.path(dir_dif_08i, "exposition_tension.xlsx")),
                           error = function(e) { message("08i : classeur Excel non produit (", conditionMessage(e), ") — les CSV restent la référence."); NA_character_ })
    }
  }
  exposition_tension <- list(metiers = met_08i, departements = expo_08i$departements, diffusion = dif_08i, diffusion_region = get0("dif_reg_08i", ifnotfound = NULL),
                             bornes = expo_08i$bornes, france = expo_08i$france)
  cat(texte_liste(sprintf(texte_perimetre("\n08i OK -> part des remplacements difficiles (exposition) : %d PCS stratégiques (%d présentes dans la Base, %d en tension en France, rang >= %d sur %d)\n"),
              nrow(met_08i), sum(met_08i$present_base), sum(met_08i$en_tension %in% TRUE), par_08i$seuil_rang, par_08i$rang_max)))
  cat(texte_liste(sprintf("  France : %s %% des départs des métiers stratégiques concernent des métiers en tension ; ces métiers portent %s %% des départs\n",
              formatC(expo_08i$france$exposition_pct, format = "f", digits = 1, decimal.mark = ","), formatC(expo_08i$france$couverture_pct, format = "f", digits = 1, decimal.mark = ","))))
  cl_08i <- table(factor(expo_08i$departements$classe_exposition, levels = CLASSES_EXPOSITION), useNA = "no")
  cat(sprintf("  Départements : Faible %d | Modérée %d | Forte %d | non interprétés %d ; à confirmer %d\n", cl_08i[[1]], cl_08i[[2]], cl_08i[[3]],
              sum(is.na(expo_08i$departements$classe_exposition)), sum(expo_08i$departements$robustesse %in% "à confirmer")))
  if (!is.null(dif_08i)) cat(sprintf("  Diffusion : %d département(s) diffusé(s), %d non diffusé(s) (secret) ; fiche : %s ; classeur : %s\n", sum(dif_08i$tension_diffusee), sum(!dif_08i$tension_diffusee), fiche_08i,
                                     if (exists("xlsx_08i") && !is.na(xlsx_08i)) xlsx_08i else "non produit"))
  rm(par_08i, src_08i, met_08i, dep_08i, expo_08i, typo_int_08i, mat_int_08i, dir_typo_08i, dir_int_08i, restituer_08i, param_08i, dif_08i, fiche_08i, cl_08i, expo_reg_08i)
  suppressWarnings(rm(reg_08i))
  suppressWarnings(rm(typo_dif_08i, mat_08i, dir_dif_08i, png_08i, typo_pour_msg, msg_08i, source_08i, xlsx_08i, dif_reg_08i, obsolete_08i))
}
