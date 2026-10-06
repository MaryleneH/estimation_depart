# ==============================================================================
# 08f_typologie_departements.R — Typologie nationale des territoires de la BITD
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `departs_cs1` du 08d (table analytique département x cs1,
#             table de diffusion), `bts_projete` (secret du département entier),
#             `stock_tous_ages` (01c, facultatif), fonctions 00h ; paramètres
#             GENERER_TYPOLOGIE, TYPO_SECRET, TYPO_* (00). Spécification : specs/10.
# PRODUIT   : objet `typologie_departements` (liste : departements, departement_cs,
#             parametres, synthese, diffusion) et sorties/typologie_territoriale/
#               interne/typologie_departements.csv      une ligne par département (COMPLET)
#               interne/typologie_departement_cs.csv    une ligne par département x grande CS
#               interne/synthese_profils.csv            une ligne par profil
#               interne/parametres_typologie.csv        seuils effectifs de l'exécution
#               interne/matrice_typologie.png           poids x intensité, taille = volume, couleur = structure
#               diffusion/typologie_departements.csv    secret appliqué (si TYPO_SECRET)
# MÉTHODE   : couche d'analyse EN AVAL : aucun départ recalculé, aucune formule
#             nouvelle. Règles métier lisibles (00h), seuils de la configuration.
# MAILLE    : département uniquement (exige GEO_ANALYSE = "departement" : sinon
#             ignoré avec message, comme région / département dans 08c / 08d).
#             Tensions DGA / France Travail : couche séparée, jamais jointe ici.
# ==============================================================================
if (!isTRUE(get0("GENERER_TYPOLOGIE", ifnotfound = FALSE))) {
  message("08f : typologie des territoires désactivée (GENERER_TYPOLOGIE = FALSE).")
} else if (!exists("departs_cs1") || !"departement" %in% departs_cs1$niveaux) {
  message("08f : typologie non produite — la table département x grande CS du 08d est absente ",
          "(GENERER_DEPARTS_CS1 = FALSE ou zonage d'analyse différent du département).")
} else {
  if (!exists("construire_typologie_departements")) stop("Exécutez d'abord 00h_fonctions_typologie.R (ou main.R).")
  if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
  library(dplyr)
  cs_dep_08f <- departs_cs1$analytique$departement                       # EXACT : avant arrondi, avant secret
  stock_08f <- if (exists("stock_tous_ages") && !is.null(stock_tous_ages)) stock_tous_ages else NULL
  typo_08f <- construire_typologie_departements(cs_dep_08f, stock_08f, parametres_typologie())
  controler_typologie(typo_08f, cs_dep_08f)
  synth_08f <- synthese_profils(typo_08f)

  dir_typo <- file.path(DIR_SORTIES, "typologie_territoriale")
  dir_int <- file.path(dir_typo, "interne"); dir.create(dir_int, showWarnings = FALSE, recursive = TRUE)
  # restitution (00g) : personnes -> entier (y compris les colonnes par grande CS), parts -> 1 décimale ; objets exacts intacts
  restituer_08f <- function(x) formater_restitution(x, personnes = c(COLONNES_PERSONNES, grep("^(departs|effectif)_", names(x), value = TRUE)))
  write.csv2(restituer_08f(typo_08f$departements), file.path(dir_int, "typologie_departements.csv"), row.names = FALSE)
  write.csv2(restituer_08f(typo_08f$departement_cs), file.path(dir_int, "typologie_departement_cs.csv"), row.names = FALSE)
  write.csv2(restituer_08f(synth_08f), file.path(dir_int, "synthese_profils.csv"), row.names = FALSE)
  write.csv2(typo_08f$parametres, file.path(dir_int, "parametres_typologie.csv"), row.names = FALSE)
  tryCatch(png_matrice_typologie(typo_08f, file.path(dir_int, "matrice_typologie.png")),
           error = function(e) message("08f : PNG non produit (", conditionMessage(e), ") — les CSV restent la restitution de référence."))

  diffusion_08f <- NULL
  if (isTRUE(get0("TYPO_SECRET", ifnotfound = TRUE))) {
    if (is.null(departs_cs1$diffusion)) {
      message("08f : TYPO_SECRET = TRUE mais le 08d n'a pas produit de table de diffusion (DEPARTS_CS1_SECRET = FALSE) -> aucun fichier diffusion/.")
    } else {
      base_dep_08f <- bts_projete |> mutate(geo_code = ifelse(is.na(geo_code), "inconnu", as.character(geo_code)))
      diffusion_08f <- appliquer_secret_typologie(typo_08f, departs_cs1$diffusion$departement, base_dep_08f, regles_secret())
      if (any(c("n_entreprises", "part_dominante_pct") %in% names(diffusion_08f))) stop("08f : indicateurs de secret présents dans la diffusion.")
      dir_dif <- file.path(dir_typo, "diffusion"); dir.create(dir_dif, showWarnings = FALSE, recursive = TRUE)
      write.csv2(restituer_08f(diffusion_08f), file.path(dir_dif, "typologie_departements.csv"), row.names = FALSE)
    }
  }
  typologie_departements <- list(departements = typo_08f$departements, departement_cs = typo_08f$departement_cs,
                                 parametres = typo_08f$parametres, synthese = synth_08f, diffusion = diffusion_08f)
  cat("\n08f OK -> typologie des territoires BITD (", nrow(typo_08f$departements), " départements ; intensité = ",
      typo_08f$indicateur, " ; base du poids = ", typo_08f$base_poids, ")\n", sep = "")
  for (i in seq_len(nrow(synth_08f)))
    cat(sprintf("  %-36s %3d département(s) | %5.1f %% de l'emploi | %5.1f %% des départs\n", synth_08f$profil_typologie[i],
                synth_08f$n_departements[i], synth_08f$poids_national_emploi_pct[i], synth_08f$poids_national_departs_pct[i]))
  cat(sprintf("  %s\n", dir_int)); if (!is.null(diffusion_08f)) cat(sprintf("  %s (%d département(s) masqué(s), %d profil(s) non diffusé(s) pour cellule CS masquée)\n",
                                                                        dir_dif, sum(diffusion_08f$masque), sum(diffusion_08f$motif_masque %in% "cellule cs masquée")))
  rm(cs_dep_08f, stock_08f, typo_08f, synth_08f, diffusion_08f, dir_typo, dir_int)
}
