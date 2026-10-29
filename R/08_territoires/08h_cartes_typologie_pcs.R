# ==============================================================================
# 08h_cartes_typologie_pcs.R — Cartes « profils et métiers » : de la région au département
# ------------------------------------------------------------------------------
# PRÉREQUIS : tables de DIFFUSION seulement :
#             DIR_SORTIES/typologie_territoriale/diffusion/typologie_departements.csv (08f)
#             DIR_SORTIES/departs_pcs/diffusion/departs_pcs_{departement,region,france}.csv (08c)
#             DIR_SORTIES/departs_cs1/diffusion/departs_cs1_france.csv (08d, total France ; facultatif)
#             fond local data/cartographie/ ; nomenclature PCS (facultative) ; fonctions 00f, 00h, 00i, 00j.
# PRODUIT   : DIR_SORTIES/typologie_territoriale/cartes/
#               carte_typologie_pcs.html            régions -> départements -> top des PCS, coloration par PCS,
#                                                   spécificité face à la France (JavaScript natif inline)
#               carte_typologie_pcs_courriel.html   AUCUN contenu actif : deux cartes, blocs dépliables natifs
#             Objet `cartes_typologie_pcs` (bilan).
# LANCEMENT : dans main.R (après 08g) OU SEUL, après la chaîne, depuis la racine du projet :
#             source("R/08_territoires/08h_cartes_typologie_pcs.R")
# SECRET    : classement des seules cellules diffusables, au plus TYPO_PCS_TOP_N lignes, jamais le
#             complément ; une seule cellule masquée = top retenu ; masquées = un compte. Une table
#             absente = message avec le chemin attendu, cartes non produites, jamais d'arrêt.
# ==============================================================================
if (!exists("GENERER_TYPOLOGIE") || !exists("generer_cartes_typologie_pcs")) {   # lancement seul
  if (!file.exists("main.R")) stop("08h : lancez ce script depuis la racine du projet (dossier contenant main.R).")
  if (!exists("GENERER_TYPOLOGIE")) { dir_prealable_08h <- get0("DIR_SORTIES", ifnotfound = NULL); source(file.path("R", "00_config_fonctions", "00_config.R"))
    if (!is.null(dir_prealable_08h)) DIR_SORTIES <- dir_prealable_08h; rm(dir_prealable_08h) }
  if (!exists("arrondir_nombre_personnes")) source(file.path("R", "00_config_fonctions", "00g_format_restitution.R"))
  if (!exists("charger_fond_carte")) source(file.path("R", "00_config_fonctions", "00f_fonctions_cartes.R"))
  if (!exists("lire_passage_geo")) source(file.path("R", "00_config_fonctions", "00c_fonctions_geo.R"))   # table de passage departement->region (SPEC-PUB-007)
  if (!exists("PROFILS_TYPOLOGIE")) source(file.path("R", "00_config_fonctions", "00h_fonctions_typologie.R"))
  if (!exists("preparer_carte_typologie")) source(file.path("R", "00_config_fonctions", "00i_fonctions_carte_typologie.R"))
  if (!exists("generer_cartes_typologie_pcs")) source(file.path("R", "00_config_fonctions", "00j_fonctions_cartes_typologie_pcs.R"))
}
if (!isTRUE(get0("GENERER_CARTES_TYPOLOGIE_PCS", ifnotfound = TRUE))) {
  message("08h : cartes profils et PCS désactivées (GENERER_CARTES_TYPOLOGIE_PCS = FALSE).")
} else {
  library(dplyr)
  sources_08h <- c(typologie = file.path(DIR_SORTIES, "typologie_territoriale", "diffusion", "typologie_departements.csv"),
                   departement = file.path(DIR_SORTIES, "departs_pcs", "diffusion", "departs_pcs_departement.csv"),
                   region = file.path(DIR_SORTIES, "departs_pcs", "diffusion", "departs_pcs_region.csv"),
                   france = file.path(DIR_SORTIES, "departs_pcs", "diffusion", "departs_pcs_france.csv"))
  absentes_08h <- sources_08h[!file.exists(sources_08h)]
  if (length(absentes_08h)) {
    message("08h : table(s) de diffusion absente(s), cartes non produites — attendu : ", paste(absentes_08h, collapse = " ; "),
            " (exécutez 08c et 08f avec GEO_ANALYSE = \"departement\", secret activé).")
    cartes_typologie_pcs <- tibble(statut = "source absente", carte = NA_character_, courriel = NA_character_, absentes = paste(basename(absentes_08h), collapse = ", "))
  } else {
    dir_08h <- file.path(DIR_SORTIES, "typologie_territoriale", "cartes"); champ_08h <- construire_libelle_champ()
    source_note_08h <- if (identical(get0("SOURCE_BTS", ifnotfound = ""), "parquet")) "BTS 2024, DREES, EACR, Insee — calculs propres" else "table test — calculs propres"
    cat("\n08h — cartes profils et métiers (régions, départements, top des PCS)\n")
    fond_dep_08h <- charger_fond_carte("departement"); fond_reg_08h <- charger_fond_carte("region"); libelles_08h <- charger_libelles_pcs()
    typo_csv_08h <- lire_typologie_diffusion(sources_08h[["typologie"]]); typo_prep_08h <- preparer_carte_typologie(typo_csv_08h, fond_dep_08h)
    f_cs1_08h <- file.path(DIR_SORTIES, "departs_cs1", "diffusion", "departs_cs1_france.csv")
    # table de passage departement->region : rattache à leur région les départements absents de la typologie (masqués, SPEC-PUB-007)
    passage_08h <- if (exists("lire_passage_geo") && exists("GEO_PASSAGES") && !is.null(GEO_PASSAGES[["departement->region"]]) && file.exists(GEO_PASSAGES[["departement->region"]])) lire_passage_geo(GEO_PASSAGES[["departement->region"]], "departement->region") else NULL
    if (is.null(passage_08h)) message("08h : table de passage departement->region non lue : les départements absents de la typologie ne sont pas rattachés à leur région.")
    prep_08h <- preparer_cartes_typologie_pcs(typo_prep_08h, typo_csv_08h, lire_csv_diffusion(sources_08h[["departement"]]), lire_csv_diffusion(sources_08h[["region"]]),
                                              lire_csv_diffusion(sources_08h[["france"]]), fond_dep_08h, fond_reg_08h, libelles = libelles_08h,
                                              cs1_fr = if (file.exists(f_cs1_08h)) lire_csv_diffusion(f_cs1_08h) else NULL, top_n = get0("TYPO_PCS_TOP_N", ifnotfound = 5),
                                              passage_regions = passage_08h)
    controler_cartes_typologie_pcs(prep_08h)
    html_08h <- file.path(dir_08h, "carte_typologie_pcs.html")
    generer_cartes_typologie_pcs(prep_08h, fond_dep_08h, fond_reg_08h, html_08h, champ = champ_08h, source_note = source_note_08h)
    courriel_08h <- NA_character_
    if (isTRUE(get0("CARTES_COURRIEL", ifnotfound = TRUE))) { courriel_08h <- file.path(dir_08h, "carte_typologie_pcs_courriel.html")
      generer_cartes_typologie_pcs_courriel(prep_08h, fond_dep_08h, fond_reg_08h, courriel_08h, champ = champ_08h, source_note = source_note_08h) }
    cartes_typologie_pcs <- tibble(statut = "ok", carte = html_08h, courriel = courriel_08h, absentes = NA_character_)
    message("08h OK -> ", dir_08h, " : carte_typologie_pcs.html", if (!is.na(courriel_08h)) " + carte_typologie_pcs_courriel.html (sans script)" else "")
    rm(dir_08h, champ_08h, source_note_08h, fond_dep_08h, fond_reg_08h, libelles_08h, typo_csv_08h, typo_prep_08h, f_cs1_08h, passage_08h, prep_08h, html_08h, courriel_08h)
  }
  rm(sources_08h, absentes_08h)
}
