# ==============================================================================
# 08g_carte_typologie.R — Cartes des profils de la typologie des départements
# ------------------------------------------------------------------------------
# PRÉREQUIS : la table de DIFFUSION écrite par 08f :
#             DIR_SORTIES/typologie_territoriale/diffusion/typologie_departements.csv
#             (lue telle quelle : aucun recalcul, aucune lecture d'interne/) ;
#             fond local data/cartographie/ ; fonctions 00f, 00h, 00i.
# PRODUIT   : DIR_SORTIES/typologie_territoriale/cartes/
#               carte_typologie.html            carte interactive (survol = détail, clic = épingler,
#                                               légende cliquable) ; JavaScript natif inline
#               carte_typologie_courriel.html   AUCUN contenu actif (envoi par messagerie) :
#                                               infobulles natives, tableau des situations
#               carte_typologie.png             image de la carte (si CARTES_PNG ; jamais bloquant)
#             Objet `carte_typologie` (bilan).
# LANCEMENT : dans main.R (après 08f) OU SEUL, après la chaîne, depuis la racine
#             du projet :  source("R/08_territoires/08g_carte_typologie.R")  —
#             il charge alors lui-même la configuration et les fonctions.
# SÉCURITÉ  : source absolue = table de diffusion ; une table sans colonne
#             `masque` est refusée ; un département masqué n'embarque que son
#             statut ; contrôle bloquant avant écriture. Un code sans géométrie
#             = ARRÊT avec la liste ; « inconnu » = hors carte, compté.
# ==============================================================================
if (!exists("GENERER_TYPOLOGIE") || !exists("generer_carte_typologie")) {   # lancement seul : charger la configuration et les fonctions
  if (!file.exists("main.R")) stop("08g : lancez ce script depuis la racine du projet (dossier contenant main.R).")
  if (!exists("GENERER_TYPOLOGIE")) {                                        # configuration absente : 00_config.R ; un DIR_SORTIES défini avant est respecté
    dir_prealable_08g <- get0("DIR_SORTIES", ifnotfound = NULL)
    source(file.path("R", "00_config_fonctions", "00_config.R"))
    if (!is.null(dir_prealable_08g)) DIR_SORTIES <- dir_prealable_08g
    rm(dir_prealable_08g)
  }
  if (!exists("arrondir_nombre_personnes")) source(file.path("R", "00_config_fonctions", "00g_format_restitution.R"))
  if (!exists("charger_fond_carte")) source(file.path("R", "00_config_fonctions", "00f_fonctions_cartes.R"))
  if (!exists("PROFILS_TYPOLOGIE")) source(file.path("R", "00_config_fonctions", "00h_fonctions_typologie.R"))
  if (!exists("generer_carte_typologie")) source(file.path("R", "00_config_fonctions", "00i_fonctions_carte_typologie.R"))
}
if (!isTRUE(get0("GENERER_CARTE_TYPOLOGIE", ifnotfound = TRUE))) {
  message("08g : carte de la typologie désactivée (GENERER_CARTE_TYPOLOGIE = FALSE).")
} else {
  library(dplyr)
  src_08g <- file.path(DIR_SORTIES, "typologie_territoriale", "diffusion", "typologie_departements.csv")
  if (!file.exists(src_08g)) {
    message("08g : table de diffusion de la typologie absente, cartes non produites — attendu : ", src_08g,
            " (exécutez 08f avec GENERER_TYPOLOGIE = TRUE, TYPO_SECRET = TRUE et GEO_ANALYSE = \"departement\").")
    carte_typologie <- tibble(source = src_08g, statut = "source absente", carte = NA_character_, courriel = NA_character_, png = NA_character_)
  } else {
    dir_08g <- file.path(DIR_SORTIES, "typologie_territoriale", "cartes")
    champ_08g <- construire_libelle_champ()
    source_note_08g <- if (identical(get0("SOURCE_BTS", ifnotfound = ""), "parquet")) "BTS 2024, DREES, EACR, Insee — calculs propres" else "table test — calculs propres"
    cat("\n08g — cartes des profils de la typologie\n  source : ", src_08g, " (", format(file.info(src_08g)$mtime, "%Y-%m-%d %H:%M"), ")\n", sep = "")
    fond_08g <- charger_fond_carte("departement"); fond_reg_08g <- charger_fond_carte("region")
    prep_08g <- preparer_carte_typologie(lire_typologie_diffusion(src_08g), fond_08g)
    controler_carte_typologie(prep_08g)
    html_08g <- file.path(dir_08g, "carte_typologie.html")
    png_08g <- if (isTRUE(get0("CARTES_PNG", ifnotfound = TRUE))) file.path(dir_08g, "carte_typologie.png") else NULL
    generer_carte_typologie(prep_08g, fond_08g, html_08g, champ = champ_08g, fond_regions = fond_reg_08g, source_note = source_note_08g, fichier_png = png_08g)
    courriel_08g <- NA_character_
    if (isTRUE(get0("CARTES_COURRIEL", ifnotfound = TRUE))) {
      courriel_08g <- file.path(dir_08g, "carte_typologie_courriel.html")
      generer_carte_typologie_courriel(prep_08g, fond_08g, courriel_08g, champ = champ_08g, fond_regions = fond_reg_08g, source_note = source_note_08g)
    }
    carte_typologie <- tibble(source = src_08g, statut = "ok", carte = html_08g, courriel = courriel_08g,
                              png = if (!is.null(png_08g) && file.exists(png_08g)) png_08g else NA_character_)
    for (k in names(prep_08g$profils)) cat(sprintf("  %-40s %3d département(s)\n", prep_08g$profils[[k]], prep_08g$comptes[[k]]))
    message("08g OK -> ", dir_08g, " : carte_typologie.html", if (!is.na(courriel_08g)) " + carte_typologie_courriel.html (sans script)" else "",
            if (!is.na(carte_typologie$png)) " + carte_typologie.png" else "")
    rm(dir_08g, champ_08g, source_note_08g, fond_08g, fond_reg_08g, prep_08g, html_08g, png_08g, courriel_08g)
  }
  rm(src_08g)
}
