# ==============================================================================
# 00i_fonctions_carte_typologie.R — Carte des profils de la typologie (fonctions pures)
# ------------------------------------------------------------------------------
# Rôle : transformer la table de DIFFUSION de la typologie (08f,
#        sorties/typologie_territoriale/diffusion/typologie_departements.csv) en
#        deux pages HTML autonomes (fond local, aucune ressource réseau) :
#          - carte_typologie.html : carte interactive (JavaScript natif inline) :
#            survol = infobulle + panneau « situation » rédigé, clic = épingler,
#            légende cliquable pour isoler un profil ;
#          - carte_typologie_courriel.html : AUCUN contenu actif (SISMEL) : mêmes
#            couleurs écrites en attributs, infobulles natives <title> du SVG,
#            tableau de toutes les situations sous la carte ;
#        + un PNG ggplot2 de la carte (facultatif, jamais bloquant).
# SOURCE ABSOLUE : la table de diffusion (secret appliqué par 08f). Une table
#        sans colonne `masque` (interne/) est refusée ; les mesures et textes
#        d'un département masqué sont effacés AVANT toute sérialisation.
# RÉUTILISE 00f (fond, projection Lambert-93, encarts DROM, CSS, contours,
#        libellés publics, champ) et 00h (titres, définitions des profils).
# Spécification : specs/10 §9 (SPEC-TYPO-060 à 064).
# ==============================================================================
library(dplyr)

# --- Couleurs des profils (catégorielles, distinctes du gris du secret et du blanc) --
COULEURS_PROFILS <- c(enjeu = "#b2182b", emergent = "#ef8a62", majeur_modere = "#2166ac", concentre = "#8c6bb1",
                      stable = "#74c476", diffus = "#dfe6ee", expertiser = "#f2c744")
LIBELLE_NON_DIFFUSE_TYPO <- "Non diffusé (secret statistique)"
LIBELLES_CARTE_TYPO <- list(
  kicker = texte_perimetre("Typologie des territoires Base · résultats diffusables"),
  titre = "Profils de renouvellement par département",
  sous_titre = texte_perimetre("Chaque département est classé par des règles simples (poids dans l’emploi Base, volume et intensité des départs attendus d’ici 2030, concentration sur une catégorie)."),
  profil_non_diffuse = "Profil non diffusé (secret statistique)",
  absent = "Département absent de la typologie",
  detail_initial = "Survolez un département pour lire sa situation ; cliquez pour la garder affichée.",
  detail_non_diffuse = "Le détail de ce département n’est pas diffusé (secret statistique).",
  tableau_titre = "Le détail par département",
  definitions_titre = "Que signifient les profils ?",
  apropos = "À propos de cette carte", retour_carte = "↑ Revenir à la carte")
COLONNES_NUM_TYPO <- c("effectif_base_total", "effectif_champ_total", "effectif_tous_ages_total", "departs_central", "departs_bas", "departs_haut",
                       "part_emploi_base_national_pct", "intensite_renouvellement_pct", "taux_depart_central_pct", "part_a_remplacer_pct",
                       "part_cs_principale_pct", "part_departs_cs_max_pct",
                       "pyramide_jeunes_pct", "pyramide_milieu_pct", "pyramide_vague_pct", "pyramide_seniors_pct", "ratio_releve", "ratio_vague_seniors",
                       "tension_exposition_pct", "tension_exposition_france_pct", "tension_couverture_pct",
                       "tension_departs_min", "tension_departs_min_bas", "tension_departs_min_haut", "tension_part_departs_min_pct")
COLONNES_TEXTE_TYPO <- c("profil_typologie", "classe_poids_base", "classe_intensite", "classe_volume_departs", "structure_emploi", "cs_principale",
                         "cs_volume_departs_max", "cs_taux_renouvellement_max", "type_concentration", "motif_expertise", "points_attention",
                         "situation_texte", "justification_profil", "indicateur_intensite", "base_poids",
                         "pyramide_source", "pyramide_libelles", "classe_releve", "classe_vague_suivante", "pyramide_masquee",
                         "tension_diffusee", "tension_classe", "tension_robustesse", "tension_causes", "tension_levier", "tension_composante", "tension_metiers",
                         "tension_cumul", "tension_texte")

# --- Lecture de la table de diffusion (write.csv2, avec ou sans BOM) --------------
lire_typologie_diffusion <- function(fichier) {
  if (!file.exists(fichier)) stop("Table de diffusion de la typologie introuvable : ", fichier)
  d <- read.csv2(fichier, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character", na.strings = c("NA", ""))
  names(d)[1] <- sub("^﻿", "", names(d)[1])
  for (v in intersect(COLONNES_NUM_TYPO, names(d))) d[[v]] <- as.numeric(sub(",", ".", d[[v]], fixed = TRUE))
  if ("masque" %in% names(d)) d$masque <- toupper(trimws(d$masque)) == "TRUE"
  # exposition au recrutement difficile (08i, specs/10 §13) : fichier VOISIN de diffusion, facultatif
  f_expo <- file.path(dirname(fichier), "exposition_tension_departements.csv")
  if (file.exists(f_expo) && "geo_code" %in% names(d)) {
    e <- read.csv2(f_expo, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character", na.strings = c("NA", ""))
    names(e)[1] <- sub("^\ufeff", "", names(e)[1])
    e <- e[, intersect(c("geo_code", grep("^tension_", names(e), value = TRUE)), names(e)), drop = FALSE]
    for (v in intersect(COLONNES_NUM_TYPO, names(e))) e[[v]] <- as.numeric(sub(",", ".", e[[v]], fixed = TRUE))
    d <- d[, setdiff(names(d), setdiff(names(e), "geo_code")), drop = FALSE] |> left_join(e, by = "geo_code")
  }
  d
}

# --- Préparation : table de diffusion -> structure de carte (sans aucune fuite) -----
# Statuts : diffuse (profil + chiffres), profil_masque (chiffres diffusés mais profil
# « Non diffusé » : cellule CS masquée), masque (département entier), sans (absent).
preparer_carte_typologie <- function(d, fond, profils = PROFILS_TYPOLOGIE) {
  if (!"masque" %in% names(d)) stop("preparer_carte_typologie : colonne 'masque' absente — seule la table de DIFFUSION est admise (jamais interne/).")
  requis <- c("geo_code", "geo_nom", "profil_typologie", "effectif_base_total", "departs_central", "departs_bas", "departs_haut",
              "part_emploi_base_national_pct", "intensite_renouvellement_pct")
  manque <- setdiff(requis, names(d)); if (length(manque)) stop("preparer_carte_typologie : colonnes absentes : ", paste(manque, collapse = ", "))
  if (!is.character(d$geo_code)) stop("preparer_carte_typologie : geo_code doit être un texte.")
  d$masque <- as.logical(d$masque); d$masque[is.na(d$masque)] <- TRUE          # doute = masqué
  for (v in setdiff(c(COLONNES_NUM_TYPO, COLONNES_TEXTE_TYPO), names(d))) d[[v]] <- NA
  # SÉCURITÉ : toute mesure et tout texte d'une ligne masquée sont retirés ici
  for (v in c(COLONNES_NUM_TYPO, setdiff(COLONNES_TEXTE_TYPO, "profil_typologie"))) d[[v]][d$masque] <- NA
  d$profil_typologie[d$masque] <- LIBELLE_NON_DIFFUSE_TYPO
  non_loc <- is.na(d$geo_code) | d$geo_code %in% c("inconnu", "")
  n_non_loc <- length(unique(d$geo_code[non_loc])); d <- d[!non_loc, ]
  if (anyDuplicated(d$geo_code) > 0) stop("preparer_carte_typologie : doublons de département.")
  absents <- setdiff(d$geo_code, names(fond))
  if (length(absents) > 0) stop("Carte de la typologie : ", length(absents), " code(s) sans géométrie dans le fond : ", paste(sort(absents), collapse = ", "), " — aucune carte partielle.")
  inconnus <- setdiff(unique(d$profil_typologie), c(unname(profils), LIBELLE_NON_DIFFUSE_TYPO))
  if (length(inconnus) > 0) stop("Carte de la typologie : profil(s) inconnu(s) dans la table : ", paste(inconnus, collapse = " ; "),
                                 " — la table et le code (PROFILS_TYPOLOGIE, 00h) ne sont pas de la même version.")
  d$cle <- names(profils)[match(d$profil_typologie, profils)]
  d$statut <- ifelse(d$masque, "masque", ifelse(is.na(d$cle), "profil_masque", "diffuse"))
  terr <- tibble(code = names(fond), nom = unname(vapply(fond, `[[`, "", "nom"))) |>
    left_join(d |> select(code = geo_code, nom_csv = geo_nom, statut, cle, all_of(c(COLONNES_NUM_TYPO, COLONNES_TEXTE_TYPO))), by = "code") |>
    mutate(nom = coalesce(nom_csv, nom), statut = coalesce(statut, "sans")) |> select(-nom_csv)
  comptes <- c(vapply(names(profils), function(k) sum(terr$cle %in% k), 0L),
               profil_masque = sum(terr$statut == "profil_masque"), masque = sum(terr$statut == "masque"), sans = sum(terr$statut == "sans"),
               hachures = sum(terr$tension_classe %in% "Forte" & terr$statut %in% c("diffuse", "profil_masque")))
  premier <- function(v) { x <- terr[[v]][!is.na(terr[[v]])]; if (length(x)) x[1] else NA_character_ }
  list(territoires = terr, comptes = comptes, n_non_localises = n_non_loc, n_csv = nrow(d),
       indicateur = premier("indicateur_intensite"), base_poids = premier("base_poids"), profils = profils)
}
controler_carte_typologie <- function(prep, prefixe = "08g") {
  t <- prep$territoires
  if (sum(t$statut != "sans") != prep$n_csv) stop(prefixe, " : départements du CSV non joints au fond.")
  m <- t$statut == "masque"
  if (any(m) && any(!is.na(as.matrix(t[m, c(COLONNES_NUM_TYPO, setdiff(COLONNES_TEXTE_TYPO, "profil_typologie"))]))))
    stop(prefixe, " : une valeur masquée subsiste dans la structure de carte.")
  cat(sprintf("  typologie x departement : départements CSV %3d | joints %3d | profil non diffusé %3d | masqués %3d | sans donnée %3d%s\n",
              prep$n_csv, sum(t$statut != "sans"), prep$comptes[["profil_masque"]], prep$comptes[["masque"]], prep$comptes[["sans"]],
              if (prep$n_non_localises > 0) sprintf(" | non localisables %d", prep$n_non_localises) else ""))
  invisible(prep$comptes)
}

# --- Détail d'un département : des phrases courtes (infobulle, <title>, tableau) -----
libelle_intensite_typo <- function(indicateur) if (identical(indicateur, "taux_depart_central_pct")) "des salariés concernés susceptibles de partir d’ici 2030" else "de l’emploi actuel à remplacer d’ici 2030"
lignes_detail_typologie <- function(r, indicateur = NULL) {
  if (r$statut == "sans") return(LIBELLES_CARTE_TYPO$absent)
  if (r$statut == "masque") return(LIBELLES_UI$secret)
  f0 <- function(x) fmt_fr_carte(x, 0); f1 <- function(x) fmt_fr_carte(x, 1)
  ind <- if (!is.null(indicateur)) indicateur else r$indicateur_intensite
  l <- c(if (r$statut == "profil_masque") LIBELLES_CARTE_TYPO$profil_non_diffuse else NULL,
         if (!is.na(r$part_emploi_base_national_pct)) sprintf(texte_perimetre("%s %% de l’emploi Base national · %s salariés"), f1(r$part_emploi_base_national_pct), f0(r$effectif_base_total)) else NULL,
         if (!is.na(r$departs_central)) sprintf("%s départs estimés d’ici 2030 (entre %s et %s)", f0(r$departs_central), f0(r$departs_bas), f0(r$departs_haut)) else NULL,
         if (!is.na(r$intensite_renouvellement_pct)) sprintf("%s %% %s", f1(r$intensite_renouvellement_pct), libelle_intensite_typo(ind)) else NULL,
         if (!is.na(r$classe_poids_base)) sprintf("Poids %s · volume %s · intensité %s", tolower(r$classe_poids_base), tolower(r$classe_volume_departs), tolower(r$classe_intensite)) else NULL,
         if (!is.na(r$structure_emploi)) { if (r$structure_emploi == "Mixte") sprintf("Emploi mixte (première catégorie : %s, %s %%)", libelle_cs_typo(r$cs_principale), f1(r$part_cs_principale_pct))
                                           else sprintf("Emploi dominé par les %s (%s %%)", libelle_cs_typo(r$cs_principale), f1(r$part_cs_principale_pct)) } else NULL,
         if (!is.na(r$cs_volume_departs_max)) sprintf("Départs portés d’abord par les %s (%s %%%s)", libelle_cs_typo(r$cs_volume_departs_max), f1(r$part_departs_cs_max_pct),
                                                     if (identical(r$type_concentration, "Concentré")) ", concentrés sur cette catégorie" else "") else NULL,
         ligne_pyramide_typologie(r),
         ligne_tension_typologie(r),
         if (!is.na(r$motif_expertise)) paste0("À examiner : ", r$motif_expertise) else NULL,
         if (!is.na(r$points_attention)) paste0("Point(s) d’attention : ", r$points_attention) else NULL)
  if (length(l) == 0) LIBELLES_CARTE_TYPO$profil_non_diffuse else l
}
# Pyramide des âges (axe descriptif, specs/10 §12) : une ligne, ou rien si absente ; jamais une tranche masquée.
libelles_pyramide_typo <- function(r) if (is.null(r[["pyramide_libelles"]]) || is.na(r$pyramide_libelles)) NULL else strsplit(r$pyramide_libelles, " | ", fixed = TRUE)[[1]]
parts_pyramide_typo <- function(r) {   # NULL si la pyramide complète n'est pas diffusée
  v <- vapply(c("pyramide_jeunes_pct", "pyramide_milieu_pct", "pyramide_vague_pct", "pyramide_seniors_pct"), function(k) if (is.null(r[[k]])) NA_real_ else as.numeric(r[[k]]), 0)
  lib <- libelles_pyramide_typo(r)
  if (anyNA(v) || length(lib) != 4) NULL else list(libelles = lib, parts = unname(v))
}
ligne_pyramide_typologie <- function(r) {
  f1 <- function(x) fmt_fr_carte(x, 1)
  if (isTRUE(as.logical(r[["pyramide_masquee"]]))) return("Pyramide des âges non diffusée (secret statistique)")
  py <- parts_pyramide_typo(r)
  if (!is.null(py)) {
    rel <- if (!is.null(r[["ratio_releve"]]) && !is.na(r$ratio_releve))
      paste0(" · ", phrase_rapport_salaries(r$ratio_releve, paste("de", py$libelles[1]), paste("de", py$libelles[4])),
             if (!is.null(r[["classe_releve"]]) && !is.na(r$classe_releve)) paste0(" (", tolower(r$classe_releve), ")") else "") else ""
    return(paste0("Pyramide des âges : ", paste(sprintf("%s %% %s", vapply(py$parts, f1, ""), py$libelles), collapse = " · "), rel))
  }
  lib <- libelles_pyramide_typo(r)
  if (!is.null(r[["ratio_vague_seniors"]]) && !is.na(r$ratio_vague_seniors) && length(lib) == 4)
    return(sprintf("Au sein du champ : %s (pyramide complète indisponible)", phrase_rapport_salaries(r$ratio_vague_seniors, paste("de", lib[3]), paste("de", lib[4]))))
  NULL
}
# Puces de la pyramide pour le panneau (rubrique « Pyramide des âges »)
puces_pyramide_typo <- function(r) {
  f0 <- function(x) fmt_fr_carte(x, 0)
  if (isTRUE(as.logical(r[["pyramide_masquee"]]))) return("Non diffusée (secret statistique)")
  py <- parts_pyramide_typo(r); lib <- libelles_pyramide_typo(r)
  if (!is.null(py)) {
    c(paste(sprintf("%s : %s %%", sub("^m", "M", py$libelles), vapply(py$parts, f0, "")), collapse = " · "),
      if (!is.null(r[["ratio_releve"]]) && !is.na(r$ratio_releve)) paste0("Relève : ", phrase_rapport_salaries(r$ratio_releve, paste("de", py$libelles[1]), paste("de", py$libelles[4])),
                                                                         if (!is.null(r[["classe_releve"]]) && !is.na(r$classe_releve)) paste0(" (", tolower(r$classe_releve), ")") else ""),
      if (!is.null(r[["classe_vague_suivante"]]) && !is.na(r$classe_vague_suivante)) sprintf("Vague de départs après 2030 : part des %s %s (%s %%)", py$libelles[3], tolower(r$classe_vague_suivante), f0(py$parts[3])))
  } else if (!is.null(r[["ratio_vague_seniors"]]) && !is.na(r$ratio_vague_seniors) && length(lib) == 4)
    sprintf("Au sein du champ : %s (pyramide complète indisponible)", phrase_rapport_salaries(r$ratio_vague_seniors, paste("de", lib[3]), paste("de", lib[4])))
  else NULL
}
# Puces du recrutement pour le panneau (rubrique « Recrutement », 08i)
puces_tension_typo <- function(r) {
  dif <- r[["tension_diffusee"]]; if (is.null(dif)) return(NULL)
  dif <- as.logical(dif); if (is.na(dif)) return(NULL)
  if (!dif) return("Part des remplacements difficiles non diffusée (secret statistique)")
  f0 <- function(x) fmt_fr_carte(x, 0)
  if (is.na(r$tension_exposition_pct) || is.na(r$tension_classe))
    return(if (!is.na(r$tension_couverture_pct)) sprintf("Les métiers stratégiques portent %s %% des départs : part des remplacements difficiles non interprétée", f0(r$tension_couverture_pct)) else NULL)
  c(sprintf("Sur 100 départs des métiers stratégiques, %s concernent des métiers en tension en France (France : %s)", f0(r$tension_exposition_pct), f0(r$tension_exposition_france_pct)),
    sprintf("Part des remplacements difficiles %s, %s", tolower(r$tension_classe), if (identical(r$tension_robustesse, "robuste")) "résultat robuste" else paste0("résultat à confirmer", if (!is.na(r$tension_causes)) paste0(" (sensible : ", r$tension_causes, ")") else "")),
    if (!is.na(r$tension_couverture_pct)) sprintf("Les métiers stratégiques suivis portent %s %% des départs du département ; tension des autres métiers non observée", f0(r$tension_couverture_pct)),
    ligne_plancher_tension_typo(r, "des départs du département"),
    if (!is.na(r$tension_levier)) sprintf("Levier le plus fréquent : %s (%s)", r$tension_levier, r$tension_composante),
    if (!is.na(r$tension_metiers)) paste0("Métiers en tension les plus concernés : ", r$tension_metiers))
}
# --- Rubriques du panneau (SPEC-TYPO-062) : construites par règles à partir des seules colonnes de DIFFUSION ---
# Retourne une liste de rubriques list(t = titre, p = puces, b = dans l'infobulle) ; vide pour un territoire masqué ou absent.
blocs_detail_typologie <- function(r, indicateur = NULL) {
  if (r$statut %in% c("sans", "masque")) return(list())
  f0 <- function(x) fmt_fr_carte(x, 0); f1 <- function(x) fmt_fr_carte(x, 1)
  ind <- if (!is.null(indicateur)) indicateur else r$indicateur_intensite
  # b = la rubrique figure dans l'infobulle ; n_bulle = nombre de puces qu'elle y montre (le panneau les montre toutes)
  bloc <- function(titre, puces, bulle = TRUE, n_bulle = NULL) { puces <- puces[!is.na(puces) & nzchar(puces)]; if (!length(puces)) return(NULL)
    x <- list(t = titre, p = as.list(unname(puces)), b = bulle); if (!is.null(n_bulle) && length(puces) > n_bulle) x$pb <- as.list(unname(puces[seq_len(n_bulle)])); x }
  defs <- DEFINITIONS_PROFILS; cle <- r[["cle"]]
  poids <- bloc(texte_perimetre("Poids dans l’emploi Base"), c(
    if (!is.na(r$part_emploi_base_national_pct)) sprintf(texte_perimetre("%s %% de l’emploi Base national, soit %s salariés"), f1(r$part_emploi_base_national_pct), f0(r$effectif_base_total)),
    if (!is.na(r$classe_poids_base)) sprintf("Poids %s", tolower(r$classe_poids_base))))
  departs <- bloc("Départs attendus d’ici 2030", c(
    if (!is.na(r$departs_central)) sprintf("%s départs estimés (entre %s et %s selon l’hypothèse de départ en retraite)", f0(r$departs_central), f0(r$departs_bas), f0(r$departs_haut)),
    if (!is.na(r$intensite_renouvellement_pct)) sprintf("%s %% %s", f1(r$intensite_renouvellement_pct), libelle_intensite_typo(ind)),
    if (!is.na(r$classe_volume_departs) && !is.na(r$classe_intensite)) sprintf("Volume de départs %s, intensité de renouvellement %s", tolower(r$classe_volume_departs), tolower(r$classe_intensite))))
  categories <- bloc("Emploi par grande catégorie", c(
    if (!is.na(r$structure_emploi)) { if (r$structure_emploi == "Mixte") sprintf("Emploi réparti sans dominante ; première catégorie : %s (%s %%)", libelle_cs_typo(r$cs_principale), f1(r$part_cs_principale_pct))
                                      else sprintf("Emploi dominé par les %s (%s %%)", libelle_cs_typo(r$cs_principale), f1(r$part_cs_principale_pct)) },
    if (!is.na(r$cs_volume_departs_max)) sprintf("Les %s portent %s %% des départs%s", libelle_cs_typo(r$cs_volume_departs_max), f1(r$part_departs_cs_max_pct),
                                                 if (identical(r$type_concentration, "Concentré")) " : renouvellement concentré sur cette catégorie" else if (identical(r$type_concentration, "Diffus")) ", sans concentration marquée" else ""),
    if (!is.na(r$cs_taux_renouvellement_max) && !identical(r$cs_taux_renouvellement_max, r$cs_volume_departs_max)) sprintf("Part à remplacer la plus élevée chez les %s", libelle_cs_typo(r$cs_taux_renouvellement_max))), bulle = FALSE)
  pyramide <- bloc("Pyramide des âges", puces_pyramide_typo(r), n_bulle = 2)
  recrutement <- bloc("Recrutement", puces_tension_typo(r), n_bulle = 2)
  lecture <- bloc("Lecture", c(
    if (r$statut == "profil_masque") LIBELLES_CARTE_TYPO$profil_non_diffuse else if (!is.na(cle)) sprintf("Profil « %s » : %s", PROFILS_TYPOLOGIE[[cle]], defs$signification[defs$cle == cle]),
    if (!is.na(r$motif_expertise)) paste0("À examiner : ", r$motif_expertise),
    if (!is.na(r$points_attention)) paste0("Point d’attention : ", strsplit(r$points_attention, " ; ", fixed = TRUE)[[1]]),
    if (!is.na(cle)) paste0("Consigne de lecture : ", defs$attention[defs$cle == cle])), bulle = FALSE)
  Filter(Negate(is.null), list(poids, departs, categories, pyramide, recrutement, lecture))
}
# Exposition au recrutement difficile (08i, specs/10 §13) : rien si la colonne est absente
ligne_tension_typologie <- function(r) {
  dif <- r[["tension_diffusee"]]; if (is.null(dif)) return(NULL)
  dif <- as.logical(dif); if (is.na(dif)) return(NULL)
  if (!dif) return("Part des remplacements difficiles non diffusée (secret statistique)")
  f0 <- function(x) fmt_fr_carte(x, 0)
  if (is.na(r$tension_exposition_pct) || is.na(r$tension_classe))
    return(if (!is.na(r$tension_couverture_pct)) sprintf("Métiers stratégiques : %s %% des départs — part des remplacements difficiles non interprétée", f0(r$tension_couverture_pct)) else NULL)
  c(sprintf("Remplacements difficiles : %s %% des départs des métiers stratégiques (France : %s %%) · part %s · %s", f0(r$tension_exposition_pct),
            f0(r$tension_exposition_france_pct), tolower(r$tension_classe), if (identical(r$tension_robustesse, "robuste")) "robuste" else "à confirmer"),
    if (!is.na(r$tension_couverture_pct)) sprintf("Métiers suivis : %s %% des départs ; tension des autres métiers non observée", f0(r$tension_couverture_pct)),
    ligne_plancher_tension_typo(r, "des départs"),
    if (!is.na(r$tension_metiers)) paste0("Métiers en tension les plus concernés : ", r$tension_metiers) else NULL)
}
# Planchers (SPEC-TYPO-109) : départs attendus dans les métiers suivis en tension et leur part dans TOUS les départs ; NULL si absents
ligne_plancher_tension_typo <- function(r, de = "des départs du département") {
  v <- r[["tension_departs_min"]]; if (is.null(v) || is.na(v)) return(NULL)
  f0 <- function(x) fmt_fr_carte(x, 0); f1 <- function(x) fmt_fr_carte(x, 1)
  sprintf("Au moins %s départs attendus dans des métiers suivis en tension (de %s à %s), soit au moins %s %% %s", f0(v), f0(r[["tension_departs_min_bas"]]), f0(r[["tension_departs_min_haut"]]), f1(r[["tension_part_departs_min_pct"]]), de)
}
titre_profil_typo <- function(r, prep) if (!is.na(r$cle)) prep$profils[[r$cle]] else if (r$statut == "sans") LIBELLES_CARTE_TYPO$absent else LIBELLE_NON_DIFFUSE_TYPO
couleur_territoire_typo <- function(r) if (!is.na(r$cle)) COULEURS_PROFILS[[r$cle]] else if (r$statut == "sans") COULEURS_CARTE$sans_donnee else COULEURS_CARTE$secret

# --- Sérialisation JSON (page interactive) : aucun chiffre d'un département masqué ---
donnees_json_typologie <- function(prep) {
  t <- prep$territoires; defs <- DEFINITIONS_PROFILS
  terr <- setNames(lapply(seq_len(nrow(t)), function(i) { r <- t[i, ]
    x <- list(nom = r$nom, st = r$statut, lignes = as.list(lignes_detail_typologie(r, prep$indicateur)), blocs = blocs_detail_typologie(r, prep$indicateur))
    if (!is.na(r$cle)) x$cle <- r$cle
    txt <- c(r$situation_texte, r$tension_texte); txt <- txt[!is.na(txt)]
    if (length(txt)) x$texte <- paste(txt, collapse = " ")
    x }), t$code)
  profils <- setNames(lapply(names(prep$profils), function(k) list(titre = prep$profils[[k]], couleur = COULEURS_PROFILS[[k]], n = prep$comptes[[k]],
                                                                   signification = defs$signification[defs$cle == k], consigne = defs$attention[defs$cle == k])), names(prep$profils))
  j <- jsonlite::toJSON(list(territoires = terr, profils = profils, comptes = as.list(prep$comptes), libelles = LIBELLES_CARTE_TYPO,
                             couleurs = COULEURS_CARTE[c("secret", "sans_donnee")]), auto_unbox = TRUE, digits = NA, na = "null", null = "null")
  gsub("</", "<\\/", j, fixed = TRUE)
}

# --- CSS et JavaScript propres à la carte de la typologie ---------------------------
css_carte_typologie <- function(S = STYLE_CONTOURS_CARTE) paste(
  '.typo-legende{list-style:none;margin:0;padding:0}.typo-legende li{margin:4px 0}',
  '.typo-legende button,.typo-legende .item{display:flex;align-items:flex-start;gap:10px;width:100%;text-align:left;font:inherit;font-size:15px;color:var(--encre);background:#fff;border:1px solid transparent;border-radius:8px;padding:6px 8px;cursor:pointer}',
  '.typo-legende .item{cursor:default}.typo-legende button:hover,.typo-legende button:focus-visible{border-color:#AEB7C2;outline:none}.typo-legende button[aria-pressed=true]{background:var(--fond);border-color:var(--bleu)}',
  sprintf('.typo-legende .sw{width:22px;height:16px;border-radius:3px;border:1px solid %s;flex:none;margin-top:3px}.typo-legende .sw.sans{border:1.5px dashed %s}', S$couleur, S$sans_couleur),
  '.typo-legende .lib{font-weight:600;line-height:1.3}.typo-legende .n{color:var(--texte);font-weight:500;margin-left:6px}.typo-legende .sig{display:block;font-size:13px;color:var(--texte);font-weight:400;line-height:1.35;margin-top:1px}',
  'path.t.dim,path.h.dim{opacity:.22}.typo-legende .sw.sw-hachures{background:repeating-linear-gradient(45deg,#1F2933 0 2px,#fff 2px 6px)}.legende-hachures{margin-top:6px!important;padding-top:6px!important;border-top:1px solid var(--filet)}',
  # infobulle FIXÉE au viewport : elle ne peut ni agrandir la zone défilable ni déplacer le contenu (stabilité au survol et au défilement)
  '.bulle{position:fixed;left:0;top:0;max-height:calc(100vh - 16px);overflow:hidden}',
  # panneau de détail : hauteur rendue STABLE par le script (hauteur du texte le plus long, bornée) ; défilement interne au-delà, jamais de troncature
  '.detail{margin:0 0 16px;border:1px solid var(--filet);border-radius:12px;padding:14px 16px;background:var(--fond);min-height:120px;box-sizing:border-box;overflow-y:auto;scrollbar-width:thin}',
  # panneau défilable (texte plus long que la hauteur stabilisée) : fondu en bas pour signaler la suite, jamais de troncature
  '.detail.defilable{-webkit-mask-image:linear-gradient(#000 calc(100% - 34px),transparent);mask-image:linear-gradient(#000 calc(100% - 34px),transparent)}.detail.defilable.bas{-webkit-mask-image:none;mask-image:none}',
  '.detail-titre{font-size:16px;font-weight:700;margin:0 0 6px}.detail-titre .pf{display:inline-block;margin-left:6px;padding:1px 8px;border-radius:999px;font-size:13px;font-weight:600;color:#fff;background:#6b7280}',
  '.detail-texte{font-size:15px;line-height:1.5;margin:0}.detail-aide{font-size:13.5px;color:var(--texte);margin:8px 0 0}',
  # rubriques et puces du panneau (SPEC-TYPO-062) ; rubriques de l'infobulle
  '.detail h3.rub{font-size:12px;letter-spacing:.05em;text-transform:uppercase;color:var(--bleu);margin:12px 0 3px;font-weight:700}.detail h3.rub:first-child{margin-top:0}.detail ul.puces{margin:0;padding-left:18px;font-size:14.5px;line-height:1.45}.detail ul.puces li{margin:3px 0}',
  '.tooltip-rub{font-weight:700;font-size:11.5px;letter-spacing:.05em;text-transform:uppercase;margin:8px 0 2px;opacity:.85}.tooltip-rub:first-child{margin-top:0}',
  '.tooltip-profil{font-weight:700;margin:0 0 8px;padding-left:9px;border-left:4px solid #6b7280;color:#fff}',
  'details.bloc-typo{margin-top:20px;border:1px solid var(--filet);border-radius:12px;padding:0 18px 4px}details.bloc-typo summary{cursor:pointer;font-size:17px;font-weight:700;padding:14px 0;list-style:none}details.bloc-typo summary::-webkit-details-marker{display:none}details.bloc-typo summary::before{content:"▸ ";color:var(--bleu)}details.bloc-typo[open] summary::before{content:"▾ "}',
  '.tab-typo{width:100%;border-collapse:collapse;font-size:14px;margin:0 0 14px}.tab-typo th,.tab-typo td{text-align:left;vertical-align:top;padding:8px 8px;border-top:1px solid var(--filet);line-height:1.45}.tab-typo th{font-size:13px;color:var(--texte);letter-spacing:.03em;text-transform:uppercase;border-top:0}.tab-typo td.num{white-space:nowrap;text-align:right}.tab-typo .sw{display:inline-block;width:12px;height:12px;border-radius:2px;vertical-align:-1px;margin-right:6px;border:1px solid #5B6470}.tab-typo td.sit{min-width:280px}',
  '.tab-defs td:first-child{white-space:nowrap;font-weight:700}@media(max-width:700px){.tab-typo td.sit{min-width:0}.tab-typo th,.tab-typo td{padding:6px 5px}}',
  sprintf('.courriel-typo use{stroke:%s;stroke-width:%s;vector-effect:non-scaling-stroke}.courriel-typo use:hover{stroke:%s;stroke-width:%s}', S$couleur, S$largeur, S$survol_couleur, S$survol_largeur),
  # PREMIER ÉCRAN des pages interactives (08g, 08h ; SPEC-TYPO-065) : en-tête compact, présentation repliée
  '.ecran .page-header{padding-bottom:10px;margin-bottom:12px}.ecran .kicker{font-size:12px;margin:0 0 2px}.ecran .page-title{font-size:24px}.ecran .page-title small{display:inline;font-size:16px;margin-left:10px}',
  '.apropos{margin:6px 0 0}.apropos summary{cursor:pointer;color:var(--bleu);font-weight:600;font-size:14px;list-style:none;display:inline-block;padding:2px 6px;border-radius:6px}.apropos summary::-webkit-details-marker{display:none}.apropos summary::before{content:"▸ "}.apropos[open] summary::before{content:"▾ "}.apropos summary:hover,.apropos summary:focus-visible{background:#e3eaf2;outline:none}',
  # légende compacte : titre et nombre sur une ligne ; la signification reste dans le HTML (title du bouton, panneau au clic, définitions)
  '.ecran .typo-legende .sig{display:none}.ecran .typo-legende li{margin:1px 0}.ecran .typo-legende button,.ecran .typo-legende .item{padding:2px 6px;font-size:13.5px}.ecran .typo-legende .sw{margin-top:2px}.ecran .legend-title{margin:0 0 4px}.ecran .legende-bloc .note{margin-top:6px}',
  '.retour-carte{display:none}',
  # écran LARGE : en-tête + scène = hauteur de la fenêtre ; carte entière dans sa colonne ; seul le panneau défile (hauteur fixe)
  '@media(min-width:901px){.page-ecran{padding-top:10px}.ecran{display:flex;flex-direction:column;height:calc(100vh - 22px);min-height:560px}@supports(height:100dvh){.ecran{height:calc(100dvh - 22px)}}',
  '.ecran .page-header{flex:none}.ecran .scene{flex:1 1 auto;min-height:0;grid-template-columns:400px minmax(0,1fr);gap:20px;align-items:stretch}',
  '.ecran .panneau{display:flex;flex-direction:column;min-height:0;overflow:hidden;padding:12px 14px}.ecran .panneau>*{flex:none}',
  '.ecran .detail{flex:1 1 0;min-height:140px;height:auto;margin:0 0 10px}.ecran .legende-bloc{flex:0 0 auto;min-height:0;max-height:38%;overflow-y:auto;scrollbar-width:thin}',
  '.ecran .carte{height:100%;min-height:0}.ecran .carte svg{height:100%;width:100%}}',
  # écran ÉTROIT : carte au-dessus, panneau dessous, lien de retour vers la carte
  '@media(max-width:900px){.ecran .carte{order:-1}.retour-carte{display:inline-block;margin:0 0 12px;font-size:14px;color:var(--bleu)}#carte{scroll-margin-top:8px}.detail{scroll-margin-top:8px}}',
  sep = "\n")
js_carte_typologie <- function() paste(
  '(function(){',
  'var sj=document.getElementById("sans-js");if(sj)sj.style.display="none";',
  'var D=JSON.parse(document.getElementById("donnees").textContent),L=D.libelles;var $=function(id){return document.getElementById(id)};',
  'var B=$("bulle"),C=$("carte"),U=$("survol"),DET=$("detail-texte"),DT=$("detail-titre"),DA=$("detail-aide"),DP=$("detail"),pin=null,filtre=null;',
  # état du survol : le contenu de l'infobulle et du panneau n'est reconstruit QUE si le département change ; la position est mise à jour seule
  'var codeBulle=null,codeDetail=null,codeSurvol=null,tScroll=0;',
  # écran large : la hauteur du panneau est donnée par la mise en page (premier écran) ; écran étroit : stabilisée par mesure
  'function large(){return window.matchMedia("(min-width:901px)").matches}',
  'function ligne(el,txt,cls){var d=document.createElement("div");if(cls)d.className=cls;d.textContent=txt;el.appendChild(d);return d}',
  # rubriques titrées + puces (SPEC-TYPO-062) ; repli sur les lignes si un territoire n'a aucune rubrique (masqué, absent)
  'function rendreBlocs(cont,t){cont.innerHTML="";if(t.blocs&&t.blocs.length){t.blocs.forEach(function(b){var h=document.createElement("h3");h.className="rub";h.textContent=b.t;cont.appendChild(h);var ul=document.createElement("ul");ul.className="puces";b.p.forEach(function(x){var li=document.createElement("li");li.textContent=x;ul.appendChild(li)});cont.appendChild(ul)})}else{cont.textContent=t.st==="sans"?L.absent:(t.st==="masque"?L.detail_non_diffuse:(t.lignes||[]).join(" · "))}}',
  'function surligner(code){if(!U)return;var p=code&&$("t-"+code);if(p){U.setAttribute("d",p.getAttribute("d"));U.classList.add("on")}else{U.classList.remove("on");U.setAttribute("d","")}}',
  'function bulle(code){var t=D.territoires[code],el=document.createElement("div");ligne(el,t.nom+" · "+code,"tooltip-title");',
  ' var p=t.cle?D.profils[t.cle]:null;var c=ligne(el,p?p.titre:(t.st==="sans"?L.absent:"Non diffusé (secret statistique)"),"tooltip-profil");c.style.borderLeftColor=p?p.couleur:(t.st==="sans"?D.couleurs.sans_donnee:D.couleurs.secret);',
  ' var b=document.createElement("div");b.className="tooltip-rows";var bl=(t.blocs||[]).filter(function(x){return x.b});',
  ' if(bl.length){bl.forEach(function(x){ligne(b,x.t,"tooltip-rub");(x.pb||x.p).forEach(function(l){ligne(b,l,"tooltip-small")})})}else{t.lignes.forEach(function(l,i){ligne(b,l,i<3?"tooltip-value":"tooltip-small")})}el.appendChild(b);return el.innerHTML}',
  'function detail(code,force){if(!DET)return;if(!force&&code===codeDetail)return;codeDetail=code;if(!code){DT.textContent="";DET.textContent=L.detail_initial;DA.textContent="";return}var t=D.territoires[code],p=t.cle?D.profils[t.cle]:null;',
  ' DT.innerHTML="";DT.appendChild(document.createTextNode(t.nom+" · "+code));if(p){var s=document.createElement("span");s.className="pf";s.textContent=p.titre;s.style.background=p.couleur;s.style.color=(t.cle==="diffus"||t.cle==="expertiser"||t.cle==="stable")?"#1F2933":"#fff";DT.appendChild(s)}',
  ' rendreBlocs(DET,t);DA.textContent=pin===code?"Épinglé : cliquez de nouveau pour libérer.":"";DP.scrollTop=0;DP.classList.toggle("defilable",DP.scrollHeight>DP.clientHeight+1);DP.classList.remove("bas")}',
  # hauteur stable du panneau : mesure (hors écran, même largeur) du contenu le plus long ; bornée à 70 % de la fenêtre (60 % sur écran étroit, où le panneau est AU-DESSUS de la carte) avec défilement interne au-delà, jamais de troncature
  'function stabiliserDetail(){if(!DP)return;if(large()){DP.style.height="";return}var m=DP.cloneNode(true);m.id="";m.style.cssText="position:absolute;visibility:hidden;height:auto;min-height:0;max-height:none;overflow:visible;width:"+DP.getBoundingClientRect().width+"px;left:-9999px;top:0";DP.parentNode.appendChild(m);',
  ' var mt=m.querySelector(".detail-titre"),mx=m.querySelector(".detail-texte"),ma=m.querySelector(".detail-aide"),h=0;',
  ' Object.keys(D.territoires).forEach(function(code){var t=D.territoires[code],p=t.cle?D.profils[t.cle]:null;mt.textContent=t.nom+" · "+code+(p?" "+p.titre:"");rendreBlocs(mx,t);ma.textContent="Épinglé : cliquez de nouveau pour libérer.";h=Math.max(h,m.offsetHeight)});',
  ' DP.parentNode.removeChild(m);var cap=Math.round(window.innerHeight*0.6);DP.style.height=Math.min(Math.ceil(h)+2,cap)+"px"}',
  # profil isolé par la légende : titre, signification et consigne dans le panneau ; retour au bon contenu quand la souris quitte la carte
  'function detailProfil(k){var p=D.profils[k];if(!DET||!p)return;codeDetail="#"+k;DT.innerHTML="";DT.appendChild(document.createTextNode("Profil isolé"));var s=document.createElement("span");s.className="pf";s.textContent=p.titre;s.style.background=p.couleur;s.style.color=(k==="diffus"||k==="expertiser"||k==="stable")?"#1F2933":"#fff";DT.appendChild(s);',
  ' DET.textContent=p.titre+" ("+p.n+" département"+(p.n>1?"s":"")+") : "+p.signification+".";DA.textContent="Consigne de lecture : "+p.consigne+". Cliquez de nouveau le profil dans la légende pour tout afficher.";DP.scrollTop=0;DP.classList.toggle("defilable",DP.scrollHeight>DP.clientHeight+1);DP.classList.remove("bas")}',
  'function revenirDetail(){if(pin)detail(pin);else if(filtre)detailProfil(filtre);else detail(null)}',
  # infobulle : contenu (si le département change) puis position, en coordonnées de la fenêtre, bornée à l'écran
  'function montrerBulle(code){if(code!==codeBulle){B.innerHTML=bulle(code);codeBulle=code}B.style.display="block"}',
  'function placerBulle(x,y){var vw=window.innerWidth,vh=window.innerHeight,w=B.offsetWidth,h=B.offsetHeight,px=x+16,py=y+16;if(px+w>vw-8)px=x-w-16;if(py+h>vh-8)py=y-h-16;if(px<4)px=4;if(py<4)py=4;B.style.left=px+"px";B.style.top=py+"px"}',
  'function survoler(code){if(code===codeSurvol)return;codeSurvol=code;surligner(code);if(!pin)detail(code)}',
  'function appliquerFiltre(){document.querySelectorAll("path.t,path.h").forEach(function(p){p.classList.toggle("dim",!!filtre&&p.getAttribute("data-cle")!==filtre)});document.querySelectorAll(".typo-legende button").forEach(function(b){b.setAttribute("aria-pressed",b.getAttribute("data-cle")===filtre?"true":"false")})}',
  'if(C){document.querySelectorAll("path.t").forEach(function(p){var code=p.getAttribute("data-code");',
  ' p.addEventListener("mousemove",function(ev){survoler(code);if(Date.now()-tScroll<150)return;montrerBulle(code);placerBulle(ev.clientX,ev.clientY)});',
  ' p.addEventListener("mouseleave",function(){B.style.display="none";codeSurvol=null;surligner(pin);revenirDetail()});',
  ' p.addEventListener("click",function(){pin=(pin===code)?null:code;surligner(pin);detail(pin||code,true);if(!large()&&DP)DP.scrollIntoView({behavior:"smooth",block:"start"})});',
  ' p.addEventListener("keydown",function(ev){if(ev.key==="Enter"||ev.key===" "){ev.preventDefault();pin=(pin===code)?null:code;detail(pin||code,true)}});',
  ' p.addEventListener("focus",function(){survoler(code);montrerBulle(code);placerBulle(12,12)});p.addEventListener("blur",function(){B.style.display="none";codeSurvol=null;surligner(pin)})})}',
  # défilement : l'infobulle est masquée (elle réapparaît au prochain mouvement) ; rien d'autre ne bouge
  'window.addEventListener("scroll",function(){tScroll=Date.now();if(B&&B.style.display==="block")B.style.display="none"},{passive:true});',
  'if(DP)DP.addEventListener("scroll",function(){DP.classList.toggle("bas",DP.scrollTop+DP.clientHeight>=DP.scrollHeight-2)},{passive:true});',
  'var tRedim=null;window.addEventListener("resize",function(){clearTimeout(tRedim);tRedim=setTimeout(function(){stabiliserDetail();if(DP)DP.classList.toggle("defilable",DP.scrollHeight>DP.clientHeight+1)},150)});',
  'document.querySelectorAll(".typo-legende button").forEach(function(b){b.addEventListener("click",function(){var k=b.getAttribute("data-cle");filtre=(filtre===k)?null:k;appliquerFiltre();if(!pin){if(filtre)detailProfil(filtre);else detail(null,true)}})});',
  'detail(null,true);stabiliserDetail();',
  '})();', sep = "\n")

# --- Briques HTML --------------------------------------------------------------------
# compact = TRUE (page interactive, premier écran) : titre seul, présentation dans « À propos de cette carte » (fermé)
html_entete_typologie <- function(prep, champ, bandeau = TRUE, sous_qui = NULL, compact = FALSE) {
  P <- construire_libelles_public(champ); LT <- LIBELLES_CARTE_TYPO
  base <- if (identical(prep$base_poids, "effectif_champ")) paste0("aux ", P$salaries_age_min, texte_perimetre(" des entreprises du périmètre Base")) else texte_perimetre("à l’emploi actuel, tous âges, des entreprises du périmètre Base")
  c('<header class="page-header">', sprintf('<p class="kicker">%s</p>', echap_html_carte(LT$kicker)),
    sprintf('<h1 class="page-title" id="titre">%s<small>Départs attendus d’ici 2030 · par département</small></h1>', echap_html_carte(LT$titre)),
    if (bandeau) html_bandeau_sans_js() else "",
    if (compact) sprintf('<details class="apropos"><summary>%s</summary>', LIBELLES_CARTE_TYPO$apropos) else "",
    sprintf('<p class="page-subtitle"><span class="cat-qui">%s</span></p>', echap_html_carte(if (is.null(sous_qui)) LT$sous_titre else sous_qui)),
    '<div class="study-scope" role="note" aria-label="Qui est concerné">',
    sprintf(texte_perimetre('<span class="scope-text"><b>%s</b> · entreprises du périmètre Base</span>'), echap_html_carte(P$salaries_age)),
    sprintf('<span class="scope-text">Poids et intensité rapportés %s.</span>', echap_html_carte(base)),
    '</div>',
    '<p class="context">Les couleurs donnent le profil ; les zones grisées correspondent à un profil ou un département non diffusé (secret statistique) ; les zones blanches sont absentes de la typologie.</p>',
    if (compact) '</details>' else "",
    '</header>')
}
html_legende_typologie <- function(prep, interactive = TRUE) {
  defs <- DEFINITIONS_PROFILS; S <- STYLE_CONTOURS_CARTE
  item <- function(col, lib, n, sig = NULL, cle = NULL, sans = FALSE) {
    inner <- sprintf('<span class="sw%s" style="background:%s"></span><span><span class="lib">%s<span class="n">%d</span></span>%s</span>',
                     if (sans) " sans" else "", col, echap_html_carte(lib), n, if (!is.null(sig)) sprintf('<span class="sig">%s</span>', echap_html_carte(sig)) else "")
    if (interactive && !is.null(cle)) sprintf('<li><button type="button" data-cle="%s" aria-pressed="false"%s>%s</button></li>', cle,
                                              if (!is.null(sig)) sprintf(' title="%s"', echap_html_carte(sig)) else "", inner) else sprintf('<li><span class="item">%s</span></li>', inner)
  }
  c('<div class="legende-bloc"><div class="legend-title">Profils (nombre de départements)</div><ul class="typo-legende" id="legende">',
    vapply(names(prep$profils), function(k) item(COULEURS_PROFILS[[k]], prep$profils[[k]], prep$comptes[[k]], defs$signification[defs$cle == k], k), ""),
    item(COULEURS_CARTE$secret, LIBELLES_CARTE_TYPO$profil_non_diffuse, prep$comptes[["profil_masque"]] + prep$comptes[["masque"]]),
    # un département réellement absent de la typologie garde sa propre entrée (jamais assimilé au secret) ; aucune entrée à 0
    if (prep$comptes[["sans"]] > 0) item(COULEURS_CARTE$sans_donnee, LIBELLES_CARTE_TYPO$absent, prep$comptes[["sans"]], sans = TRUE) else "", '</ul>', html_legende_hachures(prep),
    if (interactive) '<p class="note">Cliquez un profil pour l’isoler sur la carte et lire sa définition ; cliquez de nouveau pour tout afficher.</p>' else "", '</div>')
}
# Hachures (SPEC-TYPO-107) : départements d'exposition Forte, jamais un département masqué ; motif SVG
# sans script (admis dans la version courriel) ; aucune interaction (pointer-events:none).
svg_hachures_typo <- function(prep, lay) {
  t <- prep$territoires; k <- !is.na(t$tension_classe) & t$tension_classe == "Forte" & t$statut %in% c("diffuse", "profil_masque") & t$code %in% names(lay$chemins)
  if (!any(k)) return("")
  c('<defs><pattern id="hachures" patternUnits="userSpaceOnUse" width="7" height="7" patternTransform="rotate(135)"><rect width="2.2" height="7" fill="#1F2933" fill-opacity="0.72"></rect></pattern></defs>',
    '<g class="hachures" pointer-events="none" aria-hidden="true">',
    sprintf('<path class="h" data-code="%s"%s fill="url(#hachures)" stroke="none" d="%s"></path>', t$code[k], ifelse(is.na(t$cle[k]), "", sprintf(' data-cle="%s"', t$cle[k])), unlist(lay$chemins[t$code[k]])),
    '</g>')
}
html_legende_hachures <- function(prep) {
  n <- prep$comptes[["hachures"]]; if (is.null(n) || is.na(n) || n == 0) return("")
  sprintf('<ul class="typo-legende legende-hachures"><li><span class="item"><span class="sw sw-hachures"></span><span><span class="lib">Forte part de remplacements difficiles parmi les métiers stratégiques suivis<span class="n">%d</span></span><span class="sig">Hachures : une grande part des départs des métiers stratégiques concerne des métiers difficiles à recruter en France. La tension est celle du métier, pas celle du département.</span></span></span></li></ul>', n)
}
html_definitions_profils <- function(prep, ouvert = FALSE) {
  defs <- DEFINITIONS_PROFILS
  lignes <- vapply(names(prep$profils), function(k) { r <- defs[defs$cle == k, ]
    sprintf('<tr><td><span class="sw" style="background:%s"></span>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>', COULEURS_PROFILS[[k]],
            echap_html_carte(prep$profils[[k]]), echap_html_carte(r$regle), echap_html_carte(r$signification), echap_html_carte(r$attention)) }, "")
  c(sprintf('<details class="bloc-typo"%s><summary>%s</summary>', if (ouvert) " open" else "", echap_html_carte(LIBELLES_CARTE_TYPO$definitions_titre)),
    '<table class="tab-typo tab-defs"><thead><tr><th>Profil</th><th>Règle</th><th>Ce que cela signifie</th><th>Consigne de lecture</th></tr></thead><tbody>', lignes, '</tbody></table></details>')
}
html_tableau_typologie <- function(prep, ouvert = FALSE) {
  t <- prep$territoires; t <- t[t$statut != "sans", ]
  ordre <- match(t$cle, names(prep$profils)); ordre[is.na(ordre)] <- length(prep$profils) + 1
  t <- t[order(ordre, -ifelse(is.na(t$departs_central), -1, t$departs_central), t$code), ]
  f0 <- function(x) fmt_fr_carte(x, 0); f1 <- function(x) fmt_fr_carte(x, 1)
  lignes <- vapply(seq_len(nrow(t)), function(i) { r <- t[i, ]
    if (r$statut == "masque") return(sprintf('<tr><td>%s · %s</td><td colspan="4"><span class="sw" style="background:%s"></span>%s</td></tr>', echap_html_carte(r$nom), r$code, COULEURS_CARTE$secret, LIBELLES_UI$secret))
    sit <- if (!is.na(r$situation_texte)) r$situation_texte else paste(lignes_detail_typologie(r, prep$indicateur), collapse = " ")
    sprintf('<tr><td>%s · %s</td><td><span class="sw" style="background:%s"></span>%s</td><td class="num">%s</td><td class="num">%s<br><small>%s à %s</small></td><td class="num">%s %%</td><td class="sit">%s</td></tr>',
            echap_html_carte(r$nom), r$code, couleur_territoire_typo(r), echap_html_carte(titre_profil_typo(r, prep)),
            f0(r$effectif_base_total), f0(r$departs_central), f0(r$departs_bas), f0(r$departs_haut), f1(r$intensite_renouvellement_pct), echap_html_carte(sit)) }, "")
  c(sprintf('<details class="bloc-typo"%s><summary>%s</summary>', if (ouvert) " open" else "", echap_html_carte(LIBELLES_CARTE_TYPO$tableau_titre)),
    sprintf('<table class="tab-typo"><thead><tr><th>Département</th><th>Profil</th><th>Salariés</th><th>Départs d’ici 2030</th><th>Intensité</th><th>Situation</th></tr></thead><tbody>'),
    lignes, '</tbody></table>',
    sprintf('<p class="note">Salariés : base du poids. Intensité : part %s. Départs : estimation centrale et fourchette basse – haute.</p>', echap_html_carte(libelle_intensite_typo(prep$indicateur))), '</details>')
}
html_pied_typologie <- function(champ, source_note) {
  sprintf(paste0('<footer><b>Qui est concerné ?</b> %s ',
                 '<b>Profils.</b> Attribués par des règles ordonnées et paramétrées (specs/10) ; une « situation à expertiser » n’est pas un défaut de données mais un cas que les règles simples ne tranchent pas. ',
                 '<b>Secret statistique.</b> Un département trop petit, ou dont une catégorie est protégée, n’a pas de profil diffusé ; ses chiffres ne figurent ni dans cette page ni dans ses données. ',
                 '<b>Source.</b> %s. Fond de carte : IGN Admin Express (COG 2018) via france-geojson, licence ouverte.</footer>'),
          echap_html_carte(paste(champ$detaille, collapse = " ")), echap_html_carte(source_note))
}
html_encarts_typo <- function(lay) vapply(lay$encarts, function(e) sprintf('<rect class="encart" fill="none" stroke="#c9ced6" x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="6"></rect><text class="encart-lib" fill="#3E4C59" x="%.1f" y="%.1f">%s</text>',
                                                                       e$cadre[1] + 1, e$cadre[2] + 1, e$cadre[3] - e$cadre[1] - 2, e$cadre[4] - e$cadre[2] - 2, e$x_lib, e$y_lib, echap_html_carte(e$nom)), "")
html_limites_regions_typo <- function(fond_regions, S = STYLE_CONTOURS_CARTE) {
  if (is.null(fond_regions)) return("")
  lr <- projeter_fond(fond_regions); metro <- setdiff(names(fond_regions), names(lr$encarts))
  c('<g class="limites-reg" aria-hidden="true">', sprintf('<path fill="none" stroke="%s" stroke-width="%s" d="%s"></path>', S$region_couleur, S$region_largeur, lr$chemins[metro]), '</g>')
}

# --- Page interactive -----------------------------------------------------------------
generer_carte_typologie <- function(prep, fond, fichier_html, champ = construire_libelle_champ(), fond_regions = NULL,
                                    source_note = "calculs propres", fichier_png = NULL) {
  lay <- projeter_fond(fond); t <- prep$territoires; S <- STYLE_CONTOURS_CARTE
  titre_page <- paste0(LIBELLES_CARTE_TYPO$titre, " — départs attendus d’ici 2030")
  paths <- vapply(names(fond), function(code) { r <- t[t$code == code, ]; sans <- r$statut == "sans"
    sprintf('<path class="t%s" id="t-%s" data-code="%s" data-st="%s"%s fill="%s" stroke="%s" stroke-width="%s"%s d="%s" tabindex="0" aria-label="%s"></path>',
            if (sans) " sans" else "", code, code, r$statut, if (!is.na(r$cle)) sprintf(' data-cle="%s"', r$cle) else "", couleur_territoire_typo(r),
            if (sans) S$sans_couleur else S$couleur, if (sans) S$sans_largeur else S$largeur, if (sans) ' stroke-dasharray="3 2"' else "",
            lay$chemins[[code]], echap_html_carte(paste0(r$nom, " — ", titre_profil_typo(r, prep)))) }, "")
  html <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">', sprintf('<title>%s</title>', echap_html_carte(titre_page)),
            '<meta name="viewport" content="width=device-width, initial-scale=1">', '<style>', css_cartes(), css_carte_typologie(), '</style></head><body><div class="page page-ecran">',
            '<div class="ecran">', html_entete_typologie(prep, champ, bandeau = TRUE, compact = TRUE),
            '<div class="grille scene"><aside class="panneau">',
            sprintf('<div class="detail" id="detail" role="status"><p class="detail-titre" id="detail-titre"></p><div class="detail-texte" id="detail-texte">%s</div><p class="detail-aide" id="detail-aide"></p></div>', echap_html_carte(LIBELLES_CARTE_TYPO$detail_initial)),
            sprintf('<a class="retour-carte" href="#carte">%s</a>', LIBELLES_CARTE_TYPO$retour_carte),
            html_legende_typologie(prep, interactive = TRUE),
            if (length(lay$encarts) > 0) '<p class="note">Outre-mer en encarts, chacun à sa propre échelle.</p>' else "",
            '</aside><div class="carte" id="carte">',
            sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">', lay$largeur, lay$hauteur, echap_html_carte(titre_page)),
            html_encarts_typo(lay), paths, svg_hachures_typo(prep, lay), html_limites_regions_typo(fond_regions), '<path id="survol" fill="none" d="" aria-hidden="true"></path>',
            '</svg><div class="bulle" id="bulle" role="status"></div></div></div></div>',
            html_definitions_profils(prep), html_tableau_typologie(prep), html_pied_typologie(champ, source_note),
            '<script type="application/json" id="donnees">', donnees_json_typologie(prep), '</script>', '<script>', js_carte_typologie(), '</script>', '</div></body></html>')
  dir.create(dirname(fichier_html), showWarnings = FALSE, recursive = TRUE)
  writeLines(enc2utf8(html), fichier_html, useBytes = TRUE)
  if (!is.null(fichier_png))
    tryCatch(png_carte_typologie(prep, lay, fichier_png, champ, lay_regions = if (!is.null(fond_regions)) projeter_fond(fond_regions) else NULL),
             error = function(e) message("08g : PNG non produit (", conditionMessage(e), ") — le HTML reste la restitution de référence."))
  invisible(fichier_html)
}

# --- Version COURRIEL : aucun script, aucun contenu actif ----------------------------
# Couleurs en attributs, infobulles = <title> natifs du SVG (une par département :
# nom, profil, chiffres, motif, points d'attention), tableau des situations ouvert
# sous la carte, définitions des profils. Même structure que la page interactive.
generer_carte_typologie_courriel <- function(prep, fond, fichier_html, champ = construire_libelle_champ(), fond_regions = NULL, source_note = "calculs propres") {
  lay <- projeter_fond(fond); t <- prep$territoires; S <- STYLE_CONTOURS_CARTE
  titre_page <- paste0(LIBELLES_CARTE_TYPO$titre, " — départs attendus d’ici 2030")
  uses <- vapply(names(fond), function(code) { r <- t[t$code == code, ]; sans <- r$statut == "sans"
    sprintf('<path fill="%s" stroke="%s" stroke-width="%s"%s d="%s"><title>%s</title></path>', couleur_territoire_typo(r),
            if (sans) S$sans_couleur else S$couleur, if (sans) S$sans_largeur else S$largeur, if (sans) ' stroke-dasharray="3 2"' else "", lay$chemins[[code]],
            echap_html_carte(paste(c(paste0(r$nom, " · ", code), titre_profil_typo(r, prep), lignes_detail_typologie(r, prep$indicateur)), collapse = "\n"))) }, "")
  html <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">', sprintf('<title>%s</title>', echap_html_carte(titre_page)),
            '<meta name="viewport" content="width=device-width, initial-scale=1">', '<style>', css_cartes(), css_carte_typologie(), '</style></head><body><div class="page courriel-typo">',
            html_entete_typologie(prep, champ, bandeau = FALSE, sous_qui = "Version pour envoi par courriel, sans script : survolez un département pour lire ses chiffres ; le détail de chaque situation est sous la carte."),
            '<div class="grille"><aside class="panneau">', html_legende_typologie(prep, interactive = FALSE),
            if (length(lay$encarts) > 0) '<p class="note">Outre-mer en encarts, chacun à sa propre échelle.</p>' else "",
            '</aside><div class="carte">',
            sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">', lay$largeur, lay$hauteur, echap_html_carte(titre_page)),
            html_encarts_typo(lay), uses, svg_hachures_typo(prep, lay), html_limites_regions_typo(fond_regions), '</svg></div></div>',
            html_definitions_profils(prep, ouvert = TRUE), html_tableau_typologie(prep, ouvert = TRUE), html_pied_typologie(champ, source_note), '</div></body></html>')
  if (any(grepl("<script", html, fixed = TRUE)) || any(grepl(" on[a-z]+=", html)) || any(grepl("javascript:", html, fixed = TRUE)))
    stop("generer_carte_typologie_courriel : contenu actif détecté dans la version courriel.")
  dir.create(dirname(fichier_html), showWarnings = FALSE, recursive = TRUE)
  writeLines(enc2utf8(html), fichier_html, useBytes = TRUE)
  invisible(fichier_html)
}

# --- PNG (ggplot2) : même palette, mêmes contours que les pages HTML ----------------------
png_carte_typologie <- function(prep, lay, fichier_png, champ = construire_libelle_champ(), lay_regions = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 absent")
  S <- STYLE_CONTOURS_CARTE; t <- prep$territoires
  niveaux <- c(unname(prep$profils), LIBELLES_CARTE_TYPO$profil_non_diffuse, LIBELLES_CARTE_TYPO$absent)
  cols <- setNames(c(unname(COULEURS_PROFILS[names(prep$profils)]), COULEURS_CARTE$secret, COULEURS_CARTE$sans_donnee), niveaux)
  lib <- vapply(seq_len(nrow(t)), function(i) { r <- t[i, ]; if (!is.na(r$cle)) prep$profils[[r$cle]] else if (r$statut == "sans") LIBELLES_CARTE_TYPO$absent else LIBELLES_CARTE_TYPO$profil_non_diffuse }, "")
  long <- lay$long; long$classe <- factor(lib[match(long$code, t$code)], levels = niveaux)
  enc <- if (length(lay$encarts)) bind_rows(lapply(lay$encarts, function(e) tibble(x1 = e$cadre[1], x2 = e$cadre[3], y1 = -e$cadre[2], y2 = -e$cadre[4], lib = e$nom, xl = e$x_lib, yl = -e$y_lib))) else NULL
  g <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = long, ggplot2::aes(x = x, y = y, group = groupe, fill = classe), colour = S$png_couleur, linewidth = S$png_largeur) +
    ggplot2::scale_fill_manual(values = cols, drop = FALSE, name = NULL,
                               breaks = niveaux[seq_len(length(niveaux) - if (prep$comptes[["sans"]] > 0) 0 else 1)]) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = S$png_couleur), ncol = 2)) +
    ggplot2::coord_equal(expand = FALSE) + ggplot2::theme_void(base_size = 12) +
    ggplot2::labs(title = LIBELLES_CARTE_TYPO$titre, subtitle = paste0("Départs attendus d’ici 2030 · ", toupper(substr(champ$court, 1, 1)), substr(champ$court, 2, nchar(champ$court))),
                  caption = "Gris : profil ou département non diffusé (secret statistique) · blanc : absent de la typologie.\nFond IGN Admin Express via france-geojson.") +
    ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 10),
                   plot.title = ggplot2::element_text(face = "bold", size = 15, colour = COULEURS_CARTE$encre),
                   plot.subtitle = ggplot2::element_text(colour = COULEURS_CARTE$texte, size = 11),
                   plot.caption = ggplot2::element_text(colour = COULEURS_CARTE$texte, size = 9), plot.margin = ggplot2::margin(10, 10, 10, 10))
  if (!is.null(lay_regions)) {
    reg <- lay_regions$long[!(lay_regions$long$code %in% names(lay_regions$encarts)), ]
    g <- g + ggplot2::geom_polygon(data = reg, ggplot2::aes(x = x, y = y, group = groupe), fill = NA, colour = S$region_couleur, linewidth = S$png_region_largeur)
  }
  if (!is.null(enc)) g <- g + ggplot2::geom_rect(data = enc, ggplot2::aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1), fill = NA, colour = "#c9ced6") +
    ggplot2::geom_text(data = enc, ggplot2::aes(x = xl, y = yl, label = lib), hjust = 0, size = 3.6, colour = COULEURS_CARTE$texte)
  ggplot2::ggsave(fichier_png, g, width = 8, height = 11, dpi = 130, bg = "white")
  invisible(fichier_png)
}
