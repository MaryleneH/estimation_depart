# ==============================================================================
# 99d_controler_tension_cartes.R — Cohérence des restitutions de la tension
# ------------------------------------------------------------------------------
# USAGE   : à lancer SEUL, depuis la racine du dépôt, après main.R (session R
#           neuve possible, aucun objet de la chaîne requis) :
#             source("R/99_controles/99d_controler_tension_cartes.R")
# LIT     : DIR_SORTIES/typologie_territoriale/diffusion/typologie_departements.csv
#           DIR_SORTIES/typologie_territoriale/diffusion/exposition_tension_departements.csv
#           DIR_SORTIES/typologie_territoriale/diffusion/fiche_exposition_tension.html
#           DIR_SORTIES/typologie_territoriale/cartes/carte_typologie.html (facultatif)
# VÉRIFIE : (1) les départements hachurés de la carte des profils sont exactement
#           les départements diffusés de classe « Forte » ; (2) tension_cumul est
#           l'intersection « classe Forte » x « profil à renouvellement rapide »
#           (Pôle majeur à renouveler rapidement, Renouvellement rapide à surveiller)
#           parmi les départements diffusés ; (3) le premier message de la fiche
#           compte ce même nombre ; (4) chaque fichier est plus récent que ses sources
#           (une fiche plus ancienne que les CSV = relancer 08i, 08g).
#           Rien n'est recalculé : seuls les fichiers de diffusion et les pages sont lus.
# AFFICHE : un rapport en français ; rend (invisible) une liste `controle_tension`
#           avec les ensembles et le statut (ok / écarts / fichiers absents).
# ==============================================================================
lire_diffusion_99d <- function(fichier) {
  if (!file.exists(fichier)) return(NULL)
  d <- utils::read.csv2(fichier, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, colClasses = "character", na.strings = c("NA", ""), check.names = FALSE)
  names(d)[1] <- sub(paste0("^", intToUtf8(0xFEFF)), "", names(d)[1]); d
}
controler_tension_cartes <- function(dir_sorties = get0("DIR_SORTIES", ifnotfound = "sorties"), rapides = c("Pôle majeur à renouveler rapidement", "Renouvellement rapide à surveiller")) {
  base <- file.path(dir_sorties, "typologie_territoriale")
  f_typo <- file.path(base, "diffusion", "typologie_departements.csv"); f_expo <- file.path(base, "diffusion", "exposition_tension_departements.csv")
  f_fiche <- file.path(base, "diffusion", "fiche_exposition_tension.html"); f_carte <- file.path(base, "cartes", "carte_typologie.html")
  f_terr <- file.path(base, "diffusion", "fiche_metiers_liste.html")   # fiche des métiers par territoire (08j)
  trait <- paste(rep("-", 78), collapse = ""); l <- c(trait, "  CONTRÔLE DES RESTITUTIONS DE LA TENSION (fichiers de diffusion et pages)", trait)
  typo <- lire_diffusion_99d(f_typo); expo <- lire_diffusion_99d(f_expo)
  ecarts <- character(0); absents <- character(0)
  if (is.null(typo)) absents <- c(absents, f_typo); if (is.null(expo)) absents <- c(absents, f_expo)
  if (length(absents)) {
    l <- c(l, "  Fichier(s) absent(s) :", paste0("    ", absents), "  Marche à suivre : lancer la chaîne (main.R) avec GENERER_TYPOLOGIE = TRUE, TYPO_SECRET = TRUE et le classeur PCS -> FAP de tension présent (08f puis 08i).")
    cat(l, sep = "\n"); return(invisible(list(statut = "fichiers absents", absents = absents)))
  }
  vrai <- function(x) toupper(trimws(x)) %in% "TRUE"                     # NA (territoire non diffusé) -> FALSE, jamais une ligne fantôme
  typo$masque <- vrai(typo$masque); expo$tension_diffusee <- vrai(expo$tension_diffusee); expo$tension_cumul <- vrai(expo$tension_cumul)
  nom <- function(codes) if (length(codes)) paste(sort(codes), collapse = ", ") else "aucun"
  # (1) hachures attendues : diffusés (exposition ET typologie), classe Forte
  masques_typo <- typo$geo_code[typo$masque]
  forte <- expo$geo_code[expo$tension_diffusee & expo$tension_classe %in% "Forte" & !(expo$geo_code %in% masques_typo)]
  rapide <- typo$geo_code[!typo$masque & typo$profil_typologie %in% rapides]
  attendu_cumul <- intersect(forte, rapide); cumul <- expo$geo_code[expo$tension_cumul]
  l <- c(l, "", sprintf("  Départements diffusés de classe Forte (hachures attendues) : %d — %s", length(forte), nom(forte)),
         sprintf("  Départements à renouvellement rapide (profils %s) : %d — %s", paste(rapides, collapse = " / "), length(rapide), nom(rapide)),
         sprintf("  Intersection (cumul attendu) : %d — %s", length(attendu_cumul), nom(attendu_cumul)),
         sprintf("  tension_cumul = TRUE dans exposition_tension_departements.csv : %d — %s", length(cumul), nom(cumul)))
  if (!setequal(attendu_cumul, cumul)) ecarts <- c(ecarts, sprintf("tension_cumul (%s) différent de l'intersection attendue (%s)", nom(cumul), nom(attendu_cumul)))
  # carte : hachures effectivement dessinées
  if (file.exists(f_carte)) {
    h <- paste(readLines(f_carte, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    hach <- regmatches(h, gregexpr('<path class="h" data-code="[^"]+"', h))[[1]]; hach <- sub('.*data-code="([^"]+)"', "\\1", hach)
    l <- c(l, sprintf("  Départements hachurés dans carte_typologie.html : %d — %s", length(hach), nom(hach)))
    if (!setequal(hach, forte)) ecarts <- c(ecarts, sprintf("hachures de la carte (%s) différentes des Forte diffusés (%s) : relancer 08g après 08i", nom(hach), nom(forte)))
  } else l <- c(l, "  carte_typologie.html absente : hachures non contrôlées (lancer 08g).")
  # fiche : premier message
  if (file.exists(f_fiche)) {
    fh <- paste(readLines(f_fiche, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    m1 <- regmatches(fh, regexpr("<div class=\"msg\"><b>[^<]*</b>[^<]*", fh)); m1 <- sub("^<div class=\"msg\"><b>[^<]*</b>", "", m1)
    n_fiche <- if (grepl("aucun département ne cumule", m1)) 0L else suppressWarnings(as.integer(sub("^(\\d+) département.*", "\\1", m1)))
    l <- c(l, sprintf("  Premier message de la fiche : « %s »", substr(m1, 1, 140)), sprintf("  Départements comptés par la fiche : %s", if (is.na(n_fiche)) "non lu" else n_fiche))
    if (!is.na(n_fiche) && n_fiche != length(cumul)) ecarts <- c(ecarts, sprintf("la fiche compte %d département(s) en cumul, le CSV %d : fiche périmée, relancer 08i", n_fiche, length(cumul)))
  } else l <- c(l, "  fiche_exposition_tension.html absente (lancer 08i).")
  # horodatages : chaque restitution doit être au moins aussi récente que ses sources
  horo <- function(f) if (file.exists(f)) file.info(f)$mtime else as.POSIXct(NA)
  t <- c(typologie = horo(f_typo), exposition = horo(f_expo), fiche = horo(f_fiche), metiers = horo(f_terr), carte = horo(f_carte))
  l <- c(l, "", "  Horodatages :", sprintf("    %-12s %s", names(t), ifelse(is.na(t), "absent", format(t, "%Y-%m-%d %H:%M:%S"))))
  if (!is.na(t[["exposition"]]) && !is.na(t[["typologie"]]) && t[["exposition"]] < t[["typologie"]] - 1) ecarts <- c(ecarts, "exposition_tension_departements.csv plus ancien que typologie_departements.csv : relancer 08i")
  for (k in c("fiche", "metiers", "carte")) if (!is.na(t[[k]]) && t[[k]] < t[["exposition"]] - 1) ecarts <- c(ecarts, sprintf("%s plus ancien(ne) que exposition_tension_departements.csv : relancer %s", k, switch(k, carte = "08g", metiers = "08j", "08i")))
  l <- c(l, "", if (length(ecarts)) c("  ÉCARTS :", paste0("    - ", ecarts)) else "  Aucun écart : hachures, cumul, fiche et horodatages sont cohérents.", trait)
  cat(l, sep = "\n")
  invisible(list(statut = if (length(ecarts)) "écarts" else "ok", forte = forte, rapide = rapide, cumul = cumul, attendu_cumul = attendu_cumul, ecarts = ecarts, horodatages = t))
}
if (!isTRUE(get0("CONTROLE_TENSION_SANS_AFFICHAGE", ifnotfound = FALSE))) controle_tension <- controler_tension_cartes()
