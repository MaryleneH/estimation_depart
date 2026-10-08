# ==============================================================================
# 99c_afficher_bornes_typologie.R — Bornes de classement de la typologie BITD
# ------------------------------------------------------------------------------
# USAGE   : à lancer SEUL, depuis la racine du dépôt, après main.R (session R
#           neuve possible, aucun objet de la chaîne requis) :
#             source("R/99_controles/99c_afficher_bornes_typologie.R")
# LIT     : DIR_SORTIES/typologie_territoriale/interne/parametres_typologie.csv
#           (bornes EFFECTIVEMENT appliquées par le dernier 08f : terciles observés
#           ou seuils fixes, plancher d'effectif, indicateur d'intensité retenu) ;
#           facultatif : interne/typologie_departements.csv (classes déjà
#           attribuées, pour la répartition des départements).
# AFFICHE : les bornes des cinq indicateurs et les conditions de passage d'une
#           classe à l'autre, en français, sans rien recalculer ni inventer.
#           Fichier absent ou incomplet = message avec la marche à suivre.
# RÈGLES  : specs/10 §3 (classe supérieure à partir de la borne, incluse),
#           §2 (dominance et concentration : seuil atteint = Dominante / Concentré),
#           SPEC-TYPO-011 (plancher d'effectif -> poids « Faible »).
# ==============================================================================

# --- Lecture : parametre ; valeur (write.csv2, avec ou sans BOM) -------------------
lire_parametres_typologie <- function(fichier) {
  if (!file.exists(fichier)) return(NULL)
  d <- utils::read.csv2(fichier, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, colClasses = "character", na.strings = c("NA", ""))
  names(d)[1] <- sub("^﻿", "", names(d)[1])
  if (!all(c("parametre", "valeur") %in% names(d))) return(NULL)
  stats::setNames(as.list(trimws(d$valeur)), trimws(d$parametre))
}
lire_classes_typologie <- function(fichier) {
  if (!file.exists(fichier)) return(NULL)
  d <- utils::read.csv2(fichier, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, colClasses = "character", na.strings = c("NA", ""))
  names(d)[1] <- sub("^﻿", "", names(d)[1])
  requis <- c("classe_poids_bitd", "classe_intensite", "classe_volume_departs", "structure_emploi", "type_concentration")
  if (!all(requis %in% names(d))) return(NULL)
  d[, requis]
}

# --- Formats français ---------------------------------------------------------------
# nombre : virgule décimale, espaces pour les milliers ; décimales juste
# suffisantes : 0 si entier, sinon le plus petit nombre (1 à 3) qui restitue la
# valeur exactement, 3 au-delà (un tercile observé garde ainsi sa précision).
nb_fr <- function(x, decimales = NULL) {
  x <- as.numeric(x); if (is.na(x)) return("n.d.")
  d <- if (!is.null(decimales)) decimales else { k <- 3; for (i in 0:3) if (abs(x - round(x, i)) < 1e-9) { k <- i; break }; k }
  formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ",")
}
decimales_pour <- function(bas, haut) NULL   # chaque borne porte ses propres décimales (voir nb_fr)
pct_fr <- function(x, d = NULL) paste0(nb_fr(x, d), " %")

# --- Composition du texte (vecteur de lignes, sans rien afficher) ------------------------
# params : liste nommée (lire_parametres_typologie) ; classes : table des classes (ou NULL).
formater_bornes_typologie <- function(params, classes = NULL) {
  requis <- c("methode_classes", "indicateur_intensite", "base_poids", "seuil_dominance_cs_pct", "seuil_concentration_departs_pct", "effectif_min",
              "poids_bas_pct", "poids_haut_pct", "intensite_bas_pct", "intensite_haut_pct", "volume_bas", "volume_haut")
  manquants <- setdiff(requis, names(params))
  if (length(manquants)) stop("parametres_typologie.csv incomplet : paramètre(s) absent(s) : ", paste(manquants, collapse = ", "))
  num <- function(k) suppressWarnings(as.numeric(params[[k]]))
  bornes <- function(bas, haut) { b <- num(bas); h <- num(haut)
    if (is.na(b) || is.na(h)) stop("parametres_typologie.csv incomplet : bornes « ", bas, " » / « ", haut, " » non numériques (", params[[bas]], " ; ", params[[haut]], ").")
    list(bas = b, haut = h, d = decimales_pour(b, h)) }
  po <- bornes("poids_bas_pct", "poids_haut_pct"); it <- bornes("intensite_bas_pct", "intensite_haut_pct"); vo <- bornes("volume_bas", "volume_haut")
  dom <- num("seuil_dominance_cs_pct"); con <- num("seuil_concentration_departs_pct"); plancher <- num("effectif_min")
  if (anyNA(c(dom, con, plancher))) stop("parametres_typologie.csv incomplet : seuil de dominance, de concentration ou plancher d'effectif non numérique.")
  methode <- params[["methode_classes"]]; indicateur <- params[["indicateur_intensite"]]; base <- params[["base_poids"]]
  trait <- strrep("=", 56); tiret <- strrep("-", 56)
  methode_txt <- if (identical(methode, "terciles")) "Terciles — règle de classement statistique provisoire : bornes calculées sur les départements de la dernière exécution"
                 else if (identical(methode, "fixes")) "Seuils fixes — bornes paramétrées dans la configuration" else paste("Méthode inconnue :", methode)
  n_ret <- if (!is.null(params[["n_departements_retenus_seuils"]]) && !is.null(params[["n_departements"]]))
    sprintf("          (%s départements, dont %s au-dessus du plancher pour fixer les bornes)", nb_fr(params[["n_departements"]]), nb_fr(params[["n_departements_retenus_seuils"]])) else NULL
  intensite_lecture <- if (identical(indicateur, "part_a_remplacer_pct"))
    c("   Lecture : sur 100 salariés actuellement en poste (tous âges)", "   dans la BITD du département, combien correspondent aux", "   départs estimés d'ici 2030.")
  else c("   Lecture : ATTENTION, repli : le stock tous âges n'était pas", "   disponible ; l'intensité rapporte les départs aux seuls", "   salariés du champ (âge minimal et plus), pas à l'emploi actuel.", "   Elle n'est pas comparable à une part de l'emploi à remplacer.")
  poids_base <- if (identical(base, "effectif_tous_ages")) "   Lecture : part de l'emploi BITD français (tous âges) située" else "   Lecture : part des salariés du champ BITD français située"
  l <- c(trait, "  TYPOLOGIE BITD — BORNES DE CLASSEMENT", trait, "",
    paste0("MÉTHODE : ", methode_txt), n_ret, "",
    "1. POIDS DANS L'EMPLOI BITD NATIONAL", tiret,
    sprintf("   Faible : moins de %s", pct_fr(po$bas, po$d)),
    sprintf("   Moyen  : de %s à moins de %s", pct_fr(po$bas, po$d), pct_fr(po$haut, po$d)),
    sprintf("   Fort   : %s ou plus", pct_fr(po$haut, po$d)), "",
    sprintf("   Exception : un département de moins de %s salariés", nb_fr(plancher)),
    "   (plancher d'effectif) est toujours classé « Faible »,",
    "   même si sa part nationale dépasse une borne.", "",
    poids_base, "   dans le département.", "",
    "2. VOLUME DES DÉPARTS D'ICI 2030", tiret,
    sprintf("   Faible : moins de %s départs", nb_fr(vo$bas, vo$d)),
    sprintf("   Modéré : de %s à moins de %s départs", nb_fr(vo$bas, vo$d), nb_fr(vo$haut, vo$d)),
    sprintf("   Élevé  : %s départs ou plus", nb_fr(vo$haut, vo$d)), "",
    "   Lecture : nombre de départs définitifs estimés d'ici 2030", "   (estimation centrale).", "",
    "3. INTENSITÉ DU RENOUVELLEMENT", tiret,
    sprintf("   Faible  : moins de %s", pct_fr(it$bas, it$d)),
    sprintf("   Modérée : de %s à moins de %s", pct_fr(it$bas, it$d), pct_fr(it$haut, it$d)),
    sprintf("   Élevée  : %s ou plus", pct_fr(it$haut, it$d)), "",
    intensite_lecture, "",
    "4. STRUCTURE DE L'EMPLOI", tiret,
    sprintf("   Mixte     : aucune catégorie n'atteint %s de l'emploi", pct_fr(dom)),
    sprintf("   Dominante : la première catégorie atteint %s ou plus", pct_fr(dom)), "",
    "   Lecture : répartition des salariés entre les grandes", "   catégories socioprofessionnelles. Décrit le département ;", "   n'entre pas dans le choix du profil.", "",
    "5. CONCENTRATION DES DÉPARTS", tiret,
    sprintf("   Diffus    : aucune catégorie n'atteint %s des départs", pct_fr(con)),
    sprintf("   Concentré : une catégorie porte %s des départs ou plus", pct_fr(con)), "",
    "   Lecture : les départs concernent-ils surtout une", "   catégorie de salariés ?", "",
    "Règle commune : une valeur égale à une borne appartient à la", "classe supérieure (ou à « Dominante » / « Concentré »).", trait)
  if (!is.null(classes) && nrow(classes) > 0) {
    cpt <- function(v, niveaux) vapply(niveaux, function(k) sum(v %in% k), 0L)
    po_n <- cpt(classes$classe_poids_bitd, c("Faible", "Moyen", "Fort")); vo_n <- cpt(classes$classe_volume_departs, c("Faible", "Modéré", "Élevé"))
    it_n <- cpt(classes$classe_intensite, c("Faible", "Modérée", "Élevée"))
    st_n <- c(Mixte = sum(classes$structure_emploi %in% "Mixte"), Dominante = sum(grepl("^Dominante", classes$structure_emploi)))
    co_n <- cpt(classes$type_concentration, c("Diffus", "Concentré"))
    nc <- function(v) sum(is.na(v))
    l <- c(l, "", sprintf("RÉPARTITION DES %s DÉPARTEMENTS (classes déjà attribuées)", nb_fr(nrow(classes))), "",
      sprintf("Poids         : Faible %d | Moyen %d | Fort %d", po_n[1], po_n[2], po_n[3]),
      sprintf("Volume        : Faible %d | Modéré %d | Élevé %d", vo_n[1], vo_n[2], vo_n[3]),
      sprintf("Intensité     : Faible %d | Modérée %d | Élevée %d", it_n[1], it_n[2], it_n[3]),
      sprintf("Structure     : Mixte %d | Dominante %d", st_n[["Mixte"]], st_n[["Dominante"]]),
      sprintf("Concentration : Diffus %d | Concentré %d", co_n[1], co_n[2]))
    nas <- nc(classes$classe_poids_bitd) + nc(classes$classe_intensite) + nc(classes$classe_volume_departs) + nc(classes$structure_emploi) + nc(classes$type_concentration)
    if (nas > 0) l <- c(l, "", "(classes non renseignées : non comptées)")
    l <- c(l, trait)
  }
  l
}

# --- Affichage : lit les fichiers de la dernière exécution, écrit dans la console -----------
afficher_bornes_typologie <- function(dir_sorties = get0("DIR_SORTIES", ifnotfound = "sorties")) {
  dir_int <- file.path(dir_sorties, "typologie_territoriale", "interne")
  f_par <- file.path(dir_int, "parametres_typologie.csv"); f_dep <- file.path(dir_int, "typologie_departements.csv")
  params <- lire_parametres_typologie(f_par)
  if (is.null(params)) {
    message("Bornes de la typologie : fichier introuvable ou illisible : ", f_par, "\n",
            "  Marche à suivre : exécuter la chaîne (source(\"main.R\")) avec GENERER_TYPOLOGIE = TRUE et GEO_ANALYSE = \"departement\",\n",
            "  ou source(\"R/08_territoires/08f_typologie_departements.R\") après le 08d, puis relancer ce script depuis la racine du dépôt.")
    return(invisible(NULL))
  }
  classes <- lire_classes_typologie(f_dep)
  lignes <- tryCatch(formater_bornes_typologie(params, classes), error = function(e) {
    message("Bornes de la typologie : ", conditionMessage(e), "\n  Marche à suivre : relancer le 08f (dernière version du code) pour réécrire ", f_par, ".")
    NULL })
  if (is.null(lignes)) return(invisible(NULL))
  cat(lignes, sep = "\n")
  cat(sprintf("\nSource : %s (%s)%s\n", f_par, format(file.info(f_par)$mtime, "%d/%m/%Y %H:%M"),
              if (is.null(classes)) " ; répartition non affichée (typologie_departements.csv absent)" else ""))
  invisible(lignes)
}

if (!isTRUE(get0("BORNES_TYPOLOGIE_SANS_AFFICHAGE", ifnotfound = FALSE))) afficher_bornes_typologie()
