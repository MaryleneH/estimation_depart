# ==============================================================================
# 99c_afficher_bornes_typologie.R — lecture des bornes EFFECTIVES de la typologie
# (SPEC-TYPO-070). Données synthétiques ; aucun calcul de typologie ici.
# ==============================================================================
EB <- new.env(); assign("BORNES_TYPOLOGIE_SANS_AFFICHAGE", TRUE, envir = EB)
sys.source(chemin_script("99c_afficher_bornes_typologie.R"), envir = EB)
params_csv <- function(dir, methode = "terciles", indicateur = "part_a_remplacer_pct", base = "effectif_tous_ages",
                       poids = c(0.5, 2), intensite = c(8.1234, 15), volume = c(50, 300), plancher = 50, bom = FALSE) {
  d <- data.frame(parametre = c("methode_classes", "indicateur_intensite", "base_poids", "seuil_dominance_cs_pct", "seuil_concentration_departs_pct", "effectif_min",
                                "poids_bas_pct", "poids_haut_pct", "intensite_bas_pct", "intensite_haut_pct", "volume_bas", "volume_haut",
                                "marge_frontiere_pct", "nb_criteres_frontiere_expertise", "facteur_intensite_extreme", "n_departements", "n_departements_retenus_seuils"),
                  valeur = c(methode, indicateur, base, 40, 50, plancher, poids[1], poids[2], intensite[1], intensite[2], volume[1], volume[2], 3, 2, 2, 96, 90), stringsAsFactors = FALSE)
  dir.create(file.path(dir, "typologie_territoriale", "interne"), showWarnings = FALSE, recursive = TRUE)
  f <- file.path(dir, "typologie_territoriale", "interne", "parametres_typologie.csv")
  write.csv2(d, f, row.names = FALSE, fileEncoding = if (bom) "UTF-8" else "")
  if (bom) { x <- readBin(f, "raw", file.info(f)$size); writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), x), f) }   # BOM comme 99b
  f
}
classes_csv <- function(dir) {
  d <- data.frame(geo_code = c("01", "02", "03", "04", "05"), classe_poids_bitd = c("Faible", "Moyen", "Fort", "Fort", NA),
                  classe_intensite = c("Faible", "Modérée", "Élevée", "Élevée", "Modérée"), classe_volume_departs = c("Faible", "Modéré", "Élevé", "Modéré", "Modéré"),
                  structure_emploi = c("Mixte", "Dominante ouvriers", "Dominante cadres", "Mixte", "Mixte"), type_concentration = c("Diffus", "Concentré", "Diffus", "Diffus", "Diffus"),
                  profil_typologie = "x", stringsAsFactors = FALSE)
  f <- file.path(dir, "typologie_territoriale", "interne", "typologie_departements.csv"); write.csv2(d, f, row.names = FALSE); f
}

test_that("SPEC-TYPO-070 — lecture des bornes effectives : fichier parametres_typologie.csv (avec ou sans BOM), cinq indicateurs, méthode terciles, plancher, formats français, bornes incluses dans la classe supérieure", {
  dir <- file.path(tempdir(), "bornes_terciles"); unlink(dir, recursive = TRUE)
  f <- params_csv(dir, bom = TRUE)
  p <- EB$lire_parametres_typologie(f)
  expect_identical(p[["methode_classes"]], "terciles"); expect_identical(p[["poids_haut_pct"]], "2"); expect_identical(p[["intensite_bas_pct"]], "8.1234")
  l <- EB$formater_bornes_typologie(p)
  txt <- paste(l, collapse = "\n")
  expect_true(grepl("TYPOLOGIE BITD — BORNES DE CLASSEMENT", txt, fixed = TRUE))
  expect_true(grepl("MÉTHODE : Terciles — règle de classement statistique provisoire", txt, fixed = TRUE))
  expect_true(grepl("(96 départements, dont 90 au-dessus du plancher", txt, fixed = TRUE))
  for (s in c("1. POIDS DANS L'EMPLOI BITD NATIONAL", "2. VOLUME DES DÉPARTS D'ICI 2030", "3. INTENSITÉ DU RENOUVELLEMENT", "4. STRUCTURE DE L'EMPLOI", "5. CONCENTRATION DES DÉPARTS"))
    expect_true(grepl(s, txt, fixed = TRUE), label = s)
  # bornes exactes, virgule décimale, borne incluse dans la classe supérieure (« moins de » / « ou plus »)
  expect_true(grepl("   Faible : moins de 0,5 %\n   Moyen  : de 0,5 % à moins de 2 %\n   Fort   : 2 % ou plus", txt, fixed = TRUE))
  expect_true(grepl("   Faible : moins de 50 départs\n   Modéré : de 50 à moins de 300 départs\n   Élevé  : 300 départs ou plus", txt, fixed = TRUE))
  expect_true(grepl("   Faible  : moins de 8,123 %\n   Modérée : de 8,123 % à moins de 15 %\n   Élevée  : 15 % ou plus", txt, fixed = TRUE))   # précision juste suffisante : 3 décimales pour un tercile observé, 0 pour un entier
  expect_true(grepl("   Mixte     : aucune catégorie n'atteint 40 % de l'emploi\n   Dominante : la première catégorie atteint 40 % ou plus", txt, fixed = TRUE))
  expect_true(grepl("   Diffus    : aucune catégorie n'atteint 50 % des départs\n   Concentré : une catégorie porte 50 % des départs ou plus", txt, fixed = TRUE))
  expect_true(grepl("Exception : un département de moins de 50 salariés", txt, fixed = TRUE))           # plancher d'effectif, valeur réelle
  expect_true(grepl("sur 100 salariés actuellement en poste (tous âges)", txt, fixed = TRUE))           # intensité = part de l'emploi actuel
  expect_false(grepl("repli", txt, fixed = TRUE))
  expect_false(grepl("tibble|data.frame|<chr>|\\$", txt))                                                   # aucun tableau brut ni jargon
  expect_false(grepl("RÉPARTITION", txt, fixed = TRUE))                                                   # pas de table des départements : bornes seules
  # milliers : espace ; décimales : 0 si entier
  p2 <- p; p2$volume_bas <- "1234.5"; p2$volume_haut <- "12000"
  expect_true(grepl("de 1 234,5 à moins de 12 000 départs", paste(EB$formater_bornes_typologie(p2), collapse = "\n"), fixed = TRUE))
})

test_that("SPEC-TYPO-070 — méthode fixes, repli d'intensité sur le champ, base du poids = champ, répartition par classe (classes déjà attribuées, NA non comptés)", {
  dir <- file.path(tempdir(), "bornes_fixes"); unlink(dir, recursive = TRUE)
  f <- params_csv(dir, methode = "fixes", indicateur = "taux_depart_central_pct", base = "effectif_champ", poids = c(1, 5), intensite = c(5, 10), volume = c(100, 800), plancher = 30)
  classes_csv(dir)
  txt <- paste(EB$formater_bornes_typologie(EB$lire_parametres_typologie(f), EB$lire_classes_typologie(file.path(dir, "typologie_territoriale", "interne", "typologie_departements.csv"))), collapse = "\n")
  expect_true(grepl("MÉTHODE : Seuils fixes — bornes paramétrées", txt, fixed = TRUE)); expect_false(grepl("provisoire", txt, fixed = TRUE))
  expect_true(grepl("   Faible : moins de 1 %\n   Moyen  : de 1 % à moins de 5 %\n   Fort   : 5 % ou plus", txt, fixed = TRUE))
  expect_true(grepl("   Faible  : moins de 5 %\n   Modérée : de 5 % à moins de 10 %\n   Élevée  : 10 % ou plus", txt, fixed = TRUE))
  expect_true(grepl("moins de 30 salariés", txt, fixed = TRUE))
  expect_true(grepl("ATTENTION, repli", txt, fixed = TRUE)); expect_true(grepl("salariés du champ (âge minimal et plus)", txt, fixed = TRUE))
  expect_true(grepl("part des salariés du champ BITD français", txt, fixed = TRUE))
  expect_true(grepl("RÉPARTITION DES 5 DÉPARTEMENTS", txt, fixed = TRUE))
  expect_true(grepl("Poids         : Faible 1 | Moyen 1 | Fort 2", txt, fixed = TRUE))
  expect_true(grepl("Volume        : Faible 1 | Modéré 3 | Élevé 1", txt, fixed = TRUE))
  expect_true(grepl("Intensité     : Faible 1 | Modérée 2 | Élevée 2", txt, fixed = TRUE))
  expect_true(grepl("Structure     : Mixte 3 | Dominante 2", txt, fixed = TRUE))
  expect_true(grepl("Concentration : Diffus 4 | Concentré 1", txt, fixed = TRUE))
  expect_true(grepl("classes non renseignées : non comptées", txt, fixed = TRUE))
  expect_false(grepl("Faible|Moyen|Fort", regmatches(txt, regexpr("(?s)4\\. STRUCTURE.*?5\\. CONCENTRATION", txt, perl = TRUE))))   # jamais de classe faible / moyenne / forte pour structure
})

test_that("SPEC-TYPO-070 — fichiers absents ou incomplets : message avec la marche à suivre, aucune valeur inventée, aucun arrêt ; sans table des départements, bornes seules", {
  vide <- file.path(tempdir(), "bornes_vide"); unlink(vide, recursive = TRUE); dir.create(vide)
  expect_message(r <- EB$afficher_bornes_typologie(vide), "introuvable"); expect_null(r)
  expect_message(EB$afficher_bornes_typologie(vide), "main.R")
  inc <- file.path(tempdir(), "bornes_incomplet"); unlink(inc, recursive = TRUE)
  f <- params_csv(inc); d <- read.csv2(f, stringsAsFactors = FALSE); d <- d[d$parametre != "volume_haut", ]; write.csv2(d, f, row.names = FALSE)
  expect_message(r2 <- EB$afficher_bornes_typologie(inc), "incomplet"); expect_null(r2)
  d <- read.csv2(f, stringsAsFactors = FALSE); d <- rbind(d, data.frame(parametre = "volume_haut", valeur = "abc")); write.csv2(d, f, row.names = FALSE)
  expect_message(EB$afficher_bornes_typologie(inc), "non numériques")
  ok <- file.path(tempdir(), "bornes_ok"); unlink(ok, recursive = TRUE); params_csv(ok)
  out <- capture.output(r3 <- EB$afficher_bornes_typologie(ok)); txt <- paste(out, collapse = "\n")
  expect_true(grepl("BORNES DE CLASSEMENT", txt, fixed = TRUE)); expect_false(grepl("RÉPARTITION", txt, fixed = TRUE))
  expect_true(grepl("répartition non affichée", txt, fixed = TRUE)); expect_type(r3, "character")
})

test_that("SPEC-TYPO-070 — session R vierge : source() depuis la racine, sans main.R ni objet de la chaîne, lit DIR_SORTIES s'il est défini", {
  dir <- file.path(tempdir(), "bornes_session"); unlink(dir, recursive = TRUE); params_csv(dir); classes_csv(dir)
  log <- file.path(tempdir(), "bornes_session.log"); old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  statut <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(sprintf('DIR_SORTIES <- "%s"; source("R/99_controles/99c_afficher_bornes_typologie.R")', dir))), stdout = log, stderr = log)
  expect_equal(statut, 0L); txt <- paste(readLines(log, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("TYPOLOGIE BITD — BORNES DE CLASSEMENT", txt, fixed = TRUE)); expect_true(grepl("Fort   : 2 % ou plus", txt, fixed = TRUE))
  expect_true(grepl("RÉPARTITION DES 5 DÉPARTEMENTS", txt, fixed = TRUE)); expect_true(grepl("Source :", txt, fixed = TRUE))
  expect_false(grepl("Error|Erreur", txt))
  # sans DIR_SORTIES ni fichier : message, code de sortie 0
  statut2 <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote('source("R/99_controles/99c_afficher_bornes_typologie.R")')), stdout = log, stderr = log)
  expect_equal(statut2, 0L)
})
