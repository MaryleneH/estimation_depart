# ==============================================================================
# Cartes des résultats diffusables (00f + 08e) : fond local, jointure sur les
# codes, secret statistique (aucune valeur masquée embarquée), couverture PCS
# partiellement diffusable, BOM, fichiers absents, chaîne réelle.
# ==============================================================================
EC <- new.env()
.old_wd <- setwd(RACINE)
source(chemin_script("00_config.R"), local = EC)
setwd(.old_wd)
assign("DIR_DATA", file.path(RACINE, "data"), envir = EC)
sys.source(chemin_script("00f_fonctions_cartes.R"), envir = EC)
FOND_DEP <- EC$charger_fond_carte("departement"); FOND_REG <- EC$charger_fond_carte("region")
mesures_vides <- function(n) list(effectif_champ = rep(NA_real_, n), departs_central = rep(NA_real_, n), departs_bas = rep(NA_real_, n),
                                  departs_haut = rep(NA_real_, n), taux_depart_central_pct = rep(NA_real_, n))
# table synthétique de DIFFUSION département x pcs : A = partiel, B = tout masqué, C = tout diffusé
TABLE_ABC <- tibble::tibble(
  region_code = c("75", "75", "75", "75", "84", "84"), region_nom = "R",
  geo_code = c("33", "33", "64", "64", "01", "01"), geo_nom = c("Gironde", "Gironde", "Pyrénées-Atlantiques", "Pyrénées-Atlantiques", "Ain", "Ain"),
  pcs = rep(c("311D", "622A"), 3), cs1 = rep(c("Cadres", "Ouvriers"), 3),
  effectif_champ = c(84, NA, NA, NA, 40, 25), departs_central = c(27.4, NA, NA, NA, 0, 9.5),
  departs_bas = c(25, NA, NA, NA, 0, 9), departs_haut = c(30, NA, NA, NA, 0, 10),
  taux_depart_central_pct = c(32.6, NA, NA, NA, 0, 38), masque = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
  motif_masque = c(NA, "primaire", "primaire", "secondaire", NA, NA))

test_that("fond local : 101 départements et 18 régions, codes texte, DROM et Corse présents, aucune connexion", {
  expect_length(FOND_DEP, 101); expect_length(FOND_REG, 18)
  expect_type(names(FOND_DEP), "character")
  for (k in c("01", "09", "2A", "2B", "971", "972", "973", "974", "976")) expect_true(k %in% names(FOND_DEP), label = k)
  for (k in c("01", "02", "03", "04", "06", "11", "94", "84")) expect_true(k %in% names(FOND_REG), label = k)
  expect_identical(FOND_DEP[["2A"]]$nom, "Corse-du-Sud"); expect_identical(FOND_DEP[["971"]]$nom, "Guadeloupe")
  expect_identical(FOND_REG[["01"]]$nom, "Guadeloupe"); expect_identical(FOND_REG[["94"]]$nom, "Corse")
  # chaque code du référentiel projet trouve son polygone, et le nom concorde (codes 01 / 09 jamais 1 / 9)
  ref <- read.csv2(file.path(RACINE, "data", "ref_departement.csv"), colClasses = "character", fileEncoding = "UTF-8-BOM")
  expect_true(all(ref$code %in% names(FOND_DEP)))
  expect_identical(FOND_DEP[["09"]]$nom, "Ariège"); expect_identical(FOND_DEP[["01"]]$nom, "Ain")
  expect_false(any(grepl("insee.fr|http", readLines(chemin_script("00f_fonctions_cartes.R"), warn = FALSE) |> grep("download|url\\(", x = _, value = TRUE))))
  # projection : Paris à l'est de Brest, Lille au nord de Marseille (Lambert-93 : x croît vers l'est, y vers le nord)
  p <- EC$projeter_lambert93(c(-4.49, 2.35, 3.06, 5.37), c(48.39, 48.86, 50.63, 43.30))
  expect_lt(p[1, "x"], p[2, "x"]); expect_gt(p[3, "y"], p[4, "y"])
  expect_equal(unname(p[2, ]), c(652469, 6862035), tolerance = 2e-3)       # Paris : coordonnées Lambert-93 connues (~ 652 km, 6 862 km)
})

test_that("mise en page : métropole dominante, Corse à droite, DROM en encarts sous la métropole", {
  lay <- EC$projeter_fond(FOND_DEP)
  expect_setequal(names(lay$encarts), c("971", "972", "973", "974", "976"))
  expect_equal(lay$hauteur, 1200); expect_true(all(nchar(lay$chemins) > 0))
  xy <- function(code) { m <- lay$long[lay$long$code == code, ]; c(mean(m$x), -mean(m$y)) }
  expect_gt(xy("2A")[1], xy("13")[1]); expect_gt(xy("2B")[1], xy("06")[1])   # Corse à l'est du continent
  expect_gt(xy("971")[2], 1000); expect_gt(xy("976")[1], xy("971")[1])      # DROM dans la rangée d'encarts, ordre 971 -> 976
  expect_lt(xy("59")[2], xy("13")[2])                                        # Nord en haut (y SVG croissant vers le bas)
  layr <- EC$projeter_fond(FOND_REG); expect_setequal(names(layr$encarts), c("01", "02", "03", "04", "06"))
})

test_that("FONDAMENTAL — PCS partiellement diffusables : couverture A = 50 %, B = 0 % (gris), C = 100 % ; sélection PCS 1 et PCS 2", {
  p <- EC$preparer_carte_departs(TABLE_ABC, "pcs", "departement", FOND_DEP)
  cv <- p$couverture |> dplyr::arrange(code)
  expect_identical(cv$code, c("01", "33", "64"))
  expect_equal(cv$part_diff_pct[cv$code == "33"], 50); expect_equal(cv$part_diff_pct[cv$code == "64"], 0); expect_equal(cv$part_diff_pct[cv$code == "01"], 100)
  expect_identical(cv$statut, c("diffusable", "diffusable", "masque"))                 # B (64) = gris, pas « sans donnée »
  expect_identical(p$territoires$statut[p$territoires$code == "64"], "masque")
  expect_identical(p$territoires$statut[p$territoires$code == "75"], "sans_donnee")    # Paris absent de la table : pas de donnée, jamais secret
  st <- function(code, cat) { r <- p$cellules[p$cellules$code == code & p$cellules$cat == cat, ]; if (nrow(r) == 0) "sans" else r$statut }
  expect_identical(c(st("33", "311D"), st("64", "311D"), st("01", "311D")), c("diffuse", "masque", "diffuse"))   # PCS 1 : A coloré, B gris, C coloré
  expect_identical(c(st("33", "622A"), st("64", "622A"), st("01", "622A")), c("masque", "masque", "diffuse"))    # PCS 2 : A gris, B gris, C coloré
  expect_identical(st("75", "311D"), "sans")
  # valeur nulle diffusée (01 x 311D : 0 départ) : statut diffuse, valeur 0 conservée, distincte de « sans donnée »
  r0 <- p$cellules[p$cellules$code == "01" & p$cellules$cat == "311D", ]
  expect_identical(r0$statut, "diffuse"); expect_equal(r0$departs_central, 0)
  # catégories triées avec leur grande CS
  expect_identical(p$categories$code, c("311D", "622A")); expect_identical(p$categories$cs1, c("Cadres", "Ouvriers"))
})

test_that("SECRET : aucune valeur masquée dans la structure, le JSON, le HTML ni les infobulles ; table interne refusée", {
  # valeurs « internes » distinctives glissées dans des lignes masquées : elles doivent disparaître
  fuite <- TABLE_ABC
  fuite$effectif_champ[fuite$masque] <- 987654; fuite$departs_central[fuite$masque] <- 54321.9
  fuite$departs_bas[fuite$masque] <- 11111.1; fuite$departs_haut[fuite$masque] <- 22222.2; fuite$taux_depart_central_pct[fuite$masque] <- 77.77
  p <- EC$preparer_carte_departs(fuite, "pcs", "departement", FOND_DEP)
  expect_true(all(is.na(p$cellules$departs_central[p$cellules$statut == "masque"])))
  expect_true(all(is.na(p$cellules$effectif_champ[p$cellules$statut == "masque"])))
  j <- EC$donnees_json_carte(p)
  for (v in c("987654", "54321", "11111", "22222", "77.77", "77,77")) expect_false(grepl(v, j, fixed = TRUE), label = v)
  jj <- jsonlite::fromJSON(j, simplifyVector = FALSE)
  expect_identical(jj$cellules[["64"]][["311D"]], list(s = "m"))                       # statut seul
  expect_identical(jj$cellules[["33"]][["622A"]], list(s = "m"))
  expect_equal(jj$cellules[["33"]][["311D"]]$c, 27.4)                                   # valeur diffusée bien présente
  expect_equal(jj$couverture[["64"]], list(o = 2L, d = 0L, m = 2L, p = 0))
  f <- file.path(tempdir(), "carte_fuite.html")
  EC$generer_carte_departs(p, FOND_DEP, f)
  html <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (v in c("987654", "54321", "11111", "22222", "77.77", "77,77")) expect_false(grepl(v, html, fixed = TRUE), label = v)
  expect_true(grepl("Non diffusé — secret statistique", html, fixed = TRUE)); expect_true(grepl("Pas de donnée observée", html, fixed = TRUE))
  expect_true(grepl("27.4", html, fixed = TRUE))                                        # la valeur diffusée, elle, y est
  expect_false(grepl("<script src=|https?://cdn|@import", html))                        # aucune ressource externe
  expect_true(grepl('data-code="64"', html, fixed = TRUE) && grepl('data-code="975"', html, fixed = TRUE) == FALSE)
  # table interne (sans colonne masque) refusée ; masque NA = masqué
  expect_error(EC$preparer_carte_departs(TABLE_ABC |> dplyr::select(-masque), "pcs", "departement", FOND_DEP), "DIFFUSION")
  na <- TABLE_ABC; na$masque[1] <- NA
  expect_identical(EC$preparer_carte_departs(na, "pcs", "departement", FOND_DEP)$cellules$statut[1], "masque")
})

test_that("jointure géographique : codes texte conservés, code sans géométrie = ARRÊT avec la liste, « inconnu » hors carte et compté", {
  t <- TABLE_ABC; t$geo_code <- c("01", "01", "2A", "2A", "976", "976"); t$geo_nom <- NA_character_
  p <- EC$preparer_carte_departs(t, "pcs", "departement", FOND_DEP)
  expect_identical(sort(unique(p$cellules$code)), c("01", "2A", "976"))
  expect_identical(p$territoires$nom[p$territoires$code == "2A"], "Corse-du-Sud")      # nom du fond quand la table n'en donne pas
  expect_identical(p$territoires$nom[p$territoires$code == "01"], "Ain")               # 01 = Ain (jamais 1, jamais Guadeloupe)
  expect_identical(p$territoires$nom[p$territoires$code == "976"], "Mayotte")
  p2 <- EC$preparer_carte_departs(TABLE_ABC, "pcs", "departement", FOND_DEP)
  expect_identical(p2$territoires$nom[p2$territoires$code == "64"], "Pyrénées-Atlantiques")   # libellé de la table (référentiel projet) prioritaire
  t2 <- TABLE_ABC; t2$geo_code[5:6] <- "99"; t2$geo_code[3:4] <- "2C"
  expect_error(EC$preparer_carte_departs(t2, "pcs", "departement", FOND_DEP), "2 code\\(s\\) sans géométrie.*2C, 99")
  t3 <- TABLE_ABC; t3$geo_code[5:6] <- "inconnu"
  p3 <- EC$preparer_carte_departs(t3, "pcs", "departement", FOND_DEP)
  expect_equal(p3$n_non_localises, 1); expect_false("inconnu" %in% p3$cellules$code)
  # région : jointure sur region_code ; DROM région 01 = Guadeloupe, pas l'Ain
  r <- TABLE_ABC |> dplyr::transmute(region_code = c("84", "84", "01", "01", "94", "94"), region_nom = c("ARA", "ARA", "Guadeloupe", "Guadeloupe", "Corse", "Corse"),
                                     pcs, cs1, effectif_champ, departs_central, departs_bas, departs_haut, taux_depart_central_pct, masque, motif_masque)
  pr <- EC$preparer_carte_departs(r, "pcs", "region", FOND_REG)
  expect_identical(pr$territoires$nom[pr$territoires$code == "01"], "Guadeloupe")
  expect_identical(pr$territoires$statut[pr$territoires$code == "01"], "masque")
  expect_identical(pr$territoires$statut[pr$territoires$code == "11"], "sans_donnee")
  expect_equal(sum(pr$territoires$statut != "sans_donnee"), 3)
  out <- capture.output(n <- EC$controler_carte_departs(pr))
  expect_equal(unname(n), c(3, 3, 1, 2, 15)); expect_true(grepl("joints   3", out))
})

test_that("CSV de diffusion avec BOM UTF-8 : noms de colonnes propres, codes texte, décimales françaises, masque logique", {
  f <- file.path(tempdir(), "diff_bom.csv")
  write.csv2(TABLE_ABC, f, row.names = FALSE)
  b <- readBin(f, "raw", file.size(f)); writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), b), f)
  d <- EC$lire_csv_diffusion(f)
  expect_identical(names(d)[1], "region_code"); expect_false(any(grepl("﻿", names(d))))
  expect_type(d$geo_code, "character"); expect_identical(d$geo_code[5], "01"); expect_type(d$pcs, "character")
  expect_equal(d$departs_central[1], 27.4); expect_identical(d$masque, TABLE_ABC$masque)
  expect_true(is.na(d$departs_central[2]))
  p <- EC$preparer_carte_departs(d, "pcs", "departement", FOND_DEP)
  expect_equal(p$couverture$part_diff_pct[p$couverture$code == "33"], 50)
  expect_error(EC$lire_csv_diffusion(file.path(tempdir(), "nexiste_pas.csv")), "introuvable")
})

test_that("cs1 et France entière : couverture par CS, carte-support nationale sans pseudo-échelle, valeur nationale affichée", {
  cs <- tibble::tibble(geo_code = c("33", "33", "33", "33", "01"), geo_nom = "x", cs1 = c("Cadres", "Employes", "Ouvriers", "Prof. intermediaires", "Ouvriers"),
                       effectif_champ = c(70, NA, 90, 60, 40), departs_central = c(16.6, NA, 40, 20, 12), departs_bas = 1, departs_haut = 2,
                       taux_depart_central_pct = c(23.7, NA, 44, 33, 30), masque = c(FALSE, TRUE, FALSE, FALSE, FALSE), motif_masque = NA)
  p <- EC$preparer_carte_departs(cs, "cs1", "departement", FOND_DEP)
  expect_equal(p$couverture$n_obs[p$couverture$code == "33"], 4); expect_equal(p$couverture$n_masq[p$couverture$code == "33"], 1)
  expect_true(all(is.na(p$categories$cs1))); expect_identical(p$categories$code, c("Cadres", "Employes", "Ouvriers", "Prof. intermediaires"))
  fr <- tibble::tibble(cs1 = c("Cadres", "Employes"), effectif_champ = c(1396, NA), departs_central = c(350.9, NA), departs_bas = c(326.9, NA),
                       departs_haut = c(376.1, NA), taux_depart_central_pct = c(25.1, NA), masque = c(FALSE, TRUE), motif_masque = c(NA, "primaire"))
  pf <- EC$preparer_carte_departs(fr, "cs1", "france", FOND_DEP)
  expect_identical(pf$territoires$code, "FR"); expect_identical(pf$cellules$code, c("FR", "FR"))
  f <- file.path(tempdir(), "carte_fr.html"); EC$generer_carte_departs(pf, FOND_DEP, f)
  html <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_true(grepl("France entière — résultat national", html, fixed = TRUE))
  expect_true(grepl('id="national"', html, fixed = TRUE)); expect_false(grepl('name="vue"', html, fixed = TRUE))   # pas de vue couverture territoriale
  expect_equal(length(gregexpr('data-code="FR"', html, fixed = TRUE)[[1]]), 101)                                 # tous les polygones portent le résultat national
  expect_true(grepl("350.9", html, fixed = TRUE))
})

test_that("08e sur les sorties réelles du dépôt : 6 cartes HTML (+ PNG), comptages, fichier absent = message, aucun fichier = arrêt", {
  src <- file.path(RACINE, "sorties")
  skip_if_not(file.exists(file.path(src, "departs_pcs", "diffusion", "departs_pcs_departement.csv")), "sorties absentes")
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  sys.source(chemin_script("00f_fonctions_cartes.R"), envir = env)
  d <- file.path(tempdir(), "cartes_reelles"); unlink(d, recursive = TRUE); dir.create(d)
  for (dm in c("pcs", "cs1")) { dir.create(file.path(d, paste0("departs_", dm), "diffusion"), recursive = TRUE)
    file.copy(list.files(file.path(src, paste0("departs_", dm), "diffusion"), full.names = TRUE), file.path(d, paste0("departs_", dm), "diffusion")) }
  assign("DIR_SORTIES", d, envir = env)
  out <- capture.output(suppressMessages(sys.source(chemin_script("08e_cartes_departs.R"), envir = env)))
  b <- env$cartes_departs
  expect_equal(nrow(b), 6); expect_true(all(b$statut == "ok"))
  for (dm in c("pcs", "cs1")) for (nv in c("france", "region", "departement")) {
    expect_true(file.exists(file.path(d, "cartes_departs", dm, sprintf("carte_%s_%s.html", dm, nv))), label = paste(dm, nv))
    expect_true(file.exists(file.path(d, "cartes_departs", dm, sprintf("carte_%s_%s.png", dm, nv))), label = paste(dm, nv))
  }
  expect_true(all(b$territoires_csv == b$joints))                                        # aucun territoire perdu
  expect_equal(b$joints[b$niveau == "departement"], c(20, 20)); expect_equal(b$sans_donnee[b$niveau == "departement"], c(81, 81))
  expect_true(any(grepl("pcs x departement", out)))
  # cohérence avec les CSV : cellules masquées = lignes masque = TRUE ; aucun nombre d'une ligne masquée dans le HTML (table interne si présente)
  csv <- env$lire_csv_diffusion(file.path(d, "departs_pcs", "diffusion", "departs_pcs_departement.csv"))
  html <- paste(readLines(file.path(d, "cartes_departs", "pcs", "carte_pcs_departement.html"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  jj <- jsonlite::fromJSON(sub(".*<script type=\"application/json\" id=\"donnees\">(.*?)</script>.*", "\\1", html), simplifyVector = FALSE)
  for (i in which(csv$masque)) expect_identical(jj$cellules[[csv$geo_code[i]]][[csv$pcs[i]]], list(s = "m"), label = paste(csv$geo_code[i], csv$pcs[i]))
  for (i in which(!csv$masque)[1:10]) expect_equal(jj$cellules[[csv$geo_code[i]]][[csv$pcs[i]]]$c, csv$departs_central[i])
  # un fichier absent : message, 5 cartes ; aucun : arrêt
  unlink(file.path(d, "departs_cs1", "diffusion", "departs_cs1_region.csv")); unlink(file.path(d, "cartes_departs"), recursive = TRUE)
  expect_message(capture.output(sys.source(chemin_script("08e_cartes_departs.R"), envir = env)), "absente.*departs_cs1_region.csv")
  expect_equal(sum(env$cartes_departs$statut == "ok"), 5)
  unlink(file.path(d, "departs_pcs"), recursive = TRUE); unlink(file.path(d, "departs_cs1"), recursive = TRUE)
  expect_error(suppressMessages(capture.output(sys.source(chemin_script("08e_cartes_departs.R"), envir = env))), "aucune table de diffusion")
  assign("GENERER_CARTES_DEPARTS", FALSE, envir = env)
  expect_message(sys.source(chemin_script("08e_cartes_departs.R"), envir = env), "désactivée")
})
