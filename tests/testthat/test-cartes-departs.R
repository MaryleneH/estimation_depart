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
sys.source(chemin_script("00g_format_restitution.R"), envir = EC)
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
  expect_equal(lay$hauteur, 1230); expect_true(all(nchar(lay$chemins) > 0))
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
  expect_true(grepl("Résultat non diffusé — secret statistique", html, fixed = TRUE)); expect_true(grepl("Aucun salarié observé", html, fixed = TRUE))
  expect_true(grepl("27.4", html, fixed = TRUE))                                        # la valeur diffusée, elle, y est
  expect_false(grepl("<script src=|https?://cdn|@import", html))                        # aucune ressource externe
  expect_true(grepl('data-code="64"', html, fixed = TRUE)); expect_false(grepl('data-code="975"', html, fixed = TRUE))
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
  expect_equal(unname(n), c(3, 3, 1, 2, 15)); expect_true(any(grepl("joints   3", out)))   # + une ligne « libellés PCS » en dimension pcs
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
                       departs_haut = c(376.1, NA), taux_depart_central_pct = c(25.1, NA), dep_retraite = c(304.9, NA), dep_invalidite = c(21.8, NA),
                       dep_deces = c(24.2, NA), masque = c(FALSE, TRUE), motif_masque = c(NA, "primaire"))
  pf <- EC$preparer_carte_departs(fr, "cs1", "france")
  expect_identical(pf$territoires$code, "FR"); expect_identical(pf$cellules$code, c("FR", "FR"))
  f <- file.path(tempdir(), "dash_fr.html"); EC$generer_carte_departs(pf, NULL, f)
  html <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  # tableau de bord national : AUCUNE carte, KPI, fourchette, composition, onglets cs1, champ en tête
  expect_false(grepl("<svg", html, fixed = TRUE)); expect_false(grepl('name="vue"', html, fixed = TRUE))
  for (id in c("kpi-c", "kpi-t", "kpi-e", "f-b", "f-c", "f-h", "barre", "causes", "zone-masque", "cat-affichee")) expect_true(grepl(sprintf('id="%s"', id), html, fixed = TRUE), label = id)
  expect_true(grepl('class="tabs"', html, fixed = TRUE)); expect_true(grepl('data-cat="Employes"', html, fixed = TRUE))
  expect_true(grepl("France entière", html, fixed = TRUE)); expect_true(grepl("salariés concernés", html, fixed = TRUE))
  expect_true(grepl("pourraient partir d’ici 2030", html, fixed = TRUE)); expect_true(grepl("Qui est concerné ?", html, fixed = TRUE))
  expect_true(grepl("350.9", html, fixed = TRUE))
  jj <- jsonlite::fromJSON(sub(".*<script type=\"application/json\" id=\"donnees\">(.*?)</script>.*", "\\1", html), simplifyVector = FALSE)
  expect_identical(jj$cellules$FR$Employes, list(s = "m"))                               # catégorie nationale masquée : statut seul
  expect_equal(jj$cellules$FR$Cadres$r, 304.9); expect_identical(jj$libelles$causes$r, "Retraite ou fin de carrière")
  # pcs France : filtre + liste, pas d'onglets
  pp <- EC$preparer_carte_departs(TABLE_ABC |> dplyr::select(-region_code, -region_nom, -geo_code, -geo_nom) |> dplyr::slice(1:2), "pcs", "france")
  f2 <- file.path(tempdir(), "dash_fr_pcs.html"); EC$generer_carte_departs(pp, NULL, f2)
  h2 <- paste(readLines(f2, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_true(grepl('id="rech"', h2, fixed = TRUE)); expect_false(grepl('class="tabs"', h2, fixed = TRUE))
})

test_that("CHAMP : source unique construite depuis la configuration (AGE_MIN_BTS, PCS_VERS_CS1), jamais codée en dur ; libellés UI sans nom de colonne", {
  ch <- EC$construire_libelle_champ()
  expect_identical(ch$court, sprintf("salariés de %d ans ou plus · entreprises du périmètre BITD", EC$AGE_MIN_BTS))
  expect_identical(ch$salaries_age, sprintf("salariés de %d ans ou plus", EC$AGE_MIN_BTS))
  expect_true(any(grepl("périmètre BITD", ch$detaille))); expect_false(any(grepl("champ", ch$detaille)))     # langage simple : jamais « champ »
  expect_true(any(grepl("cadres, professions intermédiaires, employés, ouvriers", ch$detaille)))
  expect_true(any(grepl("agriculteurs exploitants, artisans, commerçants et chefs d’entreprise", ch$detaille)))
  expect_true(any(grepl("d’ici 2030", ch$detaille)))
  # environnement de test : 45 -> 50 ans, Ouvriers retirés du modèle : TOUT le HTML suit
  E50 <- new.env(); .wd <- setwd(RACINE); source(chemin_script("00_config.R"), local = E50); setwd(.wd)
  assign("AGE_MIN_BTS", 50L, envir = E50); assign("PCS_VERS_CS1", c("3" = "Cadres", "4" = "Prof. intermediaires", "5" = "Employes"), envir = E50)
  assign("DIR_DATA", file.path(RACINE, "data"), envir = E50)
  sys.source(chemin_script("00f_fonctions_cartes.R"), envir = E50)
  ch50 <- E50$construire_libelle_champ()
  expect_identical(ch50$court, "salariés de 50 ans ou plus · entreprises du périmètre BITD"); expect_false(any(grepl("ouvriers", ch50$detaille)))
  expect_identical(E50$construire_libelles_public(ch50)$salaries_age, "Salariés de 50 ans ou plus")
  expect_identical(E50$construire_libelles_public(ch50)$legende$effectif, "Nombre de salariés de 50 ans ou plus")
  for (nv in c("france", "departement")) {
    p <- E50$preparer_carte_departs(if (nv == "france") TABLE_ABC[1:2, ] else TABLE_ABC, "pcs", nv, if (nv == "france") NULL else FOND_DEP)
    f <- file.path(tempdir(), sprintf("champ50_%s.html", nv)); E50$generer_carte_departs(p, if (nv == "france") NULL else FOND_DEP, f)
    h <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    expect_true(grepl("50 ans ou plus", h, fixed = TRUE), label = nv); expect_false(grepl("45 ans", h, fixed = TRUE), label = nv)
    expect_false(grepl("ouvriers", h, fixed = TRUE), label = nv)
    expect_true(grepl("Qui est concerné ?", h, fixed = TRUE), label = nv)
    expect_true(grepl("Salariés de 50 ans ou plus", h, fixed = TRUE), label = nv)   # en-tête, légende, KPI : même phrase dynamique
    expect_equal(length(gregexpr("50 ans ou plus", h, fixed = TRUE)[[1]]) >= 3, TRUE, label = nv)   # en-tête, JSON, pied : même phrase partout
    # aucun nom de colonne R dans la page
    expect_false(grepl("taux_depart_central_pct|departs_central|effectif_champ|dep_retraite|region_code|geo_code|motif_masque", h), label = nv)
  }
  # LIBELLES_UI : source unique des libellés
  expect_identical(EC$LIBELLES_UI$indicateurs[["effectif"]], "Nombre de salariés")
  expect_identical(EC$LIBELLES_UI$indicateurs[["taux"]], "Part des salariés susceptibles de partir")
  expect_identical(EC$LIBELLES_UI$couverture$libelles[4], "Tous les résultats affichables (100 %)")
  expect_identical(EC$LIBELLES_UI$couverture$titre, "Où peut-on afficher les résultats ?")
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
  champ <- env$construire_libelle_champ()
  for (dm in c("pcs", "cs1")) for (nv in c("france", "region", "departement")) {
    fh <- file.path(d, "cartes_departs", dm, sprintf("carte_%s_%s.html", dm, nv))
    expect_true(file.exists(fh), label = paste(dm, nv))
    expect_identical(file.exists(file.path(d, "cartes_departs", dm, sprintf("carte_%s_%s.png", dm, nv))), nv != "france", label = paste(dm, nv))
    h <- paste(readLines(fh, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    expect_true(grepl(sprintf("<b>Salariés de %d ans ou plus</b> · entreprises du périmètre BITD", env$AGE_MIN_BTS), h, fixed = TRUE), label = paste(dm, nv))   # qui est concerné, en tête de CHAQUE page
    expect_true(grepl("Résultat non diffusé — secret statistique", h, fixed = TRUE), label = paste(dm, nv))
    # LANGAGE PUBLIC : aucun jargon statistique dans les pages
    texte <- gsub("<script type=\"application/json\"[^<]*</script>", "", h)                    # hors données embarquées
    expect_false(grepl("Salariés du champ|Effectif du champ|Champ étudié|champ étudié|Population du champ|Salariés dans le champ|Comprendre le champ|iffusabilit|Effectif|Taux de départ estimé", texte), label = paste(dm, nv))
    expect_identical(grepl("<svg", h, fixed = TRUE), nv != "france", label = paste(dm, nv))                  # France : tableau de bord, pas de carte
    expect_false(grepl("taux_depart_central_pct|departs_central|effectif_champ|motif_masque", h), label = paste(dm, nv))
    expect_false(grepl("<script src=|https?://cdn|@import", h), label = paste(dm, nv))
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

test_that("CONTOURS : source unique STYLE_CONTOURS_CARTE, trait gris (jamais blanc), limites régionales sur la carte départementale seulement, survol au-dessus des voisins, mobile", {
  S <- EC$STYLE_CONTOURS_CARTE
  for (k in c("couleur", "largeur", "sans_couleur", "region_couleur", "region_largeur", "survol_couleur", "survol_largeur", "mobile_facteur", "png_couleur"))
    expect_true(!is.null(S[[k]]), label = k)
  expect_false(toupper(S$couleur) %in% c("#FFF", "#FFFFFF"))                               # un contour blanc disparaît sur les classes claires
  css <- EC$css_cartes()
  expect_true(grepl(sprintf("path.t{stroke:%s;stroke-width:%s;vector-effect:non-scaling-stroke", S$couleur, S$largeur), css, fixed = TRUE))
  expect_false(grepl("path.t{stroke:#fff", css, fixed = TRUE))
  expect_true(grepl(sprintf(".limites-reg path{fill:none;stroke:%s;stroke-width:%s", S$region_couleur, S$region_largeur), css, fixed = TRUE))
  expect_true(grepl(sprintf("#survol{fill:none;stroke:%s;stroke-width:%s", S$survol_couleur, S$survol_largeur), css, fixed = TRUE))
  expect_true(grepl(sprintf("@media (max-width:640px){path.t{stroke-width:%s}", S$largeur * S$mobile_facteur), css, fixed = TRUE))
  # carte départementale avec fond des régions : 13 limites régionales (métropole), DROM exclus ; sans fond : aucune ; région : jamais
  p <- EC$preparer_carte_departs(TABLE_ABC, "pcs", "departement", FOND_DEP)
  n_reg <- function(html) { g <- regmatches(html, regexpr('<g class="limites-reg"[^§]*?</g>', html, perl = TRUE)); if (length(g) == 0) 0L else lengths(regmatches(g, gregexpr("<path ", g, fixed = TRUE))) }
  f <- file.path(tempdir(), "contours_dep.html"); EC$generer_carte_departs(p, FOND_DEP, f, fond_regions = FOND_REG)
  html <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_equal(n_reg(html), 13L)
  expect_true(grepl('<path id="survol" fill="none" d=""', html, fixed = TRUE)); expect_true(grepl('id="t-33"', html, fixed = TRUE))
  expect_true(grepl('surligner(code)', html, fixed = TRUE))
  f0 <- file.path(tempdir(), "contours_dep0.html"); EC$generer_carte_departs(p, FOND_DEP, f0)
  expect_equal(n_reg(paste(readLines(f0, encoding = "UTF-8", warn = FALSE), collapse = "\n")), 0L)
  tr <- TABLE_ABC |> dplyr::transmute(region_code = c("11", "11", "24", "24", "27", "27"), region_nom = region_code, pcs, cs1, effectif_champ, departs_central, departs_bas, departs_haut, taux_depart_central_pct, masque, motif_masque)
  pr <- EC$preparer_carte_departs(tr, "pcs", "region", FOND_REG)
  fr <- file.path(tempdir(), "contours_reg.html"); EC$generer_carte_departs(pr, FOND_REG, fr, fond_regions = FOND_REG)   # ignoré hors département
  expect_equal(n_reg(paste(readLines(fr, encoding = "UTF-8", warn = FALSE), collapse = "\n")), 0L)
  # le style n'a touché ni les données ni le secret : même JSON embarqué avec ou sans limites régionales
  j <- function(h) regmatches(h, regexpr('<script type="application/json" id="donnees">[^§]*?</script>', h, perl = TRUE))
  expect_identical(j(html), j(paste(readLines(f0, encoding = "UTF-8", warn = FALSE), collapse = "\n")))
  # PNG : limites régionales acceptées
  skip_if_not_installed("ggplot2")
  fp <- file.path(tempdir(), "contours_dep.png")
  EC$png_carte_departs(p, EC$projeter_fond(FOND_DEP), fp, "t", lay_regions = EC$projeter_fond(FOND_REG))
  expect_true(file.exists(fp) && file.size(fp) > 1000)
})

test_that("LIBELLÉS PCS : nomenclature xlsx lue (codes normalisés), libellé affiché quand il existe, repli code + grande catégorie sinon, fichier absent = NULL", {
  skip_if_not_installed("readxl")
  XLSX_PCS <- file.path(RACINE, "data", "PCS-ESE_2017_Liste.xlsx")   # vrai fichier du dépôt (DIR_DATA relatif dans la config)
  lib <- EC$charger_libelles_pcs(XLSX_PCS)
  expect_type(lib, "character"); expect_true(length(lib) >= 10); expect_true(all(nchar(names(lib)) > 0))
  expect_identical(names(lib), EC$normaliser_code_pcs(names(lib)))   # noms déjà normalisés
  expect_null(suppressMessages(EC$charger_libelles_pcs(file.path(tempdir(), "inexistant.xlsx"))))
  expect_error(EC$charger_libelles_pcs(XLSX_PCS, cols = c(code = "Code 2017", libelle = "Colonne_absente")), "absente")
  # rapprochement insensible à la casse et aux espaces : 311D (table) <-> " 311d " (nomenclature)
  demo <- c(" 311d " = "Ingénieurs de démonstration", "999Z" = "Jamais présent")
  demo <- setNames(demo, EC$normaliser_code_pcs(names(demo)))
  p <- EC$preparer_carte_departs(TABLE_ABC, "pcs", "departement", FOND_DEP, libelles_pcs = demo)
  expect_identical(p$categories$libelle, c("Ingénieurs de démonstration", NA))
  expect_identical(p$categories$code, c("311D", "622A"))
  f <- file.path(tempdir(), "carte_lib.html"); EC$generer_carte_departs(p, FOND_DEP, f)
  html <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_true(grepl('"lib":"Ingénieurs de démonstration"', html, fixed = TRUE))
  expect_false(grepl("Jamais présent", html, fixed = TRUE))                  # seules les PCS de la table sont embarquées
  expect_true(grepl("Rechercher un code PCS ou un libellé", html, fixed = TRUE))
  # sans nomenclature : aucun libellé, structure inchangée
  p0 <- EC$preparer_carte_departs(TABLE_ABC, "pcs", "departement", FOND_DEP)
  expect_true(all(is.na(p0$categories$libelle)))
  f0 <- file.path(tempdir(), "carte_lib0.html"); EC$generer_carte_departs(p0, FOND_DEP, f0)
  expect_false(grepl('"lib":', paste(readLines(f0, encoding = "UTF-8", warn = FALSE), collapse = "\n"), fixed = TRUE))
  # dimension cs1 : jamais de libellé PCS
  pc <- EC$preparer_carte_departs(TABLE_ABC, "cs1", "departement", FOND_DEP, libelles_pcs = demo)
  expect_true(all(is.na(pc$categories$libelle)))
})

test_that("ORDRE des PCS : départs estimés diffusés décroissants, sommés sur les territoires ; masqués exclus ; sans résultat en fin ; cs1 inchangé", {
  t <- tibble::tibble(
    region_code = rep(c("11", "24"), each = 4), region_nom = "R",
    geo_code = rep(c("75", "21"), each = 4), geo_nom = rep(c("Paris", "Côte-d'Or"), each = 4),
    pcs = rep(c("A1", "B2", "C3", "D4"), 2), cs1 = "Cadres",
    effectif_champ = 100, departs_central = c(10, 30, NA, 5,   50, 2, NA, 5), departs_bas = 1, departs_haut = 99,
    taux_depart_central_pct = 10, masque = c(FALSE, FALSE, TRUE, FALSE,  FALSE, FALSE, TRUE, FALSE), motif_masque = NA)
  # A1 = 60 ; B2 = 32 ; D4 = 10 ; C3 : tout masqué -> dernier. Une cellule masquée avec une valeur ne compte jamais.
  t2 <- t; t2$departs_central[3] <- 1000                       # valeur sous masque : effacée avant tout usage
  p <- EC$preparer_carte_departs(t2, "pcs", "departement", FOND_DEP)
  expect_identical(p$categories$code, c("A1", "B2", "D4", "C3"))
  pf <- EC$preparer_carte_departs(t2 |> dplyr::filter(geo_code == "75") |> dplyr::select(-region_code, -region_nom, -geo_code, -geo_nom), "pcs", "france")
  expect_identical(pf$categories$code, c("B2", "A1", "D4", "C3"))   # national : 30 > 10 > 5, C3 masqué
  # égalité : code croissant ; la première option de la liste est la PCS la plus concernée
  t3 <- t; t3$departs_central <- 7; p3 <- EC$preparer_carte_departs(t3 |> dplyr::filter(!masque), "pcs", "departement", FOND_DEP)
  expect_identical(p3$categories$code, c("A1", "B2", "D4"))
  f <- file.path(tempdir(), "carte_ordre.html"); EC$generer_carte_departs(p, FOND_DEP, f)
  html <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_true(grepl('"categories":[{"code":"A1","cs1":"Cadres"},{"code":"B2","cs1":"Cadres"},{"code":"D4","cs1":"Cadres"},{"code":"C3","cs1":"Cadres"}]', html, fixed = TRUE))
  expect_false(grepl('"c":1000', html, fixed = TRUE))             # la valeur sous masque n’est jamais embarquée
  pc <- EC$preparer_carte_departs(t2 |> dplyr::mutate(cs1 = pcs), "cs1", "departement", FOND_DEP)
  expect_identical(pc$categories$code, c("A1", "B2", "C3", "D4"))   # grandes catégories : ordre des codes
})

test_that("SANS JAVASCRIPT (pièce jointe épurée par une messagerie) : vue initiale écrite dans le HTML — fill sur chaque territoire, légende, en-tête, liste, tableau de bord ; bandeau masqué par le JS ; aucune fuite", {
  p <- EC$preparer_carte_departs(TABLE_ABC, "pcs", "departement", FOND_DEP)
  f <- file.path(tempdir(), "carte_sans_js.html"); EC$generer_carte_departs(p, FOND_DEP, f, fond_regions = FOND_REG)
  html <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  sans <- gsub("<script\\b.*?</script>", "", html, perl = TRUE)            # ce que reçoit le destinataire
  paths <- regmatches(sans, gregexpr('<path class="t[^"]*"[^>]*>', sans))[[1]]
  expect_equal(length(paths), 101L)
  expect_true(all(grepl(' fill="#[0-9a-fA-F]{6}"', paths)))                 # jamais de tracé noir par défaut
  expect_true(all(grepl(' stroke="#', paths)))                                # contour même sans CSS
  couleurs <- sub('.* fill="(#[0-9a-fA-F]{6})".*', "\\1", paths)
  expect_true(all(couleurs %in% c(EC$COULEURS_CARTE$classes, EC$COULEURS_CARTE$secret, EC$COULEURS_CARTE$sans_donnee)))
  st <- function(code) sub('.*data-st="([a-z]+)".*', "\\1", paths[grepl(sprintf('data-code="%s"', code), paths)])
  expect_identical(st("33"), "diffuse"); expect_identical(st("64"), "masque"); expect_identical(st("01"), "diffuse"); expect_identical(st("75"), "sans")
  expect_true(grepl(sprintf('data-code="64"[^>]*fill="%s"', EC$COULEURS_CARTE$secret), sans))     # masqué = gris, aucun chiffre
  expect_true(grepl('class="t sans"[^>]*data-code="75"[^>]*stroke-dasharray', sans))
  expect_true(grepl('<g class="limites-reg"[^>]*>\\s*<path fill="none" stroke="', sans, perl = TRUE))   # limites régionales jamais remplies
  leg <- regmatches(sans, regexpr('<ul class="map-legend" id="legende">.*?</ul>', sans, perl = TRUE))
  expect_true(lengths(regmatches(leg, gregexpr("<li", leg))) >= 3)                                 # classes + secret + sans donnée
  expect_true(grepl(EC$LIBELLES_UI$secret, leg, fixed = TRUE)); expect_true(grepl(EC$LIBELLES_UI$sans_donnee, leg, fixed = TRUE))
  expect_true(grepl('<span class="cat-nom" id="sous-cat">PCS 311D · Cadres</span>', sans, fixed = TRUE))
  expect_true(grepl('<option value="311D" selected>311D — Cadres</option>', sans, fixed = TRUE))
  expect_true(grepl('id="sans-js"', sans, fixed = TRUE)); expect_true(grepl('Version statique', sans, fixed = TRUE))
  expect_true(grepl('getElementById("sans-js");if(sj)sj.style.display="none"', html, fixed = TRUE))  # le JS masque le bandeau
  for (v in c("987654", "54321", "11111", "22222", "77.77", "77,77")) expect_false(grepl(v, sans, fixed = TRUE), label = v)
  # tableau de bord France : chiffres de la première catégorie écrits dans le HTML
  tf <- TABLE_ABC |> dplyr::filter(geo_code == "33") |> dplyr::select(-region_code, -region_nom, -geo_code, -geo_nom)
  pf <- EC$preparer_carte_departs(tf, "pcs", "france")
  ff <- file.path(tempdir(), "dash_sans_js.html"); EC$generer_carte_departs(pf, NULL, ff)
  hf <- gsub("<script\\b.*?</script>", "", paste(readLines(ff, encoding = "UTF-8", warn = FALSE), collapse = "\n"), perl = TRUE)
  expect_true(grepl('id="kpi-c">27</div>', hf, fixed = TRUE)); expect_true(grepl('id="kpi-e">84</div>', hf, fixed = TRUE))
  expect_true(grepl('id="kpi-t">32,6 %</div>', hf, fixed = TRUE)); expect_true(grepl('id="f-b">25</div>', hf, fixed = TRUE))
  expect_true(grepl('id="sous-cat">PCS 311D · Cadres</span>', hf, fixed = TRUE))
  # première catégorie masquée : bloc « non diffusé » visible, KPI cachés, aucune valeur
  tm <- tf; tm$masque <- TRUE; tm[, c("effectif_champ", "departs_central", "departs_bas", "departs_haut", "taux_depart_central_pct")] <- NA
  pm <- EC$preparer_carte_departs(tm, "pcs", "france"); fm <- file.path(tempdir(), "dash_masque.html"); EC$generer_carte_departs(pm, NULL, fm)
  hm <- paste(readLines(fm, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_true(grepl('<div id="zone-kpi" style="display:none">', hm, fixed = TRUE)); expect_true(grepl('<div class="masque-national" id="zone-masque"><div', hm, fixed = TRUE))
  expect_true(grepl('id="kpi-c">n.d.</div>', hm, fixed = TRUE))
})
