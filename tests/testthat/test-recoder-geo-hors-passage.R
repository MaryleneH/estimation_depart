# ==============================================================================
# 01d : départements absents de la table de passage département -> région
# (ex. « 99 ») recodés « inconnu » en amont, sans toucher aux scripts existants.
# Données SYNTHÉTIQUES. Specs : SPEC-PREP-013.
# ==============================================================================
lancer_01d <- function(bts, stock = NULL, geo = "departement", actif = TRUE) {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  sys.source(chemin_script("00c_fonctions_geo.R"), envir = env)
  assign("GEO_ANALYSE", geo, envir = env); assign("RECODER_GEO_HORS_PASSAGE", actif, envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), paste0("geo01d_", geo, "_", actif, "_", is.null(stock))), envir = env)
  assign("bts", bts, envir = env)
  if (!is.null(stock)) assign("stock_tous_ages", stock, envir = env)
  sys.source(chemin_script("01d_recoder_geo_hors_passage.R"), envir = env)
  env
}
bts_synth <- tibble::tibble(
  id = 1:12, siren = rep(c("E1", "E2", "E3"), 4), sexe = "F", age_2024 = 50L, pcs = "3719", cs1 = "Cadres",
  geo_code = c("33", "33", "33", "99", "99", "99", "99", "99", "99", "99", "2A", NA), geo_nom = c(rep("Gironde", 3), rep("Étranger", 7), "Corse-du-Sud", NA),
  geo_type = "departement")
stock_synth <- tibble::tibble(geo_code = c("33", "99", "2A", "99"), geo_nom = c("Gironde", "Étranger", "Corse-du-Sud", "Étranger"),
                              cs1 = c("Cadres", "Cadres", "Cadres", "Ouvriers"), effectif_tous_ages = c(10L, 20L, 5L, 4L))

test_that("SPEC-PREP-013 — le code « 99 » sans région est recodé « inconnu », compté, tracé ; ajouter_region ne s'arrête plus", {
  expect_error(suppressMessages(lancer_01d(bts_synth, actif = FALSE)$ajouter_region(bts_synth, file.path(RACINE, "data", "passage_departement_region.csv"), "08c")), "99")
  expect_message(E <- lancer_01d(bts_synth, stock_synth), "7 salarié\\(s\\) dans 1 département\\(s\\)")
  b <- E$bts
  expect_equal(sum(b$geo_code == "inconnu", na.rm = TRUE), 7L)
  expect_true(all(b$geo_nom[b$geo_code %in% "inconnu"] == "Territoire inconnu"))
  expect_identical(b$geo_code[1:3], c("33", "33", "33")); expect_identical(b$geo_code[11], "2A"); expect_true(is.na(b$geo_code[12]))   # NA d'origine intact
  expect_equal(nrow(b), 12L)                                                       # aucune ligne perdue
  expect_identical(E$geo_hors_passage$geo_code, "99"); expect_equal(E$geo_hors_passage$n_salaries, 7L)
  f <- file.path(E$DIR_SORTIES, "geo_hors_passage_departement.csv"); expect_true(file.exists(f))
  expect_identical(utils::read.csv2(f, colClasses = "character")$geo_code, "99")
  # stock tous âges : même recodage, cellules regroupées sur « inconnu »
  s <- E$stock_tous_ages
  expect_false("99" %in% s$geo_code); expect_equal(s$effectif_tous_ages[s$geo_code == "inconnu" & s$cs1 == "Cadres"], 20L)
  expect_equal(s$effectif_tous_ages[s$geo_code == "inconnu" & s$cs1 == "Ouvriers"], 4L); expect_equal(sum(s$effectif_tous_ages), 39L)
  # la jointure région passe désormais, « inconnu » -> « Région inconnue »
  r <- suppressMessages(E$ajouter_region(b, file.path(RACINE, "data", "passage_departement_region.csv"), "08c"))
  expect_identical(r$region_code[r$geo_code %in% "inconnu"][1], "inconnu"); expect_identical(r$region_code[1], "75")
})

test_that("01d — sans objet hors zonage département, désactivable, et sans effet quand tout est rattaché", {
  expect_message(E0 <- lancer_01d(bts_synth, geo = "ze"), "sans objet"); expect_identical(E0$bts, bts_synth)
  expect_message(E1 <- lancer_01d(bts_synth, actif = FALSE), "désactivé"); expect_identical(E1$bts, bts_synth)
  ok <- bts_synth[bts_synth$geo_code %in% c("33", "2A"), ]
  expect_message(E2 <- lancer_01d(ok), "rien à recoder"); expect_identical(E2$bts, ok)
  expect_false(file.exists(file.path(E2$DIR_SORTIES, "geo_hors_passage_departement.csv")))
})

test_that("SPEC-PREP-013 — chaîne réelle en mode département avec un « 99 » injecté : 08c et 08d passent, les salariés sont dans « inconnu », rien n'est perdu", {
  env <- new.env(); old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), "chaine_99"), envir = env); dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(c("00c_fonctions_geo.R", "00g_format_restitution.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R",
                    "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R", "01c_stock_tous_ages.R"), env)
  b <- env$bts; n_avant <- nrow(b); idx <- which(b$geo_code == b$geo_code[1])[1:7]          # 7 salariés passent en « 99 » (hors France)
  b$geo_code[idx] <- "99"; b$geo_nom[idx] <- "Étranger"; assign("bts", b, envir = env)
  s <- env$stock_tous_ages; s <- dplyr::bind_rows(s, tibble::tibble(geo_code = "99", geo_nom = "Étranger", cs1 = "Cadres", effectif_tous_ages = 30L)); assign("stock_tous_ages", s, envir = env)
  # sans 01d : l'arrêt historique ; avec 01d : la chaîne passe
  expect_error(suppressMessages(env$ajouter_region(b, env$GEO_PASSAGES[["departement->region"]], "08c")), "99")
  expect_message(sys.source(chemin_script("01d_recoder_geo_hors_passage.R"), envir = env), "7 salarié")
  sourcer_scripts(c("02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R", "02c_importer_invalidite_eacr.R", "03_parametres_csp.R",
                    "04_projection_2030.R", "08_analyse_55plus_geo.R", "08b_departs_geo_cs.R", "08c_departs_pcs.R", "08d_departs_cs1.R"), env)
  expect_equal(nrow(env$bts_projete), n_avant)                                           # aucune ligne perdue
  expect_false("99" %in% env$bts_projete$geo_code)
  expect_equal(sum(env$bts_projete$geo_code == "inconnu"), 7L)
  dep <- env$departs_cs1$analytique$departement
  expect_true("inconnu" %in% dep$geo_code); expect_equal(sum(dep$effectif_champ[dep$geo_code == "inconnu"]), 7L)
  expect_identical(unique(dep$region_code[dep$geo_code == "inconnu"]), "inconnu")
  expect_equal(sum(dep$effectif_champ), n_avant)                                         # France entière = tout bts_projete
  expect_true(file.exists(file.path(env$DIR_SORTIES, "geo_hors_passage_departement.csv")))
})
