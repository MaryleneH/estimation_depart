# ==============================================================================
# Non-régression du mode ZE : la chaîne complète (table test, graine 2024,
# configuration par défaut) doit reproduire EXACTEMENT les résultats figés
# dans tests/testthat/reference/ avant le découplage géographique.
# Territoire à territoire, sur les 10 indicateurs du script 08.
# ==============================================================================

test_that("mode ZE : résultats du 08 identiques à la référence d'avant découplage", {
  ref_csv <- file.path(RACINE, "tests", "testthat", "reference", "analyse_55plus_par_ze.csv")
  skip_if_not(file.exists(ref_csv), "référence absente")

  # Chaîne complète dans un environnement isolé, depuis la racine du projet
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  scripts <- c("00_config.R", "00c_fonctions_geo.R", "01_fabriquer_donnees_test.R",
               "01b_agreger_pcs.R", "02_importer_nettoyer_drees.R",
               "02b_importer_mortalite_insee.R", "02c_importer_invalidite_eacr.R",
               "03_parametres_csp.R", "04_projection_2030.R")
  suppressMessages(suppressWarnings(invisible(capture.output(
    for (s in scripts) sys.source(file.path("R", s), envir = env)))))
  assign("DIR_SORTIES", tempdir(), envir = env)      # pas d'écriture dans sorties/
  suppressMessages(suppressWarnings(invisible(capture.output(
    sys.source(file.path("R", "08_analyse_55plus_geo.R"), envir = env)))))

  expect_identical(unique(env$bts_projete$geo_type), "ze")

  indicateurs <- c("effectif_43plus", "effectif_55plus", "part_55plus_pct",
                   "departs_55plus", "departs_55plus_bas", "departs_55plus_haut",
                   "dep_55_retraite", "dep_55_invalidite", "dep_55_deces",
                   "taux_depart_55plus_pct")
  ref <- read.csv2(ref_csv, check.names = FALSE, stringsAsFactors = FALSE)
  nouveau <- env$synthese_geo |>
    dplyr::mutate(dplyr::across(dplyr::all_of(indicateurs), ~ round(.x, 1)))

  expect_equal(nrow(nouveau), nrow(ref))
  cmp <- dplyr::inner_join(ref, nouveau, by = c("ze" = "geo_nom"),
                           suffix = c("_ref", "_new"))
  expect_equal(nrow(cmp), nrow(ref))                 # tous les territoires retrouvés
  # tolérance = epsilon flottant (valeurs arrondies à 1 décimale des deux côtés)
  for (v in indicateurs)
    expect_equal(cmp[[paste0(v, "_new")]], cmp[[paste0(v, "_ref")]],
                 tolerance = 1e-9, label = v)
})
