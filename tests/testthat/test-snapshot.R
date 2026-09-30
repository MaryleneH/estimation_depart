# ==============================================================================
# Instantané numérique de la chaîne (utils/utils_snapshot_resultats.R) : la
# chaîne complète, sur la table test, doit reproduire EXACTEMENT les valeurs
# figées dans tests/testthat/reference/snapshot_<zonage>.csv (effectifs, SIREN,
# totaux par grande CS, scénarios, territoires). Sert de preuve qu'une
# réorganisation du code ne change aucun résultat. À re-figer volontairement à
# chaque changement de méthode ou de paramètre, jamais pour un refactoring.
# ==============================================================================
source(file.path(RACINE, "utils", "utils_snapshot_resultats.R"), local = TRUE)

SCRIPTS_SNAPSHOT <- c("00c_fonctions_geo.R", "00d_fonctions_fiches.R", "01_fabriquer_donnees_test.R",
                      "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R",
                      "02b_importer_mortalite_insee.R", "02c_importer_invalidite_eacr.R",
                      "03_parametres_csp.R", "04_projection_2030.R", "08_analyse_55plus_geo.R",
                      "08b_departs_geo_cs.R", "09_fiches_territoriales.R")

snapshot_pour <- function(geo) {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", geo, envir = env); assign("GEO_SOURCE", geo, envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), paste0("snap_", geo)), envir = env)
  dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(SCRIPTS_SNAPSHOT, env)
  snapshot_chaine(env)
}

for (geo in c("ze", "departement")) {
  test_that(sprintf("instantané %s : aucun écart avec la référence figée", geo), {
    ref_csv <- file.path(RACINE, "tests", "testthat", "reference", sprintf("snapshot_%s.csv", geo))
    skip_if_not(file.exists(ref_csv), "référence absente")
    ref <- read.csv2(ref_csv, stringsAsFactors = FALSE)
    snap <- snapshot_pour(geo)
    expect_setequal(snap$cle, ref$cle)
    m <- merge(ref, snap, by = "cle", suffixes = c("_ref", "_new"))
    expect_equal(m$valeur_new, m$valeur_ref, tolerance = 1e-9)
  })
}
