# ==============================================================================
# Non-régression MÉTHODOLOGIQUE du champ d'étude : 45 ans et + (AGE_MIN_BTS).
# Vérifie que le changement de champ 43+ -> 45+ est effectif de bout en bout
# (filtrage, tranches, dénominateurs, restitutions) et qu'aucune référence à
# l'ancien champ ne subsiste hors des occurrences légitimes listées.
# ==============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# Chaîne complète en mode test (ZE) puis mode département, une fois par fichier
lancer_chaine <- function(geo = "ze") {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(file.path("R", "00_config.R"), envir = env)
  assign("GEO_ANALYSE", geo, envir = env); assign("GEO_SOURCE", geo, envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), paste0("champ45_", geo)), envir = env)
  dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  for (s in c("00c_fonctions_geo.R", "00d_fonctions_fiches.R", "01_fabriquer_donnees_test.R",
              "01b_agreger_pcs.R", "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
              "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R",
              "05_resultats_entreprises.R", "07b_tableau_contibution_sans_gt.R",
              "08_analyse_55plus_geo.R", "09_fiches_territoriales.R"))
    suppressMessages(suppressWarnings(invisible(capture.output(
      sys.source(file.path("R", s), envir = env)))))
  env
}
ENV_ZE <- lancer_chaine("ze")

test_that("A/B. filtrage : aucun salarié de moins de 45 ans dans bts ni bts_projete", {
  expect_identical(ENV_ZE$AGE_MIN_BTS, 45)
  expect_gte(min(ENV_ZE$bts$age_2024), 45)
  expect_gte(min(ENV_ZE$bts_projete$age_2024), 45)
  expect_false(any(ENV_ZE$bts_projete$age_2024 %in% c(43, 44)))
})

test_that("A bis. parquet : le filtre d'âge est poussé à Arrow avec AGE_MIN_BTS", {
  skip_if_not_installed("arrow")
  f <- tempfile(fileext = ".parquet")
  arrow::write_parquet(tibble::tibble(
    siren = "A", sexe = "H", age = c(43L, 44L, 45L, 60L), pcs = "3812", DEP = "33"), f)
  bts <- charger_bts_parquet(f, c(siren = "siren", sexe = "sexe", age = "age", pcs = "pcs"),
                             c(code = "DEP", nom = NA), age_min = ENV_ZE$AGE_MIN_BTS, sirens = "A")
  expect_identical(sort(bts$age_2024), c(45L, 60L))
})

test_that("C. tranches d'âge : première classe « 45-48 ans », plus aucune « 43-48 »", {
  expect_identical(ENV_ZE$LABELS_TRANCHES[1], "45-48 ans")
  expect_false(any(grepl("43", ENV_ZE$LABELS_TRANCHES)))
  tr <- cut(ENV_ZE$bts$age_2024, ENV_ZE$BREAKS_TRANCHES, ENV_ZE$LABELS_TRANCHES)
  expect_false(any(is.na(tr)))                       # tout le champ est couvert
  expect_true(all(ENV_ZE$bts$age_2024[tr == "45-48 ans"] %in% 45:48))
})

test_that("D. dénominateur : part_55plus_pct = 100 * effectif_55plus / effectif_45plus", {
  sg <- ENV_ZE$synthese_geo
  expect_true("effectif_45plus" %in% names(sg))
  expect_false("effectif_43plus" %in% names(sg))
  expect_equal(sg$part_55plus_pct, 100 * sg$effectif_55plus / sg$effectif_45plus)
  # cohérence : l'effectif du champ est bien celui des 45+
  expect_equal(sum(sg$effectif_45plus), sum(ENV_ZE$bts_projete$age_2024 >= 45))
})

test_that("E. fiches : aucune occurrence de l'ancien champ, libellés 45+", {
  fiches <- list.files(file.path(ENV_ZE$DIR_SORTIES, "fiches_ze"), pattern = "\\.html$", full.names = TRUE)
  expect_gt(length(fiches), 1)
  txt <- paste(unlist(lapply(fiches, readLines, warn = FALSE)), collapse = "\n")
  expect_false(grepl("43\\+|43 ans|43-48", txt))
  expect_true(grepl("salariés de 45 ans et \\+", txt))
  expect_true(grepl("45-48 ans", txt))
  expect_true(grepl("des salariés de 45 ans et plus", txt))     # phrase « À retenir »
})

test_that("F. quadrant : axe X et taille des points sur les 45+", {
  g <- ENV_ZE$g
  expect_match(g$labels$x, "dans l'effectif 45\\+")
  expect_identical(g$labels$size, "Effectif 45+")
  expect_false(grepl("43", g$labels$title)); expect_false(grepl("43", g$labels$caption))
  expect_match(g$labels$caption, "salariés de 45 ans et \\+")
})

test_that("G. cohérence des totaux 45+ entre 05, 07b, 08 et fiches", {
  n_champ <- nrow(ENV_ZE$bts_projete)
  expect_equal(sum(ENV_ZE$resultats_entreprises$effectif_45plus_2024), n_champ)      # 05
  expect_equal(ENV_ZE$effectif, n_champ)                                             # 07b
  expect_equal(sum(ENV_ZE$synthese_geo$effectif_45plus), n_champ)                    # 08
  j <- ENV_ZE$journal_fiches |> dplyr::filter(statut == "ok")
  expect_equal(sum(j$effectif), n_champ)                                             # 09 (tous diffusables)
  expect_equal(sum(j$departs), sum(ENV_ZE$bts_projete$p_central), tolerance = 1e-9)
  expect_equal(round(sum(ENV_ZE$bts_projete$p_central)), ENV_ZE$tab_contribution$departs[4])
})

test_that("département : même champ 45+, fiches et sorties suffixées", {
  env <- lancer_chaine("departement")
  expect_gte(min(env$bts_projete$age_2024), 45)
  expect_true(file.exists(file.path(env$DIR_SORTIES, "analyse_55plus_par_departement.csv")))
  txt <- paste(readLines(file.path(env$DIR_SORTIES, "fiches_departement", "33_gironde.html"), warn = FALSE), collapse = "\n")
  expect_false(grepl("43", txt))
})

test_that("contrôle exhaustif : plus de « 43 » hors occurrences légitimes (sources externes)", {
  fichiers <- c(list.files(file.path(RACINE, "R"), pattern = "\\.R$", full.names = TRUE),
                list.files(file.path(RACINE, "utils"), pattern = "\\.R$", full.names = TRUE),
                file.path(RACINE, "main.R"))
  # Liste blanche NOMINATIVE : bornes de tranches de SOURCES méthodologiques
  # (invalidité EIR/EACR, population active) et codes PCS de la table test.
  legitimes <- list(
    "00_config.R" = c("43,         49,",                    # TRANCHES_DEFAUT (bande source)
                      "\"H\",   43, 0.0023", "\"F\",   43, 0.0029",  # T_INVALIDITE_BASE
                      "commence à 43 ans = borne de la SOURCE"),
    "02c_importer_invalidite_eacr.R" = c("43_49", "borne_inf = c(43,50,55,60,62)", "le taux 43-49"),
    "01_fabriquer_donnees_test.R" = c("\"6543\""),         # code PCS-ESE 6543 (ouvriers)
    "00d_fonctions_fiches.R" = character(0), "08_analyse_55plus_geo.R" = character(0))
  residus <- character(0)
  for (f in fichiers) {
    l <- readLines(f, warn = FALSE)
    hits <- grep("\\b43\\b|43\\+|43plus|43-48", l)
    for (i in hits) {
      ok <- any(vapply(legitimes[[basename(f)]] %||% character(0),
                       function(p) grepl(p, l[i], fixed = TRUE), logical(1)))
      if (!ok) residus <- c(residus, sprintf("%s:%d: %s", basename(f), i, trimws(l[i])))
    }
  }
  expect_identical(residus, character(0))
})
