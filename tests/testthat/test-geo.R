# ==============================================================================
# Tests de la couche géographique générique (R/00c_fonctions_geo.R)
# ==============================================================================

test_that("mapping : une colonne source au nom arbitraire devient geo_code", {
  # colonne « DEP_ETAB », comme pourrait la livrer une extraction
  src <- tibble::tibble(DEP_ETAB = c("33", "01", "2A"), age = c(50, 60, 45))
  contrat <- c(geo_code = "DEP_ETAB")
  df <- src |> dplyr::rename(dplyr::all_of(contrat))
  df <- normaliser_geo(df, "departement", "departement")
  expect_true(all(c("geo_code", "geo_nom", "geo_type") %in% names(df)))
  expect_false("DEP_ETAB" %in% names(df))
  expect_identical(df$geo_code, c("33", "01", "2A"))
  expect_identical(unique(df$geo_type), "departement")

  # même chose avec un autre nom, sans toucher au code métier
  src2 <- tibble::tibble(X_GEO_42 = c("64", "75"))
  df2 <- src2 |> dplyr::rename(dplyr::all_of(c(geo_code = "X_GEO_42")))
  df2 <- normaliser_geo(df2, "departement", "departement")
  expect_identical(df2$geo_code, c("64", "75"))
})

test_that("codes : toujours en texte, « 01 », « 2A », « 2B », « 33 » intacts", {
  df <- tibble::tibble(geo_code = c("01", "2A", "2B", "33", " 09 "))
  out <- normaliser_geo(df, "departement", "departement")
  expect_type(out$geo_code, "character")
  expect_identical(out$geo_code, c("01", "2A", "2B", "33", "09"))
})

test_that("codes : une colonne numérique est complétée par des zéros si GEO_CODE_LARGEUR", {
  df <- tibble::tibble(geo_code = c(1L, 9L, 33L))
  expect_warning(normaliser_geo(df, "departement", "departement"),
                 "NUMÉRIQUE")
  out <- normaliser_geo(df, "departement", "departement", largeur = 2)
  expect_identical(out$geo_code, c("01", "09", "33"))
})

test_that("contrôle de schéma : colonne géographique absente -> erreur explicite", {
  expect_error(controler_colonnes_geo(c("siren", "age", "DEP"),
                                      c(code = "DEP_ETAB", nom = NA)),
               "Colonne géographique configurée 'DEP_ETAB' absente")
  expect_error(controler_colonnes_geo(c("siren"), c(code = "DEP_ETAB")),
               "Colonnes présentes")
  expect_silent(controler_colonnes_geo(c("siren", "DEP_ETAB"), c(code = "DEP_ETAB", nom = NA)))
})

test_that("référentiel : libellés complétés, code inconnu conservé et signalé", {
  ref <- ecrire_ref_dep()
  df <- tibble::tibble(geo_code = c("33", "01", "99"))
  expect_message(
    out <- normaliser_geo(df, "departement", "departement",
                          referentiels = list(departement = ref)),
    "1 observation\\(s\\) sans libellé")
  expect_identical(out$geo_nom, c("Gironde", "Ain", "99"))   # code affiché si inconnu
  expect_identical(out$geo_code, c("33", "01", "99"))         # jamais supprimé
})

test_that("code + nom dans la source : les deux colonnes sont utilisées", {
  df <- tibble::tibble(geo_code = c("33", "01"), geo_nom = c("Gironde", ""))
  ref <- ecrire_ref_dep()
  out <- normaliser_geo(df, "departement", "departement",
                        referentiels = list(departement = ref))
  expect_identical(out$geo_nom, c("Gironde", "Ain"))  # nom vide complété
})

test_that("nom seul : correspondance inverse via référentiel, sinon avertissement", {
  ref <- ecrire_ref_dep()
  df <- tibble::tibble(geo_nom = c("Gironde", "Ain", "Atlantide"))
  expect_warning(
    out <- normaliser_geo(df, "departement", "departement",
                          referentiels = list(departement = ref)),
    "libellé sert d'identifiant")
  expect_identical(out$geo_code, c("33", "01", "Atlantide"))
  # sans aucun référentiel : jamais d'identifiant fabriqué en silence
  expect_warning(out2 <- normaliser_geo(df, "departement", "departement"),
                 "Aucune colonne code")
  expect_identical(out2$geo_code, out2$geo_nom)
})

test_that("passage : source plus fine (commune) -> département, sans correspondance = inconnu", {
  passage <- tempfile(fileext = ".csv")
  writeLines(c("code_source;code_cible;nom_cible",
               "33063;33;Gironde", "01053;01;Ain"), passage)
  df <- tibble::tibble(geo_code = c("33063", "01053", "99999"))
  expect_message(
    out <- normaliser_geo(df, "commune", "departement",
                          passages = list("commune->departement" = passage)),
    "1 observation\\(s\\) sans correspondance commune->departement")
  expect_identical(out$geo_code, c("33", "01", NA))
  expect_identical(out$geo_nom,  c("Gironde", "Ain", NA))
  # table de passage absente = bloquant
  expect_error(normaliser_geo(df, "commune", "departement", passages = list()),
               "Table de passage 'commune->departement' introuvable")
})

test_that("GEO_INTERET : hors périmètre comptés, codes absents signalés, zéro match = arrêt", {
  df <- tibble::tibble(geo_code = c("33", "33", "01", "64", "75"))
  expect_warning(
    expect_message(out <- filtrer_geo_interet(df, c("33", "01", "98")),
                   "2 observation\\(s\\) hors liste écartée\\(s\\)"),
    "1 code\\(s\\) absent\\(s\\) des données : 98")
  expect_identical(sort(unique(out$geo_code)), c("01", "33"))
  # vecteur nommé : les codes sont les names(), les valeurs des libellés
  expect_identical(codes_geo_interet(c("33" = "Gironde", "01" = "Ain")), c("33", "01"))
  expect_error(suppressWarnings(filtrer_geo_interet(df, c("98", "97"))),
               "aucune correspondance")
  # NULL = pas de filtre
  expect_identical(filtrer_geo_interet(df, NULL), df)
})

test_that("GEO_INTERET nommé sert de table de libellés", {
  df <- tibble::tibble(geo_code = c("33", "01"))
  out <- normaliser_geo(df, "departement", "departement",
                        geo_interet = c("33" = "Gironde", "01" = "Ain"))
  expect_identical(out$geo_nom, c("Gironde", "Ain"))
})

test_that("noms de fichiers de sortie suffixés par le zonage", {
  expect_identical(basename(fichier_sortie_geo("analyse_55plus_par_%s.csv", "ze", tempdir())),
                   "analyse_55plus_par_ze.csv")
  expect_identical(basename(fichier_sortie_geo("criticite_55plus_%s_cs.csv", "departement", tempdir())),
                   "criticite_55plus_departement_cs.csv")
  expect_error(zonage_geo("epci"), "inconnu de GEO_ZONAGES")
})

test_that("garde de migration : l'ancienne configuration ZE est refusée", {
  env <- new.env()
  assign("ZE_INTERET", c("8401"), envir = env)
  expect_error(garde_migration_geo(env), "OBSOLÈTE")
  expect_true(garde_migration_geo(new.env()))
})

test_that("Arrow : seules les colonnes déclarées sont lues, renommage générique", {
  skip_if_not_installed("arrow")
  f <- tempfile(fileext = ".parquet")
  arrow::write_parquet(tibble::tibble(
    siren = c("A", "A", "B"), sexe = c("H", "F", "H"), age = c(50L, 30L, 60L),
    pcs = c("3812", "4620", "6543"), DEP_ETAB = c("33", "01", "2A"),
    inutile = c(1, 2, 3)), f)
  bts <- charger_bts_parquet(f, c(siren = "siren", sexe = "sexe", age = "age", pcs = "pcs"),
                             c(code = "DEP_ETAB", nom = NA), age_min = 45, sirens = c("A", "B"))
  expect_setequal(names(bts), c("siren", "sexe", "age_2024", "pcs", "geo_code", "id", "generation"))
  expect_identical(bts$geo_code, c("33", "2A"))     # filtre âge appliqué
  expect_error(charger_bts_parquet(f, c(siren = "siren", sexe = "sexe", age = "age", pcs = "pcs"),
                                   c(code = "ABSENTE", nom = NA), 45, c("A")),
               "absente du fichier")
})

test_that("référentiel départemental livré : data/ref_departement.csv (code;nom, 101 départements)", {
  f <- file.path(RACINE, "data", "ref_departement.csv")
  expect_true(file.exists(f))
  ref <- lire_referentiel_geo(f)
  expect_identical(names(ref), c("code", "nom"))
  expect_equal(nrow(ref), 101)                                     # 96 métropole + 5 DROM
  expect_equal(anyDuplicated(ref$code), 0)
  expect_true(all(c("01", "2A", "2B", "75", "971", "976") %in% ref$code))
  expect_identical(ref$nom[ref$code == "33"], "Gironde")
  expect_identical(ref$nom[ref$code == "2A"], "Corse-du-Sud")
  expect_identical(ref$nom[ref$code == "974"], "La Réunion")
  expect_type(ref$code, "character")
  # codes seuls dans la source -> libellés complétés par le référentiel du projet
  df <- tibble::tibble(geo_code = c("33", "2B", "971", "09"))
  out <- normaliser_geo(df, "departement", "departement", referentiels = list(departement = f))
  expect_identical(out$geo_nom, c("Gironde", "Haute-Corse", "Guadeloupe", "Ariège"))
})

test_that("encodage : un référentiel enregistré en Windows-1252 (latin1) est lu sans erreur, libellés corrects", {
  f <- tempfile(fileext = ".csv")
  con <- file(f, open = "wb")
  writeBin(iconv("code;nom\n07;Ardèche\n2A;Corse-du-Sud\n33;Gironde\n", from = "UTF-8", to = "latin1", toRaw = TRUE)[[1]], con)
  close(con)
  expect_false(all(validUTF8(readLines(f, warn = FALSE))))            # le fichier n'est PAS en UTF-8
  ref <- lire_referentiel_geo(f)
  expect_identical(ref$nom[ref$code == "07"], "Ardèche")
  expect_true(all(validUTF8(ref$nom)))
  out <- normaliser_geo(tibble::tibble(geo_code = c("07", "33")), "departement", "departement",
                        referentiels = list(departement = f))
  expect_identical(out$geo_nom, c("Ardèche", "Gironde"))
  # libellés portés par la source elle-même, en latin1
  src <- tibble::tibble(geo_code = "07", geo_nom = iconv("Ardèche", "UTF-8", "latin1"))
  expect_identical(normaliser_geo(src, "departement", "departement")$geo_nom, "Ardèche")
  expect_identical(reparer_utf8(c("déjà valide", NA)), c("déjà valide", NA))
})

test_that("codage du sexe : 1/2, H/F, M/F, Hommes/Femmes acceptés ; valeur inconnue = arrêt explicite", {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  sys.source(chemin_script("00c_fonctions_geo.R"), envir = env)
  suppressMessages(suppressWarnings(invisible(capture.output(
    sys.source(chemin_script("01_fabriquer_donnees_test.R"), envir = env)))))
  f <- env$normaliser_sexe
  expect_identical(f(c("1", "2", "H", "F", "M", "f", " Hommes ", "Femmes", "Masculin", "Féminin")),
                   c("H", "F", "H", "F", "H", "F", "H", "F", "H", "F"))
  expect_identical(f(c(1L, 2L)), c("H", "F"))
  expect_error(f(c("H", "X", "9")), "codage du sexe non reconnu pour 2 ligne")
  expect_error(f(c("H", NA)), "codage du sexe non reconnu")
  expect_true(all(env$bts$sexe %in% c("H", "F")))                  # table test normalisée
})

test_that("04 : un sexe non apparié produit un arrêt explicite, pas des p_central NA", {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  for (s in c("00c_fonctions_geo.R", "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R",
              "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
              "02c_importer_invalidite_eacr.R", "03_parametres_csp.R"))
    suppressMessages(suppressWarnings(invisible(capture.output(sys.source(chemin_script(s), envir = env)))))
  env$bts$sexe <- "1"                                               # contourne le recodage du 01
  expect_error(suppressMessages(capture.output(sys.source(chemin_script("04_projection_2030.R"), envir = env))),
               "p_central est NA .* sexe non codé H/F")
})
