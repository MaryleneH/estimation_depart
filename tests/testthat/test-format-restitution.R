# ==============================================================================
# Convention d'arrondi des restitutions (00g) : personnes -> entier (demi vers
# le haut, NA conservé), taux -> 1 décimale, composantes cohérentes avec leur
# total (plus forts restes), objets analytiques jamais modifiés.
# ==============================================================================
EF <- new.env()
sys.source(chemin_script("00g_format_restitution.R"), envir = EF)

test_that("arrondir_nombre_personnes : cas limites documentés, demi vers le haut, NA jamais 0", {
  x <- c(0, 0.1, 0.4, 0.5, 0.6, 1.4, 1.5, 1.6, 2.5, 27.4, 27.5, 27.6, NA)
  attendu <- c(0, 0, 0, 1, 1, 1, 2, 2, 3, 27, 28, 28, NA)
  expect_equal(EF$arrondir_nombre_personnes(x), attendu)
  expect_true(is.na(EF$arrondir_nombre_personnes(NA_real_)))              # secret : NA reste NA
  expect_equal(EF$arrondir_nombre_personnes(c(NA, 3.2)), c(NA, 3))
  expect_true(is.na(EF$arrondir_nombre_personnes(NaN)))
  expect_equal(EF$arrondir_nombre_personnes(118.6), 119); expect_equal(EF$arrondir_nombre_personnes(3.2), 3); expect_equal(EF$arrondir_nombre_personnes(1.7), 2)
  expect_equal(EF$arrondir_nombre_personnes(1234567.5), 1234568)
  # différence assumée avec round() de R (demi vers le pair) : 0,5 -> 0 et 2,5 -> 2 en R base
  expect_equal(round(c(0.5, 2.5)), c(0, 2)); expect_equal(EF$arrondir_nombre_personnes(c(0.5, 2.5)), c(1, 3))
  expect_equal(EF$arrondir_taux(c(32.64, 32.65, 0.049, NA)), c(32.6, 32.6, 0, NA))   # taux : 1 décimale (round R)
  expect_identical(EF$fmt_personnes(c(27.4, 1234.5, NA)), c("27", "1 235", "n.d."))
})

test_that("arrondir_composantes_avec_total : la somme affichée des composantes = le total affiché", {
  cas <- list(c(24.4, 1.4, 1.4), c(10.6, 2.6, 1.6), c(0.5, 0.5, 0.5), c(33.3, 33.3, 33.4), c(1.25, 1.25, 1.25, 1.25),
              c(99.9, 0.05, 0.05), c(0, 0, 0), c(12, 7, 3), c(4.49, 4.49, 4.49, 4.49), runif(7) * 50)
  for (x in cas) {
    r <- EF$arrondir_composantes_avec_total(x)
    expect_equal(sum(r), EF$arrondir_nombre_personnes(sum(x)), label = paste(x, collapse = ","))
    expect_true(all(r == round(r)) && all(r >= floor(x)) && all(r <= ceiling(x)), label = paste(x, collapse = ","))   # chaque composante : plancher ou plafond
  }
  expect_equal(EF$arrondir_composantes_avec_total(c(24.4, 1.4, 1.4)), c(25, 1, 1))     # 27,2 -> 27 : restes égaux (0,4) -> l'unité va à la plus grande composante (distorsion relative minimale)
  expect_equal(sum(EF$arrondir_composantes_avec_total(c(24.4, 1.4, 1.4))), 27)
  expect_equal(round(24.4) + round(1.4) + round(1.4), 26)                                 # l'arrondi naïf ne retombe PAS sur 27
  expect_equal(EF$arrondir_composantes_avec_total(c(10.6, 2.6, 1.6)), c(11, 3, 1))      # 14,8 -> 15 ; naïf : 11 + 3 + 2 = 16 ; restes égaux -> plus grandes composantes d'abord
  expect_equal(EF$arrondir_composantes_avec_total(c(12, 7, 3)), c(12, 7, 3))            # entiers : inchangés
  # total fourni explicitement (le total EXACT du modèle, pas la somme des composantes affichées)
  expect_equal(sum(EF$arrondir_composantes_avec_total(c(24.4, 1.4, 1.4), total = 27.2)), 27)
  expect_equal(sum(EF$arrondir_composantes_avec_total(c(24.4, 1.4, 1.4), total = 27.6)), 28)
  # NA (secret) : aucune contrainte, NA conservé
  expect_equal(EF$arrondir_composantes_avec_total(c(24.4, NA, 1.4)), c(24, NA, 1))
  expect_equal(EF$arrondir_composantes_avec_total(c(24.4, 1.4, 1.4), total = NA), c(24, 1, 1))
  expect_length(EF$arrondir_composantes_avec_total(numeric(0)), 0)
  expect_error(EF$arrondir_composantes_avec_total(c(-1, 2)), "négatives")
})

test_that("formater_restitution : personnes -> entier, taux -> 1 décimale, codes intacts, NA conservés, décompositions par ligne ; l'objet d'origine est intact", {
  t <- tibble::tibble(geo_code = c("01", "2A", "971"), pcs = c("311D", "622A", "0000"), siren = "012345678",
                      effectif_champ = c(84L, 40L, NA), departs_central = c(27.2, 14.8, NA), departs_bas = c(25.5, 13.5, NA), departs_haut = c(29.49, 16.5, NA),
                      dep_retraite = c(24.4, 10.6, NA), dep_invalidite = c(1.4, 2.6, NA), dep_deces = c(1.4, 1.6, NA),
                      taux_depart_central_pct = c(32.619, 37.04, NA), part_dominante_pct = c(12.34, 100, NA), n_entreprises = c(5L, 3L, NA),
                      masque = c(FALSE, FALSE, TRUE), motif_masque = c(NA, NA, "primaire"))
  t0 <- t
  f <- EF$formater_restitution(t)
  expect_identical(t, t0)                                                                  # jamais de modification en place
  expect_equal(f$departs_central, c(27, 15, NA)); expect_equal(f$departs_bas, c(26, 14, NA)); expect_equal(f$departs_haut, c(29, 17, NA))
  expect_equal(f$dep_retraite + f$dep_invalidite + f$dep_deces, f$departs_central)         # cohérence total / causes, ligne par ligne
  expect_equal(f$dep_retraite, c(25, 11, NA)); expect_equal(f$dep_invalidite, c(1, 3, NA)); expect_equal(f$dep_deces, c(1, 1, NA))
  expect_equal(f$taux_depart_central_pct, c(32.6, 37, NA)); expect_equal(f$part_dominante_pct, c(12.3, 100, NA))
  expect_identical(f$geo_code, c("01", "2A", "971")); expect_identical(f$pcs, t$pcs); expect_identical(f$siren, t$siren)   # codes intacts
  expect_identical(f$effectif_champ, t$effectif_champ); expect_identical(f$n_entreprises, t$n_entreprises)              # entiers observés intacts
  expect_identical(f$masque, t$masque); expect_identical(f$motif_masque, t$motif_masque)
  expect_true(all(is.na(f[3, c("departs_central", "dep_retraite", "taux_depart_central_pct")])))                        # masqué : NA, jamais 0
  # autres familles de colonnes du projet
  g <- EF$formater_restitution(tibble::tibble(departs_55plus = 118.4, dep_55_retraite = 100.6, dep_55_invalidite = 9.4, dep_55_deces = 8.4,
                                              taux_depart_55plus_pct = 41.26, departs_2030 = 82.5, part_departs_pct = 32.65, effectif_tous_ages = 300L))
  expect_equal(g$departs_55plus, 118); expect_equal(g$dep_55_retraite + g$dep_55_invalidite + g$dep_55_deces, 118)
  expect_equal(g$departs_2030, 83); expect_equal(g$taux_depart_55plus_pct, 41.3); expect_equal(g$part_departs_pct, 32.6); expect_identical(g$effectif_tous_ages, 300L)
  # tableau 55+ : departs / dont_* / bas / haut / taux_pct
  h <- EF$formater_restitution(tibble::tibble(territoire = "Gironde", effectif = 200L, departs = 82.5, dont_retraite = 70.2, dont_invalidite = 6.2, dont_deces = 6.1,
                                              bas = 78.4, haut = 86.6, taux_pct = 41.25, est_total = FALSE))
  expect_equal(h$departs, 83); expect_equal(h$dont_retraite + h$dont_invalidite + h$dont_deces, 83); expect_equal(h$bas, 78); expect_equal(h$haut, 87)
  expect_equal(h$taux_pct, 41.2); expect_identical(h$territoire, "Gironde"); expect_identical(h$est_total, FALSE)
})

test_that("NON-RÉGRESSION numérique : la chaîne produit des objets analytiques EXACTS, identiques avant/après restitution ; CSV entiers, taux exacts", {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), "fmt_chaine"), envir = env); dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(c("00c_fonctions_geo.R", "00g_format_restitution.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R",
                    "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R",
                    "02b_importer_mortalite_insee.R", "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R",
                    "05_resultats_entreprises.R", "08_analyse_55plus_geo.R", "08b_departs_geo_cs.R", "08c_departs_pcs.R", "08d_departs_cs1.R"), env)
  A <- env$departs_pcs$analytique$departement
  # 1. objets analytiques : précision complète conservée (ni entiers, ni 1 décimale)
  expect_false(all(A$departs_central == round(A$departs_central)))
  expect_false(all(A$departs_central == round(A$departs_central, 1)))
  expect_equal(sum(A$departs_central), sum(env$bts_projete$p_central), tolerance = 1e-9)   # = somme exacte des probabilités
  expect_false(all(env$synthese_geo$departs_55plus == round(env$synthese_geo$departs_55plus)))
  # 2. l'export est une fonction PURE de l'objet exact : relecture du CSV = formater_restitution(objet)
  lu <- read.csv2(file.path(env$DIR_SORTIES, "departs_pcs", "interne", "departs_pcs_departement.csv"), stringsAsFactors = FALSE,
                  colClasses = c(geo_code = "character", region_code = "character", pcs = "character"), fileEncoding = "UTF-8-BOM")
  att <- env$formater_restitution(A)
  expect_equal(lu$departs_central, att$departs_central); expect_equal(lu$dep_retraite, att$dep_retraite)
  expect_true(all(lu$departs_central == round(lu$departs_central)))                      # personnes : entiers
  expect_equal(lu$dep_retraite + lu$dep_invalidite + lu$dep_deces, lu$departs_central)   # causes cohérentes
  expect_equal(lu$taux_depart_central_pct, round(A$taux_depart_central_pct, 1))          # taux : 1 décimale, calculé sur l'EXACT...
  expect_false(isTRUE(all.equal(lu$taux_depart_central_pct, round(100 * lu$departs_central / lu$effectif_champ, 1))))   # ...jamais sur les départs arrondis
  # 3. diffusion : masqués NA (jamais 0), visibles entiers
  d <- read.csv2(file.path(env$DIR_SORTIES, "departs_pcs", "diffusion", "departs_pcs_departement.csv"), stringsAsFactors = FALSE,
                 colClasses = c(geo_code = "character", region_code = "character", pcs = "character"), fileEncoding = "UTF-8-BOM")
  expect_true(all(is.na(d$departs_central[d$masque]))); expect_true(all(d$departs_central[!d$masque] == round(d$departs_central[!d$masque])))
  expect_false(any(d$departs_central[d$masque] %in% 0))
  # 4. autres CSV : 05, 08 (synthèse, criticité, tableau 55+), 08b, 08d
  e05 <- read.csv2(file.path(env$DIR_SORTIES, "departs_2030_par_entreprise.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  expect_true(all(e05$departs_2030 == round(e05$departs_2030))); expect_true(any(e05$part_departs_pct != round(e05$part_departs_pct)))
  s08 <- read.csv2(file.path(env$DIR_SORTIES, "analyse_55plus_par_departement.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  expect_true(all(s08$departs_55plus == round(s08$departs_55plus)))
  expect_equal(s08$dep_55_retraite + s08$dep_55_invalidite + s08$dep_55_deces, s08$departs_55plus)
  expect_true(any(s08$taux_depart_55plus_pct != round(s08$taux_depart_55plus_pct), na.rm = TRUE))
  t08 <- read.csv2(file.path(env$DIR_SORTIES, "tableau_departs_55plus_departement.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  expect_equal(t08$dont_retraite + t08$dont_invalidite + t08$dont_deces, t08$departs); expect_true(all(t08$bas == round(t08$bas)))
  b08 <- read.csv2(file.path(env$DIR_SORTIES, "departs_par_departement_cs.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  expect_true(all(b08$departs_2030[!b08$masque] == round(b08$departs_2030[!b08$masque])))
  c08 <- read.csv2(file.path(env$DIR_SORTIES, "departs_cs1", "diffusion", "departs_cs1_region.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  expect_true(all(c08$departs_central[!c08$masque] == round(c08$departs_central[!c08$masque])))
  # 5. HTML du tableau 55+ : total et « dont » cohérents ligne par ligne, taux à 1 décimale
  h <- readLines(file.path(env$DIR_SORTIES, "tableau_departs_55plus_departement.html"), encoding = "UTF-8", warn = FALSE)
  lignes <- grep("<tr", h, value = TRUE); lignes <- lignes[grepl("<td", lignes)]
  for (l in lignes) {
    v <- regmatches(l, gregexpr("<td class=\"num\">[^<]*</td>", l))[[1]]; v <- gsub("<[^>]+>| | ", "", v)
    expect_equal(as.numeric(v[3]) + as.numeric(v[4]) + as.numeric(v[5]), as.numeric(v[2]), label = l)
    expect_false(grepl(",", v[2], fixed = TRUE), label = l); expect_true(grepl(",", v[6], fixed = TRUE) || v[6] == "–", label = l)
  }
})
