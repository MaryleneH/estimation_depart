# ==============================================================================
# Départs par GRANDE CS (00e + 08d) : même fonction que le 08c avec la dimension
# cs1 ; mêmes mesures, mêmes mailles, même base, même secret. Contrôles :
# France, Σ régions, Σ départements, région/département, PCS -> cs1, 08b.
# ==============================================================================
SCRIPTS_CS1 <- c("00c_fonctions_geo.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R",
                 "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R", "01c_stock_tous_ages.R",
                 "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
                 "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R",
                 "08_analyse_55plus_geo.R", "08b_departs_geo_cs.R", "08c_departs_pcs.R", "08d_departs_cs1.R")
lancer_cs1 <- function(geo = "departement", geo_interet = NULL, secret = TRUE, avec_08c = TRUE) {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", geo, envir = env); assign("GEO_SOURCE", geo, envir = env)
  assign("GEO_INTERET", geo_interet, envir = env); assign("DEPARTS_CS1_SECRET", secret, envir = env)
  if (!avec_08c) assign("GENERER_DEPARTS_PCS", FALSE, envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), paste0("cs1_", geo, "_", length(geo_interet), "_", secret, "_", avec_08c)), envir = env)
  dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(SCRIPTS_CS1, env)
  env
}
E <- lancer_cs1("departement")
A <- E$departs_cs1$analytique; D <- E$departs_cs1$diffusion
P <- E$departs_pcs$analytique; M <- E$MESURES_DEPARTS_PCS
regrouper <- function(t, grp) t |> dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
  dplyr::summarise(dplyr::across(dplyr::all_of(M), sum), .groups = "drop") |> dplyr::arrange(dplyr::across(dplyr::all_of(grp)))

test_that("A. France x cs1 : 4 grandes CS du modèle, Σ effectifs = bts_projete, mêmes colonnes que PCS sans pcs", {
  expect_setequal(names(A), c("france", "region", "departement"))
  fr <- A$france
  expect_equal(nrow(fr), 4); expect_identical(names(fr)[1], "cs1")
  expect_setequal(fr$cs1, unique(E$bts_projete$cs1)); expect_true(all(fr$cs1 %in% unname(E$PCS_VERS_CS1)))
  expect_equal(sum(fr$effectif_champ), nrow(E$bts_projete))
  expect_equal(sum(fr$departs_central), sum(E$bts_projete$p_central), tolerance = 1e-9)
  expect_equal(fr$taux_depart_central_pct, 100 * fr$departs_central / fr$effectif_champ)
  for (n in names(A)) {                                  # colonnes = celles de PCS, pcs en moins
    expect_identical(names(A[[n]]), setdiff(names(P[[n]]), "pcs"), label = n)
    expect_identical(names(D[[n]]), setdiff(names(E$departs_pcs$diffusion[[n]]), "pcs"), label = n)
    expect_type(A[[n]]$cs1, "character"); expect_false(anyNA(A[[n]]$cs1))
  }
  expect_type(A$departement$geo_code, "character"); expect_type(A$region$region_code, "character")
  expect_identical(names(A$region)[1:3], c("region_code", "region_nom", "cs1"))
  expect_identical(names(A$departement)[1:5], c("region_code", "region_nom", "geo_code", "geo_nom", "cs1"))
})

test_that("B. CONTRÔLE FONDAMENTAL PCS -> cs1 : la table cs1 = la table PCS regroupée par cs1, 7 mesures, chaque niveau", {
  for (n in names(A)) {
    cles <- setdiff(names(A[[n]]), c("cs1", M, "taux_depart_central_pct", "n_entreprises", "part_dominante_pct"))
    a <- regrouper(P[[n]], c(cles, "cs1")); b <- A[[n]] |> dplyr::arrange(dplyr::across(dplyr::all_of(c(cles, "cs1"))))
    expect_equal(nrow(a), nrow(b), label = n)
    expect_identical(do.call(paste, a[c(cles, "cs1")]), do.call(paste, b[c(cles, "cs1")]), label = n)
    for (m in M) expect_equal(a[[m]], b[[m]], tolerance = 1e-12, label = paste(n, m))
  }
  expect_true(E$controler_cs1_vs_pcs(A, P))
  faux <- A; faux$region$departs_bas[1] <- faux$region$departs_bas[1] + 0.5
  expect_error(E$controler_cs1_vs_pcs(faux, P), "region : la table par cs1")
  # et contre la base directement (même regroupement que controler_pcs_vs_cs du 08c)
  b <- E$ajouter_parts_causes(E$bts_projete) |> dplyr::group_by(cs1) |>
    dplyr::summarise(effectif_champ = dplyr::n(), departs_central = sum(p_central), departs_bas = sum(p_bas),
                     departs_haut = sum(p_haut), dep_retraite = sum(part_ret), dep_invalidite = sum(part_inv),
                     dep_deces = sum(part_dec), .groups = "drop") |> dplyr::arrange(cs1)
  for (m in M) expect_equal(A$france[[m]], b[[m]], tolerance = 1e-12, label = m)
})

test_that("C/D/E. cohérence géographique par cs1 : Σ régions = France, Σ départements = France, Σ départements d'une région = région", {
  fr <- regrouper(A$france, "cs1")
  for (n in c("region", "departement")) {
    x <- regrouper(A[[n]], "cs1"); expect_identical(x$cs1, fr$cs1)
    for (m in M) expect_equal(x[[m]], fr[[m]], tolerance = 1e-9, label = paste(n, m))
  }
  d <- regrouper(A$departement, c("region_code", "cs1")); r <- A$region |> dplyr::arrange(region_code, cs1)
  expect_identical(paste(d$region_code, d$cs1), paste(r$region_code, r$cs1))
  for (m in M) expect_equal(d[[m]], r[[m]], tolerance = 1e-9, label = m)
  expect_true(E$controler_departs(A, E$base_cs1, dimension = "cs1"))
  faux <- A; faux$departement$effectif_champ[1] <- faux$departement$effectif_champ[1] + 1L
  expect_error(E$controler_departs(faux, E$base_cs1, dimension = "cs1"), "contrôle ÉCHOUÉ")
  # région construite comme au 08c : même table de passage, mêmes codes texte
  expect_identical(unique(A$departement$region_code[A$departement$geo_code == "2A"]), "94")
  expect_identical(sort(unique(A$region$region_code)), sort(unique(P$region$region_code)))
})

test_that("G. comparaison 08b : mêmes cellules département x cs1 -> mêmes chiffres ; différences de convention documentées", {
  b08 <- E$calculer_departs_geo_cs(E$base_geo_cs)$brut            # 08b non masqué, non arrondi
  j <- dplyr::inner_join(b08, A$departement, by = c("geo_code", "cs1"))
  expect_equal(nrow(j), nrow(b08)); expect_equal(nrow(j), nrow(A$departement))   # même périmètre sans GEO_INTERET
  expect_equal(j$effectif_champ.x, j$effectif_champ.y)
  expect_equal(j$departs_2030, j$departs_central, tolerance = 1e-9)          # departs_2030 (08b) = departs_central (08d)
  expect_equal(j$departs_bas.x, j$departs_bas.y, tolerance = 1e-9)
  expect_equal(j$departs_haut.x, j$departs_haut.y, tolerance = 1e-9)
  expect_equal(j$part_departs_pct, j$taux_depart_central_pct, tolerance = 1e-9) # part_departs_pct (08b) = taux_depart_central_pct (08d)
  # conventions qui diffèrent, volontairement : noms, région et causes (08d seul), stock tous âges (08b seul)
  expect_false(any(c("departs_central", "region_code", "dep_retraite") %in% names(b08)))
  expect_false(any(c("departs_2030", "part_a_remplacer_pct") %in% names(A$departement)))
  # 08b suit GEO_INTERET, 08d jamais : France x cs1 et département x cs1 inchangés sous restriction
  e2 <- lancer_cs1("departement", geo_interet = c("33", "64", "2A"))
  expect_lt(dplyr::n_distinct(e2$departs_geo_cs$geo_code), dplyr::n_distinct(A$departement$geo_code))
  expect_equal(e2$departs_cs1$analytique$departement, A$departement)
  expect_equal(e2$departs_cs1$analytique$france, A$france)
  # masques primaires identiques (même règle, mêmes indicateurs) ; le 08d ne peut qu'en ajouter en secondaire
  jm <- dplyr::inner_join(E$departs_geo_cs |> dplyr::select(geo_code, cs1, m08b = masque),
                          D$departement |> dplyr::select(geo_code, cs1, m08d = masque, motif_masque), by = c("geo_code", "cs1"))
  expect_false(any(jm$m08b & !jm$m08d))
  expect_true(all(jm$motif_masque[jm$m08d & !jm$m08b] %in% "secondaire"))
})

test_that("H. secret : MÊME implémentation que departs_pcs (règle Insee primaire, secondaire par blocs), conventions inchangées", {
  r <- E$regles_secret()
  for (n in names(D)) {
    d <- D[[n]]; a <- A[[n]]
    expect_identical(d, E$appliquer_secret_pcs(a, n, r, dimension = "cs1"), label = n)   # la fonction du 08c, dimension cs1
    prim <- E$secret_primaire(a$effectif_champ, a$n_entreprises, a$part_dominante_pct, r)
    expect_identical(d$motif_masque %in% "primaire", prim, label = n)
    expect_false(any(!d$masque & prim), label = n)
    expect_true(all(is.na(d$departs_central[d$masque])) && all(!is.na(d$departs_central[!d$masque])), label = n)
    expect_false(any(d$departs_central[!d$masque] == 0 & a$departs_central[!d$masque] > 0), label = n)
    expect_true(all(d$motif_masque[d$masque] %in% c("primaire", "secondaire")) && all(is.na(d$motif_masque[!d$masque])))
    expect_false(anyNA(a$effectif_champ)); expect_false(anyNA(a$departs_central))      # interne complète
    expect_false(any(c("n_entreprises", "part_dominante_pct") %in% names(d)))
    for (bloc in E$blocs_secret(n, d, "cs1")) {                       # aucun bloc avec UNE seule cellule masquée
      k <- d |> dplyr::group_by(dplyr::across(dplyr::all_of(bloc))) |> dplyr::summarise(m = sum(masque), t = dplyr::n(), .groups = "drop")
      expect_false(any(k$m == 1 & k$t > 2), label = paste(n, paste(bloc, collapse = "x")))
    }
  }
  # blocs de la dimension pcs STRICTEMENT inchangés
  expect_identical(E$blocs_secret("france", P$france, "pcs"), list(c("cs1")))
  expect_identical(E$blocs_secret("region", P$region, "pcs"), list(c("pcs"), c("region_code", "cs1")))
  expect_identical(E$blocs_secret("departement", P$departement, "pcs"), list(c("region_code", "pcs"), c("geo_code", "cs1")))
  expect_identical(E$blocs_secret("departement", A$departement, "cs1"), list(c("region_code", "cs1"), c("geo_code")))
  # secondaire sur une table artificielle région x cs1 : primaire (3 salariés) puis la plus
  # petite cellule de la région (bloc région) ; le bloc cs1 n'ajoute rien (une seule autre cellule)
  t <- tibble::tibble(region_code = rep(c("A", "B"), each = 4), region_nom = region_code,
                      cs1 = rep(c("Cadres", "Employes", "Ouvriers", "Prof. intermediaires"), 2),
                      effectif_champ = c(3, 30, 40, 50, 60, 70, 80, 90), departs_central = effectif_champ / 2,
                      departs_bas = departs_central * .9, departs_haut = departs_central * 1.1,
                      dep_retraite = departs_central * .8, dep_invalidite = departs_central * .1, dep_deces = departs_central * .1,
                      taux_depart_central_pct = 50, n_entreprises = 5L, part_dominante_pct = 50)
  s <- E$appliquer_secret_pcs(t, "region", r, dimension = "cs1")
  expect_identical(s$masque, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(s$motif_masque[1:2], c("primaire", "secondaire"))
  expect_true(all(is.na(s$effectif_champ[1:2]))); expect_equal(s$effectif_champ[3], 40L)
  # France x cs1 : le bloc est la table entière
  f <- t[1:4, ] |> dplyr::select(-region_code, -region_nom)
  expect_identical(E$appliquer_secret_pcs(f, "france", r, dimension = "cs1")$masque, c(TRUE, TRUE, FALSE, FALSE))
  # usage interne : aucune règle
  expect_false(any(E$appliquer_secret_pcs(t, "region", NULL, dimension = "cs1")$masque))
})

test_that("fichiers : interne/ toujours, diffusion/ selon DEPARTS_CS1_SECRET ; 08d indépendant du 08c ; zonage ZE = France seule", {
  d <- file.path(E$DIR_SORTIES, "departs_cs1")
  expect_setequal(list.files(d, recursive = TRUE),
                  c(paste0("interne/departs_cs1_", c("france", "region", "departement"), ".csv"),
                    paste0("diffusion/departs_cs1_", c("france", "region", "departement"), ".csv")))
  csv <- read.csv2(file.path(d, "diffusion", "departs_cs1_departement.csv"), stringsAsFactors = FALSE,
                   colClasses = c(geo_code = "character", region_code = "character"))
  expect_identical(names(csv), names(D$departement)); expect_true(all(is.na(csv$departs_central[csv$masque])))
  expect_true(all(c("01", "2A", "2B") %in% csv$geo_code))
  int <- read.csv2(file.path(d, "interne", "departs_cs1_france.csv"), stringsAsFactors = FALSE)
  expect_true(all(c("n_entreprises", "part_dominante_pct") %in% names(int))); expect_false(anyNA(int$departs_central))
  e0 <- lancer_cs1("departement", secret = FALSE)
  expect_false(dir.exists(file.path(e0$DIR_SORTIES, "departs_cs1", "diffusion"))); expect_null(e0$departs_cs1$diffusion)
  expect_equal(e0$departs_cs1$analytique$departement, A$departement)
  e1 <- lancer_cs1("departement", avec_08c = FALSE)                 # 08c désactivé : 08d calcule tout seul
  expect_false(exists("departs_pcs", envir = e1)); expect_equal(e1$departs_cs1$analytique, A)
  ez <- lancer_cs1("ze")
  expect_identical(ez$departs_cs1$niveaux, "france")
  expect_equal(sum(ez$departs_cs1$analytique$france$effectif_champ), nrow(ez$bts_projete))
})
