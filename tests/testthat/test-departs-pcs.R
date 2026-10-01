# ==============================================================================
# Départs par PCS fine (00e + 08c) : pcs = dimension, cs1 = clé ; France entière
# indépendante de toute sélection territoriale ; région via le contrat géo ;
# cohérence arithmétique ; secret sur la couche de diffusion seulement.
# ==============================================================================
SCRIPTS_PCS <- c("00c_fonctions_geo.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R",
                 "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R", "01c_stock_tous_ages.R",
                 "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
                 "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R",
                 "08_analyse_55plus_geo.R", "08b_departs_geo_cs.R", "08c_departs_pcs.R")
lancer_pcs <- function(geo = "departement", geo_interet = NULL, secret = TRUE, niveaux = NULL) {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", geo, envir = env); assign("GEO_SOURCE", geo, envir = env)
  assign("GEO_INTERET", geo_interet, envir = env); assign("DEPARTS_PCS_SECRET", secret, envir = env)
  if (!is.null(niveaux)) assign("DEPARTS_PCS_NIVEAUX", niveaux, envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), paste0("pcs_", geo, "_", length(geo_interet), "_", secret)), envir = env)
  dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(SCRIPTS_PCS, env)
  env
}
E   <- lancer_pcs("departement")
A   <- E$departs_pcs$analytique; D <- E$departs_pcs$diffusion; M <- E$MESURES_DEPARTS_PCS
`%||%` <- function(a, b) if (is.null(a)) b else a
PASSAGE_REGION <- file.path(RACINE, E$GEO_PASSAGES[["departement->region"]])   # chemin absolu (cwd des tests)

test_that("pcs : conservée jusqu'aux résultats, plusieurs PCS d'une même cs1 restent distinctes", {
  expect_true("pcs" %in% names(E$bts_projete))
  expect_setequal(names(A), c("france", "region", "departement"))
  fr <- A$france
  expect_setequal(fr$pcs, unique(E$bts_projete$pcs))
  expect_equal(anyDuplicated(fr$pcs), 0)
  par_cs <- fr |> dplyr::count(cs1)
  expect_true(all(par_cs$n >= 2))                                    # plusieurs pcs par grande CS
  expect_true(all(substr(fr$pcs, 1, 1) == names(E$PCS_VERS_CS1)[match(fr$cs1, E$PCS_VERS_CS1)]))
  expect_identical(names(fr)[1:2], c("pcs", "cs1"))
})

test_that("CONTRÔLE FONDAMENTAL : pcs regroupées par cs1 = résultats par grande CS, exactement (7 mesures)", {
  base <- E$ajouter_parts_causes(E$bts_projete)
  ref <- base |> dplyr::group_by(cs1) |>
    dplyr::summarise(effectif_champ = dplyr::n(), departs_central = sum(p_central), departs_bas = sum(p_bas),
                     departs_haut = sum(p_haut), dep_retraite = sum(part_ret), dep_invalidite = sum(part_inv),
                     dep_deces = sum(part_dec), .groups = "drop") |> dplyr::arrange(cs1)
  agg <- A$france |> dplyr::group_by(cs1) |> dplyr::summarise(dplyr::across(dplyr::all_of(M), sum), .groups = "drop") |> dplyr::arrange(cs1)
  expect_identical(agg$cs1, ref$cs1)
  for (m in M) expect_equal(agg[[m]], ref[[m]], tolerance = 1e-12, label = m)
  # et avec les restitutions existantes : 08b (territoire x cs1, brut) sommé par cs1
  b08 <- E$calculer_departs_geo_cs(E$base_geo_cs)$brut |> dplyr::group_by(cs1) |>
    dplyr::summarise(n = sum(effectif_champ), d = sum(departs_2030), bas = sum(departs_bas), haut = sum(departs_haut), .groups = "drop") |> dplyr::arrange(cs1)
  expect_equal(agg$effectif_champ, b08$n); expect_equal(agg$departs_central, b08$d, tolerance = 1e-9)
  expect_equal(agg$departs_bas, b08$bas, tolerance = 1e-9); expect_equal(agg$departs_haut, b08$haut, tolerance = 1e-9)
  expect_true(E$controler_pcs_vs_cs(A$france, E$bts_projete))
  # un écart artificiel est refusé
  faux <- A$france; faux$departs_central[1] <- faux$departs_central[1] + 1
  expect_error(E$controler_pcs_vs_cs(faux, E$bts_projete), "contrôle ÉCHOUÉ")
})

test_that("cohérence arithmétique (avant arrondi, avant secret) : France, Σ régions, Σ départements, Σ départements d'une région", {
  b <- E$ajouter_parts_causes(E$bts_projete)
  expect_equal(sum(A$france$effectif_champ), nrow(b))
  expect_equal(sum(A$france$departs_central), sum(b$p_central), tolerance = 1e-9)
  expect_equal(sum(A$france$dep_retraite + A$france$dep_invalidite + A$france$dep_deces), sum(b$p_central), tolerance = 1e-9)
  somme_pcs <- function(t) t |> dplyr::group_by(pcs) |> dplyr::summarise(dplyr::across(dplyr::all_of(M), sum), .groups = "drop") |> dplyr::arrange(pcs)
  fr <- somme_pcs(A$france)
  for (n in c("region", "departement")) {
    x <- somme_pcs(A[[n]]); expect_identical(x$pcs, fr$pcs)
    for (m in M) expect_equal(x[[m]], fr[[m]], tolerance = 1e-9, label = paste(n, m))
  }
  d <- A$departement |> dplyr::group_by(region_code, pcs) |> dplyr::summarise(dplyr::across(dplyr::all_of(M), sum), .groups = "drop") |> dplyr::arrange(region_code, pcs)
  r <- A$region |> dplyr::arrange(region_code, pcs)
  expect_identical(paste(d$region_code, d$pcs), paste(r$region_code, r$pcs))
  for (m in M) expect_equal(d[[m]], r[[m]], tolerance = 1e-9, label = m)
  expect_true(E$controler_departs_pcs(A, E$bts_projete))
  expect_equal(A$france$taux_depart_central_pct, 100 * A$france$departs_central / A$france$effectif_champ)
})

test_that("France entière : indépendante de GEO_INTERET et de toute sélection de restitution", {
  e2 <- lancer_pcs("departement", geo_interet = c("33", "64", "2A"))
  expect_lt(nrow(e2$synthese_geo), nrow(E$synthese_geo))                      # le 08 est bien restreint...
  expect_equal(e2$departs_pcs$analytique$france, A$france)                     # ...pas la France x pcs
  expect_equal(e2$departs_pcs$analytique$region, A$region)
  expect_equal(e2$departs_pcs$analytique$departement, A$departement)
  expect_equal(nrow(e2$base_pcs), nrow(e2$bts_projete))
})

test_that("géographie : codes en texte (01, 2A, 2B, DROM), région correcte, département inconnu = arrêt", {
  dep <- A$departement
  expect_type(dep$geo_code, "character"); expect_type(dep$region_code, "character")
  expect_true(all(c("01", "2A", "2B") %in% dep$geo_code))
  expect_identical(unique(dep$region_code[dep$geo_code == "2A"]), "94")
  expect_identical(unique(dep$region_nom[dep$geo_code == "33"]), "Nouvelle-Aquitaine")
  expect_identical(unique(dep$region_code[dep$geo_code == "01"]), "84")           # Ain, pas Guadeloupe
  # table complète : les cinq DROM et 2A/2B, tout en texte
  p <- E$lire_passage_geo(PASSAGE_REGION, "departement->region", nom_requis = TRUE)
  expect_equal(nrow(p), 101); expect_equal(anyDuplicated(p$code_source), 0)
  for (k in c("01", "2A", "2B", "971", "972", "973", "974", "976")) expect_true(k %in% p$code_source, label = k)
  expect_type(p$code_source, "character"); expect_type(p$code_cible, "character")
  expect_identical(p$code_cible[p$code_source == "971"], "01"); expect_identical(p$nom_cible[p$code_source == "976"], "Mayotte")
  expect_identical(p$code_cible[p$code_source == "974"], "04")
  src <- tibble::tibble(geo_code = c("01", "2A", "2B", "971", "972", "973", "974", "976", "inconnu"), geo_type = "departement")
  out <- E$ajouter_region(src, PASSAGE_REGION)
  expect_identical(out$region_code, c("84", "94", "94", "01", "02", "03", "04", "06", "inconnu"))
  expect_type(out$region_code, "character")
  expect_error(E$ajouter_region(tibble::tibble(geo_code = c("33", "99"), geo_type = "departement"),
                                PASSAGE_REGION), "sans région dans la table de passage : 99")
  expect_error(E$ajouter_region(tibble::tibble(geo_code = "8401", geo_type = "ze"), PASSAGE_REGION),
               "departement")
})

test_that("secret : règle Insee (primaire) + secondaire minimale sur la diffusion ; tables internes complètes ; NA jamais 0", {
  r <- E$regles_secret()
  for (n in names(D)) {
    d <- D[[n]]; a <- A[[n]]
    prim <- E$secret_primaire(a$effectif_champ, a$n_entreprises, a$part_dominante_pct, r)
    expect_false(any(!d$masque & prim), label = n)                           # rien de visible ne viole la règle
    expect_identical(d$motif_masque %in% "primaire", prim, label = n)        # primaire = exactement la règle Insee
    expect_false(any(!is.na(d$effectif_champ) & d$effectif_champ < r$min_salaries), label = n)
    expect_true(all(is.na(d$departs_central[d$masque])) && all(!is.na(d$departs_central[!d$masque])), label = n)
    expect_false(any(d$departs_central[!d$masque] == 0 & a$departs_central[!d$masque] > 0), label = n)
    expect_true(all(d$motif_masque[d$masque] %in% c("primaire", "secondaire")))
    expect_false(anyNA(a$effectif_champ)); expect_false(anyNA(a$departs_central))   # interne complète
    expect_true(all(c("n_entreprises", "part_dominante_pct") %in% names(a)))          # indicateurs : interne oui...
    expect_false(any(c("n_entreprises", "part_dominante_pct") %in% names(d)))         # ...diffusion non
    expect_true(all(a$n_entreprises >= 1L & a$part_dominante_pct > 0 & a$part_dominante_pct <= 100))
    for (bloc in E$blocs_secret_pcs(n, d)) {                          # aucun bloc avec UNE seule cellule masquée
      k <- d |> dplyr::group_by(dplyr::across(dplyr::all_of(bloc))) |> dplyr::summarise(m = sum(masque), t = dplyr::n(), .groups = "drop")
      expect_false(any(k$m == 1 & k$t > 2), label = paste(n, paste(bloc, collapse = "x")))
    }
  }
  expect_gt(sum(D$departement$masque), 0)                              # département x pcs : très masqué, attendu
  expect_lte(sum(D$region$motif_masque %in% "secondaire"), 10)         # secondaire minimale, pas « jusqu'à tout masquer »
  csv <- read.csv2(file.path(E$DIR_SORTIES, "departs_pcs", "diffusion", "departs_pcs_departement.csv"),
                   stringsAsFactors = FALSE, colClasses = c(geo_code = "character", region_code = "character", pcs = "character"))
  expect_type(csv$geo_code, "character"); expect_true(all(is.na(csv$departs_central[csv$masque])))
})

test_that("fichiers : interne/ toujours, diffusion/ seulement avec DEPARTS_PCS_SECRET ; pcs_hors_champ agrégée avec total = exclusions du 01b", {
  d <- file.path(E$DIR_SORTIES, "departs_pcs")
  expect_setequal(list.files(d, recursive = TRUE),
                  c(paste0("interne/departs_pcs_", c("france", "region", "departement"), ".csv"),
                    paste0("diffusion/departs_pcs_", c("france", "region", "departement"), ".csv"), "pcs_hors_champ.csv"))
  hc <- read.csv2(file.path(d, "pcs_hors_champ.csv"), stringsAsFactors = FALSE, colClasses = c(pcs = "character"))
  expect_identical(names(hc), c("pcs", "premier_caractere", "effectif_exclu"))
  expect_equal(hc$effectif_exclu[which(hc$pcs %in% "TOTAL")], E$n_exclus_01b)
  expect_equal(sum(hc$effectif_exclu[!(hc$pcs %in% "TOTAL")]), E$n_exclus_01b)
  expect_false(any(hc$premier_caractere %in% names(E$PCS_VERS_CS1)))   # aucune PCS rattachable dedans
  expect_false(any(A$france$pcs %in% hc$pcs[!(hc$pcs %in% "TOTAL")]))        # et aucune dans les résultats
  e0 <- lancer_pcs("departement", secret = FALSE)
  expect_false(dir.exists(file.path(e0$DIR_SORTIES, "departs_pcs", "diffusion")))
  expect_true(file.exists(file.path(e0$DIR_SORTIES, "departs_pcs", "interne", "departs_pcs_region.csv")))
  expect_null(e0$departs_pcs$diffusion)
})

test_that("NON-RÉGRESSION : les 6 fichiers PCS (interne + diffusion) sont IDENTIQUES aux références figées avant la généralisation (dimension)", {
  # références : tests/testthat/reference/departs_pcs/, figées sur main 2876edc (table test, zonage département)
  for (couche in c("interne", "diffusion")) for (n in c("france", "region", "departement")) {
    ref <- file.path(RACINE, "tests", "testthat", "reference", "departs_pcs", sprintf("%s_departs_pcs_%s.csv", couche, n))
    skip_if_not(file.exists(ref), "référence absente")
    nouveau <- file.path(E$DIR_SORTIES, "departs_pcs", couche, sprintf("departs_pcs_%s.csv", n))
    expect_identical(readLines(nouveau, encoding = "UTF-8"), readLines(ref, encoding = "UTF-8"), label = paste(couche, n))
  }
  # l'enveloppe historique EST la fonction générique en dimension pcs ; idem blocs et secret
  for (n in names(A)) {
    expect_identical(E$calculer_departs_pcs(E$base_pcs, n), E$calculer_departs(E$base_pcs, n, dimension = "pcs"), label = n)
    expect_identical(E$blocs_secret_pcs(n, A[[n]]), E$blocs_secret(n, A[[n]], "pcs"), label = n)
    expect_identical(E$appliquer_secret_pcs(A[[n]], n), E$appliquer_secret_pcs(A[[n]], n, dimension = "pcs"), label = n)
    expect_identical(D[[n]], E$appliquer_secret_pcs(A[[n]], n), label = n)
  }
  expect_true(E$controler_departs_pcs(A, E$base_pcs)); expect_true(E$controler_departs(A, E$base_pcs, "pcs"))
})

test_that("zonage zone d'emploi : région et département ignorés avec message, France seule", {
  ez <- lancer_pcs("ze")
  expect_identical(ez$departs_pcs$niveaux, "france")
  expect_equal(sum(ez$departs_pcs$analytique$france$effectif_champ), nrow(ez$bts_projete))
  expect_false(file.exists(file.path(ez$DIR_SORTIES, "departs_pcs", "interne", "departs_pcs_region.csv")))
})

test_that("table de passage : doublon, code vide ou libellé manquant = arrêt explicite", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("code_source;code_cible;nom_cible", "33;75;Nouvelle-Aquitaine", "33;76;Occitanie"), f)
  expect_error(E$lire_passage_geo(f, "test"), "code_source en double .* 33")
  writeLines(c("code_source;code_cible;nom_cible", "33;;Nouvelle-Aquitaine"), f)
  expect_error(E$lire_passage_geo(f, "test"), "code_cible vide pour : 33")
  writeLines(c("code_source;code_cible;nom_cible", ";75;Nouvelle-Aquitaine"), f)
  expect_error(E$lire_passage_geo(f, "test"), "code_source vide")
  writeLines(c("code_source;code_cible;nom_cible", "33;75;"), f)
  expect_error(E$lire_passage_geo(f, "test", nom_requis = TRUE), "nom_cible manquant pour : 33")
  expect_silent(E$lire_passage_geo(f, "test"))                           # libellé facultatif par défaut
})
