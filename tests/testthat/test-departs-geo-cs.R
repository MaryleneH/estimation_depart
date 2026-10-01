# ==============================================================================
# Export territoire x grande CS (R/08b) : mêmes univers, filtres, champ, départ,
# CS et secret que le reste de la chaîne — vérifié contre 04, 08 et 09.
# ==============================================================================

lancer_chaine_08b <- function(geo = "departement", geo_interet = NULL, min_salaries = NULL) {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", geo, envir = env); assign("GEO_SOURCE", geo, envir = env)
  assign("GEO_INTERET", geo_interet, envir = env)
  if (!is.null(min_salaries)) assign("SECRET_MIN_SALARIES", min_salaries, envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), paste0("s08b_", geo, "_", length(geo_interet), "_", min_salaries %||% "d")), envir = env)
  dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  for (s in c("00c_fonctions_geo.R", "00d_fonctions_fiches.R", "01_fabriquer_donnees_test.R",
              "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
              "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R",
              "08_analyse_55plus_geo.R", "08b_departs_geo_cs.R", "09_fiches_territoriales.R"))
    suppressMessages(suppressWarnings(invisible(capture.output(
      sys.source(chemin_script(s), envir = env)))))
  env
}
`%||%` <- function(a, b) if (is.null(a)) b else a
ENV <- lancer_chaine_08b("departement")
D   <- ENV$departs_geo_cs

test_that("fichier : créé dans DIR_SORTIES, suffixé par le zonage, colonnes attendues", {
  f <- file.path(ENV$DIR_SORTIES, "departs_par_departement_cs.csv")
  expect_true(file.exists(f))
  lu <- read.csv2(f, check.names = FALSE, stringsAsFactors = FALSE, colClasses = c(geo_code = "character"))
  expect_identical(names(lu), c("geo_code", "geo_nom", "cs1", "effectif_champ", "departs_2030",
                                "departs_bas", "departs_haut", "part_departs_pct",
                                "effectif_tous_ages", "part_a_remplacer_pct", "masque"))
  # effectifs tous âges (01c) : jamais inférieurs au champ ; part = départs / tous âges
  ok <- !lu$masque
  expect_true(all(lu$effectif_tous_ages[ok] >= lu$effectif_champ[ok]))
  expect_equal(lu$part_a_remplacer_pct[ok], round(100 * lu$departs_2030[ok] / lu$effectif_tous_ages[ok], 1), tolerance = 0.06)
  expect_true(all(is.na(lu$effectif_tous_ages[!ok])))
  expect_type(lu$geo_code, "character")
  expect_true(all(c("01", "09", "2A") %in% lu$geo_code))            # zéros initiaux et codes corses
  expect_equal(nrow(lu), nrow(D))
})

test_that("1. aucun doublon territoire x CS ; tri code puis ordre métier des CS", {
  expect_equal(anyDuplicated(D[, c("geo_code", "cs1")]), 0)
  expect_identical(D$geo_code, sort(D$geo_code))
  ordre <- unname(ENV$PCS_VERS_CS1)
  for (g in unique(D$geo_code)) {
    cs <- D$cs1[D$geo_code == g]
    expect_identical(cs, ordre[ordre %in% cs], label = g)
  }
})

test_that("2. totaux cohérents avec 04 (p_central), 08 (effectifs par cellule) et 09 (départs par territoire)", {
  base <- ENV$base_geo_cs
  brut <- ENV$calculer_departs_geo_cs(base)$brut        # non arrondi, non masqué
  expect_equal(sum(brut$departs_2030), sum(base$p_central), tolerance = 1e-9)   # 04
  expect_equal(sum(brut$effectif_champ), nrow(base))
  par_cs <- brut |> dplyr::group_by(cs1) |> dplyr::summarise(d = sum(departs_2030), n = sum(effectif_champ))
  ref_cs <- base |> dplyr::group_by(cs1) |> dplyr::summarise(d = sum(p_central), n = dplyr::n())
  cmp <- dplyr::inner_join(par_cs, ref_cs, by = "cs1")
  expect_equal(nrow(cmp), nrow(ref_cs))
  expect_equal(cmp$n.x, cmp$n.y); expect_equal(cmp$d.x, cmp$d.y, tolerance = 1e-9)
  # 08 : mêmes cellules, mêmes effectifs (même base, même périmètre)
  j <- dplyr::inner_join(brut, ENV$criticite_geo_cs, by = c("geo_code", "cs1"))
  expect_equal(nrow(j), nrow(brut)); expect_equal(j$effectif_champ.x, j$effectif_champ.y)
  # 09 : départs de la fiche = somme des cellules du territoire
  j9 <- ENV$journal_fiches |> dplyr::filter(statut == "ok")
  par_terr <- brut |> dplyr::group_by(geo_code) |> dplyr::summarise(d = sum(departs_2030))
  jj <- dplyr::inner_join(j9, par_terr, by = c("code" = "geo_code"))
  expect_equal(nrow(jj), nrow(j9)); expect_equal(jj$d, jj$departs, tolerance = 1e-9)
  # formule du taux : celle du 05 (part_departs_pct)
  expect_equal(brut$part_departs_pct, 100 * brut$departs_2030 / brut$effectif_champ)
  # le CSV diffusé = brut arrondi à 0,1 hors cellules masquées
  expect_equal(D$departs_2030[!D$masque], round(brut$departs_2030[!D$masque], 1))
})

test_that("3. tous les territoires du périmètre ; 4. aucune CS inattendue", {
  expect_setequal(unique(D$geo_code), unique(ENV$base_geo_cs$geo_code))
  expect_setequal(unique(D$geo_code), unique(ENV$synthese_geo$geo_code))
  expect_true(all(D$cs1 %in% unname(ENV$PCS_VERS_CS1)))
  base_bidon <- ENV$base_geo_cs; base_bidon$cs1[1] <- "Agriculteurs"
  expect_error(ENV$calculer_departs_geo_cs(base_bidon), "hors nomenclature")
})

test_that("5. secret statistique : règle Insee (salariés, entreprises, dominance) + secondaire, NA + masque, ligne conservée", {
  # seuil salariés élevé pour forcer des masquages sur la table test
  r <- ENV$calculer_departs_geo_cs(ENV$base_geo_cs, regles = ENV$regles_secret(min_salaries = 60))
  d <- r$diffusable; b <- r$brut
  expect_equal(nrow(d), nrow(b))                                     # aucune ligne supprimée
  expect_true(any(d$masque))
  expect_false(any(!is.na(d$effectif_champ) & d$effectif_champ < 60))
  expect_true(all(is.na(d$departs_2030[d$masque])) && all(!is.na(d$departs_2030[!d$masque])))
  # même décision que masquer_cellules (00d) territoire par territoire
  attendu <- unlist(lapply(split(b, b$geo_code), function(x)
    ENV$masquer_cellules(x$effectif_champ, x$n_entreprises, x$part_dominante_pct, ENV$regles_secret(min_salaries = 60))))
  expect_identical(unname(attendu), d$masque[order(d$geo_code)])
  # suppression secondaire : jamais UNE seule cellule masquée dans un territoire de 3+ CS
  par_t <- d |> dplyr::group_by(geo_code) |> dplyr::summarise(m = sum(masque), k = dplyr::n())
  expect_false(any(par_t$m == 1 & par_t$k - par_t$m > 1))
  # règle par défaut du projet : rien de visible ne viole la règle Insee ; indicateurs
  # de secret présents dans brut (interne), absents du CSV diffusé
  b0 <- ENV$calculer_departs_geo_cs(ENV$base_geo_cs)$brut
  expect_true(all(c("n_entreprises", "part_dominante_pct") %in% names(b0)))
  expect_false(any(c("n_entreprises", "part_dominante_pct") %in% names(D)))
  expect_false(any(!D$masque & ENV$secret_primaire(b0$effectif_champ, b0$n_entreprises, b0$part_dominante_pct)))
  expect_false(any(!is.na(D$effectif_champ) & D$effectif_champ < ENV$SECRET_MIN_SALARIES))
  # une cellule dominée par une entreprise est masquée même avec beaucoup de salariés
  base_dom <- ENV$base_geo_cs |> dplyr::mutate(siren = ifelse(geo_code == "33" & cs1 == "Ouvriers", "UNIQUE", siren))
  dd <- ENV$calculer_departs_geo_cs(base_dom)$diffusable
  expect_true(dd$masque[dd$geo_code == "33" & dd$cs1 == "Ouvriers"])
  expect_true(is.na(dd$effectif_champ[dd$geo_code == "33" & dd$cs1 == "Ouvriers"]))
})

test_that("périmètre : GEO_INTERET (liste de départements) respecté sans duplication du paramétrage", {
  e2 <- lancer_chaine_08b("departement", geo_interet = c("33", "64", "2A"))
  expect_setequal(unique(e2$departs_geo_cs$geo_code), c("33", "64", "2A"))
  expect_setequal(unique(e2$departs_geo_cs$geo_code), unique(e2$synthese_geo$geo_code))
  # mêmes chiffres qu'en périmètre complet pour ces départements (le filtre ne change pas les cellules)
  a <- D |> dplyr::filter(geo_code %in% c("33", "64", "2A")) |> dplyr::arrange(geo_code, cs1)
  b <- e2$departs_geo_cs |> dplyr::arrange(geo_code, cs1)
  expect_equal(a$departs_2030, b$departs_2030); expect_equal(a$effectif_champ, b$effectif_champ)
})

test_that("zonage : le même script produit departs_par_ze_cs.csv en mode ZE (sans rien changer d'autre)", {
  e3 <- lancer_chaine_08b("ze")
  expect_true(file.exists(file.path(e3$DIR_SORTIES, "departs_par_ze_cs.csv")))
  expect_equal(sum(e3$departs_geo_cs$effectif_champ, na.rm = TRUE) +
                 sum(e3$calculer_departs_geo_cs(e3$base_geo_cs, NULL)$brut$effectif_champ[e3$departs_geo_cs$masque]),
               nrow(e3$bts_projete))
})

# --- 8. Invariance : l'export est une opération SANS effet de bord ------------
test_that("8. sourcer 08b ne modifie, ne supprime ni ne regroupe aucun objet existant ; indicateurs identiques", {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), "s08b_invariance"), envir = env); dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  for (s in c("00c_fonctions_geo.R", "00d_fonctions_fiches.R", "01_fabriquer_donnees_test.R",
              "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
              "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R",
              "08_analyse_55plus_geo.R", "09_fiches_territoriales.R"))
    suppressMessages(suppressWarnings(invisible(capture.output(sys.source(chemin_script(s), envir = env)))))
  indicateurs <- function(e) {
    b <- e$bts_projete
    list(n_bts = nrow(e$bts), n_projete = nrow(b), n55 = sum(b$age_2024 >= e$AGE_SENIOR),
         departs = sum(b$p_central), departs55 = sum(b$p_central[b$age_2024 >= e$AGE_SENIOR]),
         departs55_08 = sum(e$synthese_geo$departs_55plus),
         par_cs = b |> dplyr::group_by(cs1) |> dplyr::summarise(d = sum(p_central), .groups = "drop"),
         par_terr = b |> dplyr::group_by(geo_code) |> dplyr::summarise(d = sum(p_central), .groups = "drop"),
         groupes = dplyr::group_vars(b), fiches = e$journal_fiches)
  }
  avant_ind <- indicateurs(env)
  avant <- mget(ls(env, all.names = TRUE), envir = env)          # TOUS les objets de la session
  expect_true("brut" %in% names(avant))                            # l'objet du 02c existe (EACR présent)

  suppressMessages(suppressWarnings(invisible(capture.output(
    sys.source(chemin_script("08b_departs_geo_cs.R"), envir = env)))))

  apres <- mget(ls(env, all.names = TRUE), envir = env)
  disparus <- setdiff(names(avant), names(apres))
  modifies <- names(avant)[!vapply(names(avant), function(n) identical(avant[[n]], apres[[n]]), logical(1))]
  expect_identical(disparus, character(0))                         # rien supprimé (ex. `brut` du 02c)
  expect_identical(modifies, character(0))                         # rien modifié (bts_projete, synthese_geo, ...)
  expect_setequal(setdiff(names(apres), names(avant)),             # seuls des objets NOUVEAUX
                  c("ZON_08B", "base_geo_cs", "calculer_departs_geo_cs", "departs_geo_cs", "fichier_08b"))
  expect_true("stock_tous_ages" %in% names(avant))                 # le 01c a tourné avant, intact
  expect_identical(indicateurs(env), avant_ind)                    # mêmes comptages, aucun grouping résiduel
  expect_identical(dplyr::group_vars(env$bts_projete), character(0))
  expect_true(file.exists(file.path(env$DIR_SORTIES, "departs_par_departement_cs.csv")))
})
