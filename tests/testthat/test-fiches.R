# ==============================================================================
# Tests des fiches « chiffres clés » territoriales (R/00d_fonctions_fiches.R)
# ==============================================================================

test_that("mode « tous » : une fiche par territoire diffusable, index, journal", {
  dir <- file.path(tempdir(), "fiches_tous"); unlink(dir, recursive = TRUE)
  j <- suppressMessages(generer_fiches(BASE_FICHES, dir, mode = "tous", zonage = ZONAGE_DEP))
  ok <- j |> dplyr::filter(statut == "ok")
  expect_setequal(ok$code, c("01", "2A", "2B", "33", "09"))       # 48 sous seuil
  expect_true(all(file.exists(file.path(dir, ok$fichier))))
  expect_true(file.exists(file.path(dir, "index.html")))
  ecarte <- j |> dplyr::filter(statut != "ok")
  expect_identical(ecarte$code, "48")
  expect_match(ecarte$motif, "seuil de diffusion")
})

test_that("mode « selection » : seulement les territoires demandés, noms de fichiers stables", {
  dir <- file.path(tempdir(), "fiches_sel"); unlink(dir, recursive = TRUE)
  j <- suppressMessages(generer_fiches(BASE_FICHES, dir, mode = "selection",
                                       selection = c("33", "2A"), zonage = ZONAGE_DEP))
  expect_setequal(list.files(dir, pattern = "\\.html$"),
                  c("33_gironde.html", "2a_corse-du-sud.html", "index.html"))
  expect_error(suppressMessages(generer_fiches(BASE_FICHES, dir, mode = "selection",
                                               selection = NULL, zonage = ZONAGE_DEP)),
               "aucun code")
  expect_error(generer_fiches(BASE_FICHES, dir, mode = "autre", zonage = ZONAGE_DEP),
               "FICHES_MODE")
})

test_that("codes département : 01, 2A, 2B, 33 intacts dans l'en-tête et le nom de fichier", {
  expect_identical(slug_fiche(c("01", "2A", "2B", "33"), c("Ain", "Corse-du-Sud", "Haute-Corse", "Gironde")),
                   c("01_ain", "2a_corse-du-sud", "2b_haute-corse", "33_gironde"))
  ctx <- calculer_contexte_perimetre(BASE_FICHES)
  ind <- calculer_indicateurs_territoire(BASE_FICHES, "01", ctx)
  html <- paste(generer_html_fiche(ind, ctx, ZONAGE_DEP), collapse = "\n")
  expect_match(html, "Département · 01")
  expect_identical(ind$code, "01")
})

test_that("les fiches ignorent le schéma source : seul le contrat geo_* est utilisé", {
  # une table dont la colonne d'origine s'appelait DEP_ETAB, passée par la
  # normalisation géographique, produit exactement la même fiche
  src <- BASE_FICHES |> dplyr::rename(DEP_ETAB = geo_code) |> dplyr::select(-geo_type)
  norm <- src |> dplyr::rename(dplyr::all_of(c(geo_code = "DEP_ETAB"))) |>
    normaliser_geo("departement", "departement")
  ctx1 <- calculer_contexte_perimetre(BASE_FICHES); ctx2 <- calculer_contexte_perimetre(norm)
  i1 <- calculer_indicateurs_territoire(BASE_FICHES, "33", ctx1)
  i2 <- calculer_indicateurs_territoire(norm, "33", ctx2)
  expect_identical(i1$population, i2$population)
  expect_identical(i1$departs, i2$departs)
  # aucun nom de zonage ni de colonne source dans le code des fiches
  code <- c(readLines(file.path(RACINE, "R", "00d_fonctions_fiches.R")),
            readLines(file.path(RACINE, "R", "09_fiches_territoriales.R")))
  code <- sub("#.*$", "", code)                                   # hors commentaires
  expect_false(any(grepl("\\bze\\b|ze_code|ze_nom|COL_GEO|DEP_ETAB", code)))
})

test_that("secret statistique : cellule < seuil -> n.d., pas de barre, suppression secondaire", {
  expect_identical(masquer_cellules(c(5, 30, 40, 50), 20), c(TRUE, TRUE, FALSE, FALSE))  # secondaire
  expect_identical(masquer_cellules(c(5, 8, 40, 50), 20),  c(TRUE, TRUE, FALSE, FALSE))  # déjà 2 masquées
  expect_identical(masquer_cellules(c(30, 40, 50), 20),    c(FALSE, FALSE, FALSE))
  ctx <- calculer_contexte_perimetre(BASE_FICHES)
  ind <- calculer_indicateurs_territoire(BASE_FICHES, "2B", ctx)
  expect_true(ind$cs_masquee)
  expect_identical(sum(ind$cs$masque), 2L)                        # Cadres (5) + la plus petite (Employés)
  html <- paste(generer_html_fiche(ind, ctx, ZONAGE_DEP), collapse = "\n")
  # page 1 : aucune ligne du tableau pour une CS masquée, une note discrète, jamais « n.d. »
  expect_false(grepl('<th scope="row">Cadres', html))
  expect_match(html, "2 catégories ne sont pas affichées en application du secret statistique")
  expect_false(grepl("n\\.d\\.", html))
  # et la phrase sur la catégorie concentrant les départs n'est pas générée
  expect_false(any(grepl("concentrent", phrases_a_retenir(ind))))
})

test_that("cohérence interne : total = somme des CS ; 55+ = sous-population", {
  ctx <- calculer_contexte_perimetre(BASE_FICHES)
  ind <- calculer_indicateurs_territoire(BASE_FICHES, "33", ctx)
  expect_equal(sum(ind$cs$n), ind$population$n_champ)
  expect_equal(sum(ind$cs$n55), ind$population$n55)
  expect_equal(sum(ind$cs$departs), ind$departs$central)
  expect_equal(sum(ind$ages$n), ind$population$n_champ)
  expect_equal(sum(ind$causes$pct), 100, tolerance = 1e-9)
  expect_true(ind$departs$seniors_central <= ind$departs$central)
  d33 <- BASE_FICHES |> dplyr::filter(geo_code == "33")
  expect_equal(ind$departs$seniors_central, sum(d33$p_central[d33$age_2024 >= 55]))
})

test_that("résilience : territoire absent signalé, autres fiches produites ; sans senior -> n.d.", {
  dir <- file.path(tempdir(), "fiches_res"); unlink(dir, recursive = TRUE)
  expect_warning(
    j <- suppressMessages(generer_fiches(BASE_FICHES, dir, mode = "selection",
                                         selection = c("33", "99"), zonage = ZONAGE_DEP)),
    "absent")
  expect_identical(j$statut[j$code == "33"], "ok")
  expect_identical(j$motif[j$code == "99"], "absent des données")
  ctx <- calculer_contexte_perimetre(BASE_FICHES)
  ind <- calculer_indicateurs_territoire(BASE_FICHES, "09", ctx)
  expect_true(is.na(ind$departs$taux_seniors))
  html <- paste(generer_html_fiche(ind, ctx, ZONAGE_DEP), collapse = "\n")
  expect_match(html, "aucun salarié de 55 ans et \\+")
})

test_that("règles textuelles : position, « À retenir » en 2-3 phrases factuelles, sans répéter le chiffre clé", {
  expect_identical(position_mediane(35, 30)$classe, "dessus")
  expect_identical(position_mediane(25, 30)$classe, "dessous")
  expect_identical(position_mediane(31, 30, seuil_proche = 2)$classe, "proche")
  expect_identical(position_mediane(NA, 30)$libelle, "n.d.")
  ctx <- calculer_contexte_perimetre(BASE_FICHES)
  ind <- calculer_indicateurs_territoire(BASE_FICHES, "33", ctx)  # 40 % de seniors : au-dessus
  ph <- phrases_a_retenir(ind, ctx)
  expect_gte(length(ph), 2); expect_lte(length(ph), 3)
  expect_match(ph[1], sprintf("des salariés de %d ans et \\+ ont %d ans ou plus", AGE_MIN_BTS, AGE_SENIOR))
  expect_match(ph[2], "devraient avoir quitté l’emploi d’ici 2030")
  expect_false(any(grepl("pénurie|recrut|doit|il faut|tension|fragile", ph, ignore.case = TRUE)))
  expect_false(any(grepl(paste0("\\b", fmt_n(ind$departs$central), "\\b"), ph)))   # le HERO n'est pas répété
  html <- paste(generer_html_fiche(ind, ctx, ZONAGE_DEP), collapse = "\n")
  expect_match(html, "au-dessus de la médiane")
  expect_equal(sum(gregexpr("class=\"zone", html)[[1]] > 0), 5)                      # cinq zones
  expect_false(grepl("Points d’attention|Quel âge ont-ils", html))
})

test_that("tableau des départs par CS : effectifs actuels tous âges (01c), part à remplacer, ligne Ensemble ; repli sans stock", {
  ctx <- calculer_contexte_perimetre(BASE_FICHES)
  ind <- calculer_indicateurs_territoire(BASE_FICHES, "33", ctx, stock = STOCK_FICHES)
  expect_true(ind$stock_disponible)
  expect_equal(ind$population$n_tous_ages, 3L * ind$population$n_champ)
  expect_equal(ind$cs$n_tous, 3L * ind$cs$n)
  html <- paste(generer_html_fiche(ind, ctx, ZONAGE_DEP), collapse = "\n")
  expect_match(html, '<table class="tab"')
  expect_match(html, "Salariés aujourd’hui<small>tous âges</small>")
  expect_match(html, "Part de la catégorie<small>à remplacer d’ici 2030</small>")
  expect_match(html, sprintf('<tr class="total"><th scope="row">Ensemble</th><td>%s</td><td class="fort">%s</td><td>%s</td></tr>',
                             fmt_n(ind$population$n_tous_ages), fmt_n(ind$departs$central),
                             fmt_pct(100 * ind$departs$central / ind$population$n_tous_ages)), fixed = TRUE)
  o <- ind$cs |> dplyr::arrange(dplyr::desc(departs))               # lignes triées par départs décroissants
  expect_match(html, sprintf('<th scope="row">%s</th><td>%s</td><td class="fort">%s</td><td>%s</td>',
                             libelle_cs(o$cs1[1], TRUE), fmt_n(o$n_tous[1]), fmt_n(o$departs[1]),
                             fmt_pct(100 * o$departs[1] / o$n_tous[1])), fixed = TRUE)
  ph <- phrases_a_retenir(ind, ctx)
  expect_match(ph[3], "des salariés actuels \\(tous âges\\) devraient être partis, jusqu’à")
  expect_false(grepl("n\\.d\\.|NA", html))
  # repli sans stock : dénominateur = salariés du champ, libellés explicites
  ind0 <- calculer_indicateurs_territoire(BASE_FICHES, "33", ctx)
  expect_false(ind0$stock_disponible)
  h0 <- paste(generer_html_fiche(ind0, ctx, ZONAGE_DEP), collapse = "\n")
  expect_match(h0, sprintf("Salariés de %d ans et \\+<small>aujourd’hui</small>", AGE_MIN_BTS))
  expect_false(grepl("tous âges", h0))
  expect_match(phrases_a_retenir(ind0, ctx)[3], "représentent .* des départs attendus du territoire")
  # stock incomplet (une CS absente) : repli, jamais un taux faux
  ind1 <- calculer_indicateurs_territoire(BASE_FICHES, "33", ctx, stock = STOCK_FICHES |> dplyr::filter(cs1 != "Cadres"))
  expect_false(ind1$stock_disponible)
  # CS masquée : la phrase ne nomme pas de catégorie
  ind2 <- calculer_indicateurs_territoire(BASE_FICHES, "2B", ctx, stock = STOCK_FICHES)
  expect_match(phrases_a_retenir(ind2, ctx)[3], "devraient être partis\\.$")
})

test_that("annexe optionnelle : absente par défaut, seconde page sur demande", {
  ctx <- calculer_contexte_perimetre(BASE_FICHES)
  ind <- calculer_indicateurs_territoire(BASE_FICHES, "33", ctx)
  h0 <- paste(generer_html_fiche(ind, ctx, ZONAGE_DEP), collapse = "\n")
  h1 <- paste(generer_html_fiche(ind, ctx, ZONAGE_DEP, annexe = TRUE), collapse = "\n")
  expect_false(grepl("Annexe", h0))
  expect_match(h1, "Annexe — éléments détaillés")
  expect_match(h1, LABELS_TRANCHES[1], fixed = TRUE)                # structure par âge en annexe
  expect_match(h1, "Retraite ou fin de carrière : ")
})

test_that("index : recherche native, une ligne par fiche, méthode complète, champ paramétré", {
  dir <- file.path(tempdir(), "fiches_idx"); unlink(dir, recursive = TRUE)
  j <- suppressMessages(generer_fiches(BASE_FICHES, dir, mode = "tous", zonage = ZONAGE_DEP))
  idx <- paste(readLines(file.path(dir, "index.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(idx, '<input class="recherche" type="search"')
  expect_match(idx, "<script>")
  for (k in j$code[j$statut == "ok"]) expect_match(idx, sprintf('<span class="code">%s</span>', k))
  expect_match(idx, "Méthode\\.")
  expect_match(idx, sprintf("salariés de %d ans et plus en 2024", AGE_MIN_BTS))
  expect_match(idx, "1 territoire\\(s\\) sans fiche : 48")
})

test_that("cohérence avec le 08 sur la chaîne (mode test département)", {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(file.path("R", "00_config.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
  assign("DIR_SORTIES", tempdir(), envir = env)
  for (s in c("00c_fonctions_geo.R", "00d_fonctions_fiches.R", "01_fabriquer_donnees_test.R",
              "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
              "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R",
              "08_analyse_55plus_geo.R", "09_fiches_territoriales.R"))
    suppressMessages(suppressWarnings(invisible(capture.output(
      sys.source(file.path("R", s), envir = env)))))
  j  <- env$journal_fiches |> dplyr::filter(statut == "ok")
  sg <- env$synthese_geo
  expect_equal(nrow(j), nrow(sg))                                   # même nombre de territoires
  cmp <- dplyr::inner_join(j, sg, by = c("code" = "geo_code"))
  expect_equal(cmp$effectif,   cmp$effectif_champ)
  expect_equal(cmp$effectif55, cmp$effectif_55plus)
  # départs des 55+ de la fiche == tableau territorial du 08
  ctx <- env$calculer_contexte_perimetre(env$base_fiches)
  ind <- env$calculer_indicateurs_territoire(env$base_fiches, "33", ctx)
  expect_equal(ind$departs$seniors_central, sg$departs_55plus[sg$geo_code == "33"])
  expect_equal(ctx$med_part, env$med_part); expect_equal(ctx$med_taux, env$med_taux)
  # 01c sur la chaîne : stock >= champ cellule à cellule, et les fiches l'utilisent
  st <- env$stock_tous_ages
  expect_true(all(c("geo_code", "geo_nom", "cs1", "effectif_tous_ages") %in% names(st)))
  v <- env$bts_projete |> dplyr::count(geo_code, cs1) |> dplyr::inner_join(st, by = c("geo_code", "cs1"))
  expect_equal(nrow(v), nrow(st)); expect_true(all(v$effectif_tous_ages >= v$n))
  f33 <- paste(readLines(file.path(env$DIR_SORTIES, "fiches_departement", "33_gironde.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(f33, "tous âges")
  expect_match(f33, sprintf("<td>%s</td>", fmt_n(sum(st$effectif_tous_ages[st$geo_code == "33"]))), fixed = TRUE)
})

test_that("STOCK_TOUS_AGES = FALSE : chaîne inchangée, fiches en repli, CSV 08b sans colonnes tous âges", {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(file.path("R", "00_config.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), "sans_stock"), envir = env); dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  assign("STOCK_TOUS_AGES", FALSE, envir = env)
  for (s in c("00c_fonctions_geo.R", "00d_fonctions_fiches.R", "01_fabriquer_donnees_test.R",
              "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
              "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R",
              "08b_departs_geo_cs.R", "09_fiches_territoriales.R"))
    suppressMessages(suppressWarnings(invisible(capture.output(sys.source(file.path("R", s), envir = env)))))
  expect_null(env$stock_tous_ages)
  expect_false("effectif_tous_ages" %in% names(env$departs_geo_cs))
  f33 <- paste(readLines(file.path(env$DIR_SORTIES, "fiches_departement", "33_gironde.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("tous âges", f33))
  expect_match(f33, sprintf("Salariés de %d ans et \\+<small>aujourd’hui</small>", env$AGE_MIN_BTS))
})

test_that("usage interne (seuil 0) : toutes les fiches, aucun masquage, bloc entreprises par SIREN, bandeau ; diffusable inchangé", {
  dir <- file.path(tempdir(), "fiches_interne"); unlink(dir, recursive = TRUE)
  ref <- tibble::tibble(code = c("E01", "E02"), nom = c("Alpha Défense", "Bêta Systèmes"))
  j <- suppressMessages(generer_fiches(BASE_FICHES, dir, mode = "tous", zonage = ZONAGE_DEP, seuil = 0,
                                       detail_entreprises = TRUE, ref_siren = ref))
  expect_setequal(j$code[j$statut == "ok"], c("01", "2A", "2B", "33", "09", "48"))   # 48 (12 salariés) a sa fiche
  h2b <- paste(readLines(file.path(dir, "2b_haute-corse.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(h2b, "usage interne · secret statistique non appliqué")
  expect_match(h2b, '<th scope="row">Cadres</th>')                   # cellule de 5 : affichée
  expect_false(grepl("ne sont pas affichées|n’est pas affichée", h2b))
  expect_match(h2b, "<h2>Entreprises du territoire</h2>")
  d2b <- BASE_FICHES |> dplyr::filter(geo_code == "2B")
  for (s in unique(d2b$siren)) expect_match(h2b, sprintf('<td class="siren">%s</td>', s), fixed = TRUE)
  expect_match(h2b, "Alpha Défense")
  expect_match(h2b, sprintf("%d SIREN", dplyr::n_distinct(d2b$siren)))
  idx <- paste(readLines(file.path(dir, "index.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(idx, "USAGE INTERNE"); expect_match(idx, '<span class="code">48</span>')
  # diffusable (défaut) : ni bandeau, ni entreprises, ni SIREN
  ctx <- calculer_contexte_perimetre(BASE_FICHES)
  h <- paste(generer_html_fiche(calculer_indicateurs_territoire(BASE_FICHES, "2B", ctx), ctx, ZONAGE_DEP), collapse = "\n")
  expect_false(grepl("usage interne|Entreprises du territoire|class=\"siren\"", h))
  # entreprise = SIREN : le compteur de la fiche est le nombre de SIREN distincts
  ind <- calculer_indicateurs_territoire(BASE_FICHES, "33", ctx)
  expect_equal(ind$population$n_entreprises, dplyr::n_distinct(BASE_FICHES$siren[BASE_FICHES$geo_code == "33"]))
})

test_that("FICHES_SECRET = FALSE sur la chaîne : dossier _interne, tous les territoires, journal sans écarté", {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(file.path("R", "00_config.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), "interne_chaine"), envir = env); dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  assign("FICHES_SECRET", FALSE, envir = env)
  for (s in c("00c_fonctions_geo.R", "00d_fonctions_fiches.R", "01_fabriquer_donnees_test.R",
              "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
              "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R", "09_fiches_territoriales.R"))
    suppressMessages(suppressWarnings(invisible(capture.output(sys.source(file.path("R", s), envir = env)))))
  expect_true(dir.exists(file.path(env$DIR_SORTIES, "fiches_departement_interne")))
  expect_false(dir.exists(file.path(env$DIR_SORTIES, "fiches_departement")))
  expect_true(all(env$journal_fiches$statut == "ok"))
  f <- paste(readLines(file.path(env$DIR_SORTIES, "fiches_departement_interne", "33_gironde.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(f, "Entreprises du territoire"); expect_match(f, "ne pas diffuser")
})
