# ==============================================================================
# Pyramide des âges de la typologie (00h, 01c, 01d, 08f, cartes 00i / 00j) —
# specs/10 §12 (SPEC-TYPO-090 à 095), SPEC-PREP-014, SPEC-SEC-033.
# Données exclusivement SYNTHÉTIQUES. Chaque bloc cite les identifiants de spec.
# ==============================================================================
EP <- new.env()
.old_wd <- setwd(RACINE)
source(chemin_script("00_config.R"), local = EP)
setwd(.old_wd)
for (f in c("00g_format_restitution.R", "00c_fonctions_geo.R", "00d_fonctions_fiches.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R",
            "00i_fonctions_carte_typologie.R", "00j_fonctions_cartes_typologie_pcs.R")) sys.source(chemin_script(f), envir = EP)
CS_P <- c("Cadres", "Prof. intermediaires", "Employes", "Ouvriers")
# paramètres FIXES et explicites (jamais de tercile ici), pyramide comprise
PARAMS_P <- list(seuil_dominance_cs = 40, seuil_concentration = 50, effectif_min = 50, methode_classes = "fixes",
                 poids_faible = 1, poids_fort = 5, intensite_faible = 5, intensite_elevee = 10, volume_faible = 100, volume_eleve = 800,
                 marge_frontiere_pct = 5, nb_frontiere_expertise = 2, facteur_intensite_extreme = 2, indicateur_intensite = "part_a_remplacer_pct",
                 methode_pyramide = "fixes", releve_fragile = 1, releve_solide = 2, vague_faible = 15, vague_forte = 25,
                 age_jeunes = 30, age_min = 45, age_senior = 55)
cellule_p <- function(code, nom, eff, dep) tibble::tibble(
  region_code = "R1", region_nom = "R1", geo_code = code, geo_nom = nom, cs1 = CS_P,
  effectif_champ = as.integer(eff), departs_central = dep, departs_bas = dep * 0.95, departs_haut = dep * 1.05)
stock_p <- function(cs_dep, k = 2) cs_dep |> dplyr::transmute(geo_code, geo_nom, cs1, effectif_tous_ages = as.integer(round(effectif_champ * k)))
# pyramide « tous âges » synthétique : effectifs par tranche (jeunes, milieu, vague, seniors), entreprises et dominance réglables
pyr_p <- function(codes, effectifs, n_ent = 10L, dom = 20) {
  t <- dplyr::bind_rows(lapply(seq_along(codes), function(i) tibble::tibble(geo_code = codes[i], tranche_age = EP$CLES_PYRAMIDE, effectif = effectifs[[i]])))
  t$n_entreprises <- rep_len(n_ent, nrow(t)); t$part_dominante_pct <- rep_len(dom, nrow(t))
  EP$preparer_pyramide_departements(list(territoire = t, cs = NULL), NULL, EP$tranches_stock_ages(30, 45, 55))
}
# trois départements : A (relève solide, vague faible), B (relève fragile, vague forte), C (intermédiaire)
CS3 <- dplyr::bind_rows(cellule_p("A1", "Alpha", c(5000, 5000, 5000, 5000), c(1200, 1200, 1200, 1200)), # intensité 12 % (Élevée)
                        cellule_p("B2", "Beta", c(2500, 2500, 2500, 2500), c(25, 25, 25, 25)),          # intensité 0,5 % (Faible)
                        cellule_p("C3", "Gamma", c(2000, 2000, 2000, 2000), c(100, 100, 100, 100)))     # intensité 2,5 % (Faible)
PYR3 <- list(c(4000, 30000, 2000, 4000), c(3000, 2000, 10000, 5000), c(2400, 9000, 2600, 2000))

test_that("SPEC-TYPO-090 — tranches tirées de la configuration (AGE_BORNE_JEUNES, AGE_MIN_BTS, AGE_SENIOR), jamais en dur ; borne des jeunes contrôlée", {
  tr <- EP$tranches_stock_ages(30, 45, 55)
  expect_identical(tr$cle, c("jeunes", "milieu", "vague", "seniors"))
  expect_identical(tr$libelle, c("moins de 30 ans", "30-44 ans", "45-54 ans", "55 ans et plus"))
  expect_identical(tr$dans_champ, c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(EP$affecter_tranche_stock(c(18, 29, 30, 44, 45, 54, 55, 70, NA), tr),
                   c("jeunes", "jeunes", "milieu", "milieu", "vague", "vague", "seniors", "seniors", NA))       # âge manquant : aucune tranche
  tr47 <- EP$tranches_stock_ages(30, 47, 56)                                                                     # le champ et la borne senior déplacent les tranches
  expect_identical(tr47$libelle, c("moins de 30 ans", "30-46 ans", "47-55 ans", "56 ans et plus"))
  expect_identical(EP$affecter_tranche_stock(c(46, 47, 55, 56), tr47), c("milieu", "vague", "vague", "seniors"))
  defaut <- EP$tranches_stock_ages()                                                                             # valeurs de la configuration
  expect_identical(defaut$borne_inf[2:4], c(EP$AGE_BORNE_JEUNES, EP$AGE_MIN_BTS, EP$AGE_SENIOR))
  expect_error(EP$tranches_stock_ages(45, 45, 55), "non ordonnées")
  # configuration : borne des jeunes non entière, trop basse ou dans le champ = arrêt
  conf <- readLines(chemin_script("00_config.R"), encoding = "UTF-8")
  for (v in c("29.5", "12", "45")) {
    l <- conf; l[grep("^AGE_BORNE_JEUNES\\s*<-", l)] <- paste("AGE_BORNE_JEUNES <-", v)
    f <- tempfile(fileext = ".R"); writeLines(l, f, useBytes = TRUE)
    old <- setwd(RACINE); expect_error(source(f, local = new.env(), encoding = "UTF-8"), "AGE_BORNE_JEUNES", label = v); setwd(old)
  }
})

test_that("SPEC-TYPO-091 — pyramide complète : parts, relève, vague suivante ; aucun senior = ratios NA", {
  pyr <- pyr_p(c("A1", "B2", "C3"), list(c(20, 50, 20, 10), c(30, 30, 40, 0), c(10, 10, 10, 10)))
  ind <- EP$indicateurs_pyramide(pyr)
  a <- ind[ind$geo_code == "A1", ]
  expect_equal(c(a$pyramide_jeunes_pct, a$pyramide_milieu_pct, a$pyramide_vague_pct, a$pyramide_seniors_pct), c(20, 50, 20, 10))
  expect_equal(a$ratio_releve, 2); expect_equal(a$ratio_vague_seniors, 2); expect_identical(a$pyramide_source, "tous_ages")
  expect_identical(a$pyramide_libelles, "moins de 30 ans | 30-44 ans | 45-54 ans | 55 ans et plus")
  b <- ind[ind$geo_code == "B2", ]; expect_true(is.na(b$ratio_releve)); expect_true(is.na(b$ratio_vague_seniors))   # jamais Inf
  expect_true(all(is.finite(unlist(ind[, grep("_pct$", names(ind))]))))
  # une tranche absente du stock est une tranche vide (0), jamais une ligne perdue
  t <- tibble::tibble(geo_code = "Z9", tranche_age = c("jeunes", "vague", "seniors"), effectif = c(5, 10, 5), n_entreprises = 4L, part_dominante_pct = 30)
  pz <- EP$preparer_pyramide_departements(list(territoire = t), NULL, EP$tranches_stock_ages(30, 45, 55))
  expect_equal(nrow(pz$departement), 4L); expect_equal(pz$departement$effectif[pz$departement$tranche_age == "milieu"], 0)
  expect_equal(EP$indicateurs_pyramide(pz)$pyramide_milieu_pct, 0)
  expect_error(EP$preparer_pyramide_departements(list(territoire = dplyr::mutate(t, tranche_age = "autre")), NULL), "inconnue")
})

test_that("SPEC-TYPO-092 — repli sur le champ (bts_projete) : seul le rapport vague / seniors ; sans source, rien n'est créé et les textes sont inchangés", {
  base <- tibble::tibble(geo_code = rep(c("A1", "B2", "C3"), each = 40), siren = rep(sprintf("E%d", 1:8), 15),
                         age_2024 = rep(c(rep(50L, 30), rep(58L, 10)), 3), p_central = 0.1)
  pyr <- EP$preparer_pyramide_departements(NULL, base, EP$tranches_stock_ages(30, 45, 55))
  expect_identical(pyr$source, "champ"); expect_setequal(unique(pyr$departement$tranche_age), c("vague", "seniors")); expect_null(pyr$cs)
  t <- EP$construire_typologie_departements(CS3, stock_p(CS3), PARAMS_P, pyramide = pyr)
  d <- t$departements
  expect_equal(d$ratio_vague_seniors, c(3, 3, 3)); expect_true(all(d$pyramide_source == "champ"))
  for (v in c("pyramide_jeunes_pct", "pyramide_vague_pct", "ratio_releve", "classe_releve", "classe_vague_suivante")) expect_true(all(is.na(d[[v]])), label = v)
  expect_true(all(grepl("Au sein du champ, 3,0 salarié(s) de 45 à 54 ans pour un de 55 ans et plus (pyramide complète indisponible).", d$situation_texte, fixed = TRUE)))
  expect_identical(t$parametres$valeur[t$parametres$parametre == "pyramide_source"], "champ")
  # un salarié du champ sans tranche du champ (âge manquant) = arrêt explicite, jamais une perte
  expect_error(EP$preparer_pyramide_departements(NULL, dplyr::mutate(base, age_2024 = replace(age_2024, 1, NA))), "hors des tranches")
  # aucune source : NULL, aucune colonne de pyramide, textes identiques à la typologie sans pyramide
  expect_null(EP$preparer_pyramide_departements(NULL, NULL))
  t0 <- EP$construire_typologie_departements(CS3, stock_p(CS3), PARAMS_P)
  expect_false(any(grepl("pyramide|ratio_|classe_releve|classe_vague", names(t0$departements))))
  expect_false(any(grepl("Pyramide|Au sein du champ", t0$departements$situation_texte)))
  expect_false("pyramide_source" %in% t0$parametres$parametre)
})

test_that("SPEC-TYPO-093 — classes de relève et de vague suivante (borne incluse en haut), bornes écrites ; profils strictement inchangés", {
  t0 <- EP$construire_typologie_departements(CS3, stock_p(CS3), PARAMS_P)
  t <- EP$construire_typologie_departements(CS3, stock_p(CS3), PARAMS_P, pyramide = pyr_p(c("A1", "B2", "C3"), PYR3))
  d <- t$departements
  expect_equal(d$ratio_releve, c(1, 0.6, 1.2))                                   # A : 4000 / 4000 = 1 = borne basse -> intermédiaire
  expect_identical(d$classe_releve, c("Relève intermédiaire", "Relève fragile", "Relève intermédiaire"))
  expect_equal(d$pyramide_vague_pct, c(5, 50, 16.25))
  expect_identical(d$classe_vague_suivante, c("Faible", "Forte", "Modérée"))
  expect_identical(EP$classer_par_seuils(c(1.99, 2, 0.99), c(bas = 1, haut = 2), EP$CLASSES_PYRAMIDE$releve),
                   c("Relève intermédiaire", "Relève solide", "Relève fragile"))
  p <- setNames(t$parametres$valeur, t$parametres$parametre)
  expect_identical(unname(p[c("pyramide_source", "methode_pyramide", "releve_bas", "releve_haut", "vague_bas_pct", "vague_haut_pct", "age_borne_jeunes", "age_min_champ", "age_senior")]),
                   c("tous_ages", "fixes", "1", "2", "15", "25", "30", "45", "55"))
  # axe DESCRIPTIF : profils, motifs, classes des trois axes et frontières identiques avec ou sans pyramide
  for (v in c("profil_typologie", "motif_expertise", "classe_poids_base", "classe_intensite", "classe_volume_departs", "n_criteres_frontiere", "justification_profil"))
    expect_identical(d[[v]], t0$departements[[v]], label = v)
  expect_identical(t$parametres[seq_len(nrow(t0$parametres)), ], t0$parametres)
  # terciles : mêmes départements retenus, trois classes
  pt <- modifyList(PARAMS_P, list(methode_pyramide = "terciles"))
  tt <- EP$construire_typologie_departements(CS3, stock_p(CS3), pt, pyramide = pyr_p(c("A1", "B2", "C3"), PYR3))
  expect_setequal(tt$departements$classe_releve, EP$CLASSES_PYRAMIDE$releve)
})

test_that("SPEC-TYPO-094 — points d'attention « relève fragile » et « vague après 2030 », phrase de la pyramide par gabarit", {
  cs <- dplyr::bind_rows(CS3, cellule_p("D4", "Delta", c(3000, 3000, 3000, 3000), c(500, 500, 500, 500)))   # D : intensité ~ 8 % (Modérée)
  t <- EP$construire_typologie_departements(cs, stock_p(cs), PARAMS_P,
                                            pyramide = pyr_p(c("A1", "B2", "C3", "D4"), c(list(c(2000, 30000, 4000, 4000)), PYR3[2:3], list(c(1000, 9000, 9000, 5000)))))
  d <- t$departements
  a <- d[d$geo_code == "A1", ]; b <- d[d$geo_code == "B2", ]; dd <- d[d$geo_code == "D4", ]
  expect_identical(a$classe_intensite, "Élevée"); expect_identical(a$classe_releve, "Relève fragile")
  expect_true(grepl("relève fragile : 0,5 salarié(s) de moins de 30 ans pour un salarié de 55 ans et plus, alors que le renouvellement d’ici 2030 est rapide", a$points_attention, fixed = TRUE))
  expect_identical(b$classe_intensite, "Faible"); expect_identical(b$classe_vague_suivante, "Forte")
  expect_true(grepl("vague de départs importante après 2030 : 50,0 % des salariés ont de 45 à 54 ans, alors que le renouvellement d’ici 2030 est lent", b$points_attention, fixed = TRUE))
  expect_false(grepl("relève|vague", dplyr::coalesce(dd$points_attention, "")))                # fragile mais intensité modérée ; vague forte mais intensité non faible : aucun point
  expect_true(grepl("Pyramide des âges : 5,0 % de moins de 30 ans, 75,0 % de 30-44 ans, 10,0 % de 45-54 ans et 10,0 % de 55 ans et plus ; 0,5 salarié(s) de moins de 30 ans pour un de 55 ans et plus (relève fragile).",
                    a$situation_texte, fixed = TRUE))
})

test_that("SPEC-TYPO-095 / SPEC-SEC-033 — secret : une tranche masquée retire toute la pyramide ; effectifs par tranche jamais diffusés ; textes muets", {
  cs <- dplyr::bind_rows(cellule_p("A1", "Alpha", c(5000, 5000, 5000, 5000), c(1200, 1200, 1200, 1200)),
                         cellule_p("B2", "Beta", c(2500, 2500, 2500, 2500), c(25, 25, 25, 25)),
                         cellule_p("C3", "Gamma", c(2000, 2000, 2000, 2000), c(100, 100, 100, 100)),
                         cellule_p("D4", "Delta", c(3000, 3000, 3000, 3000), c(500, 500, 500, 500)))
  t_pyr <- dplyr::bind_rows(lapply(seq_len(4), function(i) tibble::tibble(geo_code = c("A1", "B2", "C3", "D4")[i], tranche_age = EP$CLES_PYRAMIDE,
                                                                       effectif = list(c(2000, 30000, 4000, 4000), c(3000, 2000, 10000, 5000), c(3, 9000, 2600, 2000), c(1000, 9000, 9000, 5000))[[i]],
                                                                       n_entreprises = 10L, part_dominante_pct = 20)))
  t_pyr$n_entreprises[t_pyr$geo_code == "D4" & t_pyr$tranche_age == "seniors"] <- 2L                 # D4 : moins de 3 entreprises chez les seniors
  pyr <- EP$preparer_pyramide_departements(list(territoire = t_pyr), NULL, EP$tranches_stock_ages(30, 45, 55))
  t <- EP$construire_typologie_departements(cs, stock_p(cs), PARAMS_P, pyramide = pyr)
  base <- dplyr::bind_rows(lapply(seq_len(nrow(cs)), function(i) tibble::tibble(geo_code = cs$geo_code[i], cs1 = cs$cs1[i],
                                                                                  siren = sprintf("E%d", seq_len(cs$effectif_champ[i]) %% 7 + 1), p_central = 0.05)))
  dif_cs <- cs |> dplyr::mutate(masque = FALSE)
  d <- EP$appliquer_secret_typologie(t, dif_cs, base, EP$regles_secret())
  c3 <- d[d$geo_code == "C3", ]; d4 <- d[d$geo_code == "D4", ]; a1 <- d[d$geo_code == "A1", ]
  for (r in list(c3, d4)) {                                                               # C3 : 3 jeunes (primaire) ; D4 : 2 entreprises (primaire)
    expect_true(r$pyramide_masquee)
    for (v in c("pyramide_jeunes_pct", "pyramide_milieu_pct", "pyramide_vague_pct", "pyramide_seniors_pct", "ratio_releve", "ratio_vague_seniors", "classe_releve", "classe_vague_suivante"))
      expect_true(is.na(r[[v]]), label = paste(r$geo_code, v))
    expect_false(grepl("Pyramide|relève|vague|moins de 30 ans", paste(r$situation_texte, r$points_attention)))
  }
  expect_false(a1$pyramide_masquee); expect_equal(a1$ratio_releve, 0.5); expect_true(grepl("relève fragile", a1$points_attention, fixed = TRUE))
  expect_false(any(grepl("^pyramide_n_", names(d))))                                     # effectifs par tranche : interne seulement
  expect_true(all(grepl("^pyramide_n_", grep("^pyramide_n_", names(t$departements), value = TRUE))) && "pyramide_n_jeunes" %in% names(t$departements))
  # département entièrement masqué : aucune valeur de pyramide
  base2 <- base |> dplyr::mutate(siren = ifelse(geo_code == "B2", "E1", siren))
  d2 <- EP$appliquer_secret_typologie(t, dif_cs, base2, EP$regles_secret())
  b2 <- d2[d2$geo_code == "B2", ]; expect_true(b2$masque)
  expect_true(all(is.na(unlist(b2[, c("pyramide_vague_pct", "ratio_releve", "classe_releve", "pyramide_libelles")]))))
})

test_that("SPEC-PREP-014 — 01c (mode test) : pyramide tous âges, tranches du champ = champ, total = stock tous âges ; désactivable ; incohérence = arrêt", {
  lancer_01c <- function(tranches = TRUE, alterer = NULL) {
    env <- new.env(); old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
    sys.source(chemin_script("00_config.R"), envir = env)
    assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
    assign("STOCK_TOUS_AGES_TRANCHES", tranches, envir = env)
    sourcer_scripts(c("00c_fonctions_geo.R", "00g_format_restitution.R", "00d_fonctions_fiches.R", "00h_fonctions_typologie.R",
                      "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R"), env)
    if (!is.null(alterer)) assign("bts", alterer(env$bts), envir = env)
    sys.source(chemin_script("01c_stock_tous_ages.R"), envir = env)
    env
  }
  E <- suppressMessages(lancer_01c())
  s <- E$stock_tranches_ages; tr <- E$tranches_stock_ages()
  expect_named(s, c("cs", "territoire")); expect_type(s$territoire$geo_code, "character")
  expect_setequal(unique(s$cs$tranche_age), tr$cle)
  champ <- s$cs |> dplyr::filter(tranche_age %in% c("vague", "seniors")) |> dplyr::count(geo_code, cs1, wt = effectif, name = "n") |>
    dplyr::inner_join(dplyr::count(E$bts, geo_code, cs1, name = "n_champ"), by = c("geo_code", "cs1"))
  expect_equal(champ$n, champ$n_champ)                                                      # tranches du champ = champ, cellule par cellule
  tot <- s$cs |> dplyr::count(geo_code, cs1, wt = effectif, name = "n") |> dplyr::inner_join(E$stock_tous_ages, by = c("geo_code", "cs1"))
  expect_equal(tot$n, as.numeric(tot$effectif_tous_ages))                                   # quatre tranches = stock tous âges (inchangé)
  expect_true(all(s$cs$effectif >= 0))
  expect_equal(sum(s$territoire$effectif), sum(E$stock_tous_ages$effectif_tous_ages))
  expect_true(all(c("n_entreprises", "part_dominante_pct") %in% names(s$territoire)))
  E2 <- suppressMessages(lancer_01c())                                                       # reproductible (graine fixe)
  expect_identical(E2$stock_tranches_ages, s)
  E0 <- suppressMessages(lancer_01c(FALSE)); expect_null(E0$stock_tranches_ages); expect_identical(E0$stock_tous_ages, E$stock_tous_ages)
  expect_error(suppressMessages(lancer_01c(alterer = function(b) { b$age_2024[1] <- 30L; b })), "diffèrent du champ")
})

test_that("SPEC-PREP-014 / SPEC-SEC-033 — 01d : la pyramide suit le recodage « inconnu », regroupement prudent pour le secret", {
  env <- new.env(); old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env); sys.source(chemin_script("00c_fonctions_geo.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("RECODER_GEO_HORS_PASSAGE", TRUE, envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), "geo01d_pyramide"), envir = env)
  assign("bts", tibble::tibble(id = 1:4, siren = "E1", sexe = "F", age_2024 = 50L, pcs = "3719", cs1 = "Cadres",
                               geo_code = c("33", "99", "98", "33"), geo_nom = "x", geo_type = "departement"), envir = env)
  assign("stock_tranches_ages", list(
    cs = tibble::tibble(geo_code = c("33", "99", "98"), geo_nom = "x", cs1 = "Cadres", tranche_age = "jeunes", effectif = c(10, 20, 5)),
    territoire = tibble::tibble(geo_code = c("33", "99", "98"), tranche_age = "jeunes", effectif = c(10, 20, 5), n_entreprises = c(9L, 4L, 2L), part_dominante_pct = c(30, 50, 90))), envir = env)
  suppressMessages(sys.source(chemin_script("01d_recoder_geo_hors_passage.R"), envir = env))
  t <- env$stock_tranches_ages$territoire; inc <- t[t$geo_code == "inconnu", ]
  expect_equal(inc$effectif, 25); expect_equal(inc$n_entreprises, 4L); expect_equal(inc$part_dominante_pct, 90)   # max des composantes : jamais moins de masquage
  expect_equal(sum(t$effectif), 35); expect_false(any(c("98", "99") %in% t$geo_code))
  expect_equal(sum(env$stock_tranches_ages$cs$effectif), 35); expect_true("inconnu" %in% env$stock_tranches_ages$cs$geo_code)
})

test_that("SPEC-TYPO-095 — cartes : ligne de pyramide et barres, jamais pour une pyramide masquée ni un département masqué", {
  r <- list(statut = "diffuse", part_emploi_base_national_pct = NA_real_, departs_central = NA_real_, intensite_renouvellement_pct = NA_real_,
            indicateur_intensite = "part_a_remplacer_pct", classe_poids_base = NA_character_, structure_emploi = NA_character_, cs_volume_departs_max = NA_character_,
            motif_expertise = NA_character_, points_attention = NA_character_, pyramide_jeunes_pct = 20, pyramide_milieu_pct = 50, pyramide_vague_pct = 20,
            pyramide_seniors_pct = 10, ratio_releve = 2, ratio_vague_seniors = 2, classe_releve = "Relève solide", pyramide_source = "tous_ages",
            pyramide_libelles = "moins de 30 ans | 30-44 ans | 45-54 ans | 55 ans et plus", pyramide_masquee = "FALSE")
  l <- EP$lignes_detail_typologie(r)
  expect_true(any(grepl("Pyramide des âges : 20,0 % moins de 30 ans · 50,0 % 30-44 ans · 20,0 % 45-54 ans · 10,0 % 55 ans et plus · Relève solide : 2,0 de moins de 30 ans pour un de 55 ans et plus", l, fixed = TRUE)))
  h <- paste(EP$html_barres_pyramide(r), collapse = "")
  expect_true(grepl("Pyramide des âges (emploi actuel tous âges)", h, fixed = TRUE)); expect_true(grepl("width:40.00%", h, fixed = TRUE))   # 20 / 50
  expect_false(grepl("<script|on[a-z]+=", h))
  rmq <- modifyList(r, list(pyramide_jeunes_pct = NA_real_, pyramide_milieu_pct = NA_real_, pyramide_vague_pct = NA_real_, pyramide_seniors_pct = NA_real_,
                            ratio_releve = NA_real_, ratio_vague_seniors = NA_real_, classe_releve = NA_character_, pyramide_masquee = "TRUE"))
  expect_true("Pyramide des âges non diffusée (secret statistique)" %in% EP$lignes_detail_typologie(rmq))
  expect_identical(EP$html_barres_pyramide(rmq), "")
  rc <- modifyList(rmq, list(pyramide_masquee = "FALSE", pyramide_source = "champ", ratio_vague_seniors = 1.5))
  expect_true(any(grepl("Au sein du champ : 1,5 salarié(s) de 45-54 ans pour un de 55 ans et plus (pyramide complète indisponible)", EP$lignes_detail_typologie(rc), fixed = TRUE)))
  r0 <- r[setdiff(names(r), grep("pyramide|ratio_|classe_releve", names(r), value = TRUE))]                               # table sans pyramide : aucune ligne
  expect_false(any(grepl("Pyramide|champ", EP$lignes_detail_typologie(r0))))
  expect_identical(EP$lignes_detail_typologie(modifyList(r, list(statut = "masque"))), EP$LIBELLES_UI$secret)
})
