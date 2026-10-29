# ==============================================================================
# Analyse « hypothèses de départ » (R/09_analyse) — specs/11, SPEC-ANA-001 à 008.
# Données exclusivement SYNTHÉTIQUES ; la chaîne est couverte dans
# test-exposition-tension.R (bloc « 08i sur la chaîne », 09b).
# ==============================================================================
EA <- new.env()
.old_wd <- setwd(RACINE); source(chemin_script("00_config.R"), local = EA); setwd(.old_wd)
assign("DIR_DATA", file.path(RACINE, "data"), envir = EA)
for (f in c("00g_format_restitution.R", "00c_fonctions_geo.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R", "00f_fonctions_cartes.R",
            "00k_fonctions_tension.R", "09a_fonctions_analyse_hypotheses.R")) sys.source(chemin_script(f), envir = EA)
PAR_A <- EA$parametres_analyse(); REG_A <- EA$regles_secret()
set.seed(11); N_A <- 400L
BTS_A <- tibble::tibble(id = seq_len(N_A), siren = sample(sprintf("S%02d", 1:12), N_A, TRUE), sexe = sample(c("F", "H"), N_A, TRUE),
                        age_2024 = sample(45:64, N_A, TRUE), cs1 = sample(c("Cadres", "Ouvriers", "Employes"), N_A, TRUE),
                        pcs = sample(c("1A1A", "2B2B", "3C3C", "4D4D"), N_A, TRUE)) |>
  dplyr::mutate(p_cal_central = plogis((age_2024 - 60) / 2), p_inval = runif(N_A, 0, 0.05), p_deces = runif(N_A, 0, 0.03),
                p_central = 1 - (1 - p_cal_central) * (1 - p_inval) * (1 - p_deces), p_bas = 0.9 * p_central, p_haut = pmin(1, 1.1 * p_central))

test_that("SPEC-ANA-002 — hypothèses : tout vient de la configuration (plage, δ, secret, tension) ; libellé public", {
  h <- EA$table_hypotheses_analyse(list(bas = 0.5, central = 1, haut = 1.5), PAR_A, EA$construire_libelle_champ())
  expect_identical(names(h), c("Hypothèse", "Valeur retenue", "Source", "Effet sur le résultat", "Ce qui la fait varier"))
  expect_true(nrow(h) >= 9); expect_true(any(grepl(sprintf("%d → %d \\(%d ans\\)", PAR_A$annee_bts, PAR_A$fin, PAR_A$horizon), h$`Valeur retenue`)))
  expect_true(any(grepl("central 1,00 an, bas 0,50, haut 1,50", h$`Valeur retenue`, fixed = TRUE)))
  expect_true(any(grepl(sprintf("au moins %d salariés et %d entreprises", EA$SECRET_MIN_SALARIES, EA$SECRET_MIN_ENTREPRISES), h$`Valeur retenue`)))
  expect_true(any(grepl("métiers de la première sous-liste", h$`Valeur retenue`, fixed = TRUE))); expect_false(any(grepl("stratégique", unlist(h))))
  h0 <- EA$table_hypotheses_analyse(NULL, PAR_A, EA$construire_libelle_champ()); expect_true(any(grepl("réforme des retraites 2023", h0$`Valeur retenue`)))
})

test_that("SPEC-ANA-003 — volume total : salariés, départs par scénario, causes additives, par an ; cellule nationale sous la règle de secret", {
  t <- EA$table_volume_total(BTS_A, REG_A, PAR_A)
  expect_identical(names(t), c("Indicateur", "Scénario central", "Hypothèse basse (départs retardés)", "Hypothèse haute (départs avancés)")); expect_equal(t[[2]][1], N_A)
  expect_equal(t[[2]][2], EA$arrondir_nombre_personnes(sum(BTS_A$p_central))); expect_true(t[[3]][2] <= t[[2]][2] && t[[2]][2] <= t[[4]][2])
  expect_equal(t[[2]][4], EA$arrondir_nombre_personnes(sum(BTS_A$p_central) / PAR_A$horizon)); expect_true(abs(sum(t[[2]][5:7]) - t[[2]][2]) <= 2)
  expect_true(all(is.na(t[[3]][5:7]))); expect_false(attr(t, "masque")); expect_equal(attr(t, "total_central"), sum(BTS_A$p_central))
  tm <- EA$table_volume_total(BTS_A, EA$regles_secret(min_entreprises = 100), PAR_A); expect_true(attr(tm, "masque")); expect_true(all(is.na(unlist(tm[, 2:4]))))
  # intensité du renouvellement : avec le stock tous âges (01c), emploi et intensité pour les trois scénarios ; sans stock, lignes vides
  expect_identical(t$Indicateur[8:9], c("Emploi actuel, tous âges", "Intensité du renouvellement (%)")); expect_true(all(is.na(unlist(t[8:9, 2:4]))))
  st <- data.frame(geo_code = c("33", "64"), cs1 = "Cadres", effectif_tous_ages = c(500L, 700L))
  ts <- EA$table_volume_total(BTS_A, REG_A, PAR_A, st); expect_equal(unname(unlist(ts[8, 2:4])), rep(1200, 3))
  expect_equal(ts[[2]][9], round(100 * sum(BTS_A$p_central) / 1200, 1)); expect_equal(ts[[4]][9], round(100 * sum(BTS_A$p_haut) / 1200, 1)); expect_equal(attr(ts, "emploi_tous_ages"), 1200)
})


test_that("SPEC-ANA-003 — volume par groupe : ordre des niveaux, niveau absent = cellule à zéro masquée, petit groupe masqué (NA, jamais zéro), part des départs du champ", {
  g <- ifelse(BTS_A$age_2024 < 50, "45-49 ans", ifelse(BTS_A$age_2024 < 60, "50-59 ans", "60 ans et plus")); g[1:3] <- "65 ans et plus"
  t <- EA$table_volume_groupe(BTS_A, g, "Tranche d’âge", c("45-49 ans", "50-59 ans", "60 ans et plus", "65 ans et plus", "70 ans et plus"), REG_A, sum(BTS_A$p_central))
  expect_identical(t[[1]], c("45-49 ans", "50-59 ans", "60 ans et plus", "65 ans et plus", "70 ans et plus"))
  expect_true(all(is.na(unlist(t[4:5, 2:10])))); expect_identical(t$`Diffusé`[4:5], rep("non diffusé (secret statistique)", 2))
  expect_identical(t$`Diffusé`[1:3], rep("oui", 3)); expect_equal(sum(t$`Salariés`[1:3]), N_A - 3L)
  expect_equal(sum(t$`Poids dans l’ensemble des départs (%)`[1:3]), 100 - 100 * sum(BTS_A$p_central[1:3]) / sum(BTS_A$p_central), tolerance = 0.005)   # le groupe masqué manque à la somme
  expect_true(all(t$`Hypothèse basse (départs retardés)`[1:3] <= t$`Départs attendus (central)`[1:3])); expect_error(EA$table_volume_groupe(BTS_A, g[-1], "x"), "aligné")
})

test_that("SPEC-ANA-003 — volume par catégorie sociale : table de diffusion seule, ordre des CS, masqué = NA", {
  d <- tibble::tibble(cs1 = c("Ouvriers", "Cadres", "Employes"), effectif_champ = c(200L, 150L, 4L), departs_central = c(80, 45, NA), departs_bas = c(70, 40, NA), departs_haut = c(90, 50, NA),
                      taux_depart_central_pct = c(40, 30, NA), dep_retraite = c(70, 40, NA), dep_invalidite = c(6, 3, NA), dep_deces = c(4, 2, NA), masque = c(FALSE, FALSE, TRUE))
  t <- EA$table_volume_cs1(d, 125)
  expect_identical(t$`Catégorie sociale`, c("Cadres", "Employés", "Ouvriers")); expect_true(all(is.na(unlist(t[2, 2:10])))); expect_identical(t$`Diffusé`[2], "non diffusé (secret statistique)")
  expect_equal(t$`Poids dans l’ensemble des départs (%)`[c(1, 3)], c(36, 64)); expect_error(EA$table_volume_cs1(d |> dplyr::select(-masque)), "masque")
})

DIF_PCS_A <- tibble::tibble(pcs = c("1A1A", "2B2B", "3C3C", "4D4D", "5E5E", "6F6F"), cs1 = c("Cadres", "Cadres", "Ouvriers", "Ouvriers", "Employes", "Employes"),
                            effectif_champ = c(100L, 300L, 10L, 60L, 4L, 80L), departs_central = c(50, 90, 8, 12, NA, 40), departs_bas = c(45, 80, 7, 10, NA, 36), departs_haut = c(55, 100, 9, 14, NA, 44),
                            taux_depart_central_pct = c(50, 30, 80, 20, NA, 50), masque = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
test_that("SPEC-ANA-004 — classements des métiers : volume, part (effectif minimum), masqué exclu, libellés depuis la nomenclature", {
  lib <- c("1a1a" = "Métier un", "2b2b" = "Métier deux")
  v <- EA$table_metiers_classement(DIF_PCS_A, "volume", lib, 3L, 50L, 200)
  expect_identical(v$`Code PCS`, c("2B2B", "1A1A", "6F6F")); expect_identical(v$Rang, 1:3); expect_identical(v$`Métier`, c("Métier deux", "Métier un", "PCS 6F6F"))
  expect_equal(v$`Poids dans l’ensemble des départs (%)`, c(45, 25, 20)); expect_identical(v$`Catégorie sociale`[3], "Employés")
  p <- EA$table_metiers_classement(DIF_PCS_A, "part", lib, 10L, 50L, 200)
  expect_identical(p$`Code PCS`, c("1A1A", "6F6F", "2B2B", "4D4D")); expect_false("3C3C" %in% p$`Code PCS`); expect_false("5E5E" %in% p$`Code PCS`)   # 10 salariés exclu ; masqué exclu
  expect_equal(attr(p, "effectif_min"), 50L); expect_error(EA$table_metiers_classement(DIF_PCS_A |> dplyr::select(-masque)), "masque")
})

MAT_A <- tibble::tibble(statut = c("diffusé", "diffusé", "diffusé", "non diffusé (secret statistique)", "diffusé"),
                        quadrant = c(EA$QUADRANTS_TENSION[["difficile"]], EA$QUADRANTS_TENSION[["critique"]], EA$QUADRANTS_TENSION[["critique"]], NA, EA$QUADRANTS_TENSION[["controle"]]),
                        pcs_source = c("1A1A", "2B2B", "3C3C", "4D4D", "5E5E"), pcs_libelle = c("Métier un", "Métier deux", NA, "Métier quatre", "Métier cinq"),
                        fap_libelle = c("Famille un", "Famille deux", "Famille trois", NA, "Famille cinq"), en_tension = c(TRUE, TRUE, TRUE, NA, FALSE),
                        levier = c("former tôt", "anticiper", "recruter", NA, "veiller"), composante_libelle = c("dim un", "dim deux", "dim trois", NA, "dim cinq"), faible_confiance = c(FALSE, TRUE, FALSE, FALSE, FALSE),
                        departs_central = c(20, 50, 30, NA, 10), taux_depart_central_pct = c(20, 35, 45, NA, 12))
TAB_08J_A <- tibble::tibble(ordre_liste = c(2, 2, 2, 3, 3, 1), pcs = c("2B2B", "2B2B", "2B2B", "3C3C", "3C3C", "1A1A"), pcs_libelle = c("Métier deux", "Métier deux", "Métier deux", NA, NA, "Métier un"),
                            niveau = c("région", "région", "département", "région", "région", "région"), geo_code = c("75", "84", "33", "75", "94", "75"), geo_nom = c("Région 75", "Région 84", "Gironde", "Région 75", "Région 94", "Région 75"),
                            diffuse = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE), effectif_champ = c(100L, 80L, 40L, 50L, NA, 60L), departs_central = c(38, 26, 16, 25, NA, 12),
                            taux_pct = c(38, 32.5, 40, 50, NA, 20), taux_france_pct = c(35, 35, 35, 45, 45, 20), ecart_pts = c(3, -2.5, 5, 5, NA, 0),
                            lecture = c("plus forte qu’en France", "proche de la France", "plus forte qu’en France", "plus forte qu’en France", "non diffusé (secret statistique)", "proche de la France"))
test_that("SPEC-ANA-004 / 006 — métiers de la liste (ordre des situations, levier en tension seulement, confiance), noyau dur, priorités avec régions", {
  l <- EA$table_metiers_liste_analyse(MAT_A)
  expect_identical(l$`Code PCS`, c("3C3C", "2B2B", "1A1A", "5E5E", "4D4D")); expect_identical(l$Situation[4:5], c(EA$QUADRANTS_TENSION[["controle"]], "non diffusé (secret statistique)"))
  expect_identical(l$`Métier`[1], "PCS 3C3C"); expect_true(is.na(l$`Levier d’action`[4])); expect_identical(l$`Confiance du rattachement PCS → FAP`[2], "faible confiance"); expect_true(all(is.na(unlist(l[5, 9:10]))))
  expect_identical(l$`Dimension principale de la tension`, c("dim trois", "dim deux", "dim un", NA, NA))   # seulement en tension
  nd <- EA$table_noyau_dur_analyse(l); expect_identical(nd$`Code PCS`, c("3C3C", "2B2B"))
  p <- EA$table_priorites_analyse(MAT_A, TAB_08J_A, 3L)
  expect_identical(p$`Code PCS`, c("3C3C", "2B2B")); expect_identical(p$`Levier d’action`, c("recruter", "anticiper")); expect_identical(p$`En tension en France`, c("oui", "oui")); expect_identical(p$`Dimension principale de la tension`, c("dim trois", "dim deux"))
  expect_identical(p[["Région 1 (part %)"]], c("Région 75 (50,0 %)", "Région 75 (38,0 %)")); expect_identical(p[["Région 2 (part %)"]], c(NA, "Région 84 (32,5 %)")); expect_true(all(is.na(p[["Région 3 (part %)"]])))
  p0 <- EA$table_priorites_analyse(MAT_A, NULL, 2L); expect_true(all(is.na(p0[["Région 1 (part %)"]]))); expect_equal(ncol(p0), 9L)
  lr <- EA$table_liste_regions_analyse(TAB_08J_A, 15L); expect_identical(lr$`Code PCS`, c("3C3C", "2B2B")); expect_equal(lr$`Écart à la France (points)`, c(5, 3)); expect_identical(lr$`Région`[1], "Région 75 (75)")
})

DIF_DEP_A <- tibble::tibble(geo_code = rep(c("33", "64", "2A"), each = 2), geo_nom = rep(c("Gironde", "Pyrénées-Atlantiques", "Corse-du-Sud"), each = 2), cs1 = rep(c("Cadres", "Ouvriers"), 3),
                            effectif_champ = c(100L, 200L, 50L, 50L, 20L, 4L), departs_central = c(30, 60, 25, 15, 8, NA), departs_bas = c(27, 54, 22, 13, 7, NA), departs_haut = c(33, 66, 28, 17, 9, NA),
                            masque = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
test_that("SPEC-ANA-005 — territoires : total seulement sans cellule masquée, rangs, enrichissement typologie et exposition, exposition par territoire, seniors", {
  t <- EA$table_territoires_cs1(DIF_DEP_A, "département")
  expect_identical(names(t)[1:2], c("Code", "Département")); expect_identical(t$Code, c("64", "33", "2A"))           # part 40 > 30 ; masqué en fin
  expect_equal(t$`Salariés`, c(100L, 300L, NA)); expect_equal(t$`Part susceptible de partir (%)`, c(40, 30, NA)); expect_equal(t$`Rang par nombre de départs`, c(2L, 1L, NA)); expect_equal(t$`Rang par part susceptible de partir`, c(1L, 2L, NA))
  expect_identical(t$`Diffusé`, c("oui", "oui", "non diffusé (secret statistique)"))
  typo <- tibble::tibble(geo_code = c("33", "64"), profil_typologie = c("Implantation stable", "Pôle majeur"))
  expo <- tibble::tibble(geo_code = c("33", "64", "2A"), geo_nom = c("Gironde", "Pyrénées-Atlantiques", "Corse-du-Sud"), tension_diffusee = c(TRUE, FALSE, FALSE), tension_classe = c("Forte", "Faible", NA),
                         tension_robustesse = c("robuste", "fragile", NA), tension_exposition_pct = c(61.5, 20, NA), tension_exposition_france_pct = 40, tension_departs_min = c(44, 5, NA), tension_levier = c("former", NA, NA))
  e <- EA$enrichir_territoires_analyse(t, typo, expo)
  expect_identical(e$`Profil (typologie)`, c("Pôle majeur", "Implantation stable", NA)); expect_identical(e$`Classe d’exposition aux remplacements difficiles`, c("non diffusé (secret statistique)", "Forte", "non diffusé (secret statistique)"))
  expect_equal(e$`Part des remplacements difficiles (%)`, c(NA, 61.5, NA)); expect_equal(e$`Départs attendus dans les métiers suivis en tension (minimum)`, c(NA, 44, NA))
  x <- EA$table_exposition_territoires_analyse(expo, "département"); expect_identical(x$Code, c("33", "2A", "64")); expect_identical(x$`Rang par part des remplacements difficiles`, c(1L, NA, NA)); expect_identical(x$`Levier d’action`, c("former", NA, NA))
  expect_error(EA$table_territoires_cs1(DIF_DEP_A |> dplyr::select(-masque), "département"), "masque")
  syn <- tibble::tibble(geo_code = c("33", "64", "2A"), geo_nom = c("Gironde", "Pyrénées-Atlantiques", "Corse-du-Sud"), masque = c(FALSE, FALSE, TRUE), masque_55plus = c(FALSE, TRUE, TRUE),
                        effectif_champ = c(300L, 100L, NA), effectif_55plus = c(120L, NA, NA), part_55plus_pct = c(40, NA, NA), departs_55plus = c(90, NA, NA), taux_depart_55plus_pct = c(75, NA, NA))
  s <- EA$table_synthese_seniors_analyse(syn, 55L)
  expect_identical(names(s)[4:5], c("Salariés de 55 ans et plus", "Part des 55 ans et plus (%)")); expect_equal(s$`Salariés`, c(300L, 100L, NA))
  expect_identical(s$`Diffusé`, c("oui", "partiellement (55 ans et plus non diffusés)", "non diffusé (secret statistique)"))
})

test_that("SPEC-ANA-007 / 008 — classeur : titre, note, tableau en ligne 4, feuille d'un tableau absent, Lisez-moi, Paramètres ; repli writexl ; contrôles bloquants", {
  f <- file.path(tempdir(), "analyse_test.xlsx"); unlink(f)
  feuilles <- list("2a Volume" = list(titre = "Volume", note = "note du volume", table = EA$table_volume_total(BTS_A, REG_A, PAR_A)),
                   "3a Métiers volume" = list(titre = "Métiers", note = "", table = NULL, raison = "Non produit : diffusion absente."),
                   "3c Métiers de la liste" = list(titre = "Liste", note = "n", table = EA$table_metiers_liste_analyse(MAT_A)))
  lisez <- data.frame(texte = c("Titre", "ligne"), stringsAsFactors = FALSE); attr(lisez, "gras") <- c(TRUE, FALSE)
  param <- data.frame(parametre = "horizon", valeur = "6", stringsAsFactors = FALSE)
  defs <- EA$table_definitions_analyse(PAR_A); expect_identical(names(defs), c("Terme", "Définition", "Feuilles")); expect_true(nrow(defs) >= 20); expect_false(any(grepl("stratégique", unlist(defs))))
  feuilles[["2a Volume"]]$lecture <- "Lecture : phrase de test."; faits <- data.frame(Rubrique = "2 Volume", Fait = "Un fait.", Feuille = "2a Volume", stringsAsFactors = FALSE)
  expect_identical(EA$ecrire_classeur_analyse(feuilles, f, lisez, param, definitions = defs, faits = faits), f)
  expect_identical(readxl::excel_sheets(f), c("Lisez-moi", "Définitions", "Faits", "2a Volume", "3a Métiers volume", "3c Métiers de la liste", "Paramètres"))
  t3 <- readxl::read_excel(f, sheet = "2a Volume", col_names = FALSE, n_max = 3); expect_identical(as.character(t3[[1]])[3], "Lecture : phrase de test.")
  xf <- readxl::read_excel(f, sheet = "Faits"); expect_identical(names(xf), c("Rubrique", "Fait", "Feuille")); expect_equal(nrow(xf), 1L)
  xd <- readxl::read_excel(f, sheet = "Définitions"); expect_equal(nrow(xd), nrow(defs)); expect_true(any(grepl("Dimension principale", xd$Terme)))
  x <- readxl::read_excel(f, sheet = "2a Volume", skip = 3); expect_identical(names(x), c("Indicateur", "Scénario central", "Hypothèse basse (départs retardés)", "Hypothèse haute (départs avancés)")); expect_equal(x[[2]][1], N_A)
  t1 <- readxl::read_excel(f, sheet = "2a Volume", col_names = FALSE, n_max = 2); expect_identical(as.character(t1[[1]]), c("Volume", "note du volume"))
  xa <- readxl::read_excel(f, sheet = "3a Métiers volume", skip = 3); expect_identical(names(xa), "Note"); expect_identical(xa$Note[1], "Non produit : diffusion absente.")
  f2 <- file.path(tempdir(), "analyse_test_writexl.xlsx"); expect_message(EA$ecrire_classeur_analyse(feuilles, f2, lisez, param, moteur = "writexl", definitions = defs), "sans mise en forme"); expect_true(file.exists(f2))
  expect_identical(readxl::excel_sheets(f2), c("Lisez-moi", "Définitions", "2a Volume", "3a Métiers volume", "3c Métiers de la liste", "Paramètres"))   # sans faits : pas de feuille Faits
  # contrôles : effectif visible sous le seuil ; chaîne non publique
  ok <- list(a = data.frame(`Salariés` = c(10L, NA, 0L), check.names = FALSE)); expect_true(EA$controler_tables_analyse(ok, 5L))
  expect_error(EA$controler_tables_analyse(list(a = data.frame(`Salariés` = c(10L, 3L), check.names = FALSE)), 5L), "sous le seuil")
  expect_error(EA$controler_tables_analyse(list(a = data.frame(x = "métiers stratégiques", stringsAsFactors = FALSE))), "stratégique")
})

test_that("SPEC-ANA-010 — lectures par feuille et feuille Faits : gabarits fixes remplis depuis les tableaux, secret respecté, libellé public", {
  tot <- EA$table_volume_total(BTS_A, REG_A, PAR_A, data.frame(effectif_tous_ages = 2000L))
  sen <- EA$table_volume_groupe(BTS_A, ifelse(BTS_A$age_2024 >= 55, "55 ans et plus", "moins de 55 ans"), "Groupe d’âge", c("moins de 55 ans", "55 ans et plus"), REG_A, sum(BTS_A$p_central))
  cs <- EA$table_volume_cs1(tibble::tibble(cs1 = c("Ouvriers", "Cadres"), effectif_champ = c(200L, 150L), departs_central = c(80, 45), departs_bas = c(70, 40), departs_haut = c(90, 50), taux_depart_central_pct = c(40, 30), masque = FALSE), 125)
  vol <- EA$table_metiers_classement(DIF_PCS_A, "volume", c("1a1a" = "Métier un", "2b2b" = "Métier deux"), 3L, 50L, 200); part <- EA$table_metiers_classement(DIF_PCS_A, "part", NULL, 3L, 50L, 200)
  liste <- EA$table_metiers_liste_analyse(MAT_A); noyau <- EA$table_noyau_dur_analyse(liste); prio <- EA$table_priorites_analyse(MAT_A, TAB_08J_A, 2L)
  dep <- EA$table_territoires_cs1(DIF_DEP_A, "département"); lr <- EA$table_liste_regions_analyse(TAB_08J_A, 15L)
  tabs <- list("1 Hypothèses" = EA$table_hypotheses_analyse(NULL, PAR_A), "2a Volume" = tot, "2c Seniors" = sen, "2d Par catégorie" = cs, "3a Métiers volume" = vol, "3b Métiers part" = part,
               "3c Métiers de la liste" = liste, "3d Noyau dur" = noyau, "4b Départements" = dep, "4d Liste x régions" = lr, "5 Priorités" = prio, "4a Régions" = NULL)
  l <- sapply(names(tabs), function(n) EA$lecture_feuille_analyse(n, tabs[[n]], PAR_A))
  expect_true(all(startsWith(l[names(tabs) != "4a Régions"], "Lecture :"))); expect_identical(unname(l["4a Régions"]), "")
  expect_true(grepl(sprintf("%s salariés devraient", formatC(EA$arrondir_nombre_personnes(sum(BTS_A$p_central)), format = "d", big.mark = " ")), l["2a Volume"]))
  expect_true(grepl("Métier deux \\(PCS 2B2B\\) porte le plus de départs attendus : 90", l["3a Métiers volume"])); expect_true(grepl("Pyrénées-Atlantiques \\(64\\) a la part susceptible de partir la plus forte parmi les départements diffusés : 40,0 %", l["4b Départements"]))
  expect_true(grepl("PCS 3C3C : 45,0 % susceptibles de partir, 30 départs attendus ; tension due à dim trois ; levier d’action : recruter", l["3d Noyau dur"])); expect_true(grepl("première région : Région 75 \\(50,0 %\\)", l["5 Priorités"]))
  f <- EA$table_faits_analyse(tabs, PAR_A)
  expect_identical(names(f), c("Rubrique", "Fait", "Feuille")); expect_true(nrow(f) >= 10); expect_false(any(grepl("stratégique", f$Fait)))
  expect_true(any(grepl("ces volumes sont des minimums", f$Fait))); expect_true(any(grepl("intensité de renouvellement de", f$Fait))); expect_true(any(grepl("^Les 55 ans et plus représentent", f$Fait)))
  expect_true(any(grepl("Les ouvriers ont la part susceptible de partir la plus élevée \\(40,0 %\\) et les cadres la plus faible \\(30,0 %\\)", f$Fait)))
  expect_true(any(grepl("2 métiers de la liste forment le noyau dur", f$Fait))); expect_true(any(grepl("Dimension principale de la tension du noyau dur : dim deux \\(1\\), dim trois \\(1\\)", f$Fait)))
  expect_true(any(grepl("2 départements sur 3 sont diffusés ; 1 ne l’est pas \\(secret statistique\\)", f$Fait))); expect_true(any(grepl("PCS 3C3C en Région 75 \\(75\\) : 50,0 % susceptibles de partir contre 45,0 % en France \\(\\+5,0 points\\)", f$Fait)))
  expect_false(any(grepl("33,3|Corse-du-Sud", f$Fait)))   # aucune valeur d'une cellule masquée
  expect_equal(nrow(EA$table_faits_analyse(list(), PAR_A)), 0L)
})
