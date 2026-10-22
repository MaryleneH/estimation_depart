# ==============================================================================
# Exposition au recrutement difficile : les « deux horloges » (00k, 08i, 00i) —
# specs/10 §13 (SPEC-TYPO-100 à 107), SPEC-SEC-034.
# Données exclusivement SYNTHÉTIQUES (classeurs fictifs écrits dans tempdir()).
# ==============================================================================
ET2 <- new.env()
.old_wd <- setwd(RACINE)
source(chemin_script("00_config.R"), local = ET2)
setwd(.old_wd)
assign("DIR_DATA", file.path(RACINE, "data"), envir = ET2)
for (f in c("00g_format_restitution.R", "00c_fonctions_geo.R", "00d_fonctions_fiches.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R",
            "00i_fonctions_carte_typologie.R", "00k_fonctions_tension.R")) sys.source(chemin_script(f), envir = ET2)
# paramètres explicites : classes FIXES 25 / 50, couverture minimale 30 %, seuil de rang 4 sur 5
PAR_T <- modifyList(ET2$parametres_tension(), list(methode = "fixes", expo_faible = 25, expo_forte = 50, couverture_min = 30, effectif_min = 50, confiance_min = NULL))
COMPS <- vapply(ET2$TENSION_COMPOSANTES, function(k) unname(k[["colonne"]]), "")
# une ligne de classeur : rang synthétique, confiance, composantes (toutes à 1 sauf précisé)
ligne_t <- function(pcs, rang, conf = 3, comps = list()) {
  l <- list(PCS = pcs, `Lib PCS` = paste("Métier", pcs), FAP228 = paste0("F", pcs), LIB_FAP228 = "Famille fictive", poids_pcs = 1, indice_confiance = conf,
            emploi_fap_toussecteurs = 100, `emploi_pcs_défense` = 1, ind_tension_d = rang)
  for (k in names(COMPS)) l[[COMPS[[k]]]] <- if (!is.null(comps[[k]])) comps[[k]] else 1
  as.data.frame(l, check.names = FALSE)
}
classeur_t <- function(lignes, feuille = "Sheet1") {
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(setNames(list(data.frame(note = "fictif"), lignes), c("Documentation", feuille)), f); f
}
LISTE_T <- rbind(ligne_t("1A1A", 5, comps = list(main_oeuvre = 5)), ligne_t("1B1B", 2), ligne_t("2A2A", 4),
                 ligne_t("4A4A", 5, comps = list(main_oeuvre = 4, specif_form_emploi = 4)), ligne_t("4B4B", 1),
                 ligne_t("5A5A", 5, conf = 1), ligne_t("5B5B", 1), ligne_t("9Z9Z", 5), ligne_t("8U8U", NA))
# table EXACTE département x PCS (contrat 08c) : central, bas, haut ; effectif 100 par cellule
cel_t <- function(geo, pcs, c, b = c * 0.95, h = c * 1.05, n = 100) tibble::tibble(geo_code = geo, geo_nom = paste("Dép.", geo), pcs = pcs, cs1 = "Cadres",
                                                                                     effectif_champ = n, departs_central = c, departs_bas = b, departs_haut = h)
DEP_T <- dplyr::bind_rows(
  cel_t("D1", c("1a1a", "1b1b", "c001"), c(30, 70, 100)),                         # A = 30, B = 70, C = 100
  cel_t("D2", c("1a1a", "c001", "8u8u"), c(10, 90, 5)),                           # couverture 10 / 105 : non interprétée
  cel_t("D3", c("2a2a", "1b1b"), c(60, 40)),                                      # 60 % (Forte) ; seuil + 1 -> 0 %
  cel_t("D4", c("4a4a", "4b4b"), c(49, 51), b = c(45, 55), h = c(52, 48)),        # 49 % ; hypothèse haute -> 52 % (Forte)
  cel_t("D5", c("5a5a", "5b5b"), c(40, 60)))                                      # 40 % ; sans faible confiance -> 0 %
skip_if_not_installed("writexl")
MET_T <- ET2$tension_metiers(ET2$lire_pcs_fap_tension(classeur_t(LISTE_T), PAR_T), PAR_T)

test_that("SPEC-TYPO-100 — lecture contrôlée : feuille Sheet1, colonnes, doublons, rangs, colonne interdite ; rang manquant = tension inconnue", {
  expect_identical(MET_T$pcs, c("1a1a", "1b1b", "2a2a", "4a4a", "4b4b", "5a5a", "5b5b", "9z9z", "8u8u"))     # codes normalisés (casse)
  expect_identical(MET_T$en_tension, c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, NA)); expect_false(MET_T$tension_connue[9])
  expect_error(ET2$lire_pcs_fap_tension(classeur_t(LISTE_T, "Autre"), PAR_T), "feuille « Sheet1 » absente")
  expect_error(ET2$lire_pcs_fap_tension(classeur_t(LISTE_T[, setdiff(names(LISTE_T), "ind_mismatch_geo_d")]), PAR_T), "ind_mismatch_geo_d")
  expect_error(ET2$lire_pcs_fap_tension(classeur_t(rbind(LISTE_T, ligne_t("1a1a", 3))), PAR_T), "PCS en double")
  for (v in c(6, 4.5, 0, 5.3, 4.1)) expect_error(ET2$lire_pcs_fap_tension(classeur_t(rbind(LISTE_T[1:2, ], ligne_t("7C7C", v))), PAR_T), "hors de l'échelle admise", label = v)
  # sous-niveaux Dares 2023 : 5.1 / 5.2 admis sur le niveau maximal ; 5,0999 ramené à 5.1 (compté) ; niveau entier = 5 ; refusés si TENSION_SOUS_NIVEAUX = NULL
  expect_message(d5 <- ET2$lire_pcs_fap_tension(classeur_t(rbind(LISTE_T[1:2, ], ligne_t("7C7C", 5.0999), ligne_t("7D7D", 5.2))), PAR_T), "1 valeur\\(s\\) de rang ramenée\\(s\\) à une décimale")
  expect_equal(d5$rang_tension_fin[3:4], c(5.1, 5.2)); expect_identical(d5$rang_tension[3:4], c(5L, 5L))
  expect_error(ET2$lire_pcs_fap_tension(classeur_t(rbind(LISTE_T[1:2, ], ligne_t("7C7C", 5.1))), modifyList(PAR_T, list(sous_niveaux = NULL))), "hors de l'échelle admise")
  expect_message(d4 <- ET2$lire_pcs_fap_tension(classeur_t(rbind(LISTE_T[1:2, ], ligne_t("7C7C", 4.0000001))), PAR_T), "ramenée")   # imprécision sans sous-niveau : ramenée à 4, comptée
  expect_identical(d4$rang_tension[3], 4L); expect_equal(d4$rang_tension_fin[3], 4)
  expect_error(ET2$lire_pcs_fap_tension(classeur_t(cbind(LISTE_T, cs1 = "Cadres")), PAR_T), "interdite")
  expect_message(d <- ET2$lire_pcs_fap_tension(classeur_t(rbind(LISTE_T[1:2, ], ligne_t(NA, 3))), PAR_T), "sans code PCS")
  expect_equal(nrow(d), 2L)
  # déciles : réglage explicite
  expect_silent(ET2$lire_pcs_fap_tension(classeur_t(rbind(LISTE_T[1:2, ], ligne_t("7C7C", 9))), modifyList(PAR_T, list(rang_max = 10))))
})

test_that("SPEC-TYPO-100 — composante dominante, levier, ex aequo, faible confiance (règle par défaut et seuil explicite)", {
  m <- MET_T
  expect_identical(m$composante_dominante[m$pcs == "1a1a"], "main_oeuvre"); expect_false(m$composante_ex_aequo[m$pcs == "1a1a"])
  expect_identical(m$levier[m$pcs == "1a1a"], ET2$TENSION_COMPOSANTES$main_oeuvre[["levier"]])
  expect_identical(m$composante_dominante[m$pcs == "4a4a"], "main_oeuvre"); expect_true(m$composante_ex_aequo[m$pcs == "4a4a"])    # ordre de la configuration
  # ex aequo sur le niveau entier 5 : la valeur fine tranche (5.2 > 5.1), avant l'ordre de la configuration
  m5 <- ET2$tension_metiers(ET2$lire_pcs_fap_tension(classeur_t(ligne_t("6A6A", 5, comps = list(main_oeuvre = 5.1, condition_travail = 5.2))), PAR_T), PAR_T)
  expect_identical(m5$composante_dominante, "condition_travail"); expect_false(m5$composante_ex_aequo); expect_true(m5$en_tension)
  expect_true(is.na(m$composante_dominante[m$pcs == "2a2a"]))                                                                 # aucune composante en tension
  expect_identical(m$pcs[m$faible_confiance], "5a5a"); expect_equal(attr(m, "seuil_confiance"), 3)                           # plus bas niveau observé (1)
  m2 <- ET2$tension_metiers(ET2$lire_pcs_fap_tension(classeur_t(LISTE_T), PAR_T), modifyList(PAR_T, list(confiance_min = 4)))
  expect_true(all(m2$faible_confiance))
})

test_that("SPEC-TYPO-101 / 102 — exposition = A / (A + B), couverture = (A + B) / total, repère France ; couverture insuffisante non interprétée ; tension jamais départementale", {
  e <- ET2$exposition_departements(DEP_T, MET_T, PAR_T); d <- e$departements
  d1 <- d[d$geo_code == "D1", ]; expect_equal(d1$exposition_pct, 30); expect_equal(d1$couverture_pct, 50); expect_identical(d1$classe_exposition, "Modérée")
  d2 <- d[d$geo_code == "D2", ]; expect_equal(d2$couverture_pct, 100 * 10 / 105); expect_true(is.na(d2$classe_exposition)); expect_true(is.na(d2$robustesse))
  expect_equal(e$france$exposition_pct, 100 * (30 + 10 + 60 + 49 + 40) / (100 + 10 + 100 + 100 + 100))
  expect_true(all(d$exposition_france_pct == e$france$exposition_pct))
  expect_false(any(grepl("tension_departement|departement_en_tension|cs1", names(d))))
  t <- ET2$rediger_exposition_departement(d, PAR_T)
  expect_match(t[d$geo_code == "D1"], "Sur 100 départs attendus d’ici 2030 dans les métiers stratégiques, 30 concernent des métiers en tension en France", fixed = TRUE)
  expect_match(t[d$geo_code == "D1"], "Levier le plus fréquent : attirer et élargir les viviers de recrutement (manque de main-d’œuvre disponible).", fixed = TRUE)
  expect_match(t[d$geo_code == "D2"], "ne représentent que 9,5 % des départs", fixed = TRUE)
  expect_false(any(grepl("département est en tension|département en tension", t)))
})

test_that("SPEC-TYPO-103 — classes sur les mêmes bornes ; robustesse : hypothèse de départ, seuil de tension, rattachement PCS -> FAP", {
  d <- ET2$exposition_departements(DEP_T, MET_T, PAR_T)$departements
  r <- function(g) d[d$geo_code == g, ]
  expect_identical(r("D1")$robustesse, "robuste")
  expect_identical(r("D3")$classe_exposition, "Forte"); expect_identical(r("D3")$robustesse, "à confirmer"); expect_identical(r("D3")$causes_fragilite, "seuil de tension")
  expect_identical(r("D4")$classe_exposition, "Modérée"); expect_identical(r("D4")$causes_fragilite, "hypothèse de départ"); expect_equal(r("D4")$exposition_haut_pct, 52)
  expect_identical(r("D5")$causes_fragilite, "rattachement PCS → FAP")
  tt <- ET2$rediger_exposition_departement(d, PAR_T); expect_match(tt[d$geo_code == "D3"], "résultat à confirmer (sensible : seuil de tension)", fixed = TRUE)
  # terciles : bornes calculées sur les départements retenus (couverture et plancher)
  e2 <- ET2$exposition_departements(DEP_T, MET_T, modifyList(PAR_T, list(methode = "terciles")))
  expect_equal(e2$n_retenus, 4L); expect_false(anyNA(e2$bornes))
})

test_that("SPEC-TYPO-104 / 105 — matrice nationale (quadrants, cellule masquée, PCS absente), deux horloges, trois messages", {
  fr <- DEP_T |> dplyr::group_by(pcs, cs1) |> dplyr::summarise(effectif_champ = sum(effectif_champ), departs_central = sum(departs_central), .groups = "drop") |>
    dplyr::mutate(taux_depart_central_pct = 100 * departs_central / effectif_champ)
  fr_dif <- fr |> dplyr::mutate(masque = pcs == "5b5b")
  m <- ET2$matrice_metiers_tension(MET_T, fr, fr_dif, PAR_T)
  expect_equal(m$ref_x, 100 * sum(fr$departs_central) / sum(fr$effectif_champ))
  q <- setNames(m$metiers$quadrant, m$metiers$pcs); s <- setNames(m$metiers$statut, m$metiers$pcs)
  expect_identical(unname(q["2a2a"]), "Métiers critiques"); expect_identical(unname(q["1b1b"]), "Relève à organiser")
  expect_identical(unname(q["5a5a"]), "Recrutement déjà difficile"); expect_identical(unname(q["4a4a"]), "Recrutement déjà difficile")   # 49 % < repère 50,4 %
  expect_identical(unname(q["4b4b"]), "Relève à organiser")                                                                                  # 51 % >= repère, hors tension
  expect_identical(unname(s["5b5b"]), "non diffusé (secret statistique)"); expect_true(is.na(m$metiers$taux_depart_central_pct[m$metiers$pcs == "5b5b"]))
  expect_identical(unname(s["9z9z"]), "absente de la Base"); expect_identical(unname(s["8u8u"]), "tension inconnue")
  expect_identical(ET2$deux_horloges(c(ET2$PROFILS_TYPOLOGIE[["enjeu"]], ET2$PROFILS_TYPOLOGIE[["stable"]], ET2$PROFILS_TYPOLOGIE[["emergent"]]), c("Forte", "Forte", "Modérée")),
                   c(TRUE, FALSE, FALSE))
  dif <- tibble::tibble(geo_code = c("D1", "D3"), geo_nom = c("Un", "Trois"), tension_deux_horloges = c(FALSE, TRUE))
  typo <- tibble::tibble(geo_code = c("D1", "D3"), masque = FALSE, departs_central = c(300, 100))
  msg <- ET2$messages_fiche_tension(dif, typo, m)
  expect_length(msg, 3); expect_match(msg[1], "Les deux horloges sonnent ensemble dans 1 département (Trois (D3))", fixed = TRUE); expect_match(msg[1], "25 % des départs", fixed = TRUE)
  expect_match(msg[2], "métiers stratégiques sont critiques|métier stratégique est critique"); expect_match(msg[3], "^Aucune composante de la tension n’atteint le seuil")   # le seul métier critique (2A2A) n'a aucune composante en tension
  f <- tempfile(fileext = ".html"); ET2$generer_fiche_exposition_tension(msg, f, NULL, ET2$construire_libelle_champ(), PAR_T, c(bas = 25, haut = 50), 1L)
  h <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("<script|javascript:| on[a-z]+=", h)); expect_match(h, "Ce que cette page ne dit pas", fixed = TRUE); expect_match(h, "Salariés de 45 ans ou plus", fixed = TRUE)
  expect_false(grepl('c\\("', h))
})

test_that("SPEC-TYPO-106 / SPEC-SEC-034 — secret : tout ou rien, part sous le seuil, différenciation (une seule cellule masquée), département masqué, aucun effectif", {
  # base individuelle cohérente avec DEP_T (7 entreprises par cellule) + D6 (part A de 3 salariés) + D7 (deux cellules A)
  dep <- dplyr::bind_rows(DEP_T, cel_t("D6", c("1a1a", "1b1b"), c(1, 50), n = c(3, 100)), cel_t("D7", c("1a1a", "2a2a", "1b1b"), c(20, 20, 60)))
  base <- dplyr::bind_rows(lapply(seq_len(nrow(dep)), function(i) tibble::tibble(geo_code = dep$geo_code[i], pcs = dep$pcs[i],
                                                                               siren = sprintf("E%d", seq_len(dep$effectif_champ[i]) %% 7 + 1), p_central = dep$departs_central[i] / dep$effectif_champ[i])))
  e <- ET2$exposition_departements(dep, MET_T, PAR_T)
  dif_pcs <- dep |> dplyr::mutate(masque = (geo_code == "D1" & pcs == "1a1a") | (geo_code == "D7" & pcs %in% c("1a1a", "2a2a")))
  typo <- tibble::tibble(geo_code = unique(dep$geo_code), masque = unique(dep$geo_code) == "D5", profil_typologie = ET2$PROFILS_TYPOLOGIE[["enjeu"]])
  d <- ET2$appliquer_secret_exposition(e, MET_T, base, dif_pcs, typo, NULL, ET2$regles_secret(), PAR_T)
  ok <- setNames(d$tension_diffusee, d$geo_code)
  expect_false(ok[["D1"]])                         # exactement une cellule PCS masquée dans A : différenciation
  expect_true(ok[["D7"]])                          # deux cellules masquées dans A : rien n'est déductible
  expect_false(ok[["D6"]])                         # part A de 3 salariés : règle primaire
  expect_false(ok[["D5"]])                         # département masqué par la typologie
  expect_true(ok[["D3"]])
  for (g in c("D1", "D5", "D6")) expect_true(all(is.na(unlist(d[d$geo_code == g, setdiff(names(d), c("geo_code", "geo_nom", "tension_diffusee"))]))), label = g)
  expect_false(any(grepl("^departs_|^effectif_|n_entreprises|part_dominante", names(d))))
  expect_true(is.na(d$tension_metiers[d$geo_code == "D7"]))      # métiers cités : cellules diffusables seulement (les deux de A sont masquées)
  expect_identical(d$tension_metiers[d$geo_code == "D3"], "Métier 2A2A")
  expect_true(d$tension_deux_horloges[d$geo_code == "D3"])        # profil enjeu + exposition forte
})

test_that("SPEC-TYPO-107 — carte des profils : hachures pour l'exposition forte seulement, jamais un département masqué ; courriel sans script ; lecture du fichier voisin ; rien sans lui", {
  fond <- ET2$charger_fond_carte("departement")
  tout <- tibble::tibble(geo_code = names(fond), geo_nom = unname(vapply(fond, `[[`, "", "nom")), region_code = "R", region_nom = "R",
                         effectif_base_total = 1000, departs_central = 100, departs_bas = 95, departs_haut = 105, part_emploi_base_national_pct = 1,
                         intensite_renouvellement_pct = 10, indicateur_intensite = "part_a_remplacer_pct", base_poids = "effectif_tous_ages",
                         profil_typologie = ET2$PROFILS_TYPOLOGIE[["stable"]], situation_texte = "Texte.", masque = FALSE, motif_masque = NA_character_)
  tout$masque[2] <- TRUE
  dir <- file.path(tempdir(), "expo_carte"); dir.create(dir, showWarnings = FALSE)
  f_typo <- file.path(dir, "typologie_departements.csv"); utils::write.csv2(tout, f_typo, row.names = FALSE)
  p0 <- ET2$preparer_carte_typologie(ET2$lire_typologie_diffusion(f_typo), fond)
  f0 <- file.path(dir, "sans.html"); ET2$generer_carte_typologie(p0, fond, f0); h0 <- paste(readLines(f0, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl('<pattern id="hachures"', h0, fixed = TRUE)); expect_false(grepl('class="h"', h0, fixed = TRUE)); expect_false(grepl("legende-hachures\"><li>", h0, fixed = TRUE)); expect_equal(unname(p0$comptes[["hachures"]]), 0)
  expo <- tibble::tibble(geo_code = tout$geo_code[1:4], geo_nom = "x", tension_diffusee = c(TRUE, TRUE, TRUE, FALSE), tension_exposition_pct = c(60, 61, 10, NA),
                         tension_exposition_france_pct = 40, tension_couverture_pct = 70, tension_classe = c("Forte", "Forte", "Faible", NA), tension_robustesse = "robuste",
                         tension_causes = NA, tension_levier = "anticiper les recrutements", tension_composante = "forte intensité d’embauche", tension_metiers = "Métier X",
                         tension_deux_horloges = FALSE, tension_texte = c("Sur 100 départs… phrase A.", "FUITE-MASQUE", "Phrase C.", NA))
  utils::write.csv2(expo, file.path(dir, "exposition_tension_departements.csv"), row.names = FALSE)
  d <- ET2$lire_typologie_diffusion(f_typo); expect_true(all(c("tension_classe", "tension_exposition_pct") %in% names(d))); expect_type(d$tension_exposition_pct, "double")
  p <- ET2$preparer_carte_typologie(d, fond); expect_equal(unname(p$comptes[["hachures"]]), 1)          # le 2e (Forte) est masqué
  f <- file.path(dir, "avec.html"); ET2$generer_carte_typologie(p, fond, f); h <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_equal(lengths(regmatches(h, gregexpr('<path class="h" data-code="', h))), 1L)
  expect_true(grepl(sprintf('<path class="h" data-code="%s"', tout$geo_code[1]), h, fixed = TRUE)); expect_false(grepl(sprintf('class="h" data-code="%s"', tout$geo_code[2]), h, fixed = TRUE))
  expect_false(grepl("FUITE-MASQUE", h, fixed = TRUE)); expect_true(grepl("Départs portés par des métiers en tension (exposition forte)", h, fixed = TRUE))
  expect_true(grepl('querySelectorAll("path.t,path.h")', h, fixed = TRUE))
  l <- ET2$lignes_detail_typologie(p$territoires[p$territoires$code == tout$geo_code[1], ])
  expect_true(any(grepl("^Métiers en tension : 60 % des départs des métiers stratégiques \\(France : 40 %\\) · exposition forte · robuste$", l)))
  expect_true("Exposition au recrutement difficile non diffusée (secret statistique)" %in% ET2$lignes_detail_typologie(p$territoires[p$territoires$code == tout$geo_code[4], ]))
  j <- jsonlite::fromJSON(regmatches(h, regexpr('(?s)(?<=<script type="application/json" id="donnees">).*?(?=</script>)', h, perl = TRUE)))
  expect_match(j$territoires[[tout$geo_code[1]]]$texte, "Texte. Sur 100 départs… phrase A.", fixed = TRUE)
  fc <- file.path(dir, "courriel.html"); ET2$generer_carte_typologie_courriel(p, fond, fc); hc <- paste(readLines(fc, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("<script", hc, fixed = TRUE)); expect_true(grepl('fill="url(#hachures)"', hc, fixed = TRUE)); expect_false(grepl("FUITE-MASQUE", hc, fixed = TRUE))
})

test_that("SPEC-TYPO-107 — 08i sur la chaîne de test : sorties, objets amont inchangés, diffusion sans effectif ; classeur absent = message", {
  env <- new.env(); old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), "expo_chaine"), envir = env); dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(c("00c_fonctions_geo.R", "00g_format_restitution.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R",
                    "00k_fonctions_tension.R", "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R",
                    "02b_importer_mortalite_insee.R", "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R", "08_analyse_55plus_geo.R",
                    "08b_departs_geo_cs.R", "08c_departs_pcs.R", "08d_departs_cs1.R", "08f_typologie_departements.R"), env)
  assign("FICHIER_PCS_FAP_TENSION", file.path(tempdir(), "absent.xlsx"), envir = env)
  expect_message(sys.source(chemin_script("08i_exposition_tension.R"), envir = env), "classeur PCS -> FAP de tension absent")
  expect_false(exists("exposition_tension", envir = env, inherits = FALSE))
  pcs <- env$departs_pcs$analytique$france$pcs; set.seed(3); sel <- sample(pcs, 12)
  lignes <- do.call(rbind, lapply(seq_along(sel), function(i) ligne_t(toupper(sel[i]), c(1, 2, 3, 4, 5)[(i %% 5) + 1], conf = c(1, 3, 3)[(i %% 3) + 1],
                                                                    comps = list(main_oeuvre = (i %% 5) + 1, condition_travail = 5 - (i %% 5)))))
  assign("FICHIER_PCS_FAP_TENSION", classeur_t(lignes), envir = env)
  avant <- lapply(c("departs_pcs", "departs_cs1", "typologie_departements", "bts_projete"), function(o) get(o, envir = env))
  sourcer_scripts("08i_exposition_tension.R", env)
  apres <- lapply(c("departs_pcs", "departs_cs1", "typologie_departements", "bts_projete"), function(o) get(o, envir = env))
  expect_identical(avant, apres)
  dt <- file.path(env$DIR_SORTIES, "typologie_territoriale")
  for (f in c("interne/exposition_tension_departements.csv", "interne/metiers_strategiques_tension.csv", "interne/parametres_exposition_tension.csv",
              "diffusion/exposition_tension_departements.csv", "diffusion/metiers_strategiques_tension.csv", "diffusion/matrice_metiers_tension.png",
              "diffusion/fiche_exposition_tension.html")) expect_true(file.exists(file.path(dt, f)), label = f)
  dif <- utils::read.csv2(file.path(dt, "diffusion", "exposition_tension_departements.csv"), colClasses = c(geo_code = "character"))
  expect_false(any(grepl("^departs_|^effectif_|n_entreprises|part_dominante", names(dif))))
  expect_setequal(dif$geo_code, unique(env$departs_pcs$analytique$departement$geo_code))
  h <- paste(readLines(file.path(dt, "diffusion", "fiche_exposition_tension.html"), encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("<script", h, fixed = TRUE)); expect_true(grepl("data:image/png;base64,", h, fixed = TRUE))
  E <- env$exposition_tension; expect_equal(nrow(E$metiers), 12L); expect_true(all(E$metiers$present_base))
})
