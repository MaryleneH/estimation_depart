# ==============================================================================
# Exposition au recrutement difficile (00k, 08i, 00i) —
# specs/10 §13 (SPEC-TYPO-100 à 107), SPEC-SEC-034.
# Données exclusivement SYNTHÉTIQUES (classeurs fictifs écrits dans tempdir()).
# ==============================================================================
ET2 <- new.env()
.old_wd <- setwd(RACINE)
source(chemin_script("00_config.R"), local = ET2)
setwd(.old_wd)
assign("DIR_DATA", file.path(RACINE, "data"), envir = ET2); assign("TENSION_N_METIERS_SUIVIS", 9L, envir = ET2)   # N saisi : les 9 PCS de la liste fictive
for (f in c("00g_format_restitution.R", "00c_fonctions_geo.R", "00d_fonctions_fiches.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R",
            "00i_fonctions_carte_typologie.R", "00k_fonctions_tension.R", "00l_fonctions_metiers_liste.R")) sys.source(chemin_script(f), envir = ET2)
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
  expect_match(t[d$geo_code == "D1"], "Sur 100 départs attendus d’ici 2030 dans les métiers de la première sous-liste, 30 concernent des métiers en tension en France", fixed = TRUE)
  expect_match(t[d$geo_code == "D1"], "Levier le plus fréquent : attirer et élargir les viviers de recrutement (manque de main-d’œuvre disponible).", fixed = TRUE)
  expect_match(t[d$geo_code == "D2"], "ne représentent que 9,5 % des départs", fixed = TRUE)
  expect_match(t[d$geo_code == "D1"], "Ces métiers représentent 50 % des départs du département ; les autres départs (50 %) concernent des métiers dont la tension n’est pas observée.", fixed = TRUE)
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

test_that("SPEC-TYPO-104 / 105 — matrice nationale (quadrants, cellule masquée, PCS absente), cumul renouvellement rapide et exposition forte, trois messages", {
  fr <- DEP_T |> dplyr::group_by(pcs, cs1) |> dplyr::summarise(effectif_champ = sum(effectif_champ), departs_central = sum(departs_central), .groups = "drop") |>
    dplyr::mutate(taux_depart_central_pct = 100 * departs_central / effectif_champ)
  fr_dif <- fr |> dplyr::mutate(masque = pcs == "5b5b")
  m <- ET2$matrice_metiers_tension(MET_T, fr, fr_dif, PAR_T)
  expect_equal(m$ref_x, 100 * sum(fr$departs_central) / sum(fr$effectif_champ))
  q <- setNames(m$metiers$quadrant, m$metiers$pcs); s <- setNames(m$metiers$statut, m$metiers$pcs)
  expect_identical(unname(q["2a2a"]), "Départs rapides et métier en tension"); expect_identical(unname(q["1b1b"]), "Départs rapides, métier hors tension")
  expect_identical(unname(q["5a5a"]), "Départs modérés, métier en tension"); expect_identical(unname(q["4a4a"]), "Départs modérés, métier en tension")   # 49 % < repère 50,4 %
  expect_identical(unname(q["4b4b"]), "Départs rapides, métier hors tension")                                                                                  # 51 % >= repère, hors tension
  expect_identical(unname(s["5b5b"]), "non diffusé (secret statistique)"); expect_true(is.na(m$metiers$taux_depart_central_pct[m$metiers$pcs == "5b5b"]))
  expect_identical(unname(s["9z9z"]), "absente de la Base"); expect_identical(unname(s["8u8u"]), "tension inconnue")
  expect_identical(ET2$cumul_renouvellement_tension(c(ET2$PROFILS_TYPOLOGIE[["enjeu"]], ET2$PROFILS_TYPOLOGIE[["stable"]], ET2$PROFILS_TYPOLOGIE[["emergent"]]), c("Forte", "Forte", "Modérée")),
                   c(TRUE, FALSE, FALSE))
  dif <- tibble::tibble(geo_code = c("D1", "D3"), geo_nom = c("Un", "Trois"), tension_cumul = c(FALSE, TRUE))
  typo <- tibble::tibble(geo_code = c("D1", "D3"), masque = FALSE, departs_central = c(300, 100))
  msg <- ET2$messages_fiche_tension(dif, typo, m)
  expect_length(msg, 3); expect_match(msg[1], "1 département cumule un renouvellement rapide d’ici 2030 et une forte part de remplacements difficiles parmi les 9 métiers de la première sous-liste : Trois (D3).", fixed = TRUE); expect_match(msg[1], "La tension des autres métiers n’est pas observée.$"); expect_match(msg[1], "25 % des départs", fixed = TRUE)
  expect_false(any(grepl("horloge", msg, ignore.case = TRUE)))
  expect_match(msg[2], "^Parmi les 9 métiers de la première sous-liste, 1 cumule des départs plus rapides que la moyenne et une tension forte en France"); expect_false(any(grepl("^Aucun (département|métier)", msg))); expect_false(any(grepl("critique|Sous contrôle", msg))); expect_match(msg[3], "^Aucune composante de la tension n’atteint le seuil")   # le seul métier critique (2A2A) n'a aucune composante en tension
  f <- tempfile(fileext = ".html"); ET2$generer_fiche_exposition_tension(msg, f, NULL, ET2$construire_libelle_champ(), PAR_T, c(bas = 25, haut = 50), 1L, mat = m)
  h <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  # tableau des métiers par situation : quatre sections factuelles, lignes des seuls diffusables, comptes des autres, réglette de tension
  for (k in unname(ET2$QUADRANTS_TENSION)) expect_true(grepl(paste0("<th colspan=\"8\" style=\"background:#[0-9a-f]{6}\">", k), h), label = k)
  expect_false(grepl("critique|Sous contrôle|Relève à organiser", gsub("<[^>]+>", " ", h)))                        # texte visible : aucun libellé jugeant (les clés CSS ne comptent pas)
  expect_true(grepl("<b>Métier 2A2A</b><br><small>PCS 2A2A</small>", h, fixed = TRUE)); expect_false(grepl("Métier 5B5B", h, fixed = TRUE))   # 5b5b masqué en France : jamais dans le tableau
  expect_true(grepl("Hors tableau : 1 métier non diffusé(s) (secret statistique) ; 1 métier de tension inconnue (rang manquant) ; 1 métier sans salarié du champ dans la Base.", h, fixed = TRUE))
  expect_true(grepl('<span class="reg" aria-label="tension 4 sur 5"><i class="on"></i><i class="on"></i><i class="on"></i><i class="on"></i><i></i></span>', h, fixed = TRUE))
  expect_true(grepl('<span class="conf">à confirmer</span>', h, fixed = TRUE))                                            # 5a5a : faible confiance
  expect_true(grepl("<h2>Les métiers de la première sous-liste, situation par situation</h2>", h, fixed = TRUE)); expect_false(grepl("<img", h, fixed = TRUE))   # sans PNG : pas d'annexe
  png <- tempfile(fileext = ".png"); ET2$png_matrice_metiers_tension(m, png); expect_true(file.exists(png))   # le PNG reste un fichier à part
  # annexe repliée : matrice SVG sans script, un <title> par métier diffusable (libellé au survol), aucun libellé imprimé, aucun masqué
  expect_true(grepl('<details class="annexe"><summary>Version graphique (annexe)</summary>', h, fixed = TRUE)); expect_true(regexpr("<details class=\"annexe\"", h) > regexpr("<table class=\"tab-met\"", h))
  expect_false(grepl("data:image/png", h, fixed = TRUE))
  svg <- regmatches(h, regexpr("(?s)<svg class=\"mat\".*?</svg>", h, perl = TRUE)); expect_length(svg, 1)
  n_dif <- sum(m$metiers$statut == "diffusé"); expect_equal(lengths(regmatches(svg, gregexpr("<circle ", svg))), n_dif); expect_equal(lengths(regmatches(svg, gregexpr("<title>", svg))), n_dif)
  expect_true(grepl("<title>Métier 2A2A (PCS 2A2A)\nFamille professionnelle : Famille fictive (F2A2A)\nDéparts attendus d’ici 2030 : 60\nPart des salariés du métier susceptibles de partir : 60,0 % (France, tous métiers : 50,4 %)\nTension du métier en France : 4 sur 5 : en tension\nSituation : Départs rapides et métier en tension\nRattachement PCS → FAP : fiable</title>", svg, fixed = TRUE))
  expect_true(grepl("<title>Métier 5A5A (PCS 5A5A)", svg, fixed = TRUE)); expect_true(grepl('fill="#ffffff" stroke="#67a9cf"', svg, fixed = TRUE))   # faible confiance : point creux, couleur de la situation
  expect_false(grepl("5B5B|8U8U|9Z9Z", svg)); expect_false(grepl("<text[^>]*>Métier", svg))                                                             # masqué, inconnu, absent : jamais ; aucun libellé imprimé
  expect_true(grepl('>5 (la plus forte)</text>', svg, fixed = TRUE)); expect_true(grepl("stroke-dasharray", svg, fixed = TRUE))
  expect_true(grepl('<ul class="leg-mat">', h, fixed = TRUE)); expect_true(grepl("point creux : rattachement PCS → FAP à confirmer", h, fixed = TRUE))
  m0 <- m; m0$metiers$statut <- "non diffusé (secret statistique)"; expect_identical(ET2$svg_matrice_metiers_tension(m0, PAR_T), "")
  ET2$generer_fiche_exposition_tension(msg, f, png, ET2$construire_libelle_champ(), PAR_T, c(bas = 25, haut = 50), 1L, mat = m0); h0 <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("<details", h0, fixed = TRUE)); expect_false(grepl("<svg", h0, fixed = TRUE))                                                        # rien de diffusable : pas d'annexe
  expect_false(grepl("<script|javascript:| on[a-z]+=", h)); expect_match(h, "Ce que cette page ne dit pas", fixed = TRUE); expect_match(h, "Salariés de 45 ans ou plus", fixed = TRUE)
  expect_false(grepl("horloge", h, ignore.case = TRUE)); expect_match(h, "<h1>Départs attendus et difficultés de recrutement</h1>", fixed = TRUE)
  # information partielle (N = TENSION_N_METIERS_SUIVIS) : univers nommé dans le sous-titre, l'encadré, les situations vides ; jamais « Aucun » sans univers
  expect_true(grepl("· 9 métiers de la première sous-liste, dont 8 présents dans la Base</p>", h, fixed = TRUE))
  expect_true(grepl("La tension n’est connue que pour les 9 métiers de la première sous-liste : un zéro ou une situation vide veut dire « aucun parmi les métiers suivis », pas « aucun »", h, fixed = TRUE))
  expect_true(grepl("Aucun métier dans cette situation parmi les métiers de la première sous-liste et diffusables.", h, fixed = TRUE)); expect_false(grepl("Aucun métier de la première sous-liste diffusable", h, fixed = TRUE))
  msg0 <- ET2$messages_fiche_tension(dif |> dplyr::mutate(tension_cumul = FALSE), typo, m)
  expect_identical(msg0[1], "Parmi les 9 métiers de la première sous-liste, aucun département ne cumule un renouvellement rapide d’ici 2030 et une forte part de remplacements difficiles. La tension des autres métiers n’est pas observée.")
  m0 <- m; m0$metiers$quadrant[m0$metiers$quadrant %in% ET2$QUADRANTS_TENSION[["critique"]]] <- ET2$QUADRANTS_TENSION[["difficile"]]
  expect_match(ET2$messages_fiche_tension(dif, typo, m0)[2], "^Parmi les 9 métiers de la première sous-liste, aucun ne cumule"); expect_false(any(grepl("^Aucun (département|métier)", ET2$messages_fiche_tension(dif, typo, m0))))
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
  expect_true(d$tension_cumul[d$geo_code == "D3"])        # profil enjeu + exposition forte
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
                         tension_cumul = FALSE, tension_texte = c("Sur 100 départs… phrase A.", "FUITE-MASQUE", "Phrase C.", NA))
  utils::write.csv2(expo, file.path(dir, "exposition_tension_departements.csv"), row.names = FALSE)
  d <- ET2$lire_typologie_diffusion(f_typo); expect_true(all(c("tension_classe", "tension_exposition_pct") %in% names(d))); expect_type(d$tension_exposition_pct, "double")
  p <- ET2$preparer_carte_typologie(d, fond); expect_equal(unname(p$comptes[["hachures"]]), 1)          # le 2e (Forte) est masqué
  f <- file.path(dir, "avec.html"); ET2$generer_carte_typologie(p, fond, f); h <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_equal(lengths(regmatches(h, gregexpr('<path class="h" data-code="', h))), 1L)
  expect_true(grepl(sprintf('<path class="h" data-code="%s"', tout$geo_code[1]), h, fixed = TRUE)); expect_false(grepl(sprintf('class="h" data-code="%s"', tout$geo_code[2]), h, fixed = TRUE))
  expect_false(grepl("FUITE-MASQUE", h, fixed = TRUE)); expect_true(grepl("Forte part de remplacements difficiles parmi les métiers de la première sous-liste", h, fixed = TRUE)); expect_false(grepl("exposition", gsub("<[^>]+>", " ", h)))
  expect_true(grepl('querySelectorAll("path.t,path.h")', h, fixed = TRUE))
  l <- ET2$lignes_detail_typologie(p$territoires[p$territoires$code == tout$geo_code[1], ])
  expect_true(any(grepl("^Remplacements difficiles : 60 % des départs des métiers de la première sous-liste \\(France : 40 %\\) · part forte · robuste$", l)))
  expect_true("Part des remplacements difficiles non diffusée (secret statistique)" %in% ET2$lignes_detail_typologie(p$territoires[p$territoires$code == tout$geo_code[4], ]))
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
                    "00k_fonctions_tension.R", "00l_fonctions_metiers_liste.R", "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R",
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
  # SPEC-TYPO-109 : une ancienne fiche des territoires laissée par une exécution antérieure est retirée, avec un message
  dt <- file.path(env$DIR_SORTIES, "typologie_territoriale"); dir.create(file.path(dt, "diffusion"), showWarnings = FALSE, recursive = TRUE)
  obsolete <- file.path(dt, "diffusion", "fiche_territoires_tension.html"); writeLines("<html>ancienne fiche</html>", obsolete)
  expect_message(invisible(capture.output(sys.source(chemin_script("08i_exposition_tension.R"), envir = env))), "fichier obsolète retiré")
  expect_false(file.exists(obsolete))
  apres <- lapply(c("departs_pcs", "departs_cs1", "typologie_departements", "bts_projete"), function(o) get(o, envir = env))
  expect_identical(avant, apres)
  for (f in c("interne/exposition_tension_departements.csv", "interne/metiers_strategiques_tension.csv", "interne/parametres_exposition_tension.csv",
              "diffusion/exposition_tension_departements.csv", "diffusion/metiers_strategiques_tension.csv", "diffusion/matrice_metiers_tension.png",
              "diffusion/fiche_exposition_tension.html", "interne/exposition_tension_regions.csv", "diffusion/exposition_tension_regions.csv")) expect_true(file.exists(file.path(dt, f)), label = f)
  # 08j (SPEC-TYPO-109) : métiers de la liste territoire par territoire, sur les tables de diffusion du 08c ; libellé public sans « stratégique »
  sourcer_scripts("08j_metiers_liste_territoires.R", env); expect_true(exists("metiers_liste_territoires", envir = env, inherits = FALSE))
  for (f in c("diffusion/metiers_liste_territoires.csv", "diffusion/fiche_metiers_liste.html", "diffusion/tableau_metiers_liste.html", "diffusion/metiers_liste_territoires.xlsx")) expect_true(file.exists(file.path(dt, f)), label = f)
  tl <- utils::read.csv2(file.path(dt, "diffusion", "metiers_liste_territoires.csv"), colClasses = c(pcs = "character", geo_code = "character"), fileEncoding = "UTF-8-BOM")
  expect_false(any(c("region_code", "region_nom") %in% names(tl)))
  # tableau en un écran (SPEC-TYPO-110) : une case par métier x région, données embarquées = cellules diffusées de la table, aucune valeur masquée, libellé public, autonome
  ht <- paste(readLines(file.path(dt, "diffusion", "tableau_metiers_liste.html"), encoding = "UTF-8"), collapse = "\n")
  nR <- dplyr::n_distinct(tl$geo_code[tl$niveau == "région"]); expect_equal(lengths(regmatches(ht, gregexpr('<td class="c ', ht))), dplyr::n_distinct(tl$pcs) * nR)
  expect_false(grepl("stratégique", ht)); expect_false(grepl(" on[a-z]+=", ht)); expect_false(grepl('href="[^#]', ht))
  Dt <- jsonlite::fromJSON(sub('(?s).*<script type="application/json" id="donnees">(.*?)</script>.*', "\\1", ht, perl = TRUE), simplifyVector = FALSE)
  for (i in seq_len(nrow(tl))) { k <- paste0(if (tl$niveau[i] == "région") "r" else "d", tl$geo_code[i]); cc <- Dt$cel[[tl$pcs[i]]][[k]]
    if (tl$diffuse[i]) expect_equal(cc$t, tl$taux_pct[i], label = k) else expect_null(cc, label = k) }
  expect_equal(length(Dt$metiers), dplyr::n_distinct(tl$pcs)); expect_equal(length(Dt$cols), nR)
  nT <- dplyr::n_distinct(paste(tl$niveau, tl$geo_code)); expect_equal(nrow(tl), dplyr::n_distinct(tl$pcs) * nT); expect_equal(dplyr::n_distinct(tl$pcs), sum(env$exposition_tension$metiers$present_base))
  expect_true(all(is.na(tl$taux_pct[!tl$diffuse])) && all(!is.na(tl$taux_pct[tl$diffuse]))); expect_true(all(tl$lecture[!tl$diffuse] == "non diffusé (secret statistique)"))
  dd8 <- env$departs_pcs$diffusion$departement |> dplyr::mutate(pcs_n = env$normaliser_code_pcs(pcs), geo_code = as.character(geo_code))
  jl <- dplyr::inner_join(tl[tl$diffuse & tl$niveau == "département", ] |> dplyr::mutate(pcs_n = env$normaliser_code_pcs(pcs)), dd8, by = c("pcs_n", "geo_code"))
  expect_equal(nrow(jl), sum(tl$diffuse & tl$niveau == "département")); expect_false(any(jl$masque)); expect_equal(jl$taux_pct, round(jl$taux_depart_central_pct, 1))   # chaque cellule visible = diffusion 08c
  hm <- paste(readLines(file.path(dt, "diffusion", "fiche_metiers_liste.html"), encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("<script", hm, fixed = TRUE)); expect_false(grepl("stratégique", hm)); expect_true(grepl("métiers de la première sous-liste", hm, fixed = TRUE))
  expect_equal(lengths(regmatches(hm, gregexpr('<section class="metier"', hm))), dplyr::n_distinct(tl$pcs)); expect_equal(lengths(regmatches(hm, gregexpr('<details class="terr">', hm))), nT)
  for (f in c("fiche_exposition_tension.html")) expect_false(grepl("stratégique", gsub("<[^>]+>", " ", paste(readLines(file.path(dt, "diffusion", f), encoding = "UTF-8"), collapse = " "))), label = f)
  expect_identical(readxl::excel_sheets(file.path(dt, "diffusion", "metiers_liste_territoires.xlsx")), c("Lisez-moi", "Métiers x territoires", "Par territoire", "Paramètres"))
  xl <- readxl::read_excel(file.path(dt, "diffusion", "metiers_liste_territoires.xlsx"), sheet = "Métiers x territoires"); expect_equal(nrow(xl), nrow(tl)); expect_false(any(grepl("stratégique", names(xl))))
  # 09b (specs/11) : analyse « hypothèses de départ » sur la chaîne de test : classeur et CSV, feuilles attendues, volume = modèle, aucun effectif sous le seuil, libellé public
  sourcer_scripts(c("09a_fonctions_analyse_hypotheses.R", "09b_analyse_hypotheses_depart.R"), env); expect_true(exists("analyse_hypotheses_depart", envir = env, inherits = FALSE))
  da <- file.path(env$DIR_SORTIES, "analyse_hypotheses_depart", env$GEO_ANALYSE); xa <- file.path(da, "analyse_hypotheses_depart.xlsx"); expect_true(file.exists(xa))
  fa <- readxl::excel_sheets(xa); expect_identical(fa[1:3], c("Lisez-moi", "1 Hypothèses", "2a Volume")); expect_true(all(c("3d Noyau dur", "4b Départements", "4c Exposition régions", "5 Priorités", "Paramètres") %in% fa))
  for (f in c("01_hypotheses.csv", "02a_volume.csv", "02b_par_age.csv", "03a_metiers_volume.csv", "03c_metiers_liste.csv", "04a_regions.csv", "04b_departements.csv", "04d_liste_regions.csv", "05_priorites.csv", "parametres.csv")) expect_true(file.exists(file.path(da, f)), label = f)
  ta <- env$analyse_hypotheses_depart$tables; expect_false(any(vapply(ta, is.null, TRUE)))
  expect_equal(ta[["2a Volume"]][[2]][1], nrow(env$bts_projete)); expect_equal(ta[["2a Volume"]][[2]][2], env$arrondir_nombre_personnes(sum(env$bts_projete$p_central)))
  d4b <- ta[["4b Départements"]]; expect_true(all(d4b$`Salariés`[!is.na(d4b$`Salariés`)] >= env$SECRET_MIN_SALARIES)); expect_true("Profil (typologie)" %in% names(d4b)); expect_true(any(grepl("Classe d’exposition", names(d4b))))
  expect_true(any(d4b$`Diffusé` == "non diffusé (secret statistique)")); expect_true(all(is.na(d4b$`Salariés`[d4b$`Diffusé` != "oui"])))
  expect_equal(nrow(ta[["3c Métiers de la liste"]]), nrow(env$exposition_tension$metiers)); expect_true(nrow(ta[["4a Régions"]]) > 0)
  x3 <- readxl::read_excel(xa, sheet = "3c Métiers de la liste", skip = 3); expect_true("Situation" %in% names(x3)); expect_false(any(grepl("stratégique", c(names(x3), unlist(x3)))))
  x1 <- readxl::read_excel(xa, sheet = "1 Hypothèses", skip = 3); expect_true(any(grepl("métiers de la première sous-liste", x1$`Valeur retenue`, fixed = TRUE)))
  env1 <- new.env(parent = env); assign("GENERER_ANALYSE", FALSE, envir = env1); expect_message(sys.source(chemin_script("09b_analyse_hypotheses_depart.R"), envir = env1), "GENERER_ANALYSE")
  # sans liste (08i non exécuté) : message, aucun arrêt
  env0 <- new.env(parent = env); assign("exposition_tension", NULL, envir = env0); expect_message(sys.source(chemin_script("08j_metiers_liste_territoires.R"), envir = env0), "liste de métiers absente")
  # planchers de la carte (SPEC-TYPO-109) : présents, entiers, jamais une valeur pour un territoire non diffusé ; région cohérente (somme des départements de la région, à l'arrondi près)
  dr <- utils::read.csv2(file.path(dt, "diffusion", "exposition_tension_regions.csv"), colClasses = c(geo_code = "character"), fileEncoding = "UTF-8-BOM")
  expect_true(all(c("tension_departs_min", "tension_departs_min_bas", "tension_departs_min_haut", "tension_part_departs_min_pct") %in% names(dr)))
  expect_true(all(is.na(dr$tension_departs_min[!dr$tension_diffusee]))); expect_true(all(dr$tension_departs_min[dr$tension_diffusee] == round(dr$tension_departs_min[dr$tension_diffusee])))
  expect_true(any(grepl("des départs de la région", dr$tension_texte)))
  dd <- utils::read.csv2(file.path(dt, "diffusion", "exposition_tension_departements.csv"), colClasses = c(geo_code = "character"), fileEncoding = "UTF-8-BOM")
  expect_true(all(dd$tension_departs_min_bas[dd$tension_diffusee] <= dd$tension_departs_min[dd$tension_diffusee] & dd$tension_departs_min[dd$tension_diffusee] <= dd$tension_departs_min_haut[dd$tension_diffusee]))
  expect_true(all(dd$tension_part_departs_min_pct[dd$tension_diffusee] <= dd$tension_couverture_pct[dd$tension_diffusee] + 0.1))   # A / total <= (A + B) / total
  dif <- utils::read.csv2(file.path(dt, "diffusion", "exposition_tension_departements.csv"), colClasses = c(geo_code = "character"))
  expect_false(any(grepl("^departs_|^effectif_|n_entreprises|part_dominante", names(dif))))
  expect_setequal(dif$geo_code, env$univers_territoires("departement")$geo_code); expect_true(all(unique(env$departs_pcs$analytique$departement$geo_code) %in% dif$geo_code))   # SPEC-SEC-035
  expect_true(all(!dif$tension_diffusee[!dif$geo_code %in% env$departs_pcs$analytique$departement$geo_code])); expect_setequal(dr$geo_code, env$univers_territoires("region")$geo_code)
  h <- paste(readLines(file.path(dt, "diffusion", "fiche_exposition_tension.html"), encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("<script", h, fixed = TRUE)); expect_false(grepl("data:image/png", h, fixed = TRUE)); expect_true(grepl('<svg class="mat"', h, fixed = TRUE))
  E <- env$exposition_tension; expect_equal(nrow(E$metiers), 12L); expect_true(all(E$metiers$present_base))
  # classeur Excel (SPEC-TYPO-108) : produit sur la chaîne, relu ; mêmes valeurs que les CSV de diffusion ; désactivable
  fx <- file.path(dt, "diffusion", "exposition_tension.xlsx"); expect_true(file.exists(fx))
  expect_identical(readxl::excel_sheets(fx), c("Lisez-moi", "Départements", "Métiers", "Paramètres"))
  xd <- readxl::read_excel(fx, sheet = "Départements"); expect_identical(xd$Code, sort(dif$geo_code))
  expect_equal(xd$`Part des remplacements difficiles (%)`[match(dif$geo_code, xd$Code)], dif$tension_exposition_pct); expect_identical(xd$Classe[match(dif$geo_code, xd$Code)][!is.na(dif$tension_classe)], dif$tension_classe[!is.na(dif$tension_classe)])
  xm <- readxl::read_excel(fx, sheet = "Métiers"); cm <- utils::read.csv2(file.path(dt, "diffusion", "metiers_strategiques_tension.csv"), colClasses = c(pcs = "character"))
  expect_setequal(xm$`Code PCS`, cm$pcs); expect_equal(xm$`Départs d’ici 2030`[match(cm$pcs, xm$`Code PCS`)], cm$departs_central)
  xp <- readxl::read_excel(fx, sheet = "Paramètres"); expect_identical(names(xp), c("parametre", "valeur")); expect_true("seuil_rang" %in% xp$parametre)
  unlink(fx); assign("TENSION_CLASSEUR_EXCEL", FALSE, envir = env); sourcer_scripts("08i_exposition_tension.R", env); expect_false(file.exists(fx))
  assign("TENSION_CLASSEUR_EXCEL", TRUE, envir = env)
})

test_that("SPEC-TYPO-108 — classeur Excel : quatre feuilles issues des tables de diffusion, colonnes en clair, non diffusés sans valeur, aucun effectif, relecture ; repli writexl", {
  fr <- DEP_T |> dplyr::group_by(pcs, cs1) |> dplyr::summarise(effectif_champ = sum(effectif_champ), departs_central = sum(departs_central), .groups = "drop") |>
    dplyr::mutate(taux_depart_central_pct = 100 * departs_central / effectif_champ)
  m <- ET2$matrice_metiers_tension(MET_T, fr, fr |> dplyr::mutate(masque = pcs == "5b5b"), PAR_T)
  dif <- tibble::tibble(geo_code = c("D3", "D1", "D2", "D6"), geo_nom = c("Trois", "Un", "Deux", "Six"), tension_diffusee = c(TRUE, TRUE, TRUE, FALSE),
                        tension_exposition_pct = c(60, 30, NA, NA), tension_exposition_france_pct = c(40.26, 40.26, 40.26, NA), tension_couverture_pct = c(100, 50, 9.52, NA),
                        tension_classe = c("Forte", "Modérée", NA, NA), tension_robustesse = c("à confirmer", "robuste", NA, NA), tension_causes = c("seuil de tension", NA, NA, NA),
                        tension_levier = c(NA, "attirer et élargir les viviers de recrutement", NA, NA), tension_composante = c(NA, "manque de main-d’œuvre disponible", NA, NA),
                        tension_metiers = c("Métier 2A2A", "Métier 1A1A", NA, NA), tension_cumul = c(TRUE, FALSE, NA, NA), tension_texte = c("Phrase D3.", "Phrase D1.", "Phrase D2.", NA))
  param <- tibble::tibble(parametre = c("seuil_rang", "rang_max"), valeur = c("4", "5"))
  tb <- ET2$tables_classeur_tension(dif, m, c("M1.", "M2.", "M3."), param, PAR_T, c(bas = 25, haut = 50), 1L, ET2$construire_libelle_champ(), "table test", as.Date("2030-01-01"))
  expect_identical(names(tb), c("Lisez-moi", "Départements", "Métiers", "Paramètres"))
  d <- tb$Départements
  expect_identical(names(d), c("Code", "Département", "Diffusé", "Part des remplacements difficiles (%)", "Repère France (%)", "Couverture (%)", "Classe", "Robustesse", "Sensible à", "Levier", "Composante",
                               "Métiers en tension les plus concernés", "Renouvellement rapide et forte part de remplacements difficiles",
                               "Départs dans les métiers suivis en tension (au moins)", "Part de ces départs dans tous les départs (%, au moins)", "Texte"))
  expect_true(all(is.na(d$`Départs dans les métiers suivis en tension (au moins)`)))                       # colonnes absentes de la table d'entrée : NA, jamais 0
  expect_identical(d$Code, c("D1", "D2", "D3", "D6")); expect_identical(d$Diffusé, c("oui", "oui", "oui", "non diffusé (secret statistique)"))
  expect_equal(d$`Part des remplacements difficiles (%)`, c(30, NA, 60, NA)); expect_equal(d$`Repère France (%)`, c(40.3, 40.3, 40.3, NA)); expect_equal(d$`Couverture (%)`[2], 9.5)
  expect_identical(d$Classe, c("Modérée", "non interprétée (couverture insuffisante)", "Forte", NA)); expect_identical(d$`Renouvellement rapide et forte part de remplacements difficiles`, c("non", NA, "oui", NA))
  expect_true(all(is.na(unlist(d[d$Code == "D6", -(1:3)]))))                                                                     # non diffusé : aucune valeur
  expect_false(any(grepl("^departs_|^effectif_|n_entreprises|part_dominante", names(d))))
  mt <- tb$Métiers
  expect_identical(names(mt), c("Situation", "Métier", "Code PCS", "Famille professionnelle", "Code FAP", "Départs d’ici 2030", "Fourchette basse", "Fourchette haute", "Part susceptible de partir (%)",
                                "Repère France (%)", "Tension (rang)", "Tension (valeur fine)", "En tension", "Composante qui pèse le plus", "Levier", "Rattachement PCS → FAP", "Statut"))
  expect_equal(nrow(mt), nrow(MET_T))
  expect_identical(mt$Situation[1:6], c("Départs rapides et métier en tension", "Départs rapides, métier hors tension", "Départs rapides, métier hors tension", "Départs modérés, métier en tension", "Départs modérés, métier en tension", "Départs modérés, métier en tension"))
  expect_equal(mt$`Départs d’ici 2030`[1:6], c(60, 110, 51, 49, 40, 40)); expect_identical(mt$`Code PCS`[5:6], c("1A1A", "5A5A"))   # ordre : situation, départs décroissants, puis code
  expect_identical(mt$Statut[7:9], c("non diffusé (secret statistique)", "tension inconnue", "absente de la Base")); expect_true(all(is.na(mt$Situation[7:9])))
  expect_true(all(is.na(unlist(mt[7:9, c("Départs d’ici 2030", "Fourchette basse", "Fourchette haute", "Part susceptible de partir (%)", "Repère France (%)")]))))
  expect_identical(mt$`Rattachement PCS → FAP`[mt$`Code PCS` == "5A5A"], "à confirmer"); expect_identical(mt$`Tension (rang)`[mt$`Code PCS` == "1A1A"], 5L)
  li <- tb$`Lisez-moi`; expect_identical(names(li), "texte"); expect_identical(li$texte[1], "Départs attendus et difficultés de recrutement"); expect_true(attr(li, "gras")[1]); expect_false(attr(li, "gras")[2])
  expect_true("Trois messages" %in% li$texte); expect_true(any(grepl("^Renouvellement rapide et métiers en tension : M1\\.$", li$texte))); expect_true(any(grepl("Classeur produit le 01/01/2030", li$texte, fixed = TRUE)))
  expect_true(any(grepl("Ce que ce classeur ne dit pas", li$texte, fixed = TRUE))); expect_true(any(grepl("La tension n’est connue que pour les 9 métiers de la première sous-liste", li$texte, fixed = TRUE))); expect_true(any(grepl("faible sous 25,0 %, forte à partir de 50,0 % (seuils fixes)", li$texte, fixed = TRUE)))
  expect_false(any(grepl("horloge|critique|Sous contrôle", li$texte, ignore.case = TRUE)))
  expect_identical(tb$Paramètres$parametre, c("seuil_rang", "rang_max"))
  # garde : un effectif dans la table des départements arrête la construction (le 08i le transforme en message)
  expect_error(ET2$tables_classeur_tension(dif |> dplyr::mutate(departs_total = 1), m, c("a", "b", "c"), param, PAR_T), "effectif ou indicateur de secret")
  # écriture (openxlsx) puis relecture : quatre feuilles, mêmes valeurs ; mise en forme présente ; repli writexl : même contenu
  skip_if_not_installed("openxlsx")
  fx <- tempfile(fileext = ".xlsx"); expect_identical(ET2$ecrire_classeur_tension(tb, fx), fx); expect_true(file.exists(fx))
  expect_identical(readxl::excel_sheets(fx), names(tb))
  xd <- readxl::read_excel(fx, sheet = "Départements"); expect_identical(xd$Code, d$Code); expect_equal(xd$`Part des remplacements difficiles (%)`, d$`Part des remplacements difficiles (%)`); expect_identical(xd$Texte, d$Texte)
  xm <- readxl::read_excel(fx, sheet = "Métiers"); expect_identical(xm$`Code PCS`, mt$`Code PCS`); expect_equal(xm$`Départs d’ici 2030`, mt$`Départs d’ici 2030`); expect_equal(xm$`Tension (valeur fine)`, mt$`Tension (valeur fine)`)
  xl <- readxl::read_excel(fx, sheet = "Lisez-moi", col_names = FALSE); expect_identical(xl[[1]][1], li$texte[1]); expect_true(all(li$texte[li$texte != ""] %in% xl[[1]]))
  x <- openxlsx::loadWorkbook(fx); expect_identical(names(x), names(tb))
  dz <- file.path(tempdir(), "xlsx_ctl"); unlink(dz, recursive = TRUE); utils::unzip(fx, exdir = dz)
  s2 <- paste(readLines(file.path(dz, "xl", "worksheets", "sheet2.xml"), warn = FALSE), collapse = ""); st <- paste(readLines(file.path(dz, "xl", "styles.xml"), warn = FALSE), collapse = "")
  expect_true(grepl('state="frozen"', s2, fixed = TRUE)); expect_true(grepl("<autoFilter ", s2, fixed = TRUE))
  for (c in toupper(sub("#", "FF", unname(c(ET2$COULEURS_CLASSES_EXPOSITION[c("Modérée", "Forte")], ET2$COULEURS_SITUATIONS[c("critique", "releve", "difficile")]))))) expect_true(grepl(c, st, fixed = TRUE), label = c)   # fonds des classes et situations présentes
  expect_true(grepl('formatCode="0.0"', st, fixed = TRUE))
  fw <- tempfile(fileext = ".xlsx")
  expect_message(ET2$ecrire_classeur_tension(tb, fw, moteur = "writexl"), "sans mise en forme")
  expect_identical(readxl::excel_sheets(fw), names(tb)); expect_equal(readxl::read_excel(fw, sheet = "Métiers")$`Départs d’ici 2030`, mt$`Départs d’ici 2030`)
})
