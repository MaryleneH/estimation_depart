# ==============================================================================
# Cartes des profils de la typologie (00i + 08g) — specs/10 §9 (SPEC-TYPO-060 à 064).
# Données exclusivement SYNTHÉTIQUES (table de DIFFUSION fabriquée) + chaîne de test.
# ==============================================================================
EK <- new.env()
.old_wd <- setwd(RACINE)
source(chemin_script("00_config.R"), local = EK)
setwd(.old_wd)
assign("DIR_DATA", file.path(RACINE, "data"), envir = EK)
sys.source(chemin_script("00g_format_restitution.R"), envir = EK)
sys.source(chemin_script("00f_fonctions_cartes.R"), envir = EK)
sys.source(chemin_script("00h_fonctions_typologie.R"), envir = EK)
sys.source(chemin_script("00i_fonctions_carte_typologie.R"), envir = EK)
FOND_DEP_K <- EK$charger_fond_carte("departement"); FOND_REG_K <- EK$charger_fond_carte("region")
P_K <- EK$PROFILS_TYPOLOGIE
# table de DIFFUSION synthétique : A (33) diffusée, B (64) masquée (valeurs volontairement laissées : doivent être effacées),
# C (01) profil non diffusé (cellule CS masquée : chiffres publiés, catégories NA), D (2A) situation à expertiser
TABLE_TYPO <- tibble::tibble(
  region_code = c("75", "75", "84", "94"), region_nom = "R",
  geo_code = c("33", "64", "01", "2A"), geo_nom = c("Gironde", "Pyrénées-Atlantiques", "Ain", "Corse-du-Sud"),
  effectif_bitd_total = c(12345, 9876543, 4321, 210), departs_central = c(1234, 5432109, 321, 40), departs_bas = c(1200, 5432100, 300, 36), departs_haut = c(1300, 5432199, 350, 44),
  part_emploi_bitd_national_pct = c(10.7, 88.8, 3.8, 0.2), intensite_renouvellement_pct = c(10, 99.9, 7.4, 19), indicateur_intensite = "part_a_remplacer_pct", base_poids = "effectif_tous_ages",
  classe_poids_bitd = c("Fort", "Fort", "Moyen", "Faible"), classe_intensite = c("Élevée", "Faible", "Modérée", "Élevée"), classe_volume_departs = c("Élevé", "Élevé", "Modéré", "Faible"),
  structure_emploi = c("Dominante cadres", "Mixte", NA, "Mixte"), cs_principale = c("Cadres", "Ouvriers", NA, "Ouvriers"), part_cs_principale_pct = c(45.5, 30, NA, 35),
  cs_volume_departs_max = c("Cadres", "Ouvriers", NA, "Ouvriers"), part_departs_cs_max_pct = c(61, 33, NA, 40), cs_taux_renouvellement_max = c("Cadres", "Employes", NA, "Ouvriers"),
  type_concentration = c("Concentré", "Diffus", NA, "Diffus"),
  profil_typologie = c(P_K[["enjeu"]], "Non diffusé (secret statistique)", "Non diffusé (secret statistique)", P_K[["expertiser"]]),
  motif_expertise = c(NA, NA, NA, "intensité de renouvellement extrême sur un très petit effectif"),
  points_attention = c("une seule catégorie porte 61,0 % des départs (cadres)", NA, NA, NA),
  situation_texte = c("Gironde (33) pèse 10,7 % de l’emploi BITD national (12 345 salariés), un poids fort. Phrase de test.", "SECRET-FUITE-TEXTE", "Ain (01) pèse 3,8 %. Phrase sans catégorie.", NA),
  masque = c(FALSE, TRUE, FALSE, FALSE), motif_masque = c(NA, "territoire", "cellule cs masquée", NA))
# espace fine insécable (U+202F, séparateur de milliers de fmt_fr_carte) ramenée à une espace pour les comparaisons
nrm <- function(x) gsub("\u202f", " ", x)
lire_html <- function(f) nrm(paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n"))

test_that("SPEC-TYPO-060 — source : seule la diffusion est admise (masque requis), code sans géométrie = arrêt, « inconnu » compté, profil inconnu = arrêt, CSV relu avec virgule décimale", {
  expect_error(EK$preparer_carte_typologie(TABLE_TYPO |> dplyr::select(-masque), FOND_DEP_K), "masque")
  expect_error(EK$preparer_carte_typologie(TABLE_TYPO |> dplyr::mutate(geo_code = c("33", "64", "999", "2A")), FOND_DEP_K), "999")
  expect_error(EK$preparer_carte_typologie(TABLE_TYPO |> dplyr::mutate(profil_typologie = c("Profil inventé", profil_typologie[-1])), FOND_DEP_K), "Profil inventé")
  p <- EK$preparer_carte_typologie(dplyr::bind_rows(TABLE_TYPO, TABLE_TYPO[1, ] |> dplyr::mutate(geo_code = "inconnu", geo_nom = "Territoire inconnu")), FOND_DEP_K)
  expect_equal(p$n_non_localises, 1L); expect_equal(p$n_csv, 4L); expect_equal(nrow(p$territoires), 101L)
  expect_identical(p$territoires$statut[match(c("33", "64", "01", "2A", "75"), p$territoires$code)], c("diffuse", "masque", "profil_masque", "diffuse", "sans"))
  expect_equal(unname(p$comptes[c("enjeu", "expertiser", "profil_masque", "masque", "sans")]), c(1, 1, 1, 1, 97))
  # CSV write.csv2 (virgule décimale, BOM facultatif) relu tel quel
  f <- file.path(tempdir(), "typo_diff.csv"); write.csv2(EK$formater_restitution(TABLE_TYPO), f, row.names = FALSE)
  d <- EK$lire_typologie_diffusion(f)
  expect_type(d$geo_code, "character"); expect_type(d$departs_central, "double"); expect_equal(d$part_emploi_bitd_national_pct[1], 10.7); expect_identical(d$masque, c(FALSE, TRUE, FALSE, FALSE))
  expect_error(EK$lire_typologie_diffusion(file.path(tempdir(), "absent.csv")), "introuvable")
})

test_that("SPEC-TYPO-061/062 — page interactive : couleur par profil écrite dans le HTML, gris / blanc, légende avec comptes et significations, JSON de détail, survol et épinglage câblés, définitions et tableau", {
  p <- EK$preparer_carte_typologie(TABLE_TYPO, FOND_DEP_K); expect_type(EK$controler_carte_typologie(p), "integer")
  f <- file.path(tempdir(), "carte_typo.html"); png <- file.path(tempdir(), "carte_typo.png")
  EK$generer_carte_typologie(p, FOND_DEP_K, f, fond_regions = FOND_REG_K, fichier_png = if (requireNamespace("ggplot2", quietly = TRUE)) png else NULL)
  html <- lire_html(f); sans <- gsub("<script\\b.*?</script>", "", html, perl = TRUE)
  paths <- regmatches(sans, gregexpr('<path class="t[^>]*>', sans))[[1]]; expect_length(paths, 101L)
  attr_ <- function(code, a) sub(sprintf('.* %s="([^"]*)".*', a), "\\1", paths[grepl(sprintf('data-code="%s"', code), paths)])
  expect_identical(attr_("33", "fill"), EK$COULEURS_PROFILS[["enjeu"]]); expect_identical(attr_("2A", "fill"), EK$COULEURS_PROFILS[["expertiser"]])
  expect_identical(attr_("64", "fill"), EK$COULEURS_CARTE$secret); expect_identical(attr_("01", "fill"), EK$COULEURS_CARTE$secret)
  expect_identical(attr_("75", "fill"), EK$COULEURS_CARTE$sans_donnee); expect_true(grepl('class="t sans"[^>]*data-code="75"[^>]*stroke-dasharray', sans))
  expect_identical(attr_("33", "data-cle"), "enjeu"); expect_identical(attr_("64", "data-st"), "masque"); expect_identical(attr_("01", "data-st"), "profil_masque")
  expect_true(grepl('<g class="limites-reg"', sans, fixed = TRUE)); expect_true(grepl('<path id="survol"', sans, fixed = TRUE))
  # légende statique : 7 profils cliquables + non diffusé + absent, comptes et significations
  leg <- regmatches(sans, regexpr('(?s)<ul class="typo-legende" id="legende">.*?</ul>', sans, perl = TRUE))
  expect_equal(lengths(regmatches(leg, gregexpr('<button type="button" data-cle="', leg))), 7L)
  expect_true(grepl(sprintf('<span class="lib">%s<span class="n">1</span></span>', EK$PROFILS_TYPOLOGIE[["enjeu"]]), leg, fixed = TRUE))
  expect_true(grepl('Profil non diffusé (secret statistique)<span class="n">2</span>', leg, fixed = TRUE))
  expect_true(grepl('Département absent de la typologie<span class="n">97</span>', leg, fixed = TRUE))
  expect_true(grepl(EK$DEFINITIONS_PROFILS$signification[EK$DEFINITIONS_PROFILS$cle == "stable"], leg, fixed = TRUE))
  # JSON de détail : lignes de l'infobulle, texte de situation, profils avec consigne ; survol / clic / légende câblés
  j <- jsonlite::fromJSON(regmatches(html, regexpr('(?s)(?<=<script type="application/json" id="donnees">).*?(?=</script>)', html, perl = TRUE)))
  g <- j$territoires[["33"]]; expect_identical(g$cle, "enjeu"); expect_match(g$texte, "^Gironde \\(33\\) pèse 10,7 %")
  expect_identical(nrm(g$lignes[1:2]), c("10,7 % de l’emploi BITD national · 12 345 salariés", "1 234 départs estimés d’ici 2030 (entre 1 200 et 1 300)"))
  expect_true(any(grepl("^10,0 % de l’emploi actuel à remplacer", g$lignes))); expect_true("Poids fort · volume élevé · intensité élevée" %in% g$lignes)
  expect_true("Emploi dominé par les cadres (45,5 %)" %in% g$lignes); expect_true("Départs portés d’abord par les cadres (61,0 %, concentrés sur cette catégorie)" %in% g$lignes)
  expect_true(any(grepl("^Point\\(s\\) d’attention : une seule catégorie", g$lignes)))
  expect_true(any(grepl("^À examiner : intensité de renouvellement extrême", j$territoires[["2A"]]$lignes)))
  expect_identical(j$profils$enjeu$titre, EK$PROFILS_TYPOLOGIE[["enjeu"]]); expect_equal(j$profils$enjeu$n, 1L); expect_true(nzchar(j$profils$enjeu$consigne))
  for (s in c('addEventListener("mousemove"', 'addEventListener("click"', 'pin=(pin===code)?null:code', '.typo-legende button', 'classList.toggle("dim"', 'getElementById("sans-js")'))
    expect_true(grepl(s, html, fixed = TRUE), label = s)
  expect_true(grepl('id="detail-texte"', sans, fixed = TRUE)); expect_true(grepl('id="sans-js"', sans, fixed = TRUE))
  # définitions et tableau des situations (dans l'ordre des profils, situation_texte)
  expect_true(grepl('<summary>Que signifient les profils ?</summary>', sans, fixed = TRUE)); expect_equal(lengths(regmatches(sans, gregexpr('<tr><td><span class="sw"', sans))), 7L)
  tab <- regmatches(sans, regexpr('(?s)<summary>Le détail par département</summary>.*?</details>', sans, perl = TRUE))
  expect_true(grepl("Gironde · 33", tab, fixed = TRUE)); expect_true(grepl("Phrase de test.", tab, fixed = TRUE))
  expect_lt(regexpr("Gironde · 33", tab, fixed = TRUE), regexpr("Corse-du-Sud · 2A", tab, fixed = TRUE))     # enjeu avant expertiser
  expect_true(grepl('Pyrénées-Atlantiques · 64</td><td colspan="4">', tab, fixed = TRUE))
  for (v in c("insee.fr", "http://", "https://")) expect_false(grepl(v, html, fixed = TRUE), label = v)      # page autonome
  if (requireNamespace("ggplot2", quietly = TRUE)) expect_true(file.exists(png))
})

test_that("SPEC-TYPO-063/064 — version courriel : aucun contenu actif, <title> natifs, tableau et définitions ouverts ; aucune fuite d'un département masqué, profil non diffusé sans catégorie", {
  p <- EK$preparer_carte_typologie(TABLE_TYPO, FOND_DEP_K)
  f <- file.path(tempdir(), "carte_typo_courriel.html"); EK$generer_carte_typologie_courriel(p, FOND_DEP_K, f, fond_regions = FOND_REG_K)
  html <- lire_html(f)
  expect_false(grepl("<script", html, fixed = TRUE)); expect_false(grepl(" on[a-z]+=", html)); expect_false(grepl("javascript:", html, fixed = TRUE))
  expect_false(grepl('id="donnees"', html, fixed = TRUE)); expect_false(grepl('id="sans-js"', html, fixed = TRUE))
  expect_equal(lengths(regmatches(html, gregexpr("<title>", html))), 102L)                       # page + un par département
  expect_true(grepl('<title>Gironde · 33\nPôle majeur à renouveler rapidement\n10,7 % de l’emploi BITD national · 12 345 salariés\n', html, fixed = TRUE))
  expect_true(grepl('<title>Pyrénées-Atlantiques · 64\nNon diffusé (secret statistique)\nRésultat non diffusé — secret statistique</title>', html, fixed = TRUE))
  expect_true(grepl('<title>Paris · 75\nDépartement absent de la typologie\nDépartement absent de la typologie</title>', html, fixed = TRUE))
  expect_true(grepl('<title>Ain · 01\nNon diffusé (secret statistique)\nProfil non diffusé (secret statistique)\n3,8 % de l’emploi BITD national · 4 321 salariés\n', html, fixed = TRUE))
  expect_true(grepl('<details class="bloc-typo" open><summary>Le détail par département</summary>', html, fixed = TRUE))
  expect_true(grepl('<details class="bloc-typo" open><summary>Que signifient les profils ?</summary>', html, fixed = TRUE))
  expect_true(grepl(sprintf('<path fill="%s"', EK$COULEURS_PROFILS[["enjeu"]]), html, fixed = TRUE))
  expect_true(grepl("use:hover", html, fixed = TRUE) || grepl(":hover", html, fixed = TRUE))
  # SPEC-TYPO-064 : rien du département masqué (64), dans aucune des deux pages, ni dans la structure
  fi <- file.path(tempdir(), "carte_typo2.html"); EK$generer_carte_typologie(p, FOND_DEP_K, fi)
  for (page in list(html, lire_html(fi))) for (v in c("9876543", "9 876 543", "5432109", "5 432 109", "5 432 100", "88,8", "99,9", "SECRET-FUITE", "Employes"))
    expect_false(grepl(v, page, fixed = TRUE), label = v)
  m <- p$territoires[p$territoires$code == "64", ]; expect_true(all(is.na(m[, c("departs_central", "situation_texte", "cs_principale", "motif_expertise")])))
  expect_identical(m$profil_typologie, "Non diffusé (secret statistique)")
  # profil non diffusé (01) : chiffres présents, aucune catégorie, aucune concentration
  a <- jsonlite::fromJSON(regmatches(lire_html(fi), regexpr('(?s)(?<=<script type="application/json" id="donnees">).*?(?=</script>)', lire_html(fi), perl = TRUE)))$territoires[["01"]]
  expect_identical(a$st, "profil_masque"); expect_null(a$cle); expect_false(any(grepl("cadres|ouvriers|Emploi|portés", a$lignes)))
  # contenu actif injecté = arrêt
  p2 <- p; p2$territoires$nom[p2$territoires$code == "33"] <- "Gironde <script>alert(1)</script>"
  f2 <- file.path(tempdir(), "carte_typo_courriel2.html"); EK$generer_carte_typologie_courriel(p2, FOND_DEP_K, f2)
  expect_false(grepl("<script", lire_html(f2), fixed = TRUE))                                     # échappé, jamais actif
})

test_that("SPEC-TYPO-060/061/063 — 08g sur la chaîne de test en mode département : trois fichiers, bilan, fichier absent = message ; lancement seul depuis la racine", {
  skip_if_not_installed("ggplot2")
  env <- new.env(); old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), "typo_carte_chaine"), envir = env); dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(c("00c_fonctions_geo.R", "00g_format_restitution.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R", "00f_fonctions_cartes.R",
                    "00h_fonctions_typologie.R", "00i_fonctions_carte_typologie.R",
                    "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
                    "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R", "08_analyse_55plus_geo.R", "08b_departs_geo_cs.R",
                    "08c_departs_pcs.R", "08d_departs_cs1.R", "08f_typologie_departements.R"), env)
  avant <- lapply(c("departs_cs1", "typologie_departements", "bts_projete"), function(o) get(o, envir = env))
  expect_output(sys.source(chemin_script("08g_carte_typologie.R"), envir = env), "08g — cartes des profils")
  expect_identical(avant, lapply(c("departs_cs1", "typologie_departements", "bts_projete"), function(o) get(o, envir = env)))   # rien n'est touché en amont
  b <- env$carte_typologie; expect_identical(b$statut, "ok")
  dir_c <- file.path(env$DIR_SORTIES, "typologie_territoriale", "cartes")
  for (f in c("carte_typologie.html", "carte_typologie_courriel.html", "carte_typologie.png")) expect_true(file.exists(file.path(dir_c, f)), label = f)
  html <- lire_html(file.path(dir_c, "carte_typologie.html")); courriel <- lire_html(file.path(dir_c, "carte_typologie_courriel.html"))
  expect_false(grepl("<script", courriel, fixed = TRUE))
  d <- env$typologie_departements$diffusion
  n_prof <- sum(d$profil_typologie %in% env$PROFILS_TYPOLOGIE)
  expect_equal(lengths(regmatches(html, gregexpr('data-cle="', html))) - 7L, n_prof)                # un data-cle par département profilé (+ 7 boutons de légende)
  expect_true(grepl(sprintf("Salariés de %d ans ou plus", env$AGE_MIN_BTS), html, fixed = TRUE))    # champ depuis la configuration
  # lancement SEUL (processus neuf, sans la chaîne) depuis la racine : mêmes fichiers
  unlink(dir_c, recursive = TRUE)
  log <- file.path(tempdir(), "08g_seul.log")
  statut <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(sprintf('DIR_SORTIES <- "%s"; source("R/08_territoires/08g_carte_typologie.R")', env$DIR_SORTIES))), stdout = log, stderr = log)
  expect_equal(statut, 0L, label = paste(tail(readLines(log, warn = FALSE), 5), collapse = "\n"))
  expect_true(file.exists(file.path(dir_c, "carte_typologie_courriel.html")))
  # fichier de diffusion absent : message avec le chemin, aucun arrêt
  env2 <- new.env(); sys.source(chemin_script("00_config.R"), envir = env2)
  assign("DIR_SORTIES", file.path(tempdir(), "typo_carte_vide"), envir = env2); dir.create(env2$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(c("00g_format_restitution.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R", "00i_fonctions_carte_typologie.R"), env2)
  expect_message(sys.source(chemin_script("08g_carte_typologie.R"), envir = env2), "absente")
  expect_identical(env2$carte_typologie$statut, "source absente")
  # désactivée : message, rien
  env3 <- new.env(); sys.source(chemin_script("00_config.R"), envir = env3); assign("GENERER_CARTE_TYPOLOGIE", FALSE, envir = env3)
  sourcer_scripts(c("00g_format_restitution.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R", "00i_fonctions_carte_typologie.R"), env3)
  expect_message(sys.source(chemin_script("08g_carte_typologie.R"), envir = env3), "désactivée")
})

test_that("SPEC-TYPO-065 — stabilité du survol et légende : infobulle fixée au viewport, contenu reconstruit seulement au changement, panneau à hauteur stable, masquage au défilement ; aucune entrée « absent » à zéro, sept profils et secret conservés", {
  # (a) légende : table couvrant TOUS les départements du fond -> aucune entrée « absent »
  tout <- tibble::tibble(geo_code = names(FOND_DEP_K), geo_nom = unname(vapply(FOND_DEP_K, `[[`, "", "nom")), region_code = "R", region_nom = "R",
                         effectif_bitd_total = 1000, departs_central = 100, departs_bas = 95, departs_haut = 105, part_emploi_bitd_national_pct = 100 / 101,
                         intensite_renouvellement_pct = 10, indicateur_intensite = "part_a_remplacer_pct", base_poids = "effectif_tous_ages",
                         classe_poids_bitd = "Moyen", classe_intensite = "Modérée", classe_volume_departs = "Modéré", structure_emploi = "Mixte", cs_principale = "Cadres",
                         part_cs_principale_pct = 30, cs_volume_departs_max = "Cadres", part_departs_cs_max_pct = 30, cs_taux_renouvellement_max = "Cadres", type_concentration = "Diffus",
                         profil_typologie = P_K[["stable"]], motif_expertise = NA_character_, points_attention = NA_character_, situation_texte = "Texte.", masque = FALSE, motif_masque = NA_character_)
  tout$profil_typologie[1] <- P_K[["enjeu"]]; tout$masque[2] <- TRUE
  p <- EK$preparer_carte_typologie(tout, FOND_DEP_K); expect_equal(unname(p$comptes[["sans"]]), 0)
  for (inter in c(TRUE, FALSE)) {
    leg <- paste(EK$html_legende_typologie(p, interactive = inter), collapse = "\n")
    expect_false(grepl(EK$LIBELLES_CARTE_TYPO$absent, leg, fixed = TRUE), label = paste("absent masqué, interactive =", inter))
    for (k in names(P_K)) expect_true(grepl(sprintf('%s<span class="n">%d</span>', P_K[[k]], unname(p$comptes[[k]])), leg, fixed = TRUE), label = k)   # sept profils + comptes
    expect_true(grepl(sprintf('%s<span class="n">%d</span>', EK$LIBELLES_CARTE_TYPO$profil_non_diffuse, 1L), leg, fixed = TRUE))                     # secret conservé
  }
  expect_equal(lengths(regmatches(paste(EK$html_legende_typologie(p, TRUE), collapse = ""), gregexpr('<button type="button" data-cle="', paste(EK$html_legende_typologie(p, TRUE), collapse = "")))), 7L)
  # département réellement absent : entrée explicite, distincte du secret, jamais assimilée au masqué
  p2 <- EK$preparer_carte_typologie(tout[-c(5, 6), ], FOND_DEP_K); expect_equal(unname(p2$comptes[["sans"]]), 2)
  leg2 <- paste(EK$html_legende_typologie(p2, TRUE), collapse = "\n")
  expect_true(grepl(sprintf('<span class="sw sans" style="background:%s"></span><span><span class="lib">%s<span class="n">2</span>', EK$COULEURS_CARTE$sans_donnee, EK$LIBELLES_CARTE_TYPO$absent), leg2, fixed = TRUE))
  expect_true(grepl(sprintf('%s<span class="n">1</span>', EK$LIBELLES_CARTE_TYPO$profil_non_diffuse), leg2, fixed = TRUE))
  expect_identical(p2$territoires$statut[p2$territoires$code %in% names(FOND_DEP_K)[5:6]], c("sans", "sans"))
  # (b) page interactive : les deux versions sont écrites et la légende y suit la même règle
  f <- file.path(tempdir(), "carte_typo_stable.html"); EK$generer_carte_typologie(p, FOND_DEP_K, f, fond_regions = FOND_REG_K)
  html <- lire_html(f); sans_js <- gsub("<script\\b.*?</script>", "", html, perl = TRUE)
  expect_false(grepl(EK$LIBELLES_CARTE_TYPO$absent, regmatches(sans_js, regexpr('(?s)<ul class="typo-legende" id="legende">.*?</ul>', sans_js, perl = TRUE)), fixed = TRUE))
  fc <- file.path(tempdir(), "carte_typo_stable_courriel.html"); EK$generer_carte_typologie_courriel(p, FOND_DEP_K, fc, fond_regions = FOND_REG_K)
  hc <- lire_html(fc); expect_false(grepl("<script", hc, fixed = TRUE)); expect_false(grepl(EK$LIBELLES_CARTE_TYPO$absent, regmatches(hc, regexpr('(?s)<ul class="typo-legende" id="legende">.*?</ul>', hc, perl = TRUE)), fixed = TRUE))
  # (c) stabilité : infobulle fixée au viewport et bornée à l'écran ; contenu reconstruit seulement si le département change ;
  #     panneau de détail inchangé si même département, hauteur stabilisée (mesure hors écran) ; infobulle masquée pendant le défilement
  css <- regmatches(html, regexpr("(?s)<style>.*?</style>", html, perl = TRUE))
  expect_true(grepl(".bulle{position:fixed;", css, fixed = TRUE)); expect_true(grepl("overflow-y:auto", regmatches(css, regexpr("\\.detail\\{[^}]*\\}", css)), fixed = TRUE))
  js <- regmatches(html, regexpr("(?s)<script>\\s*\\(function\\(\\)\\{.*?</script>", html, perl = TRUE))
  for (s in c("if(code!==codeBulle){B.innerHTML=bulle(code);codeBulle=code}", "if(!force&&code===codeDetail)return;", "function stabiliserDetail()", "DP.style.height=",
              'addEventListener("scroll",function(){tScroll=Date.now();if(B&&B.style.display==="block")B.style.display="none"},{passive:true})',
              "if(px+w>vw-8)px=x-w-16;if(py+h>vh-8)py=y-h-16;", "detail(pin||code,true)", 'classList.toggle("dim"'))
    expect_true(grepl(s, js, fixed = TRUE), label = s)
  expect_false(grepl("C.getBoundingClientRect()", js, fixed = TRUE))                                    # position en coordonnées de fenêtre, plus relative au conteneur
  # secret : rien du département masqué (2e du fond) dans la page
  m <- tout$geo_code[2]; j <- jsonlite::fromJSON(regmatches(html, regexpr('(?s)(?<=<script type="application/json" id="donnees">).*?(?=</script>)', html, perl = TRUE)))
  expect_identical(j$territoires[[m]]$st, "masque"); expect_null(j$territoires[[m]]$texte); expect_identical(unlist(j$territoires[[m]]$lignes), EK$LIBELLES_UI$secret)
})
