# ==============================================================================
# Cartes « profils et PCS » : de la région au département (00j + 08h) — specs/10 §11 (SPEC-TYPO-080 à 085).
# Données exclusivement SYNTHÉTIQUES (tables de DIFFUSION fabriquées) + chaîne de test.
# ==============================================================================
EP <- new.env()
.old_wd <- setwd(RACINE)
source(chemin_script("00_config.R"), local = EP)
setwd(.old_wd)
assign("DIR_DATA", file.path(RACINE, "data"), envir = EP)
for (s in c("00g_format_restitution.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R", "00i_fonctions_carte_typologie.R", "00j_fonctions_cartes_typologie_pcs.R"))
  sys.source(chemin_script(s), envir = EP)
FOND_DEP_P <- EP$charger_fond_carte("departement"); FOND_REG_P <- EP$charger_fond_carte("region")
P_P <- EP$PROFILS_TYPOLOGIE
nrm <- function(x) gsub(" ", " ", x)
lire_html <- function(f) nrm(paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
json_de <- function(html) jsonlite::fromJSON(regmatches(html, regexpr('(?s)(?<=<script type="application/json" id="donnees">).*?(?=</script>)', html, perl = TRUE)))
# --- typologie synthétique : région 75 = 33 (enjeu) + 64 (masqué) + 24 (stable) ; région 84 = 01 (stable) + 69 (enjeu) ; région 94 = 2A (expertiser)
ligne_typo <- function(code, nom, reg, profil, dc, masque = FALSE) tibble::tibble(
  region_code = reg, region_nom = "R", geo_code = code, geo_nom = nom, effectif_base_total = dc * 10, departs_central = dc, departs_bas = dc - 10, departs_haut = dc + 10,
  part_emploi_base_national_pct = 5, intensite_renouvellement_pct = 10, indicateur_intensite = "part_a_remplacer_pct", base_poids = "effectif_tous_ages",
  classe_poids_base = "Moyen", classe_intensite = "Modérée", classe_volume_departs = "Modéré", structure_emploi = "Mixte", cs_principale = "Cadres", part_cs_principale_pct = 30,
  cs_volume_departs_max = "Cadres", part_departs_cs_max_pct = 30, cs_taux_renouvellement_max = "Cadres", type_concentration = "Diffus",
  profil_typologie = if (masque) "Non diffusé (secret statistique)" else P_P[[profil]], motif_expertise = NA_character_, points_attention = NA_character_,
  situation_texte = if (masque) "SECRET-FUITE-TEXTE" else "Texte.", masque = masque, motif_masque = if (masque) "territoire" else NA_character_)
TYPO_P <- dplyr::bind_rows(ligne_typo("33", "Gironde", "75", "enjeu", 1000), ligne_typo("64", "Pyrénées-Atlantiques", "75", "enjeu", 7654321, masque = TRUE),
                           ligne_typo("24", "Dordogne", "75", "stable", 200), ligne_typo("01", "Ain", "84", "stable", 300), ligne_typo("69", "Rhône", "84", "enjeu", 900),
                           ligne_typo("2A", "Corse-du-Sud", "94", "expertiser", 50))
# --- PCS synthétiques (table de diffusion 08c) : 33 = 6 PCS dont 2 masquées ; 24 = 3 PCS dont EXACTEMENT 1 masquée (top retenu) ;
#     01 = 3 PCS diffusées ; 69 = 4 PCS diffusées ; 2A = 1 PCS ; 64 = tout masqué
cel <- function(geo, nom, reg, pcs, cs1, dc, masque = FALSE) tibble::tibble(
  region_code = reg, region_nom = "R", geo_code = geo, geo_nom = nom, pcs = pcs, cs1 = cs1, effectif_champ = if (masque) 7000001 else dc * 10,
  departs_central = if (masque) 8000002 else dc, departs_bas = if (masque) 8000001 else dc - 1, departs_haut = if (masque) 8000003 else dc + 1,
  dep_retraite = dc, dep_invalidite = 0, dep_deces = 0, taux_depart_central_pct = if (masque) 77.7 else 10, masque = masque, motif_masque = if (masque) "primaire" else NA_character_)
PCS_DEP_P <- dplyr::bind_rows(
  cel("33", "Gironde", "75", "382a", "Cadres", 300), cel("33", "Gironde", "75", "473b", "Prof. intermediaires", 250), cel("33", "Gironde", "75", "623a", "Ouvriers", 250),
  cel("33", "Gironde", "75", "543a", "Employes", 100), cel("33", "Gironde", "75", "624d", "Ouvriers", 40), cel("33", "Gironde", "75", "386d", "Cadres", 30),
  cel("33", "Gironde", "75", "388a", "Cadres", 15, masque = TRUE), cel("33", "Gironde", "75", "676e", "Ouvriers", 15, masque = TRUE),
  cel("24", "Dordogne", "75", "382a", "Cadres", 120), cel("24", "Dordogne", "75", "623a", "Ouvriers", 70), cel("24", "Dordogne", "75", "543a", "Employes", 10, masque = TRUE),
  cel("01", "Ain", "84", "382a", "Cadres", 100), cel("01", "Ain", "84", "623a", "Ouvriers", 150), cel("01", "Ain", "84", "473b", "Prof. intermediaires", 50),
  cel("69", "Rhône", "84", "382a", "Cadres", 400), cel("69", "Rhône", "84", "473b", "Prof. intermediaires", 300), cel("69", "Rhône", "84", "623a", "Ouvriers", 150), cel("69", "Rhône", "84", "543a", "Employes", 50),
  cel("2A", "Corse-du-Sud", "94", "623a", "Ouvriers", 50),
  cel("64", "Pyrénées-Atlantiques", "75", "382a", "Cadres", 9, masque = TRUE), cel("64", "Pyrénées-Atlantiques", "75", "623a", "Ouvriers", 9, masque = TRUE))
PCS_REG_P <- dplyr::bind_rows(cel("", "", "75", "382a", "Cadres", 420), cel("", "", "75", "623a", "Ouvriers", 320), cel("", "", "75", "473b", "Prof. intermediaires", 250),
                              cel("", "", "84", "623a", "Ouvriers", 300), cel("", "", "84", "382a", "Cadres", 500), cel("", "", "84", "473b", "Prof. intermediaires", 350), cel("", "", "84", "543a", "Employes", 50),
                              cel("", "", "94", "623a", "Ouvriers", 50)) |> dplyr::select(-geo_code, -geo_nom)
PCS_FR_P <- dplyr::bind_rows(cel("", "", "", "382a", "Cadres", 920), cel("", "", "", "623a", "Ouvriers", 670), cel("", "", "", "473b", "Prof. intermediaires", 600), cel("", "", "", "543a", "Employes", 160),
                             cel("", "", "", "624d", "Ouvriers", 40), cel("", "", "", "386d", "Cadres", 30), cel("", "", "", "388a", "Cadres", 15), cel("", "", "", "676e", "Ouvriers", 15)) |> dplyr::select(-geo_code, -geo_nom, -region_code, -region_nom)
CS1_FR_P <- tibble::tibble(cs1 = c("Cadres", "Ouvriers", "Prof. intermediaires", "Employes"), effectif_champ = 1000, departs_central = c(965, 725, 600, 160), departs_bas = 1, departs_haut = 1, masque = FALSE)
LIB_P <- c("382a" = "Ingénieurs et cadres d’étude", "623a" = "Chaudronniers-tôliers qualifiés", "473b" = "Techniciens de fabrication")
PASSAGE_P <- utils::read.csv2(file.path(RACINE, "data", "passage_departement_region.csv"), colClasses = "character", fileEncoding = "UTF-8-BOM")   # région des départements absents de la table (SPEC-PUB-007)
preparer_p <- function(typo = TYPO_P, pcs_dep = PCS_DEP_P, top_n = 5, cs1 = CS1_FR_P, libelles = LIB_P, pcs_fr = PCS_FR_P, passage = PASSAGE_P) {
  tp <- EP$preparer_carte_typologie(typo, FOND_DEP_P)
  EP$preparer_cartes_typologie_pcs(tp, typo, pcs_dep, PCS_REG_P, pcs_fr, FOND_DEP_P, FOND_REG_P, libelles = libelles, cs1_fr = cs1, top_n = top_n, passage_regions = passage)
}
FUITES <- c("7654321", "7 654 321", "8000002", "8 000 002", "8000001", "8 000 001", "8000003", "7000001", "77,7", "SECRET-FUITE", "388a", "676e")   # valeurs masquées (33 : PCS 388a / 676e ; 64 ; 24 : 543a reste dans le top de 33)

test_that("SPEC-TYPO-080 — top des PCS : seules les cellules diffusables, classées par départs décroissants puis code, au plus N, libellé / catégorie / fourchette / taux / part, masquées comptées", {
  t <- EP$preparer_top_pcs(PCS_DEP_P[PCS_DEP_P$geo_code == "33", ], total = 1000, libelles = LIB_P, top_n = 5)
  expect_identical(t$statut, "ok"); expect_equal(t$n_masquees, 2L); expect_equal(t$n_pcs, 8L); expect_equal(nrow(t$top), 5L)
  expect_identical(t$top$pcs, c("382a", "473b", "623a", "543a", "624d"))                      # 250 ex aequo : 473b avant 623a (code) ; 386d (6e) hors top ; masquées jamais
  expect_identical(t$top$libelle[1:3], unname(LIB_P[c("382a", "473b", "623a")])); expect_true(is.na(t$top$libelle[4]))
  expect_identical(t$top$cs1[4], "Employes"); expect_equal(t$top$part_pct, c(30, 25, 25, 10, 4)); expect_equal(t$top$departs_bas[1], 299); expect_equal(t$top$taux_depart_central_pct[1], 10)
  expect_equal(nrow(EP$preparer_top_pcs(PCS_DEP_P[PCS_DEP_P$geo_code == "33", ], 1000, LIB_P, top_n = 3)$top), 3L)
  expect_true(all(is.na(EP$preparer_top_pcs(PCS_DEP_P[PCS_DEP_P$geo_code == "33", ], NA, LIB_P)$top$part_pct)))               # total inconnu = part n.d.
  expect_identical(EP$preparer_top_pcs(PCS_DEP_P[PCS_DEP_P$geo_code == "64", ], NA, LIB_P)$statut, "vide")                   # tout masqué (2 cellules) : rien à classer
  l <- nrm(EP$lignes_top_pcs(t)); expect_match(l[1], "^1\\. Ingénieurs et cadres d’étude : 300 départs \\(299 à 301\\) · 30,0 % des départs$")
  expect_match(l[4], "^4\\. PCS 543a · employés"); expect_identical(l[6], "2 PCS non diffusées (secret statistique)")
  # sans nomenclature : code + grande catégorie, jamais d'arrêt
  expect_true(all(is.na(EP$preparer_top_pcs(PCS_DEP_P[PCS_DEP_P$geo_code == "33", ], 1000, NULL)$top$libelle)))
})

test_that("SPEC-TYPO-081 — différenciation : exactement une cellule masquée = top retenu ; aucune valeur masquée dans la structure, le JSON, les pages ni les infobulles", {
  t <- EP$preparer_top_pcs(PCS_DEP_P[PCS_DEP_P$geo_code == "24", ], total = 200, libelles = LIB_P)
  expect_identical(t$statut, "retenu"); expect_null(t$top); expect_equal(t$n_masquees, 1L)
  expect_identical(EP$lignes_top_pcs(t), EP$LIBELLES_TYPO_PCS$top_retenu); expect_true(grepl("secret-note", EP$html_table_top(t)[1], fixed = TRUE))
  p <- preparer_p(); expect_equal(p$n_retenus, 1L); expect_identical(p$departements[["24"]]$statut, "retenu")
  expect_identical(p$departements[["64"]]$statut, "territoire_masque"); expect_null(p$departements[["64"]]$top)
  expect_output(EP$controler_cartes_typologie_pcs(p), "tops retenus \\(différenciation\\) 1")
  # PCS colorables : valeur numérique seulement pour une cellule diffusée, "m" pour une masquée, absente sinon
  v <- p$par_pcs[["382a"]]$valeurs; expect_equal(v[["33"]], 300); expect_identical(v[["64"]], "m"); expect_null(v[["75"]])
  expect_identical(p$par_pcs[["543a"]]$valeurs[["24"]], "m"); expect_false("388a" %in% names(p$par_pcs))             # une PCS masquée partout n'est dans aucun top : jamais colorable
  fi <- file.path(tempdir(), "typo_pcs.html"); fc <- file.path(tempdir(), "typo_pcs_courriel.html")
  EP$generer_cartes_typologie_pcs(p, FOND_DEP_P, FOND_REG_P, fi); EP$generer_cartes_typologie_pcs_courriel(p, FOND_DEP_P, FOND_REG_P, fc)
  for (page in list(lire_html(fi), lire_html(fc))) for (f in FUITES) expect_false(grepl(f, page, fixed = TRUE), label = f)
  j <- json_de(lire_html(fi)); expect_identical(j$departements[["64"]]$st, "masque"); expect_identical(unlist(j$departements[["64"]]$lignes), EP$LIBELLES_UI$secret)
  expect_identical(j$departements[["24"]]$top$statut, "retenu"); expect_length(j$departements[["24"]]$top$top, 0L)
  expect_identical(j$pcs[["382a"]]$v[["64"]], "m")
  expect_error(EP$preparer_cartes_typologie_pcs(EP$preparer_carte_typologie(TYPO_P, FOND_DEP_P), TYPO_P, PCS_DEP_P |> dplyr::select(-masque), PCS_REG_P, PCS_FR_P, FOND_DEP_P, FOND_REG_P), "masque")
  expect_error(preparer_p(pcs_dep = PCS_DEP_P |> dplyr::mutate(geo_code = replace(geo_code, 1, "999"))), "999")
})

test_that("SPEC-TYPO-082 — régions : profil le plus fréquent des départements, égalité, comptes et non diffusés (absents de la table compris), total régional (départements, sinon table régionale, sinon n.d.), top régional ; clic câblé", {
  p <- preparer_p(); r75 <- p$regions[["75"]]; r84 <- p$regions[["84"]]; r94 <- p$regions[["94"]]; r11 <- p$regions[["11"]]
  expect_true(is.na(r75$cle)); expect_true(r75$egalite); expect_equal(r75$n_departements, 12L); expect_equal(r75$n_non_diffuses, 10L)   # 1 enjeu, 1 stable, 1 masqué + 9 absents de la table (SPEC-PUB-007)
  expect_equal(r75$comptes$enjeu, 1L); expect_equal(r75$comptes$stable, 1L); expect_true(all(c("24", "33", "64") %in% r75$departements)); expect_length(r75$departements, 12L)
  expect_equal(r75$total, 990)                                                                                                         # départements non tous diffusés -> somme des cellules région x PCS (aucune masquée)
  pr_m <- PCS_REG_P; pr_m$masque[pr_m$region_code == "75"][1] <- TRUE
  expect_true(is.na(EP$preparer_cartes_typologie_pcs(EP$preparer_carte_typologie(TYPO_P, FOND_DEP_P), TYPO_P, PCS_DEP_P, pr_m, PCS_FR_P, FOND_DEP_P, FOND_REG_P, libelles = LIB_P, cs1_fr = CS1_FR_P, passage_regions = PASSAGE_P)$regions[["75"]]$total))   # une cellule régionale masquée -> n.d.
  expect_identical(EP$titre_region_typo(r75), "Profils à égalité"); expect_identical(EP$couleur_region_typo(r75), EP$COULEUR_EGALITE)
  expect_true(is.na(r84$cle)); expect_true(r84$egalite); expect_equal(r84$total, 1200)                                                # 1 stable, 1 enjeu ; total = somme publiée
  expect_identical(r94$cle, "expertiser"); expect_equal(r94$total, 50); expect_identical(EP$couleur_region_typo(r94), EP$COULEURS_PROFILS[["expertiser"]])
  expect_true(is.na(r11$cle)); expect_false(r11$egalite); expect_identical(EP$titre_region_typo(r11), EP$LIBELLES_TYPO_PCS$non_diffuse); expect_identical(EP$couleur_region_typo(r11), EP$COULEURS_CARTE$secret)
  expect_equal(r11$n_departements, 8L); expect_equal(r11$n_non_diffuses, 8L)                                                         # Île-de-France : 8 départements absents de la table = 8 non diffusés (SPEC-PUB-007)
  expect_identical(r84$top_pcs$top$pcs, c("382a", "473b", "623a", "543a")); expect_equal(r84$top_pcs$top$part_pct[1], 500 / 12)
  expect_equal(r75$top_pcs$top$part_pct[1], 100 * 420 / 990)                                                                           # parts sur le total régional publié (table régionale)
  expect_identical(r11$top_pcs$statut, "vide"); expect_equal(p$n_sans_region, 0L)
  expect_equal(preparer_p(passage = NULL)$n_sans_region, 101L - 6L)                                                                  # sans table de passage : absents non rattachés, comptés
  # région majoritaire franche : 2 enjeu + 1 stable
  typo2 <- TYPO_P; typo2$profil_typologie[typo2$geo_code == "64"] <- P_P[["enjeu"]]; typo2$masque[typo2$geo_code == "64"] <- FALSE
  r <- preparer_p(typo = typo2)$regions[["75"]]; expect_identical(r$cle, "enjeu"); expect_equal(r$n_non_diffuses, 9L)                 # les 9 départements absents de la table restent non diffusés
  # page : région colorée, comptes, clic région -> niveau département, départements hors région estompés
  fi <- file.path(tempdir(), "typo_pcs_reg.html"); EP$generer_cartes_typologie_pcs(p, FOND_DEP_P, FOND_REG_P, fi); html <- lire_html(fi)
  sans <- gsub("<script\\b.*?</script>", "", html, perl = TRUE)
  expect_true(grepl(sprintf('<path class="r" id="r-94" data-code="94" fill="%s"', EP$COULEURS_PROFILS[["expertiser"]]), sans, fixed = TRUE))
  expect_true(grepl(sprintf('<path class="r" id="r-75" data-code="75" fill="%s"', EP$COULEUR_EGALITE), sans, fixed = TRUE))
  expect_true(grepl(sprintf('<path class="r" id="r-11" data-code="11" fill="%s"', EP$COULEURS_CARTE$secret), sans, fixed = TRUE)); expect_false(grepl('class="[rt] sans"|stroke-dasharray', sans))
  expect_equal(lengths(regmatches(sans, gregexpr('<path class="r', sans))), 18L); expect_equal(lengths(regmatches(sans, gregexpr('<path class="t', sans))), 101L)
  leg <- regmatches(sans, regexpr('(?s)<ul class="typo-legende" id="legende">.*?</ul>', sans, perl = TRUE))
  expect_true(grepl('Profils à égalité<span class="n">2</span>', leg, fixed = TRUE)); expect_true(grepl(sprintf('%s<span class="n">1</span>', P_P[["expertiser"]]), leg, fixed = TRUE))
  expect_true(grepl('Non diffusé (secret statistique)<span class="n">15</span>', leg, fixed = TRUE)); expect_false(grepl("renseigné|absent|Aucun salarié", leg))
  j <- json_de(html); expect_identical(j$regions[["75"]]$titre, "Profils à égalité"); expect_equal(j$regions[["75"]]$n_non_diffuses, 10L); expect_equal(j$regions[["75"]]$comptes$enjeu, 1L)
  expect_match(j$regions[["75"]]$resume, "10 non diffusé"); expect_identical(j$departements[["33"]]$region, "75"); expect_equal(j$comptes$regions_egalite, 2L)
  js <- regmatches(html, regexpr("(?s)<script>\\s*\\(function\\(\\)\\{.*?</script>", html, perl = TRUE))
  for (s in c('brancher("path.r","region")', 'if(niveau==="region"){etat.region=code;etat.dep=null;etat.niveau="departement"}', 'classList.toggle("hors"', 'body.niveau-region #g-deps{display:none}'))
    expect_true(grepl(s, html, fixed = TRUE), label = s)
})

test_that("SPEC-TYPO-083 — département : profil, chiffres, top en barres, spécificité face à la France, coloration par PCS avec classes rondes et retour aux profils", {
  p <- preparer_p(); t33 <- p$departements[["33"]]
  expect_equal(p$total_france, 2450); expect_equal(t33$top$part_france_pct, 100 * c(920, 600, 670, 160, 40) / 2450)
  expect_identical(p$par_pcs[["382a"]]$libelle, LIB_P[["382a"]]); expect_identical(p$par_pcs[["382a"]]$cs1, "Cadres")
  # total France : repli sur la table PCS France si cs1 absente ou incomplète, elle-même seulement si entièrement diffusée ; sinon NA (parts et spécificité n.d.)
  cs1_m <- CS1_FR_P |> dplyr::mutate(masque = c(FALSE, TRUE, FALSE, FALSE)); pcs_fr_m <- PCS_FR_P |> dplyr::mutate(masque = c(rep(FALSE, 7), TRUE))
  expect_equal(preparer_p(cs1 = NULL)$total_france, 2450); expect_equal(preparer_p(cs1 = cs1_m)$total_france, 2450)
  p_nd <- preparer_p(cs1 = cs1_m, pcs_fr = pcs_fr_m); expect_true(is.na(p_nd$total_france)); expect_true(all(is.na(p_nd$departements[["33"]]$top$part_france_pct)))
  fi <- file.path(tempdir(), "typo_pcs_dep.html"); EP$generer_cartes_typologie_pcs(p, FOND_DEP_P, FOND_REG_P, fi); html <- lire_html(fi)
  j <- json_de(html); d <- j$departements[["33"]]
  expect_identical(d$cle, "enjeu"); expect_identical(d$couleur, EP$COULEURS_PROFILS[["enjeu"]]); expect_true(any(grepl("^1 000 départs estimés", nrm(unlist(d$lignes)))))
  expect_true(all(c("Poids dans l’emploi Base", "Départs attendus d’ici 2030", "Lecture") %in% d$blocs$t)); expect_true(grepl("rendreBlocs(DP,t);barresPyramide(t.pyramide,DP)", html, fixed = TRUE))   # SPEC-TYPO-083 : rubriques et puces
  expect_identical(d$top$top$pcs, c("382a", "473b", "623a", "543a", "624d")); expect_equal(d$top$top$c, c(300, 250, 250, 100, 40)); expect_equal(d$top$top$part, c(30, 25, 25, 10, 4))
  expect_equal(d$top$top$pf[1], round(100 * 920 / 2450, 2)); expect_equal(d$top$n_masquees, 2L)
  expect_identical(j$pcs[["382a"]]$lib, LIB_P[["382a"]]); expect_equal(j$pcs[["382a"]]$v[["33"]], 300); expect_equal(j$pcs[["382a"]]$v[["69"]], 400)
  # disposition SPEC-TYPO-065 : premier écran (en-tête compact, panneau à défilement interne, légende en bloc), retour carte, défilement au clic sur écran étroit
  expect_true(grepl('<div class="page page-ecran"><div class="ecran">', gsub("\n", "", html), fixed = TRUE)); expect_true(grepl('<details class="apropos">', html, fixed = TRUE))
  expect_true(grepl('<div class="legende-bloc"><div class="legend-title" id="legende-titre">', html, fixed = TRUE)); expect_true(grepl('<a class="retour-carte" href="#carte">', html, fixed = TRUE))
  expect_true(grepl('if(niveau!=="region"&&!window.matchMedia("(min-width:901px)").matches)DP.scrollIntoView(', html, fixed = TRUE))
  expect_identical(j$couleurs$classes, EP$COULEURS_CARTE$classes)
  js <- regmatches(html, regexpr("(?s)<script>\\s*\\(function\\(\\)\\{.*?</script>", html, perl = TRUE))
  for (s in c("function classesRondes(vals)", "function pasRond(x)", 'etat.pcs=(etat.pcs===r.pcs)?null:r.pcs', "function specificite(t,cont)", 'el("ul","top")', 'el("ul","spec")',
              'q.setAttribute("fill",D.couleurs.secret)', "L.retour_profils", "if(Date.now()-tScroll<150)return;", '.bulle{position:fixed;'))
    expect_true(grepl(s, html, fixed = TRUE), label = s)
  expect_true(grepl("Colorer la carte par cette PCS", html, fixed = TRUE)); expect_true(grepl("Revenir aux profils", html, fixed = TRUE))
  for (v in c("insee.fr", "http://", "https://")) expect_false(grepl(v, html, fixed = TRUE), label = v)      # page autonome
})

test_that("SPEC-TYPO-084 — version courriel : aucun contenu actif ni bloc de données, deux cartes à <title> natifs, blocs dépliables par région et département, coloration annoncée réservée", {
  p <- preparer_p(); fc <- file.path(tempdir(), "typo_pcs_courriel.html"); EP$generer_cartes_typologie_pcs_courriel(p, FOND_DEP_P, FOND_REG_P, fc); html <- lire_html(fc)
  expect_false(grepl("<script", html, fixed = TRUE)); expect_false(grepl(" on[a-z]+=", html)); expect_false(grepl("javascript:", html, fixed = TRUE)); expect_false(grepl('id="donnees"', html, fixed = TRUE))
  expect_equal(lengths(regmatches(html, gregexpr("<title>", html))), 1L + 18L + 101L)
  expect_true(grepl("<title>Gironde · 33\nPôle majeur à renouveler rapidement\n", html, fixed = TRUE))
  expect_true(grepl("1. Ingénieurs et cadres d’étude : 300 départs (299 à 301) · 30,0 % des départs\n2. Techniciens de fabrication : 250 départs", html, fixed = TRUE))
  expect_true(grepl("<title>Pyrénées-Atlantiques · 64\nNon diffusé (secret statistique)\nRésultat non diffusé — secret statistique</title>", html, fixed = TRUE))
  expect_true(grepl("<title>Dordogne · 24\n", html, fixed = TRUE)); expect_true(grepl(paste0("\n", EP$LIBELLES_TYPO_PCS$top_retenu, "</title>"), html, fixed = TRUE))
  expect_true(grepl("<title>Nouvelle-Aquitaine\nProfils à égalité\n12 département(s)\n", html, fixed = TRUE))
  expect_true(grepl('<details class="bloc-typo" open><summary>Le détail par région</summary>', html, fixed = TRUE)); expect_true(grepl('<summary>Le détail par département</summary>', html, fixed = TRUE))
  expect_equal(lengths(regmatches(html, gregexpr('<details class="terr">', html))), 18L + 101L)                 # 18 régions + tous les départements du fond (absents = non diffusés, SPEC-PUB-007)
  expect_true(grepl("Pyrénées-Atlantiques · 64 — Non diffusé (secret statistique)</summary>", html, fixed = TRUE))
  expect_true(grepl('<th>Part des départs en France</th>', html, fixed = TRUE)); expect_true(grepl("réservée à la page interactive", html, fixed = TRUE))
  expect_true(grepl('<path class="r" data-code="94" fill="', html, fixed = TRUE)); expect_true(grepl("path:hover", html, fixed = TRUE))
  # contenu actif injecté = échappé, jamais actif
  p2 <- p; p2$territoires$nom[p2$territoires$code == "33"] <- "Gironde <script>alert(1)</script>"
  f2 <- file.path(tempdir(), "typo_pcs_courriel2.html"); EP$generer_cartes_typologie_pcs_courriel(p2, FOND_DEP_P, FOND_REG_P, f2); expect_false(grepl("<script", lire_html(f2), fixed = TRUE))
})

test_that("SPEC-TYPO-085 — 08h sur la chaîne de test en mode département : deux pages, objets amont inchangés ; lancement seul depuis la racine ; table absente = message ; désactivé = message", {
  env <- new.env(); old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), "typo_pcs_chaine"), envir = env); dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(c("00c_fonctions_geo.R", "00g_format_restitution.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R", "00f_fonctions_cartes.R",
                    "00h_fonctions_typologie.R", "00i_fonctions_carte_typologie.R", "00j_fonctions_cartes_typologie_pcs.R",
                    "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
                    "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R", "08b_departs_geo_cs.R",
                    "08c_departs_pcs.R", "08d_departs_cs1.R", "08f_typologie_departements.R"), env)
  avant <- lapply(c("departs_pcs", "departs_cs1", "typologie_departements", "bts_projete"), function(o) get(o, envir = env))
  expect_output(sys.source(chemin_script("08h_cartes_typologie_pcs.R"), envir = env), "08h — cartes profils et métiers")
  expect_identical(avant, lapply(c("departs_pcs", "departs_cs1", "typologie_departements", "bts_projete"), function(o) get(o, envir = env)))   # rien n'est touché en amont
  b <- env$cartes_typologie_pcs; expect_identical(b$statut, "ok")
  dir_c <- file.path(env$DIR_SORTIES, "typologie_territoriale", "cartes")
  for (f in c("carte_typologie_pcs.html", "carte_typologie_pcs_courriel.html")) expect_true(file.exists(file.path(dir_c, f)), label = f)
  html <- lire_html(file.path(dir_c, "carte_typologie_pcs.html")); courriel <- lire_html(file.path(dir_c, "carte_typologie_pcs_courriel.html"))
  expect_false(grepl("<script", courriel, fixed = TRUE)); expect_true(grepl(sprintf("Salariés de %d ans ou plus", env$AGE_MIN_BTS), html, fixed = TRUE))
  # cohérence avec les tables de diffusion : chaque top n'a que des cellules diffusées, au plus TYPO_PCS_TOP_N, et aucune valeur masquée du CSV dans les pages
  j <- json_de(html); dif <- env$departs_pcs$diffusion$departement
  for (code in names(j$departements)) { t <- j$departements[[code]]$top; if (length(t$top) == 0) next
    expect_lte(nrow(t$top), env$TYPO_PCS_TOP_N)
    for (i in seq_len(nrow(t$top))) { ligne <- dif[dif$geo_code == code & dif$pcs == t$top$pcs[i], ]; expect_equal(nrow(ligne), 1L); expect_false(ligne$masque); expect_equal(env$arrondir_nombre_personnes(ligne$departs_central), t$top$c[i]) } }
  interne <- env$departs_pcs$analytique$departement; masquees <- dplyr::anti_join(interne, dif[!dif$masque, c("geo_code", "pcs")], by = c("geo_code", "pcs"))
  if (nrow(masquees) > 0) for (v in unique(masquees$departs_central[masquees$departs_central >= 1000])) expect_false(grepl(nrm(env$fmt_fr_carte(v)), html, fixed = TRUE))
  # lancement SEUL (processus neuf, sans la chaîne) depuis la racine : mêmes fichiers
  unlink(dir_c, recursive = TRUE)
  log <- file.path(tempdir(), "08h_seul.log")
  statut <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(sprintf('DIR_SORTIES <- "%s"; source("R/08_territoires/08h_cartes_typologie_pcs.R")', env$DIR_SORTIES))), stdout = log, stderr = log)
  expect_equal(statut, 0L, label = paste(tail(readLines(log, warn = FALSE), 5), collapse = "\n"))
  expect_true(file.exists(file.path(dir_c, "carte_typologie_pcs_courriel.html")))
  # table de diffusion absente : message avec le chemin, aucun arrêt
  env2 <- new.env(); sys.source(chemin_script("00_config.R"), envir = env2)
  assign("DIR_SORTIES", file.path(tempdir(), "typo_pcs_vide"), envir = env2); dir.create(env2$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(c("00g_format_restitution.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R", "00i_fonctions_carte_typologie.R", "00j_fonctions_cartes_typologie_pcs.R"), env2)
  expect_message(sys.source(chemin_script("08h_cartes_typologie_pcs.R"), envir = env2), "departs_pcs_departement.csv")
  expect_identical(env2$cartes_typologie_pcs$statut, "source absente")
  # désactivé : message, rien
  env3 <- new.env(); sys.source(chemin_script("00_config.R"), envir = env3); assign("GENERER_CARTES_TYPOLOGIE_PCS", FALSE, envir = env3)
  sourcer_scripts(c("00g_format_restitution.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R", "00i_fonctions_carte_typologie.R", "00j_fonctions_cartes_typologie_pcs.R"), env3)
  expect_message(sys.source(chemin_script("08h_cartes_typologie_pcs.R"), envir = env3), "désactivées")
})
