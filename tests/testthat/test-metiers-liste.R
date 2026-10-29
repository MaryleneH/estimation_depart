# ==============================================================================
# Métiers de la liste, territoire par territoire (00l + 08j) — specs/10 §13, SPEC-TYPO-109.
# Données exclusivement SYNTHÉTIQUES (tables de DIFFUSION fabriquées) ; la chaîne est
# couverte dans test-exposition-tension.R (bloc « 08i sur la chaîne »).
# ==============================================================================
EL <- new.env()
.old_wd <- setwd(RACINE); source(chemin_script("00_config.R"), local = EL); setwd(.old_wd)
assign("DIR_DATA", file.path(RACINE, "data"), envir = EL); assign("TENSION_N_METIERS_SUIVIS", 3L, envir = EL); assign("TENSION_ECART_POINTS", 3, envir = EL)
for (f in c("00g_format_restitution.R", "00c_fonctions_geo.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R", "00f_fonctions_cartes.R",
            "00k_fonctions_tension.R", "00l_fonctions_metiers_liste.R")) sys.source(chemin_script(f), envir = EL)
PAR_L <- EL$parametres_tension()
METIERS_L <- tibble::tibble(pcs = c("1a1a", "2b2b", "3c3c"), pcs_source = c("1A1A", "2B2B", "3C3C"), pcs_libelle = c("Métier un", "Métier deux", NA),
                            fap_code = c("F1", "F2", "F3"), fap_libelle = c("Famille un", "Famille deux", "Famille trois"), en_tension = c(TRUE, FALSE, NA),
                            levier = c("former tôt", "anticiper", NA), present_base = c(TRUE, TRUE, TRUE))
cel_l <- function(code, nom, reg, pcs, eff, dep, taux, masque = FALSE) tibble::tibble(
  region_code = reg, region_nom = paste("Région", reg), geo_code = code, geo_nom = nom, pcs = pcs, cs1 = "Cadres",
  effectif_champ = if (masque) NA_integer_ else eff, departs_central = if (masque) NA_real_ else dep, departs_bas = if (masque) NA_real_ else dep - 1, departs_haut = if (masque) NA_real_ else dep + 1,
  taux_depart_central_pct = if (masque) NA_real_ else taux, masque = masque, motif_masque = if (masque) "primaire" else NA_character_)
DEP_L <- dplyr::bind_rows(cel_l("33", "Gironde", "75", "1A1A", 40, 16, 40.0), cel_l("64", "Pyrénées-Atlantiques", "75", "1A1A", 30, 9, 30.0), cel_l("01", "Ain", "84", "1A1A", 50, 18.5, 37.0),
                          cel_l("2A", "Corse-du-Sud", "94", "1A1A", 3, 1, 33.3, masque = TRUE), cel_l("inconnu", "Territoire inconnu", "inconnu", "1A1A", 10, 4, 40),
                          cel_l("33", "Gironde", "75", "2B2B", 20, 8, 40.0), cel_l("64", "Pyrénées-Atlantiques", "75", "2B2B", 20, 6, 30.0), cel_l("01", "Ain", "84", "2B2B", 4, 1, 25, masque = TRUE), cel_l("2A", "Corse-du-Sud", "94", "2B2B", 20, 7, 35.0),
                          cel_l("33", "Gironde", "75", "3C3C", 10, 5, 50.0), cel_l("64", "Pyrénées-Atlantiques", "75", "3C3C", 10, 3, 30.0), cel_l("01", "Ain", "84", "3C3C", 10, 4, 40.0), cel_l("2A", "Corse-du-Sud", "94", "3C3C", 10, 3, 30.0))
REG_L <- dplyr::bind_rows(cel_l("", "", "75", "1A1A", 70, 25, 35.7), cel_l("", "", "84", "1A1A", 50, 18.5, 37.0), cel_l("", "", "94", "1A1A", 3, 1, 33.3, masque = TRUE),
                          cel_l("", "", "75", "2B2B", 40, 14, 35.0), cel_l("", "", "84", "2B2B", 4, 1, 25, masque = TRUE), cel_l("", "", "94", "2B2B", 20, 7, 35.0),
                          cel_l("", "", "75", "3C3C", 20, 8, 40.0), cel_l("", "", "84", "3C3C", 10, 4, 40.0), cel_l("", "", "94", "3C3C", 10, 3, 30.0)) |> dplyr::select(-geo_code, -geo_nom)
FR_L <- dplyr::bind_rows(cel_l("", "", "", "1A1A", 123, 44.5, 36.2), cel_l("", "", "", "2B2B", 64, 22, 34.0), cel_l("", "", "", "3C3C", 40, 15, 37.5, masque = TRUE)) |> dplyr::select(-geo_code, -geo_nom, -region_code, -region_nom)
TAB_L <- EL$table_metiers_territoires(METIERS_L, DEP_L, REG_L, FR_L, PAR_L)

test_that("SPEC-TYPO-109 — table métier x territoire : grille régions + départements, « inconnu » écarté, valeurs du 08c, non diffusés sans valeur, repère France, écart et lecture au seuil", {
  expect_equal(nrow(TAB_L), 3L * (3L + 4L)); expect_false(any(TAB_L$geo_code == "inconnu")); expect_identical(unique(TAB_L$niveau), c("région", "département"))
  r <- TAB_L[TAB_L$pcs == "1A1A" & TAB_L$geo_code == "33" & TAB_L$niveau == "département", ]
  expect_true(r$diffuse); expect_equal(r$taux_pct, 40); expect_equal(r$taux_france_pct, 36.2); expect_equal(r$ecart_pts, 3.8, tolerance = 1e-9); expect_identical(r$lecture, "plus forte qu’en France")
  expect_equal(r$effectif_france, 123L); expect_equal(r$departs_france, 44.5); expect_identical(r$pcs_libelle, "Métier un"); expect_true(r$en_tension)
  r64 <- TAB_L[TAB_L$pcs == "1A1A" & TAB_L$geo_code == "64" & TAB_L$niveau == "département", ]; expect_identical(r64$lecture, "plus faible qu’en France"); expect_equal(r64$ecart_pts, -6.2, tolerance = 1e-9)
  r01 <- TAB_L[TAB_L$pcs == "1A1A" & TAB_L$geo_code == "01" & TAB_L$niveau == "département", ]; expect_identical(r01$lecture, "proche de la France")   # +0,8 pt
  nd <- TAB_L[TAB_L$pcs == "1A1A" & TAB_L$geo_code == "2A" & TAB_L$niveau == "département", ]
  expect_false(nd$diffuse); expect_true(all(is.na(c(nd$effectif_champ, nd$departs_central, nd$departs_bas, nd$departs_haut, nd$taux_pct, nd$ecart_pts)))); expect_identical(nd$lecture, "non diffusé (secret statistique)")
  expect_equal(nd$taux_france_pct, 36.2)                                                                      # le repère France reste lisible
  fr_nd <- TAB_L[TAB_L$pcs == "3C3C" & TAB_L$diffuse, ]                                                       # France masquée : aucun écart, lecture dédiée, valeurs du territoire conservées
  expect_true(all(is.na(fr_nd$taux_france_pct)) && all(is.na(fr_nd$ecart_pts)) && all(fr_nd$lecture == "repère France non diffusé") && all(!is.na(fr_nd$taux_pct)))
  expect_true(all(is.na(c(fr_nd$effectif_france, fr_nd$departs_france))))
  reg <- TAB_L[TAB_L$pcs == "1A1A" & TAB_L$niveau == "région", ]; expect_identical(reg$geo_code, c("84", "75", "94"))   # part décroissante, non diffusé en fin
  expect_identical(reg$geo_nom[1], "Région 84"); expect_false(reg$diffuse[3])
  # ordre : liste, puis régions avant départements, puis part décroissante puis code
  expect_identical(unique(TAB_L$pcs), c("1A1A", "2B2B", "3C3C")); dep1 <- TAB_L[TAB_L$pcs == "1A1A" & TAB_L$niveau == "département", ]; expect_identical(dep1$geo_code, c("33", "01", "64", "2A"))
  # seuil : exactement au seuil = « plus forte » ; juste en deçà = « proche » ; seuil invalide = arrêt ; région sans table = départements seuls
  expect_identical(EL$lecture_ecart_liste(c(3, 2.99, -3, -2.99, NA), 3), c("plus forte qu’en France", "proche de la France", "plus faible qu’en France", "proche de la France", NA))
  expect_error(EL$lecture_ecart_liste(1, 0), "seuil"); expect_equal(nrow(EL$table_metiers_territoires(METIERS_L, DEP_L, NULL, FR_L, PAR_L)), 12L)
  expect_error(EL$table_metiers_territoires(METIERS_L, DEP_L |> dplyr::select(-masque), REG_L, FR_L, PAR_L), "masque")
  expect_error(EL$table_metiers_territoires(METIERS_L, DEP_L, REG_L, FR_L |> dplyr::select(-masque), PAR_L), "masque")
})

test_that("SPEC-TYPO-109 — libellé public de la liste : texte_liste() substitue pluriel, singulier et « PCS » ; identité quand le paramètre vaut « métiers stratégiques »", {
  expect_identical(EL$lib_liste(), "métiers de la première sous-liste")
  expect_identical(EL$texte_liste(c("Parmi les 9 métiers stratégiques suivis, aucun", "un métier stratégique diffusable", "PCS stratégiques de la liste", "les métiers stratégiques de la liste")),
                   c("Parmi les 9 métiers de la première sous-liste, aucun", "un métier de la première sous-liste diffusable", "PCS de la première sous-liste de la liste", "les métiers de la première sous-liste"))
  expect_identical(EL$texte_liste(NULL), NULL); expect_identical(EL$texte_liste(1:3), 1:3)
  assign("TENSION_LIBELLE_LISTE", "métiers stratégiques", envir = EL); on.exit(assign("TENSION_LIBELLE_LISTE", "métiers de la première sous-liste", envir = EL), add = TRUE)
  expect_identical(EL$texte_liste("les métiers stratégiques suivis et un métier stratégique"), "les métiers stratégiques suivis et un métier stratégique")   # identité
  expect_identical(EL$lib_liste(), "métiers stratégiques")
  assign("TENSION_LIBELLE_LISTE", "métiers de la première sous-liste", envir = EL)
  # la phrase par département et les messages de la fiche passent par le libellé
  d <- tibble::tibble(geo_code = "33", geo_nom = "Gironde", exposition_pct = 60, exposition_france_pct = 40, couverture_pct = 70, classe_exposition = "Forte", robustesse = "robuste",
                      causes_fragilite = NA_character_, levier = "anticiper les recrutements", composante_levier = "forte intensité d’embauche", effectif_min_ok = TRUE)
  txt <- tryCatch(EL$rediger_exposition_departement(d, PAR_L), error = function(e) NA_character_)
  if (!is.na(txt)) { expect_false(grepl("stratégique", txt)); expect_true(grepl("métiers de la première sous-liste", txt, fixed = TRUE)) }
})

test_that("SPEC-TYPO-109 — page des métiers : sans script, un bloc par métier dans l'ordre de la liste, régions puis départements, non diffusés sur une ligne sans valeur, France masquée dite, vue par territoire, libellé public", {
  f <- file.path(tempdir(), "fiche_metiers_liste.html")
  EL$generer_fiche_metiers_liste(TAB_L, METIERS_L, f, EL$construire_libelle_champ(), PAR_L, "source fictive")
  h <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("<script", h, fixed = TRUE)); expect_false(grepl(" on[a-z]+=", h)); expect_false(grepl("stratégique", h))
  expect_true(grepl("Métiers de la première sous-liste : où les départs sont-ils les plus forts ?", h, fixed = TRUE)); expect_true(grepl("les 3 métiers de la première sous-liste", h, fixed = TRUE))
  secs <- regmatches(h, gregexpr('<section class="metier" id="m-[^"]+">', h))[[1]]; expect_identical(secs, sprintf('<section class="metier" id="m-%s">', c("1A1A", "2B2B", "3C3C")))
  expect_true(grepl("<h2>Métier un <small>PCS 1A1A</small></h2>", h, fixed = TRUE)); expect_true(grepl("<h2>PCS 3C3C <small>PCS 3C3C</small></h2>", h, fixed = TRUE))   # libellé absent -> code
  expect_true(grepl("Famille professionnelle : Famille un (FAP F1) · En tension en France : <b>oui</b> · Levier : former tôt", h, fixed = TRUE))
  expect_true(grepl("En tension en France : <b>inconnue</b>", h, fixed = TRUE)); expect_false(grepl("En tension en France : <b>non</b> · Levier", h, fixed = TRUE))   # levier seulement en tension
  expect_true(grepl("France : 123 salariés du métier · 45 départs attendus d’ici 2030 (44 à 46) · <b>36,2 %</b> susceptibles de partir.", h, fixed = TRUE))
  expect_true(grepl("France : non diffusé (secret statistique).", h, fixed = TRUE))
  s1 <- regmatches(h, regexpr('(?s)<section class="metier" id="m-1A1A">.*?</section>', h, perl = TRUE))
  expect_true(grepl('<tr class="grp"><th colspan="7">Régions</th></tr>', s1, fixed = TRUE)); expect_true(grepl('<tr class="grp"><th colspan="7">Départements</th></tr>', s1, fixed = TRUE))
  expect_true(grepl("Non diffusés (secret statistique), 1 : Corse-du-Sud (2A)", s1, fixed = TRUE)); expect_true(grepl("<td>Gironde (33)</td><td class=\"num\">40</td>", s1, fixed = TRUE))
  expect_true(grepl("+3,8 pts</td><td>plus forte qu’en France</td>", s1, fixed = TRUE)); expect_true(grepl("−6,2 pts</td><td>plus faible qu’en France</td>", s1, fixed = TRUE))
  expect_false(grepl("33,3", s1, fixed = TRUE)); expect_false(grepl("Corse-du-Sud (2A)</td><td class=\"num\">3<", s1, fixed = TRUE))   # aucune valeur masquée
  expect_equal(lengths(regmatches(h, gregexpr('<details class="terr">', h))), 7L)
  t33 <- regmatches(h, regexpr('(?s)<details class="terr"><summary>Gironde \\(33\\).*?</details>', h, perl = TRUE))
  expect_true(grepl("3 métiers diffusables sur 3", t33, fixed = TRUE)); expect_true(grepl("Métier un<br><small>PCS 1A1A · Famille un</small>", t33, fixed = TRUE))
  t2a <- regmatches(h, regexpr('(?s)<details class="terr"><summary>Corse-du-Sud \\(2A\\).*?</details>', h, perl = TRUE))
  expect_true(grepl("2 métiers diffusables sur 3", t2a, fixed = TRUE)); expect_true(grepl("Métiers non diffusés (secret statistique), 1 : Métier un", t2a, fixed = TRUE))
  expect_true(grepl("à partir de 3,0 points d’écart", h, fixed = TRUE)); expect_true(grepl('href="fiche_exposition_tension.html"', h, fixed = TRUE))
})

test_that("SPEC-TYPO-109 — classeur : quatre feuilles, table longue = table, vue par territoire triée par territoire puis écart, non diffusés sans valeur, libellé public, relecture writexl", {
  tb <- EL$tables_classeur_metiers_liste(TAB_L, PAR_L, EL$construire_libelle_champ(), "source fictive", as.Date("2030-01-01"))
  expect_identical(names(tb), c("Lisez-moi", "Métiers x territoires", "Par territoire", "Paramètres"))
  long <- tb[["Métiers x territoires"]]; expect_equal(nrow(long), nrow(TAB_L))
  expect_identical(names(long)[1:4], c("Métier", "Code PCS", "Famille professionnelle", "Code FAP")); expect_true("Écart à la France (points)" %in% names(long))
  expect_identical(long$`Métier`[long$`Code PCS` == "3C3C"][1], "PCS 3C3C"); expect_true(all(is.na(long$`Part susceptible de partir (%)`[long$`Diffusé` != "oui"])))
  g <- long[long$`Code PCS` == "1A1A" & long$Niveau == "département" & long$Code == "33", ]; expect_equal(g$`Part susceptible de partir (%)`, 40); expect_equal(g$`Écart à la France (points)`, 3.8); expect_identical(g$Lecture, "plus forte qu’en France")
  pt <- tb[["Par territoire"]]; expect_identical(names(pt)[1:4], c("Niveau", "Code", "Territoire", "Diffusé")); expect_equal(nrow(pt), nrow(TAB_L))
  expect_identical(pt$Niveau[1], "région"); d33 <- pt[pt$Niveau == "département" & pt$Code == "33", ]; expect_identical(d33$`Code PCS`, c("2B2B", "1A1A", "3C3C"))   # écart décroissant (+6,0 ; +3,8), France masquée (NA) en fin
  expect_false(any(grepl("stratégique", c(names(long), tb[["Lisez-moi"]]$texte)))); expect_true(any(grepl("étiers de la première sous-liste", tb[["Lisez-moi"]]$texte, fixed = TRUE)))
  expect_identical(tb[["Paramètres"]]$valeur[tb[["Paramètres"]]$parametre == "ecart_points"], "3")
  fx <- file.path(tempdir(), "metiers_liste.xlsx"); expect_message(EL$ecrire_classeur_tension(tb, fx, moteur = "writexl"), "sans mise en forme")
  expect_identical(readxl::excel_sheets(fx), names(tb)); xl <- readxl::read_excel(fx, sheet = "Par territoire"); expect_equal(nrow(xl), nrow(TAB_L))
  expect_error(EL$tables_classeur_metiers_liste(TAB_L |> dplyr::mutate(taux_pct = ifelse(!diffuse, 1, taux_pct)), PAR_L), "non diffusée")
})
