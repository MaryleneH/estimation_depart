# ==============================================================================
# Cartes de la note sur la typologie (R/09_analyse/09c) — specs/11, SPEC-ANA-009.
# Typologie SYNTHÉTIQUE sur le fond réel (codes publics) ; la chaîne est couverte dans
# test-exposition-tension.R (bloc « 08i sur la chaîne »).
# ==============================================================================
EC <- new.env()
.old_wd <- setwd(RACINE); source(chemin_script("00_config.R"), local = EC)
assign("DIR_DATA", file.path(RACINE, "data"), envir = EC); assign("DIR_SORTIES", file.path(tempdir(), "cartes_note_vide"), envir = EC)
for (f in c("00g_format_restitution.R", "00c_fonctions_geo.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R")) sys.source(chemin_script(f), envir = EC)
MSG_C <- character(0); withCallingHandlers(sys.source(chemin_script("09c_cartes_typologie_note.R"), envir = EC),   # sans typologie : message, aucun arrêt
                    message = function(m) { MSG_C <<- c(MSG_C, conditionMessage(m)); invokeRestart("muffleMessage") })
setwd(.old_wd)
FOND_C <- suppressMessages(EC$charger_fond_carte("departement")); FOND_R <- suppressMessages(EC$charger_fond_carte("region"))
codes_c <- names(FOND_C); set.seed(5)
profils_c <- unname(EC$PROFILS_TYPOLOGIE)
TYPO_C <- tibble::tibble(geo_code = codes_c[-1], profil_typologie = sample(profils_c, length(codes_c) - 1, TRUE),
                         classe_intensite = sample(c("Faible", "Modérée", "Élevée"), length(codes_c) - 1, TRUE), masque = FALSE)
TYPO_C$masque[1:3] <- TRUE; TYPO_C$profil_typologie[1:3] <- NA; TYPO_C$classe_intensite[1:3] <- NA         # 3 masqués, 1 absent (codes_c[1])
TYPO_C$profil_typologie[4] <- "Non diffusé (secret statistique)"; TYPO_C$classe_intensite[4] <- "Élevée"        # diffusé, profil retenu (cellule CS masquée) : intensité conservée

test_that("SPEC-ANA-009 — lancement seul sans typologie : message, aucun arrêt ; préparation : masqué ou absent = non diffusé, profil inconnu = arrêt, table sans masque = arrêt", {
  expect_true(any(grepl("typologie de diffusion absente", MSG_C)))
  d <- EC$preparer_cartes_note(TYPO_C, FOND_C)
  expect_equal(nrow(d), length(codes_c)); expect_identical(d$cle[d$code == codes_c[1]], "non_diffuse"); expect_identical(d$intensite[d$code == codes_c[1]], "non diffusé")
  expect_true(all(d$cle[d$code %in% codes_c[2:5]] == "non_diffuse")); expect_equal(sum(d$cle == "non_diffuse"), 5L)
  expect_identical(d$intensite[d$code == codes_c[5]], "Élevée"); expect_true(all(d$intensite[d$code %in% codes_c[1:4]] == "non diffusé"))   # profil retenu : intensité gardée ; masqués et absent : non diffusé
  expect_true(all(d$cle[d$cle != "non_diffuse"] %in% names(EC$PROFILS_TYPOLOGIE))); expect_true(all(d$intensite %in% names(EC$PALETTE_INTENSITE_NOTE)))
  expect_error(EC$preparer_cartes_note(TYPO_C |> dplyr::select(-masque), FOND_C), "masque")
  x <- TYPO_C; x$profil_typologie[6] <- "Profil inventé"; expect_error(EC$preparer_cartes_note(x, FOND_C), "profil inconnu")
})

test_that("SPEC-ANA-009 — deux PNG produits (profils, intensité), fond blanc, sans valeur chiffrée ; relançable avec un fond région absent", {
  skip_if_not_installed("ggplot2")
  d <- file.path(tempdir(), "cartes_note"); unlink(d, recursive = TRUE)
  f <- EC$cartes_note_typologie(TYPO_C, FOND_C, FOND_R, d, EC$construire_libelle_champ(), "source fictive", dpi = 50)
  expect_identical(unname(f), file.path(d, c("carte_profils_note.png", "carte_intensite_note.png")))
  expect_true(all(file.exists(f))); expect_true(all(file.size(f) > 5000))
  f2 <- EC$cartes_note_typologie(TYPO_C, FOND_C, NULL, file.path(d, "sans_region"), EC$construire_libelle_champ(), NULL, dpi = 40); expect_true(all(file.exists(f2)))
})
