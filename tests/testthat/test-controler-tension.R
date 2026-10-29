# ==============================================================================
# 99d_controler_tension_cartes.R — cohérence hachures / cumul / fiche / horodatages
# (SPEC-TYPO-109). Fichiers de diffusion synthétiques ; aucun calcul ici.
# ==============================================================================
EC <- new.env(); assign("CONTROLE_TENSION_SANS_AFFICHAGE", TRUE, envir = EC)
sys.source(chemin_script("99d_controler_tension_cartes.R"), envir = EC)
jeu_99d <- function(dir, cumul = c(TRUE, FALSE, FALSE, FALSE), n_fiche = 1L, hach = c("A", "C"), fiche = TRUE, carte = TRUE) {
  unlink(dir, recursive = TRUE); dd <- file.path(dir, "typologie_territoriale", "diffusion"); dc <- file.path(dir, "typologie_territoriale", "cartes")
  dir.create(dd, recursive = TRUE, showWarnings = FALSE); dir.create(dc, recursive = TRUE, showWarnings = FALSE)
  typo <- data.frame(geo_code = c("A", "B", "C", "D"), masque = c(FALSE, FALSE, FALSE, TRUE),
                     profil_typologie = c("Pôle majeur à renouveler rapidement", "Renouvellement rapide à surveiller", "Implantation stable", NA), stringsAsFactors = FALSE)
  expo <- data.frame(geo_code = c("A", "B", "C", "D"), tension_diffusee = c(TRUE, TRUE, TRUE, FALSE), tension_classe = c("Forte", "Faible", "Forte", NA), tension_cumul = c(cumul[1:3], NA), stringsAsFactors = FALSE)
  utils::write.csv2(typo, file.path(dd, "typologie_departements.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv2(expo, file.path(dd, "exposition_tension_departements.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (fiche) writeLines(enc2utf8(c("<html><body>", sprintf('<div class="msg"><b>Renouvellement rapide et métiers en tension</b>%s</div>',
    if (n_fiche == 0) "Parmi les 9 métiers stratégiques suivis, aucun département ne cumule un renouvellement rapide d’ici 2030 et une forte part de remplacements difficiles."
    else sprintf("%d département%s cumule%s un renouvellement rapide d’ici 2030 et une forte part de remplacements difficiles parmi les 9 métiers stratégiques suivis.", n_fiche, if (n_fiche > 1) "s" else "", if (n_fiche > 1) "nt" else "")),
    "</body></html>")), file.path(dd, "fiche_exposition_tension.html"), useBytes = TRUE)
  if (carte) writeLines(c("<svg>", sprintf('<path class="h" data-code="%s" d="M0 0"/>', hach), "</svg>"), file.path(dc, "carte_typologie.html"))
  dir
}
test_that("SPEC-TYPO-109 — contrôle : cohérent = aucun écart ; cumul faux, hachure en trop, fiche périmée, carte absente et fichiers absents sont signalés ; le NA d'un non diffusé n'est jamais une ligne fantôme", {
  dir <- jeu_99d(file.path(tempdir(), "ctrl_ok"))
  r <- NULL; txt <- paste(capture.output(r <- EC$controler_tension_cartes(dir)), collapse = "\n")
  expect_identical(r$statut, "ok"); expect_setequal(r$forte, c("A", "C")); expect_setequal(r$rapide, c("A", "B")); expect_identical(r$cumul, "A"); expect_identical(r$attendu_cumul, "A")
  expect_true(grepl("hachures attendues) : 2 — A, C", txt, fixed = TRUE)); expect_true(grepl("Aucun écart", txt, fixed = TRUE)); expect_false(grepl("NA", txt, fixed = TRUE))
  # cumul incohérent (B marqué alors qu'il n'est pas Forte)
  r2 <- NULL; invisible(capture.output(r2 <- EC$controler_tension_cartes(jeu_99d(file.path(tempdir(), "ctrl_cumul"), cumul = c(TRUE, TRUE, FALSE, FALSE), n_fiche = 2L)))); expect_identical(r2$statut, "écarts")
  expect_true(any(grepl("tension_cumul (A, B) différent de l'intersection attendue (A)", r2$ecarts, fixed = TRUE)))
  # hachure en trop sur la carte (B n'est pas Forte)
  r3 <- NULL; invisible(capture.output(r3 <- EC$controler_tension_cartes(jeu_99d(file.path(tempdir(), "ctrl_hach"), hach = c("A", "B", "C"))))); expect_true(any(grepl("hachures de la carte (A, B, C) différentes des Forte diffusés (A, C) : relancer 08g", r3$ecarts, fixed = TRUE)))
  # fiche périmée (compte 0 alors que le CSV en a 1) ; message « aucun » lu comme 0
  r4 <- NULL; invisible(capture.output(r4 <- EC$controler_tension_cartes(jeu_99d(file.path(tempdir(), "ctrl_fiche"), n_fiche = 0L)))); expect_true(any(grepl("la fiche compte 0 département(s) en cumul, le CSV 1 : fiche périmée, relancer 08i", r4$ecarts, fixed = TRUE)))
  # carte et fiche absentes : signalées, pas d'arrêt, horodatage « absent »
  txt5 <- paste(capture.output(r5 <- EC$controler_tension_cartes(jeu_99d(file.path(tempdir(), "ctrl_sans"), fiche = FALSE, carte = FALSE))), collapse = "\n")
  expect_identical(r5$statut, "ok"); expect_true(grepl("carte_typologie.html absente", txt5, fixed = TRUE)); expect_true(grepl("fiche_exposition_tension.html absente", txt5, fixed = TRUE)); expect_true(grepl("carte        absent", txt5, fixed = TRUE))
  # fichiers de diffusion absents : statut dédié, marche à suivre
  txt6 <- paste(capture.output(r6 <- EC$controler_tension_cartes(file.path(tempdir(), "ctrl_rien"))), collapse = "\n")
  expect_identical(r6$statut, "fichiers absents"); expect_true(grepl("Marche à suivre", txt6, fixed = TRUE))
})
test_that("SPEC-TYPO-109 — horodatage : une fiche plus ancienne que le CSV d'exposition est un écart qui nomme la relance", {
  dir <- jeu_99d(file.path(tempdir(), "ctrl_horo")); f <- file.path(dir, "typologie_territoriale", "diffusion", "fiche_exposition_tension.html")
  Sys.setFileTime(f, Sys.time() - 3600); r <- NULL; invisible(capture.output(r <- EC$controler_tension_cartes(dir)))
  expect_identical(r$statut, "écarts"); expect_true(any(grepl("fiche plus ancien(ne) que exposition_tension_departements.csv : relancer 08i", r$ecarts, fixed = TRUE)))
})
test_that("SPEC-TYPO-109 — session R vierge : source() depuis la racine, sans main.R ni objet de la chaîne, lit DIR_SORTIES ; code de sortie 0 même sans fichier", {
  dir <- jeu_99d(file.path(tempdir(), "ctrl_session")); log <- file.path(tempdir(), "ctrl_session.log"); old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  statut <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(sprintf('DIR_SORTIES <- "%s"; source("R/99_controles/99d_controler_tension_cartes.R"); stopifnot(controle_tension$statut == "ok")', dir))), stdout = log, stderr = log)
  expect_equal(statut, 0L); txt <- paste(readLines(log, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("CONTRÔLE DES RESTITUTIONS DE LA TENSION", txt, fixed = TRUE)); expect_true(grepl("Aucun écart", txt, fixed = TRUE)); expect_false(grepl("Error|Erreur", txt))
  statut2 <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote('DIR_SORTIES <- "/chemin/inexistant"; source("R/99_controles/99d_controler_tension_cartes.R")')), stdout = log, stderr = log)
  expect_equal(statut2, 0L); expect_true(grepl("Fichier(s) absent(s)", paste(readLines(log, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), fixed = TRUE))
})
