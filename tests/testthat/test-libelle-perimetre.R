# ==============================================================================
# LIBELLE_PERIMETRE (00_config) — nom du périmètre d'entreprises dans toutes les
# restitutions : « Base » par défaut, « BITD » en interne en bout de course, sans
# toucher au code ; « Base Tous salariés » (source Insee) n'est jamais modifié.
# Garde-fou : aucune chaîne de restitution de R/ n'écrit « Base » en dur hors
# texte_perimetre(). Données fictives seulement.
# ==============================================================================
charger <- function(libelle = NULL) {
  env <- new.env(); old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  if (!is.null(libelle)) assign("LIBELLE_PERIMETRE", libelle, envir = env)
  for (f in c("00g_format_restitution.R", "00c_fonctions_geo.R", "00d_fonctions_fiches.R", "00f_fonctions_cartes.R", "00h_fonctions_typologie.R",
              "00i_fonctions_carte_typologie.R", "00j_fonctions_cartes_typologie_pcs.R", "00k_fonctions_tension.R")) sys.source(chemin_script(f), envir = env)
  env
}
sans_base <- function(x) { x <- paste(x, collapse = "\n"); expect_false(grepl("\\bBase\\b(?! Tous [Ss]alari)", x, perl = TRUE), label = substr(x, 1, 80)) }

test_that("texte_perimetre : mot entier, « Base Tous salariés » intact, valeur par défaut « Base »", {
  E <- charger()
  expect_identical(E$lib_perimetre(), "Base"); expect_identical(E$texte_perimetre("Poids Base · base_poids · Base64 · règle de la Base Tous salariés"), "Poids Base · base_poids · Base64 · règle de la Base Tous salariés")
  E2 <- charger("BITD")
  expect_identical(E2$texte_perimetre("Poids Base · base_poids · Base64 · règle de la Base Tous salariés · emploi Base national"), "Poids BITD · base_poids · Base64 · règle de la Base Tous salariés · emploi BITD national")
  expect_error(local({ LIBELLE_PERIMETRE <- ""; if (!nzchar(trimws(LIBELLE_PERIMETRE))) stop("LIBELLE_PERIMETRE doit être un texte non vide") }), "non vide")
})

test_that("avec LIBELLE_PERIMETRE = « BITD », les libellés publics, les profils, les en-têtes de carte et les textes de tension disent BITD, jamais Base", {
  E <- charger("BITD")
  ch <- E$construire_libelle_champ(); expect_match(ch$court, "entreprises du périmètre BITD$"); sans_base(ch$detaille)
  P <- E$construire_libelles_public(ch); sans_base(unlist(P)); expect_match(P$kpi_aide$salaries, "périmètre BITD")
  expect_true(any(grepl("emploi BITD", E$DEFINITIONS_PROFILS$signification))); sans_base(unlist(E$DEFINITIONS_PROFILS))
  expect_match(E$LIBELLES_CARTE_TYPO$kicker, "^Typologie des territoires BITD"); sans_base(unlist(E$LIBELLES_CARTE_TYPO))
  expect_match(E$LIBELLES_TYPO_PCS$kicker, "^Typologie des territoires BITD"); sans_base(unlist(E$LIBELLES_TYPO_PCS))
  fr <- tibble::tibble(pcs = c("1a1a", "9z9z"), effectif_champ = c(100, 100), departs_central = c(10, 10), taux_depart_central_pct = c(10, 10))
  met <- tibble::tibble(pcs = c("1a1a", "8u8u"), pcs_source = c("1A1A", "8U8U"), pcs_libelle = c("Métier A", "Métier B"), fap_code = "F", fap_libelle = "Famille", rang_tension = c(5L, NA), rang_tension_fin = c(5, NA),
                        en_tension = c(TRUE, NA), tension_connue = c(TRUE, FALSE), composante_dominante = NA, composante_libelle = NA, levier = NA, faible_confiance = FALSE)
  m <- E$matrice_metiers_tension(met, fr, NULL, modifyList(E$parametres_tension(), list(seuil_rang = 4)))
  expect_identical(m$metiers$statut[m$metiers$pcs == "8u8u"], "absente de la BITD")
  h <- E$html_tableau_metiers_tension(m); sans_base(h); expect_true(any(grepl("sans salarié du champ dans la BITD", h)))
  # valeur par défaut : rien ne change
  E0 <- charger(); expect_match(E0$construire_libelle_champ()$court, "périmètre Base$"); expect_identical(E0$LIBELLES_CARTE_TYPO$kicker, "Typologie des territoires Base · résultats diffusables")
})

test_that("garde-fou : aucune chaîne littérale de R/ n'écrit « Base » hors texte_perimetre() (liste blanche : Base Tous salariés)", {
  fichiers <- c(list.files(file.path(RACINE, "R"), pattern = "\\.R$", full.names = TRUE, recursive = TRUE), file.path(RACINE, "main.R"))
  fichiers <- fichiers[basename(fichiers) != "00_config.R"]                  # la définition du paramètre lui-même
  lit <- "(\"(?:[^\"\\\\\\n]|\\\\.)*?\\bBase\\b(?:[^\"\\\\\\n]|\\\\.)*?\"|'(?:[^'\\\\\\n]|\\\\.)*?\\bBase\\b(?:[^'\\\\\\n]|\\\\.)*?')"
  residus <- character(0)
  for (f in fichiers) {
    l <- readLines(f, warn = FALSE, encoding = "UTF-8"); l <- l[!grepl("^\\s*#", l)]
    l <- sub("\\s#[^\"']*$", "", l)                                   # commentaire de fin de ligne sans guillemet
    for (i in setdiff(grep("Base", l, fixed = TRUE), grep("texte_perimetre <- function", l, fixed = TRUE))) {   # la définition de repli (99c) exclue
      m <- regmatches(l[i], gregexpr(lit, l[i], perl = TRUE))[[1]]
      for (s in m) if (!grepl("Base Tous [Ss]alari", s) && !grepl(paste0("texte_perimetre\\(", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", s)), l[i], perl = TRUE) && !grepl("\\\\bBase\\\\b", s, fixed = TRUE))
        residus <- c(residus, sprintf("%s: %s", basename(f), trimws(substr(l[i], 1, 100))))
    }
  }
  expect_identical(residus, character(0))
})
