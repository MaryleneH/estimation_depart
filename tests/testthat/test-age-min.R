# ==============================================================================
# AGE_MIN_BTS = SEULE SOURCE DE VÉRITÉ du champ d'étude.
#
# Démonstration « même code, trois exécutions » : pour 43, 44 et 45 ans, on copie
# le projet (R/, main.R, data/) dans un dossier temporaire, on modifie UNE SEULE
# LIGNE (AGE_MIN_BTS dans 00_config.R) et on lance  source("main.R")  dans un
# processus R neuf. Puis on vérifie, pour chaque configuration : filtrage (A),
# projection (B), effectif du champ (C), part des seniors (D), libellés (E),
# fiches (F), quadrant (G), tranches d'âge, noms internes stables.
# Enfin : contrôles de configuration (valeurs refusées) et grep exhaustif.
# ==============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
AGES_TEST <- c(43L, 44L, 45L)

# --- Projet temporaire : copie conforme, une ligne changée ---------------------
projet_temporaire <- function(age_min, patch = identity) {
  tmp <- tempfile("projet_age_")
  dir.create(tmp)
  for (d in c("R", "data")) file.copy(file.path(RACINE, d), tmp, recursive = TRUE)
  file.copy(file.path(RACINE, "main.R"), tmp)
  cfg <- file.path(tmp, "R", "00_config.R")
  l <- readLines(cfg, encoding = "UTF-8", warn = FALSE)
  i <- grep("^AGE_MIN_BTS\\s*<-", l)
  if (length(i) != 1) stop("00_config.R doit définir AGE_MIN_BTS exactement une fois.")
  l[i] <- sprintf("AGE_MIN_BTS  <- %s", age_min)
  writeLines(patch(l), cfg, useBytes = TRUE)
  tmp
}

# --- main.R dans un processus neuf, objets de contrôle sérialisés -------------
lancer_main <- function(projet) {
  writeLines(c(
    'source("main.R")',
    'saveRDS(list(',
    '  age_min = AGE_MIN_BTS, labels = LABELS_TRANCHES, breaks = BREAKS_TRANCHES,',
    '  ages_bts = sort(unique(bts$age_2024)), ages_proj = sort(unique(bts_projete$age_2024)),',
    '  n_proj = nrow(bts_projete), departs = sum(bts_projete$p_central),',
    '  tranches = table(cut(bts_projete$age_2024, BREAKS_TRANCHES, LABELS_TRANCHES), useNA = "ifany"),',
    '  synthese_geo = synthese_geo, criticite = criticite_geo_cs,',
    '  resultats_entreprises = resultats_entreprises, journal = journal_fiches,',
    '  quadrant = g$labels), "controle.rds")'),
    file.path(projet, "controle_test.R"))
  log <- file.path(projet, "main.log")
  old <- setwd(projet); on.exit(setwd(old), add = TRUE)
  statut <- system2(file.path(R.home("bin"), "Rscript"), "controle_test.R",
                    stdout = log, stderr = log)
  if (statut != 0)
    stop("main.R a échoué dans le projet temporaire :\n",
         paste(tail(readLines(log, warn = FALSE), 25), collapse = "\n"))
  readRDS(file.path(projet, "controle.rds"))
}

lire_tout <- function(fichiers)
  paste(unlist(lapply(fichiers, readLines, warn = FALSE, encoding = "UTF-8")), collapse = "\n")

# --- Les trois exécutions (une fois par fichier de test) -----------------------
PROJETS <- lapply(AGES_TEST, projet_temporaire); names(PROJETS) <- AGES_TEST
RES     <- lapply(PROJETS, lancer_main)

test_that("même code, trois exécutions : seule la ligne AGE_MIN_BTS diffère du dépôt", {
  for (a in as.character(AGES_TEST)) {
    p <- PROJETS[[a]]
    for (f in list.files(file.path(RACINE, "R"), full.names = TRUE)) {
      orig <- readLines(f, warn = FALSE, encoding = "UTF-8")
      copie <- readLines(file.path(p, "R", basename(f)), warn = FALSE, encoding = "UTF-8")
      n_diff <- sum(orig != copie)
      if (basename(f) == "00_config.R") {
        expect_equal(n_diff, if (a == "45") 0 else 1, label = paste(a, "lignes modifiées de 00_config.R"))
        expect_match(copie[grep("^AGE_MIN_BTS\\s*<-", copie)], paste0("<- ", a, "$"))
      } else expect_equal(n_diff, 0, label = paste(a, basename(f)))
    }
    expect_identical(readLines(file.path(p, "main.R"), warn = FALSE),
                     readLines(file.path(RACINE, "main.R"), warn = FALSE))
    expect_identical(RES[[a]]$age_min, as.numeric(a))
  }
})

for (a in AGES_TEST) {
  r <- RES[[as.character(a)]]; p <- PROJETS[[as.character(a)]]
  autres <- setdiff(AGES_TEST, a)
  sg <- r$synthese_geo; re <- r$resultats_entreprises
  j  <- r$journal |> dplyr::filter(statut == "ok")

  test_that(sprintf("[%d] A. filtrage : le champ commence exactement à %d ans", a, a), {
    expect_identical(min(r$ages_bts), a)                      # pas de résidu d'un ancien seuil
    expect_true(all(c(a, a + 1L) %in% r$ages_bts))            # les premiers âges sont bien là
    expect_false(any(r$ages_bts < a))
  })

  test_that(sprintf("[%d] B. projection : aucun âge < %d dans bts_projete", a, a), {
    expect_gte(min(r$ages_proj), a)
    expect_false(any(r$ages_proj %in% setdiff(43:45, a:45)))
  })

  test_that(sprintf("[%d] C. effectif du champ = sum(age_2024 >= %d), noms internes stables", a, a), {
    expect_true(all(c("effectif_champ", "effectif_55plus") %in% names(sg)))
    expect_true("effectif_champ_2024" %in% names(re))
    expect_false(any(grepl("effectif_4[0-9]plus", c(names(sg), names(re), names(r$criticite)))))
    expect_equal(sum(sg$effectif_champ), r$n_proj)                                      # 08
    expect_equal(sum(r$criticite$effectif_champ), r$n_proj)                             # 08 (ZE x CS)
    expect_equal(sum(re$effectif_champ_2024), r$n_proj)                                 # 05
    expect_equal(sum(j$effectif), sum(sg$effectif_champ[sg$geo_code %in% j$code]))      # 09
    expect_equal(sum(j$effectif55), sum(sg$effectif_55plus[sg$geo_code %in% j$code]))
    if (nrow(j) == nrow(sg))                                                           # tous diffusables :
      expect_equal(sum(j$departs), r$departs, tolerance = 1e-9)                         # fiches = total 04
    expect_equal(sum(as.integer(r$tranches)), r$n_proj)                                  # tranches = tout le champ
  })

  test_that(sprintf("[%d] D. part des seniors = 100 * effectif_55plus / effectif_champ", a), {
    expect_equal(sg$part_55plus_pct, 100 * sg$effectif_55plus / sg$effectif_champ, tolerance = 1e-9)
    expect_equal(r$criticite$part_55plus_pct,
                 100 * r$criticite$effectif_55plus / r$criticite$effectif_champ, tolerance = 1e-9)
    expect_true(all(sg$effectif_55plus <= sg$effectif_champ))
  })

  test_that(sprintf("[%d] tranches d'âge : première classe « %d-48 ans », couverture totale", a, a), {
    expect_identical(r$labels, c(sprintf("%d-48 ans", a), "49-54 ans", "55-60 ans", "61 ans et +"))
    expect_identical(r$breaks[1], a - 1)
    expect_false(any(is.na(names(r$tranches))))                 # aucun âge hors tranches
    expect_gt(r$tranches[[sprintf("%d-48 ans", a)]], 0)
  })

  fiches <- list.files(file.path(p, "sorties", "fiches_ze"), pattern = "^[^i].*\\.html$", full.names = TRUE)
  txt_fiches <- lire_tout(fiches)
  txt_index  <- lire_tout(file.path(p, "sorties", "fiches_ze", "index.html"))

  test_that(sprintf("[%d] E. libellés : « %d+ », « %d ans et + », « %d-48 ans » ; jamais un autre âge", a, a, a, a), {
    expect_match(txt_index,  sprintf("Salariés %d\\+", a))
    expect_match(txt_fiches, sprintf("%d ans et \\+", a))
    expect_match(txt_fiches, sprintf("%d-48 ans", a))
    expect_match(r$quadrant$size, sprintf("^Effectif %d\\+$", a))
    for (o in autres) {
      motif <- sprintf("\\b%d\\+|\\b%d ans et|\\b%d-48", o, o, o)
      expect_false(grepl(motif, txt_fiches, perl = TRUE), label = sprintf("fiches sans « %d+ »", o))
      expect_false(grepl(motif, txt_index,  perl = TRUE), label = sprintf("index sans « %d+ »", o))
      expect_false(any(grepl(motif, unlist(r$quadrant), perl = TRUE)), label = sprintf("quadrant sans « %d+ »", o))
    }
    en_tete <- readLines(file.path(p, "sorties", "analyse_55plus_par_ze.csv"), n = 1)
    expect_match(en_tete, "effectif_champ"); expect_false(grepl("effectif_4[0-9]plus", en_tete))
  })

  test_that(sprintf("[%d] F. fiches : champ, tranche et phrase « À retenir » sur %d ans", a, a), {
    expect_gt(length(fiches), 1)
    expect_match(txt_fiches, sprintf("Salariés de %d ans et \\+ en 2024", a))
    expect_match(txt_fiches, sprintf("salariés de %d ans et \\+", a))
    expect_match(txt_fiches, sprintf("des salariés de %d ans et plus", a))
    expect_match(txt_fiches, sprintf("des %d ans et \\+", a))            # « x % des 44 ans et + »
    expect_match(txt_fiches, sprintf("Champ : salariés de %d ans et \\+", a))
  })

  test_that(sprintf("[%d] G. quadrant : axe X, taille et caption sur %d+", a, a), {
    expect_match(r$quadrant$x, sprintf("Part des 55 ans et \\+ dans l'effectif %d\\+ \\(2024\\)", a))
    expect_identical(r$quadrant$size, sprintf("Effectif %d+", a))
    expect_match(r$quadrant$caption, sprintf("Champ : salariés de %d ans et \\+ en 2024", a))
  })
}

# --- Contrôles de configuration : arrêt explicite ------------------------------
sourcer_config <- function(age_min, patch = identity) {
  p <- tempfile("cfg_"); dir.create(p)
  cfg <- file.path(RACINE, "R", "00_config.R")
  l <- readLines(cfg, encoding = "UTF-8", warn = FALSE)
  i <- grep("^AGE_MIN_BTS\\s*<-", l); l[i] <- sprintf("AGE_MIN_BTS  <- %s", age_min)
  f <- file.path(p, "00_config.R"); writeLines(patch(l), f, useBytes = TRUE)
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(f, envir = env)
  env
}
remplacer <- function(motif, ligne) function(l) { l[grep(motif, l)] <- ligne; l }

test_that("configuration : 43, 44, 45 acceptés ; libellés et tranches dérivés", {
  for (a in AGES_TEST) {
    e <- sourcer_config(a)
    expect_identical(e$LIB_CHAMP, sprintf("%d ans et +", a))
    expect_identical(e$LIB_CHAMP_COURT, sprintf("%d+", a))
    expect_identical(e$LABELS_TRANCHES[1], sprintf("%d-48 ans", a))
    expect_identical(e$BREAKS_TRANCHES, c(a - 1, 48, 54, 60, Inf))
  }
})

test_that("configuration : valeurs refusées avec un message explicite", {
  expect_error(sourcer_config(42),   "première tranche des sources")          # sous les sources externes
  expect_error(sourcer_config(50),   "incompatible avec les tranches configurées")
  expect_error(sourcer_config(44.5), "entier")
  expect_error(sourcer_config('"45"'), "entier")
  expect_error(sourcer_config(45, remplacer("^AGE_SENIOR <-", "AGE_SENIOR <- 44")),
               "AGE_MIN_BTS = 45 >= AGE_SENIOR = 44")
  expect_error(sourcer_config(45, remplacer("^AGE_MAX_TEST", "AGE_MAX_TEST <- 45")),
               "table test serait vide")
})

test_that("mode test : AGE_MAX_TEST <= AGE_MIN_BTS refusé par le 01 lui-même", {
  env <- new.env()
  old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(file.path("R", "00_config.R"), envir = env)
  sys.source(file.path("R", "00c_fonctions_geo.R"), envir = env)
  assign("AGE_MAX_TEST", env$AGE_MIN_BTS, envir = env)
  expect_error(suppressMessages(capture.output(sys.source(file.path("R", "01_fabriquer_donnees_test.R"), envir = env))),
               "aucun âge à simuler")
})

# --- Grep exhaustif : plus aucun âge de champ en dur hors liste blanche --------
test_that("aucun « 43 » / « 44 » / « 45 » de champ en dur dans le code (liste blanche nominative)", {
  fichiers <- c(list.files(file.path(RACINE, "R"), pattern = "\\.R$", full.names = TRUE),
                list.files(file.path(RACINE, "utils"), pattern = "\\.R$", full.names = TRUE),
                file.path(RACINE, "main.R"))
  # Occurrences LÉGITIMES : la valeur par défaut elle-même, sa documentation,
  # les bornes de SOURCES externes (invalidité EIR/EACR, population active),
  # un code PCS de la table test, la compatibilité des anciens fichiers.
  legitimes <- list(
    "00_config.R" = c("AGE_MIN_BTS  <- 45",                       # LA valeur par défaut
                      "Valeurs usuelles testées : 43, 44, 45",     # documentation du paramètre
                      "La valeur 45 n'est que le défaut ACTUEL",
                      "de 43 (borne basse des sources externes",
                      "43,         49,",                           # TRANCHES_DEFAUT (bande source)
                      "\"H\",   43, 0.0023", "\"F\",   43, 0.0029", # T_INVALIDITE_BASE
                      "commence à 43 ans = borne de la SOURCE"),
    "02c_importer_invalidite_eacr.R" = c("43_49", "borne_inf = c(43,50,55,60,62)", "le taux 43-49"),
    "01_fabriquer_donnees_test.R" = c("\"6543\"",                 # code PCS-ESE (ouvriers)
                                      "\"33\", \"35\", \"44\", \"56\""),  # codes DÉPARTEMENT du zonage test
    "utils_comparer_sorties.R" = c("effectif_43plus / effectif_45plus"))  # anciens fichiers
  residus <- character(0)
  for (f in fichiers) {
    l <- readLines(f, warn = FALSE, encoding = "UTF-8")
    l_sans_decimales <- gsub("[0-9]*\\.[0-9]+", "", l)            # 0.45, 1.45 : pas des âges
    hits <- grep("\\b4[345]\\b|4[345]\\+|4[345]plus|4[345]-48|4[345] ans", l_sans_decimales)
    for (i in hits) {
      ok <- any(vapply(legitimes[[basename(f)]] %||% character(0),
                       function(m) grepl(m, l[i], fixed = TRUE), logical(1)))
      if (!ok) residus <- c(residus, sprintf("%s:%d: %s", basename(f), i, trimws(l[i])))
    }
  }
  expect_identical(residus, character(0))
})

unlink(unlist(PROJETS), recursive = TRUE)
