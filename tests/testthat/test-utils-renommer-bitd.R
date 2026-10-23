# ==============================================================================
# utils/utils_renommer_base_bitd.R — remplacer « Base nationale » / « Base » par
# « BITD » quand il le faut : inventaire classé, rapport, décisions, application
# à la position exacte, garanties (identifiants, binaires, BOM / CRLF, dépôt changé).
# Mini-projet FICTIF écrit dans tempdir() ; aucune donnée du projet.
# ==============================================================================
RB <- new.env()
sys.source(file.path(RACINE, "utils", "utils_renommer_base_bitd.R"), envir = RB)

ecrire_octets <- function(chemin, x) { x <- if (is.raw(x)) x else charToRaw(enc2utf8(x))        # x forcé AVANT d'ouvrir (sinon le fichier lu serait déjà tronqué)
  dir.create(dirname(chemin), showWarnings = FALSE, recursive = TRUE); con <- file(chemin, open = "wb"); on.exit(close(con)); writeBin(x, con) }
lire_octets <- function(chemin) readBin(chemin, "raw", file.size(chemin))
texte_utf8 <- function(chemin) { x <- rawToChar(lire_octets(chemin)); Encoding(x) <- "UTF-8"; x }
# Un mini-projet fictif neuf à chaque appel (les tests d'application modifient les fichiers)
projet_fictif <- function() {
  p <- file.path(tempdir(), paste0("renommage_", as.integer(stats::runif(1, 1e6, 1e7)))); dir.create(p)
  ecrire_octets(file.path(p, "a.R"), paste0(c("# périmètre Base : salariés concernés",
                                              'titre <- "Typologie des territoires Base nationale"',
                                              "# --- Base commune du 08c et du 08d",
                                              "x <- effectif_Base_total + 1",
                                              "# règle Insee de la Base Tous salariés",
                                              "# Base : à décider",
                                              "# Base64 encodé ; emploi Base national ; la Base.",
                                              "y <- 2"), collapse = "\n"))                      # sans retour final
  ecrire_octets(file.path(p, "doc.md"), c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(enc2utf8("La Base est un périmètre.\r\nPoids Base, puis « Base ».\r\n"))))
  ecrire_octets(file.path(p, "bin.txt"), as.raw(c(0x89, 0x50, 0x00, 0x42, 0x61, 0x73, 0x65, 0x00)))   # « Base » dans un binaire (octet nul)
  ecrire_octets(file.path(p, "latin.txt"), as.raw(c(0x70, 0xE9, 0x72, 0x69, 0x6D, 0xE8, 0x74, 0x72, 0x65, 0x20, 0x42, 0x61, 0x73, 0x65)))   # latin1 : non UTF-8
  ecrire_octets(file.path(p, "sorties", "page.html"), "<p>périmètre Base</p>")
  ecrire_octets(file.path(p, "data", "x.txt"), "périmètre Base")
  ecrire_octets(file.path(p, "sans_ext"), "périmètre Base")
  ecrire_octets(file.path(p, "renommage_bitd.csv"), "périmètre Base"); ecrire_octets(file.path(p, "utils", "utils_renommer_base_bitd.R"), 'REMPLACEMENTS <- c("Base" = "BITD")')
  ecrire_octets(file.path(p, "tests", "test-utils-renommer-bitd.R"), '"# Base : à décider"'); ecrire_octets(file.path(p, "README_renommage_bitd.md"), "la Base")   # jamais lus
  p
}
inv <- function(p, ...) RB$inventorier_occurrences(p, sans_git = TRUE, silencieux = TRUE, ...)

test_that("inventaire : fichiers texte hors exclusions, binaires et non UTF-8 ignorés, motifs mot entier (le plus long d'abord), classement par contexte", {
  p <- projet_fictif(); on.exit(unlink(p, recursive = TRUE))
  f <- RB$lister_fichiers_texte(p, sans_git = TRUE); expect_setequal(f, c("a.R", "doc.md", "bin.txt", "latin.txt"))
  o <- inv(p)
  expect_setequal(attr(o, "ignores"), c("bin.txt", "latin.txt")); expect_false(any(grepl("^sorties|^data", o$fichier)))
  expect_identical(names(o), c("fichier", "ligne", "colonne", "motif", "remplacement", "contexte", "classe", "decision", "raison"))
  a <- o[o$fichier == "a.R", ]
  expect_identical(a$ligne, c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 7L, 7L)); expect_identical(a$motif[2], "Base nationale"); expect_equal(sum(a$ligne == 2), 1)   # jamais « Base » seul dans « Base nationale »
  expect_identical(a$classe, c("remplacer", "remplacer", "conserver", "identifiant", "conserver", "à trancher", "identifiant", "remplacer", "remplacer"))
  expect_identical(a$decision, c("remplacer", "remplacer", "conserver", "", "conserver", "", "", "remplacer", "remplacer"))
  expect_identical(a$colonne[a$ligne == 7], c(3L, 26L, 45L)); expect_identical(a$contexte[1], "# périmètre Base : salariés concernés")
  d <- o[o$fichier == "doc.md", ]; expect_equal(nrow(d), 3); expect_identical(d$classe, rep("remplacer", 3)); expect_false(any(grepl("\r", d$contexte, fixed = TRUE)))
  expect_identical(d$colonne[1], 5L); expect_identical(d$contexte[1], "La Base est un périmètre.")                 # le BOM compte pour un caractère de la ligne 1, pas dans le contexte
  expect_output(RB$inventorier_occurrences(p, sans_git = TRUE), "4 fichier(s) texte lu(s), 12 occurrence(s)", fixed = TRUE)
  expect_output(RB$inventorier_occurrences(p, sans_git = TRUE), "Ignoré(s) (binaire ou non UTF-8) :  bin.txt, latin.txt", fixed = TRUE); expect_output(RB$inventorier_occurrences(p, sans_git = TRUE), "a.R:6  # Base : à décider", fixed = TRUE)
})

test_that("rapport CSV (BOM, « ; », UTF-8) relu à l'identique ; décisions normalisées ; erreurs sur colonne absente ou décision inconnue ; CSV réenregistré en ANSI accepté", {
  p <- projet_fictif(); on.exit(unlink(p, recursive = TRUE)); o <- inv(p)
  f <- file.path(p, "rapport.csv"); RB$ecrire_rapport(o, f)
  expect_identical(lire_octets(f)[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
  d <- RB$lire_decisions(f); expect_equal(nrow(d), nrow(o)); expect_identical(d$fichier, o$fichier); expect_identical(d$ligne, o$ligne); expect_identical(d$colonne, o$colonne)
  expect_identical(d$motif, o$motif); expect_identical(d$decision, o$decision); expect_identical(d$contexte, o$contexte)
  l <- readLines(f, encoding = "UTF-8", warn = FALSE); l[2] <- sub(';"remplacer";"remplacer";', ';"remplacer";" Conserver ";', l[2], fixed = TRUE); l[7] <- sub(';"à trancher";"";', ';"à trancher";"à trancher";', l[7], fixed = TRUE)
  ecrire_octets(f, paste0(paste(l, collapse = "\n"), "\n")); d2 <- RB$lire_decisions(f); expect_identical(d2$decision[1], "conserver"); expect_identical(d2$decision[6], "")
  ecrire_octets(f, iconv(paste0(paste(sub(paste0("^", intToUtf8(0xFEFF)), "", l), collapse = "\r\n"), "\r\n"), "UTF-8", "windows-1252", sub = "?"))   # Excel « CSV (séparateur point-virgule) »
  d3 <- RB$lire_decisions(f); expect_identical(d3$contexte[1], o$contexte[1])
  ecrire_octets(f, "\"fichier\";\"ligne\"\n\"a.R\";\"1\"\n"); expect_error(RB$lire_decisions(f), "Colonnes absentes")
  ecrire_octets(f, "\"fichier\";\"ligne\";\"colonne\";\"motif\";\"decision\"\n\"a.R\";\"1\";\"1\";\"Base\";\"oui\"\n"); expect_error(RB$lire_decisions(f), "inconnue")
})

test_that("application : refus sans décision pour un « à trancher » ; --force conserve ; remplacement à la position exacte, BOM, CRLF, binaire et identifiants intacts", {
  p <- projet_fictif(); on.exit(unlink(p, recursive = TRUE))
  avant <- lapply(c("a.R", "doc.md", "bin.txt", "latin.txt"), function(f) lire_octets(file.path(p, f)))
  expect_error(RB$appliquer_renommage(p, sans_git = TRUE, silencieux = TRUE), "sans décision")
  expect_identical(lire_octets(file.path(p, "a.R")), avant[[1]])                                                    # rien n'a été écrit
  b <- RB$appliquer_renommage(p, sans_git = TRUE, force = TRUE, silencieux = TRUE)
  expect_equal(b$remplacees, 7); expect_equal(b$conservees, 2); expect_equal(b$forcees, 1); expect_setequal(b$fichiers_modifies, c("a.R", "doc.md"))
  a <- strsplit(texte_utf8(file.path(p, "a.R")), "\n", fixed = TRUE)[[1]]
  expect_identical(a, c("# périmètre BITD : salariés concernés", 'titre <- "Typologie des territoires BITD"', "# --- Base commune du 08c et du 08d", "x <- effectif_Base_total + 1",
                        "# règle Insee de la Base Tous salariés", "# Base : à décider", "# Base64 encodé ; emploi BITD national ; la BITD.", "y <- 2"))
  expect_false(endsWith(texte_utf8(file.path(p, "a.R")), "\n"))                                                     # pas de retour final ajouté
  expect_identical(lire_octets(file.path(p, "doc.md")), c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(enc2utf8("La BITD est un périmètre.\r\nPoids BITD, puis « BITD ».\r\n"))))
  expect_identical(lire_octets(file.path(p, "bin.txt")), avant[[3]]); expect_identical(lire_octets(file.path(p, "latin.txt")), avant[[4]])
  expect_identical(texte_utf8(file.path(p, "sorties", "page.html")), "<p>périmètre Base</p>")
  r <- inv(p); expect_identical(sort(r$classe), c("à trancher", "conserver", "conserver", "identifiant", "identifiant"))     # il ne reste que l'attendu
  expect_output(RB$verifier_apres(p, sans_git = TRUE), "il reste 5 occurrence")
})

test_that("application depuis un CSV de décisions : décisions appliquées (« à trancher » -> remplacer, « remplacer » -> conserver), identifiant jamais modifié, dépôt changé = arrêt", {
  p <- projet_fictif(); on.exit(unlink(p, recursive = TRUE)); o <- inv(p)
  o$decision[o$fichier == "a.R" & o$ligne == 6] <- "remplacer"; o$decision[o$fichier == "a.R" & o$ligne == 1] <- "conserver"; o$decision[o$fichier == "a.R" & o$ligne == 4] <- "remplacer"
  f <- file.path(p, "rapport.csv"); RB$ecrire_rapport(o, f)
  expect_warning(b <- RB$appliquer_renommage(p, decisions = f, sans_git = TRUE, silencieux = TRUE), "identifiant")
  expect_equal(b$remplacees, 7); expect_equal(b$conservees, 3)
  a <- strsplit(texte_utf8(file.path(p, "a.R")), "\n", fixed = TRUE)[[1]]
  expect_identical(a[c(1, 4, 6)], c("# périmètre Base : salariés concernés", "x <- effectif_Base_total + 1", "# BITD : à décider"))
  # dépôt changé entre l'inventaire et l'application : occurrence déplacée -> arrêt, rien d'écrit
  p2 <- projet_fictif(); on.exit(unlink(p2, recursive = TRUE), add = TRUE); o2 <- inv(p2); o2$decision[o2$decision == ""] <- "conserver"; f2 <- file.path(p2, "r.csv"); RB$ecrire_rapport(o2, f2)
  ecrire_octets(file.path(p2, "a.R"), paste0("# ligne ajoutée\n", texte_utf8(file.path(p2, "a.R")))); avant <- lire_octets(file.path(p2, "doc.md"))
  expect_error(RB$appliquer_renommage(p2, decisions = f2, sans_git = TRUE, silencieux = TRUE), "a changé depuis l'inventaire")
  expect_identical(lire_octets(file.path(p2, "doc.md")), avant)
  # décisions lues avec les accents convertis (CSV repassé par Excel en ANSI) : mêmes clés, même application
  p3 <- projet_fictif(); on.exit(unlink(p3, recursive = TRUE), add = TRUE); o3 <- inv(p3); o3$decision[o3$decision == ""] <- "conserver"; f3 <- file.path(p3, "r.csv"); RB$ecrire_rapport(o3, f3)
  l <- readLines(f3, encoding = "UTF-8", warn = FALSE); ecrire_octets(f3, iconv(paste0(paste(sub(paste0("^", intToUtf8(0xFEFF)), "", l), collapse = "\r\n"), "\r\n"), "UTF-8", "windows-1252", sub = "?"))
  expect_equal(RB$appliquer_renommage(p3, decisions = f3, sans_git = TRUE, silencieux = TRUE)$remplacees, 7)
})

test_that("avec Git : seuls les fichiers versionnés sont lus ; modifications non commitées = refus (sauf --force) ; options de la ligne de commande", {
  skip_if(!nzchar(Sys.which("git")), "git absent")
  p <- projet_fictif(); on.exit(unlink(p, recursive = TRUE))
  g <- function(...) suppressWarnings(system2("git", c("-C", shQuote(p), ...), stdout = FALSE, stderr = FALSE))
  g("init", "-q"); g("config", "user.email", "t@t"); g("config", "user.name", "t"); g("add", "a.R", "bin.txt"); g("commit", "-q", "-m", "init")
  f <- RB$lister_fichiers_texte(p); expect_true(attr(f, "git")); expect_setequal(f, c("a.R", "bin.txt"))                 # doc.md non versionné : hors inventaire
  o <- RB$inventorier_occurrences(p, silencieux = TRUE); expect_true(all(o$fichier == "a.R"))
  ecrire_octets(file.path(p, "a.R"), paste0(texte_utf8(file.path(p, "a.R")), "\n# modif"))
  expect_error(RB$appliquer_renommage(p, silencieux = TRUE, force = FALSE), "sans décision|non commitées")
  g("commit", "-q", "-a", "-m", "m2")
  b <- RB$appliquer_renommage(p, force = TRUE, silencieux = TRUE); expect_equal(b$remplacees, 4); expect_identical(b$fichiers_modifies, "a.R")
  o <- RB$arguments_ligne_commande(c("--racine=/x", "--appliquer", "--decisions=d.csv", "--extensions=R,md", "--exclure=", "--sans-git", "--force", "--rapport=r.csv"))
  expect_identical(o$racine, "/x"); expect_true(o$appliquer); expect_identical(o$decisions, "d.csv"); expect_identical(o$extensions, c("R", "md")); expect_identical(o$exclure, character(0))
  expect_true(o$sans_git); expect_true(o$force); expect_identical(o$rapport, "r.csv")
  expect_error(RB$arguments_ligne_commande("--inconnu"), "Option inconnue")
})
