# ==============================================================================
# 99b : BOM UTF-8 sur les CSV de sorties/ — binaire, idempotent, contenu
# conservé octet pour octet, UTF-8 invalide signalé et laissé intact.
# ==============================================================================
BOM <- as.raw(c(0xEF, 0xBB, 0xBF))
E99 <- new.env()
DIR_VIDE <- file.path(tempdir(), "csv99_vide"); dir.create(DIR_VIDE, showWarnings = FALSE)
assign("DIR_SORTIES", DIR_VIDE, envir = E99)
invisible(capture.output(sys.source(chemin_script("99b_normaliser_csv_excel.R"), envir = E99)))
octets <- function(f) readBin(f, "raw", n = file.size(f))
sans_bom <- function(b) if (length(b) >= 3 && identical(b[1:3], BOM)) b[-(1:3)] else b
ecrire_utf8 <- function(f, lignes) writeBin(charToRaw(enc2utf8(paste0(paste(lignes, collapse = "\n"), "\n"))), f)
nouveau_dossier <- function(nom) { d <- file.path(tempdir(), nom); unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE); d }

test_that("A. UTF-8 sans BOM : BOM ajouté, contenu strictement identique après les 3 octets ; rapport et bilan", {
  d <- nouveau_dossier("csv99_a"); f <- file.path(d, "regions.csv")
  ecrire_utf8(f, c("nom;region", "1;Île-de-France", "2;Auvergne-Rhône-Alpes"))
  avant <- octets(f)
  r <- E99$ajouter_bom_utf8_csv(f)
  expect_identical(as.character(r), "bom_ajoute")
  apres <- octets(f)
  expect_identical(apres[1:3], BOM); expect_identical(apres[-(1:3)], avant)
  expect_equal(length(apres), length(avant) + 3)
  # les lecteurs R en locale UTF-8 ignorent le BOM : lecture texte inchangée
  expect_identical(sub("^﻿", "", readLines(f, encoding = "UTF-8")), c("nom;region", "1;Île-de-France", "2;Auvergne-Rhône-Alpes"))
  expect_identical(read.csv2(f, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)$region, c("Île-de-France", "Auvergne-Rhône-Alpes"))
  expect_false(any(grepl("\\.bom_tmp$|\\.avant_bom$", list.files(d, all.files = TRUE))))   # aucun résidu
})

test_that("B. déjà un BOM : rien n'est modifié (taille, octets, horodatage), un seul BOM", {
  d <- nouveau_dossier("csv99_b"); f <- file.path(d, "deja.csv")
  writeBin(c(BOM, charToRaw("a;b\n1;Rhône\n")), f)
  avant <- octets(f); m <- file.mtime(f)
  expect_identical(as.character(E99$ajouter_bom_utf8_csv(f)), "deja_bom")
  expect_identical(octets(f), avant); expect_identical(file.mtime(f), m)
  expect_identical(octets(f)[1:3], BOM); expect_false(identical(octets(f)[4:6], BOM))
})

test_that("C. accents : é è ê à ù ç ô Î œ ’ conservés octet pour octet ; D. codes 01 09 2A 2B 971 et SIREN ; E. 12,5 et « ; » ; F. NA et \"\"", {
  d <- nouveau_dossier("csv99_c")
  f <- file.path(d, "accents.csv")
  ecrire_utf8(f, c("code;libelle;valeur;siren;note", "01;Ain é è ê à ù ç ô Î œ ’;12,5;012345678;NA",
                   "09;Ariège;0,1;;\"\"", "2A;Corse-du-Sud;7;000123456;", "2B;Haute-Corse;NA;123456789;x", "971;Guadeloupe;1,0;;NA"))
  avant <- octets(f)
  expect_identical(as.character(E99$ajouter_bom_utf8_csv(f)), "bom_ajoute")
  expect_identical(sans_bom(octets(f)), avant)
  l <- readLines(f, encoding = "UTF-8"); l[1] <- sub("^﻿", "", l[1])
  expect_identical(l[2], "01;Ain é è ê à ù ç ô Î œ ’;12,5;012345678;NA")
  expect_identical(substr(l[3:6], 1, 3), c("09;", "2A;", "2B;", "971"))
  expect_true(all(grepl(";12,5;|;0,1;|;1,0;|;7;", l[2:6]) | grepl(";NA;", l[2:6])))
  expect_identical(l[3], "09;Ariège;0,1;;\"\"")
  # fichier écrit par write.csv2 lui-même (NA nus, guillemets, décimale « , »)
  g <- file.path(d, "write_csv2.csv")
  write.csv2(data.frame(geo_code = c("01", "2A", "971"), x = c(12.5, NA, 0.1), nom = c("Rhône", NA, ""), stringsAsFactors = FALSE), g, row.names = FALSE)
  avant_g <- octets(g)
  expect_identical(as.character(E99$ajouter_bom_utf8_csv(g)), "bom_ajoute")
  expect_identical(sans_bom(octets(g)), avant_g)
  relu <- read.csv2(g, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, colClasses = c(geo_code = "character"))
  expect_identical(relu$geo_code, c("01", "2A", "971")); expect_identical(relu$x, c(12.5, NA, 0.1)); expect_identical(names(relu)[1], "geo_code")
})

test_that("G. UTF-8 invalide : détecté, laissé intact, warning avec le chemin ; les autres fichiers du lot sont traités", {
  d <- nouveau_dossier("csv99_g")
  bad <- file.path(d, "latin1.csv"); writeBin(c(charToRaw("code;nom\n01;Rh"), as.raw(0xE9), charToRaw("ne\n")), bad)   # é en latin1
  ok  <- file.path(d, "ok.csv"); ecrire_utf8(ok, c("a;b", "1;Rhône"))
  avant_bad <- octets(bad)
  expect_warning(r <- E99$ajouter_bom_utf8_csv(bad), "NON valide en UTF-8.*latin1\\.csv")
  expect_identical(as.character(r), "erreur_utf8"); expect_identical(octets(bad), avant_bad)
  expect_warning(out <- capture.output(rap <- E99$normaliser_csv_excel(d)), "latin1\\.csv")
  expect_identical(rap$action[order(rap$fichier)], c("erreur_utf8", "bom_ajoute"))
  expect_identical(octets(bad), avant_bad); expect_identical(octets(ok)[1:3], BOM)
  expect_true(any(grepl("Erreur UTF-8         : 1", out))); expect_true(any(grepl("latin1.csv", out, fixed = TRUE)))
  expect_false(any(grepl("99b OK", out)))
  # octet nul = pas du texte -> refusé aussi
  nul <- file.path(d, "nul.csv"); writeBin(c(charToRaw("a;b\n1;"), as.raw(0), charToRaw("\n")), nul)
  expect_warning(expect_identical(as.character(E99$ajouter_bom_utf8_csv(nul)), "erreur_utf8"))
  expect_error(E99$ajouter_bom_utf8_csv(file.path(d, "absent.csv")), "introuvable")
})

test_that("H. idempotence et récursivité : 2 passages, le second n'ajoute rien ; sous-répertoires ; .txt/.html ignorés ; vide et en-tête seul", {
  d <- nouveau_dossier("csv99_h"); dir.create(file.path(d, "departs_pcs", "diffusion"), recursive = TRUE)
  ecrire_utf8(file.path(d, "a.csv"), c("x;y", "1;Pyrénées-Atlantiques"))
  ecrire_utf8(file.path(d, "departs_pcs", "diffusion", "b.csv"), c("pcs;n", "311D;NA"))
  writeBin(raw(0), file.path(d, "vide.csv"))                                   # CSV vide
  ecrire_utf8(file.path(d, "entete.csv"), "geo_code;geo_nom")                 # en-tête seul
  writeBin(c(BOM, charToRaw("k;v\n")), file.path(d, "deja.csv"))
  ecrire_utf8(file.path(d, "notes.txt"), "pas un csv"); ecrire_utf8(file.path(d, "page.html"), "<p>é</p>")
  tailles_avant <- vapply(list.files(d, "\\.csv$", recursive = TRUE, full.names = TRUE), file.size, numeric(1))
  out1 <- capture.output(r1 <- E99$normaliser_csv_excel(d))
  expect_equal(nrow(r1), 5); expect_setequal(names(r1), c("fichier", "taille_octets", "utf8_valide", "bom_avant", "action"))
  expect_true(all(r1$utf8_valide)); expect_equal(sum(r1$action == "bom_ajoute"), 4); expect_equal(sum(r1$action == "deja_bom"), 1)
  expect_true("departs_pcs/diffusion/b.csv" %in% r1$fichier)
  expect_identical(r1$bom_avant[r1$fichier == "deja.csv"], TRUE)
  expect_equal(file.size(file.path(d, "vide.csv")), 3); expect_identical(octets(file.path(d, "vide.csv")), BOM)
  expect_identical(sans_bom(octets(file.path(d, "entete.csv"))), charToRaw("geo_code;geo_nom\n"))
  expect_true(any(grepl("CSV trouvés          : 5", out1)) && any(grepl("BOM ajouté           : 4", out1)) && any(grepl("99b OK", out1)))
  tailles_apres <- vapply(names(tailles_avant), file.size, numeric(1))
  expect_equal(unname(tailles_apres - tailles_avant), ifelse(grepl("deja", names(tailles_avant)), 0, 3))
  expect_false(file.size(file.path(d, "notes.txt")) != nchar("pas un csv\n"))  # non-CSV intact
  # second passage : 0 modification
  out2 <- capture.output(r2 <- E99$normaliser_csv_excel(d))
  expect_equal(sum(r2$action == "bom_ajoute"), 0); expect_equal(sum(r2$action == "deja_bom"), 5)
  expect_true(any(grepl("BOM ajouté           : 0", out2)))
  expect_identical(vapply(names(tailles_avant), file.size, numeric(1)), tailles_apres)
  for (f in names(tailles_avant)) expect_false(identical(octets(f)[4:6], BOM), label = f)   # jamais 2 BOM
  # répertoire sans CSV : bilan à 0, pas d'erreur ; répertoire absent : arrêt
  expect_equal(nrow(E99$normaliser_csv_excel(DIR_VIDE)), 0)
  expect_error(E99$normaliser_csv_excel(file.path(d, "nexiste_pas")), "introuvable")
})

test_that("non-régression sur les VRAIES sorties versionnées : contenu hors BOM, lignes et octets identiques, hash md5 identique", {
  src <- list.files(file.path(RACINE, "sorties"), pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  skip_if(length(src) == 0, "aucun CSV dans sorties/")
  d <- nouveau_dossier("csv99_nr")
  copies <- file.path(d, basename(dirname(src)) |> paste0("_", basename(src)))
  # copies SANS BOM (quel que soit l'état des sorties) : on exerce toujours l'ajout
  for (i in seq_along(src)) writeBin(sans_bom(octets(src[i])), copies[i])
  hash_hors_bom <- function(f) { t <- tempfile(); writeBin(sans_bom(octets(f)), t); unname(tools::md5sum(t)) }
  avant <- data.frame(f = copies, octets = vapply(copies, function(f) length(sans_bom(octets(f))), numeric(1)),
                      lignes = vapply(copies, function(f) length(readLines(f, warn = FALSE)), integer(1)),
                      md5 = vapply(copies, hash_hors_bom, character(1)), stringsAsFactors = FALSE)
  invisible(capture.output(r <- E99$normaliser_csv_excel(d)))
  expect_equal(nrow(r), length(src)); expect_true(all(r$action == "bom_ajoute")); expect_true(all(r$utf8_valide))
  for (i in seq_along(copies)) {
    b <- octets(copies[i])
    expect_identical(b[1:3], BOM, label = copies[i])
    expect_equal(length(b) - 3, avant$octets[i], label = copies[i])
    expect_equal(length(readLines(copies[i], warn = FALSE)), avant$lignes[i], label = copies[i])
    expect_identical(hash_hors_bom(copies[i]), avant$md5[i], label = copies[i])
    expect_true(validUTF8(rawToChar(b[-(1:3)])), label = copies[i])
  }
})
