# ==============================================================================
# 99b_normaliser_csv_excel.R — BOM UTF-8 sur tous les CSV de sorties/ (Excel)
# ------------------------------------------------------------------------------
# Étape TECHNIQUE de finalisation, pas statistique. Excel Windows n'interprète
# un CSV comme UTF-8 à l'ouverture directe que s'il commence par le BOM UTF-8
# (octets EF BB BF) ; sans lui, « Île-de-France » devient « ÃŽle-de-France ».
# Principe : le projet reste en UTF-8 ; on préfixe UNIQUEMENT ces trois octets.
#
# Garanties :
#   - travail en BINAIRE (readBin / writeBin) : rien n'est parsé ni réécrit ;
#     séparateur « ; », décimale « , », guillemets, NA, zéros initiaux (01, 2A,
#     971, SIREN, PCS), arrondis et ordre des colonnes sont conservés octet
#     pour octet — la seule différence autorisée est EF BB BF en tête ;
#   - idempotent : un fichier déjà doté du BOM n'est pas touché (jamais 2 BOM) ;
#   - validation UTF-8 (validUTF8, R base) AVANT toute écriture : un fichier
#     invalide est laissé intact et signalé par un warning nominatif — aucune
#     conversion silencieuse (pas d'iconv), aucune devinette d'encodage ;
#   - écriture sûre : fichier temporaire dans le même répertoire, relecture et
#     vérification, puis bascule par renommages (original jamais supprimé avant
#     que la copie vérifiée existe).
# PRODUIT : objet `controle_csv_excel` (fichier, taille_octets, utf8_valide,
#           bom_avant, action) + bilan console.
# USAGE   : source("R/99_controles/99b_normaliser_csv_excel.R") après la chaîne
#           (appelé en fin de main.R si NORMALISER_CSV_EXCEL ; relançable à
#           volonté, aussi après 07b / 07c / 99_controle qui écrivent des CSV).
# RELECTURE dans R d'un CSV avec BOM : read.csv2(f, fileEncoding = "UTF-8-BOM")
#           (sinon le premier nom de colonne porte un « ﻿ » invisible).
# ==============================================================================
BOM_UTF8 <- as.raw(c(0xEF, 0xBB, 0xBF))

# --- Fonction unitaire : un fichier -> statut --------------------------------
# "deja_bom" | "bom_ajoute" | "erreur_utf8". Arrêt si le chemin n'existe pas.
ajouter_bom_utf8_csv <- function(fichier) {
  if (length(fichier) != 1 || !file.exists(fichier)) stop("ajouter_bom_utf8_csv : fichier introuvable : ", fichier)
  taille <- file.size(fichier)
  octets <- if (taille > 0) readBin(fichier, what = "raw", n = taille) else raw(0)
  bom_avant <- length(octets) >= 3 && identical(octets[1:3], BOM_UTF8)
  corps <- if (bom_avant) octets[-(1:3)] else octets
  # Validation UTF-8 du contenu texte, sans conversion. Un octet nul = pas du
  # texte. Fichier vide ou en-tête seul : valide.
  utf8_ok <- length(corps) == 0 ||
    (!any(corps == as.raw(0)) && isTRUE(validUTF8(rawToChar(corps))))
  if (!utf8_ok) {
    warning("99b : contenu NON valide en UTF-8, fichier laissé intact : ", fichier,
            " — à corriger à la source (encodage d'écriture).", call. = FALSE)
    return(structure("erreur_utf8", taille_octets = taille, utf8_valide = FALSE, bom_avant = bom_avant))
  }
  if (bom_avant)
    return(structure("deja_bom", taille_octets = taille, utf8_valide = TRUE, bom_avant = TRUE))
  # Écriture sûre : temporaire dans le même répertoire, vérification, bascule.
  tmp <- tempfile(pattern = paste0(".", basename(fichier), "."), tmpdir = dirname(fichier), fileext = ".bom_tmp")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  writeBin(c(BOM_UTF8, octets), tmp)
  relu <- readBin(tmp, what = "raw", n = file.size(tmp))
  if (length(relu) != taille + 3 || !identical(relu[1:3], BOM_UTF8) || !identical(relu[-(1:3)], octets)) {
    stop("99b : vérification du fichier temporaire ÉCHOUÉE, original conservé : ", fichier)
  }
  sauvegarde <- paste0(fichier, ".avant_bom")
  if (!file.rename(fichier, sauvegarde)) stop("99b : impossible de renommer l'original : ", fichier)
  if (!file.rename(tmp, fichier)) {                     # restauration : l'original n'est jamais perdu
    file.rename(sauvegarde, fichier)
    stop("99b : impossible de mettre en place le fichier avec BOM, original restauré : ", fichier)
  }
  unlink(sauvegarde)
  structure("bom_ajoute", taille_octets = taille, utf8_valide = TRUE, bom_avant = FALSE)
}

# --- Fonction de haut niveau : tous les CSV d'un répertoire, récursivement ----
normaliser_csv_excel <- function(repertoire = get0("DIR_SORTIES", ifnotfound = "sorties"), prefixe = "99b") {
  if (!dir.exists(repertoire)) stop(prefixe, " : répertoire introuvable : ", repertoire)
  fichiers <- sort(list.files(repertoire, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE))
  res <- lapply(fichiers, ajouter_bom_utf8_csv)
  rapport <- data.frame(
    fichier       = if (length(fichiers)) sub(paste0("^", gsub("([.|()\\\\^{}+$*?\\[\\]])", "\\\\\\1", repertoire), "/?"), "", fichiers) else character(0),
    taille_octets = vapply(res, function(r) as.numeric(attr(r, "taille_octets")), numeric(1)),
    utf8_valide   = vapply(res, function(r) isTRUE(attr(r, "utf8_valide")), logical(1)),
    bom_avant     = vapply(res, function(r) isTRUE(attr(r, "bom_avant")), logical(1)),
    action        = vapply(res, function(r) as.character(r), character(1)),
    stringsAsFactors = FALSE)
  n <- c(trouves = nrow(rapport), deja = sum(rapport$action == "deja_bom"),
         ajoute = sum(rapport$action == "bom_ajoute"), erreur = sum(rapport$action == "erreur_utf8"))
  cat(sprintf("\n%s — normalisation CSV pour Excel (%s)\n\n", prefixe, repertoire))
  cat(sprintf("CSV trouvés          : %d\nBOM déjà présent     : %d\nBOM ajouté           : %d\nErreur UTF-8         : %d\n",
              n[["trouves"]], n[["deja"]], n[["ajoute"]], n[["erreur"]]))
  if (n[["erreur"]] > 0) {
    cat("\nFichiers NON valides en UTF-8 (laissés intacts) :\n")
    cat(paste0("  - ", rapport$fichier[rapport$action == "erreur_utf8"], "\n"), sep = "")
    cat(sprintf("\n%s : %d fichier(s) à corriger à la source.\n", prefixe, n[["erreur"]]))
  } else cat(sprintf("\n%s OK — tous les CSV sont compatibles UTF-8/Excel.\n", prefixe))
  invisible(rapport)
}

# --- Exécution : à la source du script, sur DIR_SORTIES (ou sorties/) ---------
controle_csv_excel <- normaliser_csv_excel(get0("DIR_SORTIES", ifnotfound = "sorties"))
