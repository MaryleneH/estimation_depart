# ==============================================================================
# utils/utils_renommer_base_bitd.R — Remplacer « Base nationale » et « Base » par
# « BITD » quand il le faut, dans un dépôt de travail (textes affichés, commentaires,
# documentation), sans jamais toucher au code, aux identifiants ni aux données.
# ------------------------------------------------------------------------------
# POURQUOI : tous les « Base » ne désignent pas la BITD (« Base Tous salariés » est
#   la source Insee, « Base commune », « Base de repli » ont le sens courant, et
#   `fiche_typologie_Base` est un nom de fichier). Un remplacement aveugle serait faux.
#   Le script INVENTORIE chaque occurrence, la CLASSE d'après son contexte immédiat,
#   écrit un RAPPORT que l'on relit et complète, puis APPLIQUE exactement les décisions.
# DÉPENDANCES : R de base seulement ; Git facultatif (sinon --sans-git).
# ------------------------------------------------------------------------------
# USAGE (depuis la racine du dépôt de travail, sur une branche dédiée) :
#   1. Inventaire, rien n'est modifié :
#        Rscript utils/utils_renommer_base_bitd.R
#      -> renommage_bitd.csv (une ligne par occurrence : fichier, ligne, colonne, motif,
#         contexte, classe, decision, raison) et un bilan en console.
#   2. Décision : ouvrir le CSV (Excel ou éditeur), remplir la colonne `decision`
#      (« remplacer » ou « conserver ») pour les lignes « à trancher », corriger les
#      autres si besoin, enregistrer.
#   3. Application, puis vérification automatique :
#        Rscript utils/utils_renommer_base_bitd.R --appliquer --decisions=renommage_bitd.csv
#   4. Contrôles : git diff ; suite de tests ; régénérer les images (SVG / PNG) si des
#      générateurs ont changé.
# OPTIONS : --racine=.   --rapport=renommage_bitd.csv   --decisions=<csv>   --appliquer
#   --extensions=R,Rmd,qmd,md,txt,js,json,html,svg,css,yml,yaml   --exclure=sorties,data,node_modules
#   --sans-git (fichiers listés sans Git)   --force (passe outre : modifications non
#   commitées, « à trancher » sans décision -> conservés)
# RÈGLES : le script, son test, son mode d'emploi et son rapport (noms contenant
#   renommer_base_bitd, renommage_bitd, renommer-bitd) ne sont jamais lus ; motifs en casse exacte et mot entier ; un identifiant (`_Base`, `Base64`)
#   n'est jamais modifié ; aucun fichier binaire, aucun fichier non UTF-8 ; BOM et fins
#   de ligne (CRLF) conservés ; chaque occurrence est remplacée à sa position, jamais
#   par un remplacement global ; refus si le dépôt a changé depuis l'inventaire.
# ==============================================================================

# --- Réglages (à adapter au dépôt de travail) ---------------------------------
# Motifs remplacés, le plus long d'abord ; casse exacte, mot entier.
REMPLACEMENTS <- c("Base nationale" = "BITD", "Base" = "BITD")
# Suivi (après un blanc) de l'un de ces mots : ce n'est PAS la BITD -> conserver.
CONTEXTES_CONSERVER <- c("Tous salariés", "de données", "de repli", "commune", "seniors", "du poids", "des zones", "de calcul", "de référence")
# Précédé (avant un blanc) de l'un de ces mots : c'est la BITD -> remplacer (casse ignorée).
CONTEXTES_AVANT <- c("périmètre", "la", "de", "emploi", "territoires", "départements", "typologie", "présence", "champ", "SIREN",
                     "poids", "entreprises", "cartes", "départs", "salariés", "territoriale", "territoriales")
# Suivi (après un blanc) de l'un de ces mots : c'est la BITD -> remplacer (casse ignorée).
CONTEXTES_APRES <- c("national", "nationale", "nationaux", "français", "française", "du département", "des départements", "de la France", "en France")
# Suivi d'un signe de ponctuation (ou d'une fin de ligne) : c'est la BITD -> remplacer.
# Les deux-points et le signe égal n'en font pas partie (« # --- Base : … » reste à trancher).
PONCTUATION_APRES <- "^\\s*(\\r?$|[.,;·»)\\]}—–\"'’”]|\\.\\.\\.|…)"
EXTENSIONS_DEFAUT <- c("R", "Rmd", "qmd", "md", "txt", "js", "json", "html", "svg", "css", "yml", "yaml")
EXCLURE_DEFAUT <- c("sorties", "data", "node_modules")
CLASSES <- c("remplacer", "conserver", "à trancher", "identifiant")
# Ce fichier est en UTF-8 : ses chaînes sont marquées comme telles, quelle que soit la locale de R (Windows, locale C).
marquer_utf8 <- function(x) { Encoding(x) <- "UTF-8"; x }
REMPLACEMENTS <- stats::setNames(marquer_utf8(unname(REMPLACEMENTS)), marquer_utf8(names(REMPLACEMENTS)))
CONTEXTES_CONSERVER <- marquer_utf8(CONTEXTES_CONSERVER); CONTEXTES_AVANT <- marquer_utf8(CONTEXTES_AVANT); CONTEXTES_APRES <- marquer_utf8(CONTEXTES_APRES)
PONCTUATION_APRES <- marquer_utf8(PONCTUATION_APRES); CLASSES <- marquer_utf8(CLASSES)

# --- Fichiers texte du dépôt ---------------------------------------------------
lister_fichiers_texte <- function(racine = ".", extensions = EXTENSIONS_DEFAUT, exclure = EXCLURE_DEFAUT, sans_git = FALSE) {
  racine <- normalizePath(racine, winslash = "/", mustWork = TRUE)
  git_ok <- !sans_git && nzchar(Sys.which("git")) && dir.exists(file.path(racine, ".git"))
  # Git : fichiers versionnés (core.quotepath=false : chemins accentués en clair) ; sinon parcours du dossier
  f <- if (git_ok) suppressWarnings(system2("git", c("-C", shQuote(racine), "-c", "core.quotepath=false", "ls-files"), stdout = TRUE, stderr = FALSE)) else
       list.files(racine, recursive = TRUE, all.files = FALSE, full.names = FALSE)
  f <- f[nzchar(f)]
  if (length(exclure)) f <- f[!vapply(strsplit(f, "/", fixed = TRUE), function(p) any(p[-length(p)] %in% exclure), TRUE)]
  f <- f[!grepl("(^|/)\\.git/", f)]
  f <- f[!grepl("renomm(er|age)[-_.]?(base[-_.]?)?bitd", basename(f), ignore.case = TRUE)]   # jamais le script lui-même, son test, son mode d'emploi, son rapport
  ext <- tolower(sub("^.*\\.([^./]+)$", "\\1", f)); ext[!grepl("\\.", basename(f))] <- ""
  f <- f[ext %in% tolower(extensions)]
  f <- f[file.exists(file.path(racine, f))]
  attr(f, "racine") <- racine; attr(f, "git") <- git_ok
  f
}

# Lecture octet par octet : BOM et fins de ligne conservés. NULL si binaire ou non UTF-8.
lire_lignes_fichier <- function(chemin) {
  n <- file.size(chemin); if (is.na(n) || n == 0) return(list(lignes = character(0), fin_nl = FALSE))
  octets <- readBin(chemin, "raw", n)
  if (any(octets == as.raw(0))) return(NULL)                      # binaire
  x <- rawToChar(octets); Encoding(x) <- "UTF-8"; if (!validUTF8(x)) return(NULL)   # non UTF-8
  fin_nl <- endsWith(x, "\n")
  lignes <- strsplit(x, "\n", fixed = TRUE)[[1]]
  if (length(lignes) == 0) lignes <- ""
  list(lignes = lignes, fin_nl = fin_nl)
}
ecrire_lignes_fichier <- function(chemin, lignes, fin_nl) {
  x <- paste(lignes, collapse = "\n"); if (fin_nl) x <- paste0(x, "\n")
  con <- file(chemin, open = "wb"); on.exit(close(con)); writeBin(charToRaw(enc2utf8(x)), con)
  invisible(chemin)
}

# --- Classement d'une occurrence d'après son contexte ---------------------------
motif_alternatives <- function(mots) paste0("(", paste(vapply(mots, function(m) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", m), ""), collapse = "|"), ")")
classer_occurrence <- function(ligne, debut, fin) {
  av <- substr(ligne, 1, debut - 1); ap <- substr(ligne, fin + 1, nchar(ligne))
  if (grepl("[[:alnum:]_]$", av, perl = TRUE) || grepl("^[[:alnum:]_]", ap, perl = TRUE))
    return(c(CLASSES[4], marquer_utf8("collé à un nom (variable, fichier, code) : jamais modifié")))
  if (grepl(paste0("^\\s+", motif_alternatives(CONTEXTES_CONSERVER), "([^[:alnum:]]|$)"), ap, perl = TRUE, ignore.case = TRUE))
    return(c(CLASSES[2], marquer_utf8("sens courant ou nom de source (contexte suivant)")))
  if (grepl(paste0("(^|[^[:alnum:]_])", motif_alternatives(CONTEXTES_AVANT), "\\s+$"), av, perl = TRUE, ignore.case = TRUE))
    return(c(CLASSES[1], marquer_utf8("contexte précédent caractéristique de la BITD")))
  if (grepl(paste0("^\\s+", motif_alternatives(CONTEXTES_APRES), "([^[:alnum:]]|$)"), ap, perl = TRUE, ignore.case = TRUE))
    return(c(CLASSES[1], marquer_utf8("contexte suivant caractéristique de la BITD")))
  if (grepl(PONCTUATION_APRES, ap, perl = TRUE))
    return(c(CLASSES[1], marquer_utf8("suivi d’une ponctuation ou d’une fin de ligne")))
  c(CLASSES[3], marquer_utf8("contexte non reconnu : décider à la main"))
}

# --- Inventaire : une ligne par occurrence --------------------------------------
inventorier_occurrences <- function(racine = ".", fichiers = NULL, remplacements = REMPLACEMENTS, extensions = EXTENSIONS_DEFAUT,
                                    exclure = EXCLURE_DEFAUT, sans_git = FALSE, silencieux = FALSE) {
  if (is.null(fichiers)) fichiers <- lister_fichiers_texte(racine, extensions, exclure, sans_git)
  racine <- normalizePath(racine, winslash = "/", mustWork = TRUE)
  motifs <- names(remplacements)[order(-nchar(names(remplacements)))]
  vide <- data.frame(fichier = character(0), ligne = integer(0), colonne = integer(0), motif = character(0), remplacement = character(0),
                     contexte = character(0), classe = character(0), decision = character(0), raison = character(0), stringsAsFactors = FALSE)
  out <- list(); ignores <- character(0)
  for (f in fichiers) {
    lu <- lire_lignes_fichier(file.path(racine, f))
    if (is.null(lu)) { ignores <- c(ignores, f); next }
    for (i in grep(motifs[nchar(motifs) == min(nchar(motifs))][1], lu$lignes, fixed = TRUE)) {
      l <- lu$lignes[i]; pris <- logical(nchar(l))
      for (m in motifs) {
        pos <- gregexpr(m, l, fixed = TRUE)[[1]]; if (pos[1] < 0) next
        for (p in pos) {
          q <- p + nchar(m) - 1L
          if (any(pris[p:q])) next
          cl <- classer_occurrence(l, p, q); pris[p:q] <- TRUE
          out[[length(out) + 1]] <- data.frame(fichier = f, ligne = i, colonne = p, motif = m, remplacement = unname(remplacements[[m]]),
                                               contexte = sub("\\r$", "", sub(paste0("^", intToUtf8(0xFEFF)), "", l)), classe = cl[1],   # ni CR ni BOM dans le contexte
                                               decision = if (cl[1] %in% CLASSES[1:2]) cl[1] else "", raison = cl[2], stringsAsFactors = FALSE)
        }
      }
    }
  }
  occ <- if (length(out)) do.call(rbind, out) else vide
  occ <- occ[order(occ$fichier, occ$ligne, occ$colonne, method = "radix"), , drop = FALSE]; rownames(occ) <- NULL   # ordre indépendant de la locale
  attr(occ, "ignores") <- ignores; attr(occ, "fichiers") <- length(fichiers); attr(occ, "racine") <- racine
  if (!silencieux) {
    cat(sprintf("Inventaire : %d fichier(s) texte lu(s), %d occurrence(s) de %s.\n", length(fichiers), nrow(occ), paste(sprintf("« %s »", motifs), collapse = " / ")))
    if (length(ignores)) cat("  Ignoré(s) (binaire ou non UTF-8) : ", paste(ignores, collapse = ", "), "\n")
    for (k in CLASSES) cat(sprintf("  %-12s %d\n", k, sum(occ$classe == k)))
    at <- occ[occ$classe == "à trancher", ]
    if (nrow(at)) { cat("  À trancher :\n"); for (j in seq_len(nrow(at))) cat(sprintf("    %s:%d  %s\n", at$fichier[j], at$ligne[j], trimws(substr(at$contexte[j], 1, 110)))) }
  }
  occ
}

# --- Rapport CSV (UTF-8 avec BOM, « ; », lisible dans Excel) et relecture des décisions ----
# Écriture et lecture indépendantes de la locale (octets UTF-8), pour Windows comme pour un R en locale C.
ecrire_rapport <- function(occ, fichier = "renommage_bitd.csv") {
  champ <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; paste0('"', gsub('"', '""', x, fixed = TRUE), '"') }
  lignes_csv <- c(paste(champ(names(occ)), collapse = ";"),
                  if (nrow(occ)) do.call(paste, c(lapply(occ, champ), sep = ";")))
  con <- file(fichier, open = "wb"); on.exit(close(con))
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(enc2utf8(paste0(paste(lignes_csv, collapse = "\n"), "\n")))), con)   # BOM explicite (octets)
  invisible(fichier)
}
lire_decisions <- function(fichier) {
  if (!file.exists(fichier)) stop("Fichier de décisions introuvable : ", fichier)
  octets <- readBin(fichier, "raw", file.size(fichier))
  if (length(octets) >= 3 && identical(octets[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) octets <- octets[-(1:3)]   # BOM
  brut <- rawToChar(octets); Encoding(brut) <- "UTF-8"
  if (!validUTF8(brut)) brut <- iconv(brut, "windows-1252", "UTF-8")      # Excel peut réenregistrer en ANSI
  d <- utils::read.csv2(text = brut, colClasses = "character", check.names = FALSE, na.strings = character(0), strip.white = TRUE, encoding = "UTF-8")
  requis <- c("fichier", "ligne", "colonne", "motif", "decision")
  manque <- setdiff(requis, names(d)); if (length(manque)) stop("Colonnes absentes du fichier de décisions : ", paste(manque, collapse = ", "))
  d$ligne <- as.integer(d$ligne); d$colonne <- as.integer(d$colonne); d$decision <- tolower(trimws(d$decision))
  d$decision[d$decision %in% c("a trancher", "à trancher", "na")] <- ""
  mauvaises <- setdiff(unique(d$decision), c("remplacer", "conserver", ""))
  if (length(mauvaises)) stop("Décision(s) inconnue(s) (attendu : remplacer, conserver ou vide) : ", paste(mauvaises, collapse = ", "))
  d
}

# --- Application : exactement les occurrences décidées ----------------------------
appliquer_renommage <- function(racine = ".", decisions = NULL, remplacements = REMPLACEMENTS, extensions = EXTENSIONS_DEFAUT, exclure = EXCLURE_DEFAUT,
                                sans_git = FALSE, force = FALSE, silencieux = FALSE) {
  fichiers <- lister_fichiers_texte(racine, extensions, exclure, sans_git); racine <- attr(fichiers, "racine")
  occ <- inventorier_occurrences(racine, fichiers, remplacements, silencieux = TRUE)
  cle <- function(d) paste(d$fichier, d$ligne, d$colonne, d$motif, sep = "\r")
  if (!is.null(decisions)) {
    if (is.character(decisions)) decisions <- lire_decisions(decisions)
    absentes <- setdiff(cle(decisions), cle(occ))
    if (length(absentes)) stop("Le dépôt a changé depuis l'inventaire : ", length(absentes), " occurrence(s) du fichier de décisions introuvable(s) à leur position (ex. ",
                               paste(head(gsub("\r", ":", absentes), 5), collapse = " ; "), "). Refaites l'inventaire.")
    occ$decision <- decisions$decision[match(cle(occ), cle(decisions))]; occ$decision[is.na(occ$decision)] <- ""
    nouvelles <- occ$classe == "à trancher" & occ$decision == "" & !cle(occ) %in% cle(decisions)
    if (any(nouvelles)) stop("Le dépôt a changé depuis l'inventaire : ", sum(nouvelles), " nouvelle(s) occurrence(s) à trancher. Refaites l'inventaire.")
  }
  ident <- occ$classe == "identifiant" & occ$decision == "remplacer"
  if (any(ident)) { warning(sum(ident), " occurrence(s) collée(s) à un identifiant marquée(s) « remplacer » : jamais modifiées, décision ignorée."); occ$decision[ident] <- "" }
  sans <- occ$classe == "à trancher" & occ$decision == ""
  if (any(sans) && !force) stop(sum(sans), " occurrence(s) « à trancher » sans décision : complétez la colonne decision du rapport (--decisions=…) ou passez --force pour les conserver.")
  a_faire <- occ[occ$decision == "remplacer", , drop = FALSE]
  if (attr(fichiers, "git") && !force && nrow(a_faire)) {
    st <- suppressWarnings(system2("git", c("-C", shQuote(racine), "status", "--porcelain", "--", shQuote(unique(a_faire$fichier))), stdout = TRUE, stderr = FALSE))
    if (length(st)) stop("Modifications non commitées dans ", length(st), " fichier(s) visé(s) : commitez (ou --force) pour que git diff ne montre que le renommage.")
  }
  modifies <- character(0)
  for (f in unique(a_faire$fichier)) {
    lu <- lire_lignes_fichier(file.path(racine, f)); if (is.null(lu)) next
    d <- a_faire[a_faire$fichier == f, ]; d <- d[order(d$ligne, -d$colonne), ]      # de droite à gauche : les positions restent valides
    for (j in seq_len(nrow(d))) {
      l <- lu$lignes[d$ligne[j]]; p <- d$colonne[j]; q <- p + nchar(d$motif[j]) - 1L
      if (substr(l, p, q) != d$motif[j]) stop("Incohérence interne : ", f, ":", d$ligne[j], " le motif n'est plus à sa place.")
      lu$lignes[d$ligne[j]] <- paste0(substr(l, 1, p - 1L), d$remplacement[j], substr(l, q + 1L, nchar(l)))
    }
    ecrire_lignes_fichier(file.path(racine, f), lu$lignes, lu$fin_nl); modifies <- c(modifies, f)
  }
  bilan <- list(fichiers_modifies = modifies, remplacees = nrow(a_faire), conservees = sum(occ$decision == "conserver"),
                forcees = sum(sans), identifiants = sum(occ$classe == "identifiant"), occurrences = occ)
  if (!silencieux) {
    cat(sprintf("Application : %d occurrence(s) remplacée(s) dans %d fichier(s) ; %d conservée(s) ; %d collée(s) à un identifiant (intactes)%s.\n",
                bilan$remplacees, length(modifies), bilan$conservees, bilan$identifiants, if (bilan$forcees) sprintf(" ; %d « à trancher » conservée(s) (--force)", bilan$forcees) else ""))
    verifier_apres(racine, remplacements, extensions, exclure, sans_git, modifies)
  }
  invisible(bilan)
}

# --- Vérification après application --------------------------------------------
verifier_apres <- function(racine = ".", remplacements = REMPLACEMENTS, extensions = EXTENSIONS_DEFAUT, exclure = EXCLURE_DEFAUT, sans_git = FALSE, modifies = character(0)) {
  fichiers <- lister_fichiers_texte(racine, extensions, exclure, sans_git); racine <- attr(fichiers, "racine")
  reste <- inventorier_occurrences(racine, fichiers, remplacements, silencieux = TRUE)
  cat(sprintf("Vérification : il reste %d occurrence(s) (%s).\n", nrow(reste), paste(sprintf("%s %d", CLASSES, vapply(CLASSES, function(k) sum(reste$classe == k), 0L)), collapse = ", ")))
  cible <- unique(unname(remplacements)); doublons <- character(0); casse <- character(0)
  for (f in fichiers) { lu <- lire_lignes_fichier(file.path(racine, f)); if (is.null(lu)) next
    for (c in cible) { if (any(grepl(paste(c, c), lu$lignes, fixed = TRUE))) doublons <- c(doublons, f)
      if (any(grepl(paste0("\\b", c, "\\b"), lu$lignes, ignore.case = TRUE, perl = TRUE) & !grepl(c, lu$lignes, fixed = TRUE))) casse <- c(casse, f) } }
  if (length(doublons)) cat("  Doublon « ", paste(cible, cible), " » dans : ", paste(unique(doublons), collapse = ", "), "\n", sep = "")
  if (length(casse)) cat("  Casse différente de « ", paste(cible, collapse = " / "), " » dans : ", paste(unique(casse), collapse = ", "), "\n", sep = "")
  gen <- modifies[grepl("^utils/.*gen_.*\\.js$|\\.svg$", modifies)]
  cat("À faire ensuite : git diff (relire chaque ligne), suite de tests", if (length(gen)) ", régénérer les images des générateurs modifiés (SVG / PNG)" else "", ".\n", sep = "")
  invisible(reste)
}

# --- Ligne de commande ---------------------------------------------------------
arguments_ligne_commande <- function(args = commandArgs(trailingOnly = TRUE)) {
  o <- list(racine = ".", rapport = "renommage_bitd.csv", decisions = NULL, appliquer = FALSE, extensions = EXTENSIONS_DEFAUT, exclure = EXCLURE_DEFAUT, sans_git = FALSE, force = FALSE)
  for (a in args) {
    cle <- sub("^--([^=]+)(=.*)?$", "\\1", a); val <- if (grepl("=", a, fixed = TRUE)) sub("^--[^=]+=", "", a) else NA
    switch(cle, racine = o$racine <- val, rapport = o$rapport <- val, decisions = o$decisions <- val, appliquer = o$appliquer <- TRUE,
           extensions = o$extensions <- strsplit(val, ",", fixed = TRUE)[[1]], exclure = o$exclure <- if (is.na(val) || !nzchar(val)) character(0) else strsplit(val, ",", fixed = TRUE)[[1]],
           "sans-git" = o$sans_git <- TRUE, force = o$force <- TRUE, stop("Option inconnue : ", a, " (voir l'en-tête du script)."))
  }
  o
}
if (sys.nframe() == 0L) {
  o <- arguments_ligne_commande()
  if (!o$appliquer) {
    occ <- inventorier_occurrences(o$racine, remplacements = REMPLACEMENTS, extensions = o$extensions, exclure = o$exclure, sans_git = o$sans_git)
    ecrire_rapport(occ, o$rapport)
    cat("Rapport écrit : ", o$rapport, " — rien n'a été modifié. Complétez la colonne decision, puis : --appliquer --decisions=", o$rapport, "\n", sep = "")
  } else {
    appliquer_renommage(o$racine, o$decisions, REMPLACEMENTS, o$extensions, o$exclure, o$sans_git, o$force)
  }
}
