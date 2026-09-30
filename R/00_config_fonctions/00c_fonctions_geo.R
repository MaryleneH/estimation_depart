# ==============================================================================
# 00c_fonctions_geo.R — Couche géographique GÉNÉRIQUE (fonctions pures)
# ------------------------------------------------------------------------------
# Rôle : isoler tout ce qui dépend du ZONAGE (zone d'emploi, département,
#        région...) et du SCHÉMA du fichier reçu, pour que les scripts métier
#        (01, 04, 08) ne connaissent qu'un contrat interne stable :
#
#            geo_code   character, TOUJOURS (jamais numérique) — identifiant
#            geo_nom    character — libellé affiché (= code si inconnu, tracé)
#            geo_type   character — zonage d'analyse (GEO_ANALYSE)
#
#        Les noms réels des colonnes source (DEP, CODE_DEP, ze, codgeo...) ne
#        franchissent jamais cette couche : ils vivent dans COL_GEO (00_config).
#
# Fonctions (aucune dépendance nouvelle : dplyr, readr, arrow déjà requis) :
#   garde_migration_geo()      arrêt explicite si l'ancienne config ZE traîne
#   zonage_geo(type)           libellés/suffixe d'un zonage (GEO_ZONAGES)
#   fichier_sortie_geo(gabarit) nom de fichier de sortie suffixé par le zonage
#   inspecter_schema(fichier)  diagnostic du schéma Parquet AVANT configuration
#   controler_colonnes_geo()   erreur explicite si COL_GEO vise une colonne absente
#   charger_bts_parquet()      lecture Arrow LAZY (filtres + select poussés)
#   normaliser_codes_geo()     codes en texte, zéros à gauche si demandé
#   lire_referentiel_geo()     référentiel local  code;nom
#   lire_passage_geo()         table de passage locale  code_source;code_cible
#   normaliser_geo()           -> contrat geo_code / geo_nom / geo_type
#   codes_geo_interet(), libelles_geo_interet(), filtrer_geo_interet()
#
# Sourcé par main.R juste après 00_config.R ; sourçable seul par les tests.
# ==============================================================================
library(dplyr)

# --- Garde de migration -------------------------------------------------------
# Les anciens paramètres (spécifiques ZE) ne sont plus lus : mieux vaut un
# arrêt clair qu'une config silencieusement ignorée.
garde_migration_geo <- function(env = globalenv()) {
  anciens <- c("COL_GEO_BTS", "GEO_NIVEAU", "LIBELLE_ZE", "FICHIER_LIBELLES_ZE",
               "FICHIER_COMMUNE_ZE", "ZE_INTERET", "LISTE_ZE")
  presents <- anciens[vapply(anciens, exists, logical(1),
                             envir = env, inherits = FALSE)]
  if (length(presents) > 0)
    stop("Paramètre(s) géographique(s) OBSOLÈTE(S) dans 00_config.R : ",
         paste(presents, collapse = ", "),
         ".\nLa géographie est désormais générique : remplacez-les par le bloc ",
         "GEO_* (GEO_ANALYSE, GEO_SOURCE, COL_GEO, GEO_CODE_LARGEUR, GEO_INTERET, ",
         "GEO_ZONAGES, GEO_REFERENTIELS, GEO_PASSAGES) — voir 00_config.R. ",
         "Ex. : ZE_INTERET devient GEO_INTERET (mêmes valeurs).")
  invisible(TRUE)
}

# --- Zonage : libellés de restitution et suffixe de fichiers ------------------
zonage_geo <- function(type = GEO_ANALYSE, zonages = GEO_ZONAGES) {
  if (!type %in% names(zonages))
    stop("Zonage '", type, "' inconnu de GEO_ZONAGES (connus : ",
         paste(names(zonages), collapse = ", "), "). Ajoutez-le dans 00_config.R.")
  z <- zonages[[type]]
  requis <- c("libelle", "un", "pluriel", "suffixe")
  if (!all(requis %in% names(z)))
    stop("GEO_ZONAGES$", type, " : champs attendus ", paste(requis, collapse = ", "))
  z$type <- type
  z
}

# Gabarit avec un %s à la place du suffixe : "analyse_55plus_par_%s.csv"
fichier_sortie_geo <- function(gabarit, type = GEO_ANALYSE, dir = DIR_SORTIES)
  file.path(dir, sprintf(gabarit, zonage_geo(type)$suffixe))

# --- Inspection du schéma : à lancer AVANT de renseigner COL_GEO --------------
# Affiche toutes les colonnes, leur type Arrow et quelques valeurs distinctes.
# Met en évidence (à titre INFORMATIF) les colonnes numériques — risque de
# zéros à gauche perdus — et celles dont le nom évoque une géographie. Ne
# choisit RIEN : inspection -> configuration explicite -> traitement.
inspecter_schema <- function(fichier = FICHIER_BTS, n_exemples = 5, n_lignes = 5000) {
  if (!requireNamespace("arrow", quietly = TRUE))
    stop("Le package 'arrow' est requis pour inspecter un Parquet.")
  if (!file.exists(fichier)) stop("Fichier introuvable : ", fichier)
  ds    <- arrow::open_dataset(fichier)
  noms  <- names(ds)
  types <- vapply(noms, function(n) ds$schema$GetFieldByName(n)$type$ToString(),
                  character(1))
  ech   <- ds |> head(n_lignes) |> collect()
  exemples <- vapply(noms, function(n)
    paste(head(unique(as.character(ech[[n]])), n_exemples), collapse = " | "),
    character(1))
  tab <- tibble(
    colonne      = noms,
    type         = unname(types),
    numerique    = grepl("int|double|float|decimal", types, ignore.case = TRUE),
    geo_possible = grepl("geo|dep|ze|zone|comm|reg|epci|codgeo|insee|etab",
                         noms, ignore.case = TRUE),
    exemples     = unname(exemples))
  cat("\n=== Schéma de", basename(fichier), "—", length(noms), "colonnes ===\n")
  print(as.data.frame(tab |> select(colonne, type, exemples)), row.names = FALSE)
  if (any(tab$geo_possible))
    cat("\nColonnes dont le NOM évoque une géographie (à vérifier, aucun choix automatique) : ",
        paste(tab$colonne[tab$geo_possible], collapse = ", "), "\n")
  if (any(tab$numerique & tab$geo_possible))
    cat("Attention : colonne(s) géographique(s) NUMÉRIQUE(S) — les zéros à gauche (« 01 », « 09 ») ",
        "ont pu être perdus ; renseignez GEO_CODE_LARGEUR.\n")
  cat("\nÉtape suivante : renseigner COL_GEO, GEO_SOURCE, GEO_ANALYSE dans 00_config.R.\n")
  invisible(tab)
}

# --- Contrôle des colonnes géographiques déclarées ---------------------------
controler_colonnes_geo <- function(noms_presents, col_geo) {
  col_geo <- col_geo[!is.na(col_geo)]
  if (length(col_geo) == 0)
    stop("COL_GEO ne déclare aucune colonne (code et nom vides) : renseignez au ",
         "moins COL_GEO[['code']] dans 00_config.R après inspecter_schema().")
  absentes <- col_geo[!col_geo %in% noms_presents]
  if (length(absentes) > 0)
    stop(sprintf(paste0(
      "Colonne géographique configurée '%s' absente du fichier.\n\n",
      "Colonnes présentes :\n  %s\n\n",
      "Renseignez COL_GEO dans 00_config.R après inspection du schéma ",
      "(inspecter_schema())."),
      paste(absentes, collapse = "', '"), paste(noms_presents, collapse = ", ")))
  invisible(TRUE)
}

# --- Lecture Arrow LAZY de la BTS réelle -------------------------------------
# Filtres (âge, SIREN) et sélection de colonnes POUSSÉS avant collect() : seules
# les colonnes du contrat + la (les) colonne(s) géographique(s) déclarée(s)
# sont matérialisées. Renommage GÉNÉRIQUE vers geo_code / geo_nom.
charger_bts_parquet <- function(fichier, col_bts, col_geo, age_min, sirens) {
  if (!requireNamespace("arrow", quietly = TRUE))
    stop("Le package 'arrow' est requis pour SOURCE_BTS='parquet' : install.packages('arrow').")
  if (!file.exists(fichier)) stop("Fichier BTS introuvable : ", fichier)
  ds <- arrow::open_dataset(fichier)             # LAZY : ne lit rien encore
  manquantes <- setdiff(unname(col_bts), names(ds))
  if (length(manquantes) > 0)
    stop("Colonnes absentes du Parquet : ", paste(manquantes, collapse = ", "),
         " (schéma réel : ", paste(names(ds), collapse = ", "),
         "). Ajustez COL_BTS dans 00_config.R.")
  controler_colonnes_geo(names(ds), col_geo)
  col_geo <- col_geo[!is.na(col_geo)]
  # contrat : nom interne = nom source (vecteur nommé pour rename(all_of()))
  contrat <- c(siren = unname(col_bts[["siren"]]), sexe = unname(col_bts[["sexe"]]),
               age_2024 = unname(col_bts[["age"]]), pcs = unname(col_bts[["pcs"]]))
  if ("code" %in% names(col_geo)) contrat <- c(contrat, geo_code = unname(col_geo[["code"]]))
  if ("nom"  %in% names(col_geo)) contrat <- c(contrat, geo_nom  = unname(col_geo[["nom"]]))
  col_age <- col_bts[["age"]]; col_siren <- col_bts[["siren"]]
  ds |>
    filter(.data[[col_age]] >= age_min,           # filtre âge poussé au disque
           .data[[col_siren]] %in% sirens) |>     # filtre périmètre BITD
    select(all_of(unname(contrat))) |>            # colonnes utiles seulement
    collect() |>                                  # MATÉRIALISATION (RAM) ici
    rename(all_of(contrat)) |>
    mutate(id = sprintf("ID%08d", row_number()),
           generation = 2024 - age_2024)          # exact si AGE = âge ds l'année
}

# --- Codes : toujours en texte, zéros à gauche si demandé --------------------
normaliser_codes_geo <- function(x, largeur = NA) {
  x <- trimws(reparer_utf8(x))
  x[!is.na(x) & x == ""] <- NA_character_
  if (!is.na(largeur)) {
    court <- !is.na(x) & grepl("^[0-9]+$", x) & nchar(x) < largeur
    x[court] <- formatC(as.integer(x[court]), width = largeur, flag = "0")
  }
  x
}

# --- Référentiels locaux -----------------------------------------------------
# Référentiel  code;nom  (séparateur ';', tout lu en texte). NULL si absent :
# un référentiel manquant n'est jamais bloquant (repli : code affiché, tracé).
# Robustesse d'encodage : un référentiel ou une table de passage enregistré
# depuis Excel / un éditeur Windows arrive souvent en Windows-1252 (latin1).
# Toute chaîne invalide en UTF-8 est réparée depuis latin1 ; les chaînes déjà
# valides ne sont pas touchées. Jamais d'erreur « invalid UTF-8 » sur un libellé.
reparer_utf8 <- function(x) {
  x <- as.character(x)
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
  x
}

lire_referentiel_geo <- function(chemin) {
  if (is.null(chemin) || is.na(chemin) || !file.exists(chemin)) return(NULL)
  ref <- readr::read_delim(chemin, delim = ";", show_col_types = FALSE,
                           col_types = readr::cols(.default = "c"))
  if (!all(c("code", "nom") %in% names(ref)))
    stop("Référentiel ", chemin, " : colonnes attendues  code;nom  (trouvées : ",
         paste(names(ref), collapse = ", "), ").")
  ref |>
    transmute(code = trimws(reparer_utf8(code)), nom = trimws(reparer_utf8(nom))) |>
    filter(!is.na(code), code != "") |>
    distinct(code, .keep_all = TRUE)
}

# Table de passage  code_source;code_cible[;nom_cible]. BLOQUANTE si absente :
# on ne peut pas analyser un zonage qu'on ne sait pas construire.
# Une table de passage doit être SANS AMBIGUÏTÉ : chaque code_source exactement
# une fois, aucun code vide, libellé cible présent si exigé. Tout écart ARRÊTE
# la chaîne avec la liste des codes concernés (jamais de doublon silencieux).
lire_passage_geo <- function(chemin, cle, nom_requis = FALSE) {
  if (is.null(chemin) || is.na(chemin) || !file.exists(chemin))
    stop("Table de passage '", cle, "' introuvable",
         if (!is.null(chemin) && !is.na(chemin)) paste0(" : ", chemin) else
           " (aucun chemin dans GEO_PASSAGES)",
         " — attendu : code_source;code_cible[;nom_cible].")
  p <- readr::read_delim(chemin, delim = ";", show_col_types = FALSE,
                         col_types = readr::cols(.default = "c"))
  if (!all(c("code_source", "code_cible") %in% names(p)))
    stop("Table de passage ", chemin, " : colonnes attendues  code_source;code_cible",
         "[;nom_cible]  (trouvées : ", paste(names(p), collapse = ", "), ").")
  if (!"nom_cible" %in% names(p)) p$nom_cible <- NA_character_
  p <- p |>
    transmute(code_source = trimws(reparer_utf8(code_source)), code_cible = trimws(reparer_utf8(code_cible)),
              nom_cible = trimws(reparer_utf8(nom_cible)))
  vide <- function(x) is.na(x) | x == ""
  if (any(vide(p$code_source)))
    stop("Table de passage '", cle, "' : ", sum(vide(p$code_source)), " ligne(s) avec un code_source vide.")
  if (any(vide(p$code_cible)))
    stop("Table de passage '", cle, "' : code_cible vide pour : ",
         paste(p$code_source[vide(p$code_cible)], collapse = ", "), ".")
  dbl <- unique(p$code_source[duplicated(p$code_source)])
  if (length(dbl) > 0)
    stop("Table de passage '", cle, "' : code_source en double (un code doit avoir UNE cible) : ",
         paste(dbl, collapse = ", "), ".")
  if (nom_requis && any(vide(p$nom_cible)))
    stop("Table de passage '", cle, "' : nom_cible manquant pour : ",
         paste(p$code_source[vide(p$nom_cible)], collapse = ", "), ".")
  p
}

# --- Extension du contrat : région administrative à partir du département ------
# Ajoute region_code / region_nom (texte) SANS toucher à geo_code : la table de
# passage  departement->region  (code_source;code_cible;nom_cible) est lue et
# contrôlée par lire_passage_geo(). Un territoire « inconnu » reste inconnu et
# compté ; un vrai code de département absent de la table = référentiel
# incomplet -> ARRÊT (aucune affectation inventée).
ajouter_region <- function(df, chemin_passage, prefixe = "geo") {
  if (!all(c("geo_code", "geo_type") %in% names(df)))
    stop("ajouter_region : contrat geo_code / geo_type requis.")
  if (!identical(unique(as.character(df$geo_type)), "departement"))
    stop("ajouter_region : geo_type doit valoir \"departement\" (reçu : ",
         paste(unique(df$geo_type), collapse = ", "), ").")
  p <- lire_passage_geo(chemin_passage, "departement->region", nom_requis = TRUE)
  code <- as.character(df$geo_code)
  inconnu <- is.na(code) | code == "inconnu"
  idx <- match(code, p$code_source)
  sans <- is.na(idx) & !inconnu
  if (any(sans))
    stop(prefixe, " : ", sum(sans), " salarié(s) dans ", length(unique(code[sans])),
         " département(s) sans région dans la table de passage : ",
         paste(sort(unique(code[sans])), collapse = ", "),
         " — complétez data/passage_departement_region.csv.")
  df$region_code <- ifelse(inconnu, "inconnu", p$code_cible[idx])
  df$region_nom  <- ifelse(inconnu, "Région inconnue", p$nom_cible[idx])
  message(prefixe, " : région rattachée pour ", format(sum(!inconnu), big.mark = " "), " salarié(s) (",
          length(unique(df$region_code[!inconnu])), " régions) ; ", sum(inconnu), " sans territoire connu.")
  df
}

# --- Périmètre d'intérêt (GEO_INTERET) ----------------------------------------
# Vecteur de codes, OU vecteur nommé  code = libellé  (sert alors aussi de
# table de libellés, sans fichier).
est_nomme <- function(v) !is.null(names(v)) && any(nzchar(names(v)))
codes_geo_interet <- function(geo_interet) {
  if (is.null(geo_interet)) return(NULL)
  trimws(if (est_nomme(geo_interet)) names(geo_interet)
         else unname(as.character(geo_interet)))
}
libelles_geo_interet <- function(geo_interet) {
  if (is.null(geo_interet) || !est_nomme(geo_interet)) return(NULL)
  stats::setNames(unname(as.character(geo_interet)), trimws(names(geo_interet)))
}

filtrer_geo_interet <- function(df, geo_interet, prefixe = "geo") {
  codes <- codes_geo_interet(geo_interet)
  if (is.null(codes)) return(df)
  introuvables <- setdiff(codes, unique(df$geo_code))
  if (length(introuvables) > 0)
    warning("GEO_INTERET : ", length(introuvables), " code(s) absent(s) des ",
            "données : ", paste(introuvables, collapse = ", "))
  n_avant <- nrow(df)
  df <- df |> filter(geo_code %in% codes)
  if (nrow(df) == 0)
    stop("GEO_INTERET : aucune correspondance avec geo_code — vérifiez les codes ",
         "dans 00_config.R (texte, zéros initiaux compris).")
  message(prefixe, " : périmètre restreint à ", n_distinct(df$geo_code),
          " territoire(s) de GEO_INTERET — ", n_avant - nrow(df),
          " observation(s) hors liste écartée(s) de l'analyse.")
  df
}

# --- Normalisation vers le contrat geo_code / geo_nom / geo_type -------------
# Entrée : table avec geo_code et/ou geo_nom (issus du renommage générique).
# Cas gérés :
#   A. code seul      -> nom complété par le référentiel (sinon = code, tracé)
#   B. code + nom     -> les deux, nom vide complété par le référentiel
#   C. nom seul       -> code par correspondance inverse dans le référentiel ;
#                        sinon le libellé sert d'identifiant (AVERTI, jamais
#                        d'identifiant fabriqué en silence)
#   D. source plus fine que l'analyse -> table de passage GEO_PASSAGES
# Priorité des libellés : nom source > référentiel > GEO_INTERET nommé > code.
normaliser_geo <- function(df, geo_source, geo_analyse, largeur = NA,
                           referentiels = list(), passages = list(),
                           geo_interet = NULL, zonages = GEO_ZONAGES,
                           prefixe = "geo") {
  zonage_geo(geo_analyse, zonages)                 # valide le zonage demandé
  a_code <- "geo_code" %in% names(df)
  a_nom  <- "geo_nom"  %in% names(df)
  if (!a_code && !a_nom)
    stop("normaliser_geo : ni geo_code ni geo_nom dans la table (COL_GEO vide ?).")

  if (a_code) {
    if (is.numeric(df$geo_code) && is.na(largeur))
      warning("Colonne code géographique NUMÉRIQUE : les zéros à gauche ont pu ",
              "être perdus (« 01 » lu 1). Renseignez GEO_CODE_LARGEUR dans 00_config.R.")
    df$geo_code <- normaliser_codes_geo(df$geo_code, largeur)
  }
  if (a_nom) {
    df$geo_nom <- trimws(reparer_utf8(df$geo_nom))
    df$geo_nom[!is.na(df$geo_nom) & df$geo_nom == ""] <- NA_character_
  } else df$geo_nom <- NA_character_

  ref_source <- lire_referentiel_geo(referentiels[[geo_source]])

  # C. nom seul : code par correspondance inverse, sinon nom = identifiant
  if (!a_code) {
    if (!is.null(ref_source)) {
      df$geo_code <- ref_source$code[match(df$geo_nom, ref_source$nom)]
      n_na <- sum(is.na(df$geo_code) & !is.na(df$geo_nom))
      if (n_na > 0)
        warning(n_na, " observation(s) dont le libellé n'est pas dans le référentiel '",
                geo_source, "' : le libellé sert d'identifiant.")
      df$geo_code <- coalesce(df$geo_code, df$geo_nom)
    } else {
      warning("Aucune colonne code et aucun référentiel '", geo_source,
              "' : le libellé sert d'identifiant (geo_code = geo_nom). ",
              "Déposez un référentiel  code;nom  pour disposer de vrais codes.")
      df$geo_code <- df$geo_nom
    }
  }

  # D. passage géographie source -> géographie d'analyse
  if (geo_source != geo_analyse) {
    cle <- paste0(geo_source, "->", geo_analyse)
    p   <- lire_passage_geo(passages[[cle]], cle)
    idx <- match(df$geo_code, p$code_source)
    n_sans <- sum(is.na(idx) & !is.na(df$geo_code))
    df$geo_code <- p$code_cible[idx]
    df$geo_nom  <- p$nom_cible[idx]
    if (n_sans > 0)
      message(prefixe, " : ", n_sans, " observation(s) sans correspondance ", cle,
              " (territoire d'analyse inconnu, regroupées en « inconnu »).")
  }

  # Libellés : référentiel du zonage d'analyse, puis GEO_INTERET nommé, puis code
  ref_analyse <- lire_referentiel_geo(referentiels[[geo_analyse]])
  if (!is.null(ref_analyse)) {
    manque <- is.na(df$geo_nom)
    df$geo_nom[manque] <- ref_analyse$nom[match(df$geo_code[manque], ref_analyse$code)]
  }
  noms_interet <- libelles_geo_interet(geo_interet)
  if (!is.null(noms_interet)) {
    idx <- match(df$geo_code, names(noms_interet)); ok <- !is.na(idx)
    if (any(ok)) {
      df$geo_nom[ok] <- unname(noms_interet[idx[ok]])
      message(prefixe, " : libellés appliqués depuis GEO_INTERET (",
              length(unique(df$geo_code[ok])), " territoire(s), ", sum(ok), " observations).")
    }
  }
  sans_nom <- is.na(df$geo_nom) & !is.na(df$geo_code)
  if (any(sans_nom)) {
    message(prefixe, " : ", sum(sans_nom), " observation(s) sans libellé (",
            length(unique(df$geo_code[sans_nom])), " code(s)) — le code est affiché.")
    df$geo_nom[sans_nom] <- df$geo_code[sans_nom]
  }
  df$geo_type <- geo_analyse
  df
}
