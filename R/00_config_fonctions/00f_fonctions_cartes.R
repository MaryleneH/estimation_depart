# ==============================================================================
# 00f_fonctions_cartes.R — Dataviz des résultats DIFFUSABLES (fonctions pures)
# ------------------------------------------------------------------------------
# Rôle : transformer une table de DIFFUSION (departs_pcs / departs_cs1, niveau
#        france / région / département) en une page HTML autonome (SVG inline,
#        JavaScript natif, aucune ressource externe, aucune connexion) :
#          - région / département : carte choroplèthe interactive (vue
#            « Résultats » par catégorie, vue « Diffusabilité ») + PNG ggplot2 ;
#          - France entière : TABLEAU DE BORD national de chiffres clés
#            (aucune carte : une valeur nationale ne varie pas entre territoires).
# RÈGLE DE CONCEPTION : le CHAMP de l'étude est rappelé en permanence, construit
#        par construire_libelle_champ() depuis la configuration réelle
#        (AGE_MIN_BTS, PCS_VERS_CS1, PCS_HORS_CHAMP, périmètre SIREN BITD,
#        année de référence, horizon) — jamais écrit en dur dans le HTML.
#        Champ / territoire / catégorie / indicateur sont quatre notions
#        distinctes et distinguées à l'écran.
# SOURCE ABSOLUE : la table de diffusion. Une cellule masquée (masque = TRUE)
#        n'embarque que son statut ; toute valeur en est retirée AVANT
#        sérialisation ; une table sans colonne masque est refusée.
# Trois statuts : diffuse (couleur de classe, bornes arrondies lisibles, échelle
#        commune à tous les territoires de la catégorie affichée), masque (gris
#        COULEURS_CARTE$secret « Non diffusé — secret statistique »), sans_donnee
#        (blanc, contour pointillé « Pas de donnée observée », jamais zéro).
# Fond : data/cartographie/ (local, jsonlite), métropole en Lambert-93, DROM en
#        encarts. Dépendances : jsonlite, ggplot2 (PNG), dplyr. Ni sf, ni CDN.
# ==============================================================================
library(dplyr)

# --- Constantes (couleurs et libellés UI : sources uniques) --------------------
COULEURS_CARTE <- list(
  classes      = c("#deebf7", "#9ecae1", "#4292c6", "#2171b5", "#08306b"),  # séquentielle bleue (ColorBrewer Blues)
  secret       = "#b3b3b3",   # gris RÉSERVÉ au secret statistique (catégorie cartographique)
  sans_donnee  = "#ffffff",   # blanc + contour pointillé : pas de donnée observée
  contour_sans = "#9a9a9a",
  encre = "#1F2933", texte = "#3E4C59", filet = "#D9DEE5", fond = "#F5F7FA", bleu = "#1e3a5f")
# Contours des territoires : SOURCE UNIQUE pour les quatre cartes (PCS / CS1 ×
# région / département), le SVG interactif et le PNG. Gris moyen fin pour que
# deux voisins de même classe (ou deux voisins masqués) restent distincts ;
# limites régionales plus foncées et plus épaisses sur la carte départementale ;
# survol : contour sombre dessiné AU-DESSUS des voisins. Épaisseurs en pixels
# écran (vector-effect non-scaling-stroke) : identiques sur desktop et mobile.
STYLE_CONTOURS_CARTE <- list(
  couleur = "#5B6470", largeur = 1,              # limite entre territoires : gris moyen-foncé, 1 px (lisible sur les bleus moyens et sur le gris du secret)
  sans_couleur = "#7B8794", sans_largeur = 0.8,  # pas de donnée : pointillé plus clair, distinct du secret
  region_couleur = "#2F3945", region_largeur = 1.5,   # limites régionales sur la carte départementale (plus foncées, plus épaisses)
  survol_couleur = "#111827", survol_largeur = 2.4,   # territoire survolé / focus
  mobile_facteur = 0.7,                               # écran <= 640 px : toutes les épaisseurs × 0,7 (carte plus petite)
  png_couleur = "#5B6470", png_largeur = 0.3, png_region_largeur = 0.55)   # ggplot2 (linewidth en mm)
# Libellés utilisateur : langage SIMPLE pour un public non statisticien. AUCUN
# nom de colonne R, ni « champ », « effectif », « taux » (hors légende /
# sélecteur), « diffusabilité ». Les phrases dépendant de l'âge sont
# complétées par construire_libelles_public() depuis la configuration.
LIBELLES_UI <- list(
  indicateurs = c(taux = "Part des salariés susceptibles de partir", departs = "Nombre de départs estimés", effectif = "Nombre de salariés"),
  scenarios   = c(b = "Estimation basse", c = "Estimation centrale", h = "Estimation haute"),
  causes      = c(r = "Retraite ou fin de carrière", i = "Invalidité", d = "Décès"),
  secret      = "Résultat non diffusé — secret statistique",
  secret_court = "Résultat non diffusé", secret_sous = "Secret statistique",
  secret_explication = "Ce résultat ne peut pas être affiché : il concerne trop peu de salariés ou d’entreprises, ou une seule entreprise y pèse trop.",
  sans_donnee = "Aucun salarié observé",
  vues        = c(resultats = "Résultats", affichables = "Résultats affichables"),
  couverture  = list(bornes = c(1, 25, 75, 100),
                     libelles = c("Peu de résultats affichables (1 à 24 %)", "Environ la moitié (25 à 74 %)",
                                  "La plupart (75 à 99 %)", "Tous les résultats affichables (100 %)"),
                     aucune = "Aucun résultat affichable — secret statistique",
                     titre = "Où peut-on afficher les résultats ?"),
  kpi         = c(departs = "départs estimés d’ici 2030", taux = "pourraient partir d’ici 2030", salaries = "salariés concernés"),
  denominateur = "Part des salariés de cette catégorie.")

COLONNES_MESURES_CARTE <- c(e = "effectif_champ", c = "departs_central", b = "departs_bas", h = "departs_haut",
                            t = "taux_depart_central_pct", r = "dep_retraite", i = "dep_invalidite", d = "dep_deces")
DROM_CARTE <- c("971" = "Guadeloupe", "972" = "Martinique", "973" = "Guyane", "974" = "La Réunion", "976" = "Mayotte")
DROM_REGION_CARTE <- c("01" = "971", "02" = "972", "03" = "973", "04" = "974", "06" = "976")   # région DROM -> département

# --- QUI EST CONCERNÉ ? source unique, construite depuis la configuration -------
# Lu au moment de l'appel (jamais figé) : un changement d'AGE_MIN_BTS ou de
# PCS_VERS_CS1 change toutes les pages. Langage simple, sans le mot « champ ».
construire_libelle_champ <- function(age_min = get0("AGE_MIN_BTS", ifnotfound = NA),
                                     cs = get0("PCS_VERS_CS1", ifnotfound = NULL),
                                     hors = get0("PCS_HORS_CHAMP", ifnotfound = character(0)),
                                     annee = get0("ANNEE_REF_GRAPHIQUE", ifnotfound = 2024),
                                     horizon = get0("HORIZON", ifnotfound = 6)) {
  if (is.na(age_min)) stop("construire_libelle_champ : AGE_MIN_BTS introuvable (00_config.R).")
  age_min <- as.integer(age_min); annee <- as.integer(annee); fin <- as.integer(annee + horizon)
  noms_cs <- c("Cadres" = "cadres", "Prof. intermediaires" = "professions intermédiaires", "Employes" = "employés", "Ouvriers" = "ouvriers")
  cats <- if (is.null(cs)) character(0) else unname(ifelse(cs %in% names(noms_cs), noms_cs[cs], tolower(cs)))
  hors_lib <- c("1" = "agriculteurs exploitants", "2" = "artisans, commerçants et chefs d’entreprise")
  hors_txt <- paste(c(unname(hors_lib[intersect(as.character(hors), names(hors_lib))]), "profession non renseignée"), collapse = ", ")
  salaries_age <- sprintf("salariés de %d ans ou plus", age_min)
  court <- paste0(salaries_age, " · entreprises du périmètre BITD")
  detaille <- c(
    "Ces résultats concernent les salariés des entreprises retenues dans le périmètre BITD (base industrielle et technologique de défense).",
    sprintf("Ils portent sur les salariés âgés de %d ans ou plus en %d%s.", age_min, annee,
            if (length(cats)) paste0(", appartenant aux catégories professionnelles couvertes par le modèle : ", paste(cats, collapse = ", ")) else ""),
    sprintf("Certaines professions qui ne peuvent pas être rattachées à ces catégories (%s) ne sont pas incluses dans les estimations.", hors_txt),
    sprintf("Un départ est une sortie définitive de l’emploi d’ici %d : retraite ou fin de carrière, invalidité, décès. Changer d’employeur n’est pas un départ.", fin),
    "Les nombres de départs sont des estimations, arrondies à l’entier pour la lecture ; la part de salariés susceptibles de partir est calculée parmi les salariés de la catégorie affichée.")
  list(court = court, titre = paste0("Qui est concerné ? ", substr(court, 1, 1) |> toupper(), substr(court, 2, nchar(court))),
       salaries_age = salaries_age, detaille = detaille, age_min = age_min, categories = cats, annee = annee, horizon = fin)
}
# Libellés publics complets (statiques + dépendant de l'âge), injectés dans
# chaque page : UNE seule formulation pour les six pages.
construire_libelles_public <- function(champ = construire_libelle_champ()) {
  S <- champ$salaries_age; Smaj <- paste0(toupper(substr(S, 1, 1)), substr(S, 2, nchar(S)))
  c(rapply(LIBELLES_UI, function(x) if (!is.null(names(x))) as.list(x) else x, how = "replace"),
    list(salaries_age = Smaj, salaries_age_min = S, qui_court = champ$court, qui_titre = "Qui est concerné ?",
         legende = list(taux = "Part des salariés susceptibles de partir d’ici 2030", departs = "Nombre de départs estimés d’ici 2030",
                        effectif = paste("Nombre de", S)),
         kpi_aide = list(departs = "Salariés qui devraient avoir quitté définitivement leur emploi d’ici 2030 (estimation centrale).",
                         taux = paste0("Part des ", S, " de cette catégorie."),
                         salaries = paste0(Smaj, " de cette catégorie, dans les entreprises du périmètre BITD.")),
         note_taux = paste0("Part calculée parmi les ", S, " de la catégorie affichée.")))
}

# --- Lecture d'une table de diffusion (CSV write.csv2, avec ou sans BOM) -------
lire_csv_diffusion <- function(fichier) {
  if (!file.exists(fichier)) stop("Table de diffusion introuvable : ", fichier)
  d <- read.csv2(fichier, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE,
                 colClasses = "character", na.strings = "NA")
  names(d)[1] <- sub("^\ufeff", "", names(d)[1])            # BOM jamais dans un nom de colonne
  for (v in intersect(unname(COLONNES_MESURES_CARTE), names(d))) d[[v]] <- as.numeric(sub(",", ".", d[[v]], fixed = TRUE))
  if ("masque" %in% names(d)) d$masque <- toupper(trimws(d$masque)) == "TRUE"
  d
}

# --- Fond de carte : GeoJSON local -> entités (code, nom, anneaux lon/lat) -----
charger_fond_carte <- function(niveau = c("departement", "region", "france"),
                               dossier = file.path(get0("DIR_DATA", ifnotfound = "data"), "cartographie")) {
  niveau <- match.arg(niveau)
  fichier <- file.path(dossier, if (niveau == "region") "regions.geojson" else "departements.geojson")
  if (!file.exists(fichier)) stop("Fond cartographique local introuvable : ", fichier,
                                  " (voir data/LISEZMOI.txt ; aucun téléchargement à l'exécution).")
  g <- jsonlite::fromJSON(fichier, simplifyVector = FALSE)
  ent <- lapply(g$features, function(f) {
    if (f$geometry$type != "MultiPolygon") stop("Fond : géométrie ", f$geometry$type, " inattendue pour ", f$properties$code)
    anneaux <- unlist(lapply(f$geometry$coordinates, function(poly)
      lapply(poly, function(ring) matrix(unlist(ring), ncol = 2, byrow = TRUE))), recursive = FALSE)
    list(code = as.character(f$properties$code), nom = as.character(f$properties$nom), anneaux = anneaux)
  })
  codes <- vapply(ent, `[[`, "", "code")
  if (anyDuplicated(codes) > 0) stop("Fond : codes en double : ", paste(codes[duplicated(codes)], collapse = ", "))
  structure(ent, names = codes, niveau = niveau, fichier = fichier)
}

# --- Projection Lambert-93 (RGF93 ; constantes IGN) ----------------------------
projeter_lambert93 <- function(lon, lat) {
  a <- 6378137; e <- 0.0818191910428158
  phi0 <- 46.5 * pi / 180; lam0 <- 3 * pi / 180; phi1 <- 44 * pi / 180; phi2 <- 49 * pi / 180
  X0 <- 700000; Y0 <- 6600000
  L <- function(phi) log(tan(pi / 4 + phi / 2) * ((1 - e * sin(phi)) / (1 + e * sin(phi)))^(e / 2))
  m <- function(phi) cos(phi) / sqrt(1 - e^2 * sin(phi)^2)
  n <- (log(m(phi1)) - log(m(phi2))) / (L(phi2) - L(phi1))
  C <- m(phi1) * a * exp(n * L(phi1)) / n
  Ys <- Y0 + C * exp(-n * L(phi0))
  phi <- lat * pi / 180; lam <- lon * pi / 180
  R <- C * exp(-n * L(phi)); gam <- n * (lam - lam0)
  cbind(x = X0 + R * sin(gam), y = Ys - R * cos(gam))
}

# --- Mise en page : métropole dominante + encarts DROM, en unités SVG ----------
projeter_fond <- function(fond, largeur = 1000, hauteur_encart = 230) {
  niveau <- attr(fond, "niveau")
  est_drom <- function(code) code %in% names(DROM_CARTE) || (niveau == "region" && code %in% names(DROM_REGION_CARTE))
  dep_drom <- function(code) if (code %in% names(DROM_CARTE)) code else DROM_REGION_CARTE[[code]]
  metro <- fond[!vapply(names(fond), est_drom, logical(1))]
  drom  <- fond[vapply(names(fond), est_drom, logical(1))]
  pm <- lapply(metro, function(e) lapply(e$anneaux, function(r) projeter_lambert93(r[, 1], r[, 2])))
  tous <- do.call(rbind, unlist(pm, recursive = FALSE))
  bb <- c(min(tous[, 1]), max(tous[, 1]), min(tous[, 2]), max(tous[, 2]))
  marge <- 16; ech <- (largeur - 2 * marge) / max(bb[2] - bb[1], bb[4] - bb[3])
  decal_y <- (largeur - 2 * marge - (bb[4] - bb[3]) * ech) / 2
  svg_metro <- function(m) cbind(marge + (m[, 1] - bb[1]) * ech, marge + decal_y + (bb[4] - m[, 2]) * ech)
  coords <- lapply(pm, function(rings) lapply(rings, svg_metro))
  ordre_drom <- names(DROM_CARTE)[vapply(names(DROM_CARTE), function(d) any(vapply(names(drom), function(c) dep_drom(c) == d, logical(1))), logical(1))]
  n_enc <- length(ordre_drom); encarts <- list()
  if (n_enc > 0) {
    l_enc <- largeur / max(n_enc, 1); y_enc <- largeur
    for (i in seq_along(ordre_drom)) {
      code <- names(drom)[vapply(names(drom), function(c) dep_drom(c) == ordre_drom[i], logical(1))]
      e <- drom[[code]]
      pts <- do.call(rbind, e$anneaux); lat_c <- mean(range(pts[, 2])); lon_c <- mean(range(pts[, 1]))
      loc <- lapply(e$anneaux, function(r) cbind((r[, 1] - lon_c) * cos(lat_c * pi / 180), r[, 2] - lat_c))
      tl <- do.call(rbind, loc); etendue <- max(diff(range(tl[, 1])), diff(range(tl[, 2])))
      x0 <- (i - 1) * l_enc; cadre <- c(x0, y_enc, x0 + l_enc, y_enc + hauteur_encart)
      ech_e <- (min(l_enc, hauteur_encart) - 56) / etendue
      cx <- x0 + l_enc / 2; cy <- y_enc + hauteur_encart / 2 + 12
      coords[[code]] <- lapply(loc, function(m) cbind(cx + (m[, 1] - mean(range(tl[, 1]))) * ech_e,
                                                       cy - (m[, 2] - mean(range(tl[, 2]))) * ech_e))
      encarts[[code]] <- list(code = code, nom = DROM_CARTE[[ordre_drom[i]]], cadre = cadre, x_lib = x0 + 10, y_lib = y_enc + 24)
    }
  }
  chemins <- vapply(names(fond), function(code) {
    paste(vapply(coords[[code]], function(m) paste0("M", paste(sprintf("%.1f %.1f", m[, 1], m[, 2]), collapse = "L"), "Z"), ""), collapse = "")
  }, "")
  long <- bind_rows(lapply(names(fond), function(code) bind_rows(lapply(seq_along(coords[[code]]), function(k) {
    m <- coords[[code]]; tibble(code = code, groupe = paste(code, k), x = m[[k]][, 1], y = -m[[k]][, 2]) }))))
  list(chemins = chemins, long = long, encarts = encarts, largeur = largeur,
       hauteur = largeur + if (n_enc > 0) hauteur_encart else 0, noms = vapply(fond, `[[`, "", "nom"))
}

# --- Libellés des PCS (nomenclature PCS-ESE 2017, fichier local xlsx) ----------
# Retourne un vecteur nommé  code normalisé -> libellé  (NULL + message si le
# fichier est absent : les cartes affichent alors code + grande catégorie).
# Normalisation des codes : minuscules, sans espaces (342f = 342F = " 342f ").
# Colonnes attendues : PCS_LIBELLES_COLS (00) ; colonnes absentes = ARRÊT.
normaliser_code_pcs <- function(x) gsub("\\s+", "", tolower(as.character(x)))
charger_libelles_pcs <- function(fichier = get0("FICHIER_PCS_LIBELLES", ifnotfound = file.path(DIR_DATA, "PCS-ESE_2017_Liste.xlsx")),
                                 cols = get0("PCS_LIBELLES_COLS", ifnotfound = c(code = "Code 2017", libelle = "Libelle_2017"))) {
  if (is.null(fichier) || !file.exists(fichier)) {
    message("Libellés PCS : fichier absent (", fichier, ") — les cartes afficheront le code et la grande catégorie.")
    return(NULL)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Libellés PCS : le package readxl est requis pour lire ", fichier)
  x <- readxl::read_excel(fichier, sheet = 1, col_types = "text")
  manquantes <- setdiff(unname(cols[c("code", "libelle")]), names(x))
  if (length(manquantes) > 0)
    stop("Libellés PCS : colonne(s) ", paste(manquantes, collapse = ", "), " absente(s) de ", fichier,
         " (colonnes lues : ", paste(names(x), collapse = ", "), ") — ajustez PCS_LIBELLES_COLS.")
  code <- normaliser_code_pcs(x[[cols[["code"]]]]); lib <- trimws(as.character(x[[cols[["libelle"]]]]))
  ok <- !is.na(code) & code != "" & !is.na(lib) & lib != ""
  code <- code[ok]; lib <- lib[ok]
  if (anyDuplicated(code) > 0) {
    dupl <- unique(code[duplicated(code)])
    message("Libellés PCS : ", length(dupl), " code(s) en double, premier libellé conservé : ", paste(head(dupl, 5), collapse = ", "))
    keep <- !duplicated(code); code <- code[keep]; lib <- lib[keep]
  }
  message("Libellés PCS : ", length(code), " code(s) lu(s) dans ", basename(fichier))
  setNames(lib, code)
}

# --- Préparation : table de diffusion -> structure de carte (sans aucune fuite) --
# libelles_pcs : vecteur nommé de charger_libelles_pcs() (dimension pcs) ; NULL = aucun libellé.
# ORDRE des catégories : dimension pcs -> départs estimés (centrale) DIFFUSÉS,
# sommés sur les territoires de la table, décroissants ; PCS sans aucun résultat
# diffusé en fin de liste ; égalité = code croissant. Dimension cs1 : code croissant.
preparer_carte_departs <- function(donnees, dimension = c("pcs", "cs1"),
                                   niveau = c("france", "region", "departement"), fond = NULL, libelles_pcs = NULL) {
  dimension <- match.arg(dimension); niveau <- match.arg(niveau)
  if (!"masque" %in% names(donnees))
    stop("preparer_carte_departs : colonne 'masque' absente — seule une table de DIFFUSION est admise (jamais interne/).")
  if (!dimension %in% names(donnees)) stop("preparer_carte_departs : colonne '", dimension, "' absente.")
  cle <- switch(niveau, france = NULL, region = "region_code", departement = "geo_code")
  cle_nom <- switch(niveau, france = NULL, region = "region_nom", departement = "geo_nom")
  if (!is.null(cle) && !cle %in% names(donnees)) stop("preparer_carte_departs : colonne '", cle, "' absente pour le niveau ", niveau, ".")
  mesures <- intersect(unname(COLONNES_MESURES_CARTE), names(donnees))
  d <- donnees
  d$masque <- as.logical(d$masque); d$masque[is.na(d$masque)] <- TRUE   # doute = masqué
  d$.code <- if (is.null(cle)) "FR" else as.character(d[[cle]])
  d$.nom  <- if (is.null(cle_nom) || !cle_nom %in% names(d)) NA_character_ else as.character(d[[cle_nom]])
  d$.cat  <- as.character(d[[dimension]])
  # SÉCURITÉ : toute mesure d'une ligne masquée est retirée ici, avant tout usage
  for (v in mesures) d[[v]][d$masque] <- NA_real_
  for (v in setdiff(unname(COLONNES_MESURES_CARTE), mesures)) d[[v]] <- NA_real_
  if (anyDuplicated(d[, c(".code", ".cat")]) > 0) stop("preparer_carte_departs : doublons territoire x catégorie.")
  non_loc <- d$.code %in% c("inconnu", "") | is.na(d$.code)
  n_non_loc <- length(unique(d$.code[non_loc])); d <- d[!non_loc, ]
  if (!is.null(fond) && niveau != "france") {
    absents <- setdiff(unique(d$.code), names(fond))
    if (length(absents) > 0)
      stop("Cartographie ", niveau, " : ", length(absents), " code(s) sans géométrie dans le fond : ",
           paste(sort(absents), collapse = ", "), " — aucune carte partielle.")
  }
  cats <- d |> distinct(.cat, cs1 = if ("cs1" %in% names(d)) cs1 else NA_character_) |> arrange(.cat)
  if (dimension == "cs1") cats$cs1 <- NA_character_
  cats$libelle <- NA_character_
  if (dimension == "pcs") {
    if (!is.null(libelles_pcs)) cats$libelle <- unname(libelles_pcs[normaliser_code_pcs(cats$.cat)])
    tot <- d |> filter(!masque, !is.na(departs_central)) |> group_by(.cat) |>   # valeurs diffusées seulement
      summarise(departs_total = sum(departs_central), .groups = "drop")
    cats <- cats |> left_join(tot, by = ".cat") |>
      arrange(is.na(departs_total), desc(departs_total), .cat) |> select(-departs_total)
  }
  cellules <- d |> transmute(code = .code, cat = .cat, statut = ifelse(masque, "masque", "diffuse"),
                             effectif_champ, departs_central, departs_bas, departs_haut, taux_depart_central_pct,
                             dep_retraite, dep_invalidite, dep_deces)
  couverture <- cellules |> group_by(code) |>
    summarise(n_obs = n(), n_diff = sum(statut == "diffuse"), n_masq = sum(statut == "masque"), .groups = "drop") |>
    mutate(part_diff_pct = 100 * n_diff / n_obs, statut = ifelse(n_diff > 0, "diffusable", "masque"))
  noms <- d |> filter(!is.na(.nom)) |> distinct(.code, .nom)
  territoires <- if (niveau == "france") tibble(code = "FR", nom = "France entière") else
    tibble(code = names(fond), nom = unname(vapply(fond, `[[`, "", "nom"))) |>
      left_join(noms, by = c(code = ".code")) |> mutate(nom = coalesce(.nom, nom)) |> select(code, nom)
  territoires <- territoires |> left_join(couverture |> select(code, statut), by = "code") |>
    mutate(statut = coalesce(statut, "sans_donnee"))
  list(dimension = dimension, niveau = niveau, mesures = mesures, categories = cats |> rename(code = .cat),
       cellules = cellules, couverture = couverture, territoires = territoires, n_non_localises = n_non_loc)
}

# --- Contrôle console d'une carte (comptages) ----------------------------------
controler_carte_departs <- function(prep, fond = NULL, prefixe = "08e") {
  t <- prep$territoires
  n <- c(csv = n_distinct(prep$cellules$code), joints = if (prep$niveau == "france") 1L else sum(t$statut != "sans_donnee"),
         masques = sum(t$statut == "masque"), diffusables = sum(t$statut == "diffusable"), sans = sum(t$statut == "sans_donnee"))
  if (prep$niveau != "france" && n[["csv"]] != n[["joints"]]) stop(prefixe, " : territoires du CSV non joints au fond.")
  m <- prep$cellules$statut == "masque"
  if (any(m) && any(!is.na(as.matrix(prep$cellules[m, unname(COLONNES_MESURES_CARTE)]))))
    stop(prefixe, " : une valeur masquée subsiste dans la structure de carte.")
  cat(sprintf("  %s x %-12s : territoires CSV %3d | joints %3d | masqués %3d | diffusables %3d | sans donnée %3d | catégories %d%s\n",
              prep$dimension, prep$niveau, n[["csv"]], n[["joints"]], n[["masques"]], n[["diffusables"]], n[["sans"]],
              nrow(prep$categories), if (prep$n_non_localises > 0) sprintf(" | non localisables %d", prep$n_non_localises) else ""))
  if (prep$dimension == "pcs" && "libelle" %in% names(prep$categories)) {
    n_lib <- sum(!is.na(prep$categories$libelle))
    if (n_lib < nrow(prep$categories))
      cat(sprintf("    libellés PCS : %d/%d trouvés dans la nomenclature ; sans libellé : %s\n", n_lib, nrow(prep$categories),
                  paste(head(prep$categories$code[is.na(prep$categories$libelle)], 10), collapse = ", ")))
  }
  invisible(n)
}

# --- Sérialisation JSON : cellules masquées = statut seul ----------------------
donnees_json_carte <- function(prep, champ = construire_libelle_champ()) {
  cel <- list()
  for (i in seq_len(nrow(prep$cellules))) {
    r <- prep$cellules[i, ]
    cel[[r$code]][[r$cat]] <- if (r$statut == "masque") list(s = "m") else {
      v <- list(s = "d")
      for (k in names(COLONNES_MESURES_CARTE)) { x <- r[[COLONNES_MESURES_CARTE[[k]]]]; if (!is.na(x)) v[[k]] <- x }
      v
    }
  }
  cov <- setNames(lapply(seq_len(nrow(prep$couverture)), function(i) {
    r <- prep$couverture[i, ]; list(o = r$n_obs, d = r$n_diff, m = r$n_masq, p = round(r$part_diff_pct, 1)) }), prep$couverture$code)
  cats <- lapply(seq_len(nrow(prep$categories)), function(i) {
    r <- prep$categories[i, ]; k <- list(code = r$code)
    if (!is.na(r$cs1)) k$cs1 <- r$cs1
    if (!is.null(r$libelle) && !is.na(r$libelle)) k$lib <- r$libelle
    k })
  terr <- lapply(seq_len(nrow(prep$territoires)), function(i) list(code = prep$territoires$code[i], nom = prep$territoires$nom[i]))
  j <- jsonlite::toJSON(list(dimension = prep$dimension, niveau = prep$niveau,
                             categories = cats, territoires = terr, cellules = cel, couverture = cov,
                             couleurs = COULEURS_CARTE[c("classes", "secret", "sans_donnee")],
                             libelles = construire_libelles_public(champ), champ = champ[c("court", "titre")],
                             libelles_cs = as.list(LIBELLES_CS_CARTE)),
                       auto_unbox = TRUE, digits = NA, na = "null", null = "null")
  gsub("</", "<\\/", j, fixed = TRUE)
}

# --- CSS (centralisé ; classes dédiées au champ, aux KPI, à la légende) ---------
css_cartes <- function(S = STYLE_CONTOURS_CARTE) paste(
  sprintf(':root{--encre:%s;--texte:%s;--filet:%s;--fond:%s;--bleu:%s;--secret:%s}', COULEURS_CARTE$encre, COULEURS_CARTE$texte,
          COULEURS_CARTE$filet, COULEURS_CARTE$fond, COULEURS_CARTE$bleu, COULEURS_CARTE$secret),
  '*{box-sizing:border-box}html{-webkit-text-size-adjust:100%}body{margin:0;background:#fff;color:var(--encre);font-family:-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif;font-size:16px;line-height:1.5}',
  '.page{max-width:1360px;margin:0 auto;padding:28px 32px 40px}@media(max-width:700px){.page{padding:18px 16px 32px}}',
  # en-tête : vraie zone, jamais tronquée
  '.page-header{border-bottom:1px solid var(--filet);padding-bottom:18px;margin-bottom:22px}',
  '.kicker{font-size:13px;letter-spacing:.12em;text-transform:uppercase;color:var(--texte);font-weight:600;margin:0 0 8px}',
  '.page-title{font-size:32px;line-height:1.15;font-weight:700;margin:0;letter-spacing:-.01em}.page-title small{display:block;font-size:22px;font-weight:500;color:var(--texte);margin-top:2px}',
  '.page-subtitle{margin:12px 0 0}.cat-nom{display:block;font-size:22px;font-weight:700;color:var(--encre);line-height:1.25}.cat-qui{display:block;font-size:17px;font-weight:500;color:var(--texte);margin-top:2px}',
  # champ de l'étude : premier niveau d'information
  '.study-scope{display:flex;flex-wrap:wrap;align-items:center;gap:8px 14px;margin:12px 0 0;font-size:16px;color:var(--encre);background:var(--fond);border-left:4px solid var(--bleu);padding:10px 14px;border-radius:0 8px 8px 0}',
  '.study-scope b{font-weight:700}.study-scope .scope-text{font-weight:500}',
  '.study-scope details{display:inline-block}.study-scope summary{cursor:pointer;color:var(--bleu);font-weight:600;font-size:15px;list-style:none;padding:4px 8px;border-radius:6px}.study-scope summary::-webkit-details-marker{display:none}.study-scope summary:hover,.study-scope summary:focus{background:#e3eaf2;outline:none}.study-scope summary:focus-visible{outline:2px solid var(--bleu)}',
  '.study-scope-detail{flex-basis:100%;margin:6px 0 2px;padding:0 0 0 2px;font-size:15px;color:var(--encre);line-height:1.55}.study-scope-detail p{margin:4px 0}.study-scope-detail b{color:var(--bleu)}',
  '.context{margin:10px 0 0;font-size:15px;color:var(--texte)}',
  # grille carte
  '.grille{display:grid;grid-template-columns:320px minmax(0,1fr);gap:24px;align-items:start}@media(max-width:900px){.grille{grid-template-columns:1fr}}',
  '.panneau{border:1px solid var(--filet);border-radius:12px;padding:16px 18px 18px;background:#fff}',
  '.control-label{display:block;font-size:14px;font-weight:600;color:var(--encre);margin:12px 0 5px;letter-spacing:.02em}.control-label:first-child{margin-top:0}',
  'select,input[type=search]{width:100%;font:inherit;font-size:15px;padding:9px 10px;border:1px solid #AEB7C2;border-radius:8px;background:#fff;color:var(--encre);min-height:42px}select:focus,input:focus{outline:2px solid var(--bleu);outline-offset:1px}',
  '.radios{display:flex;gap:8px;margin:0}.radios label{flex:1;border:1px solid #AEB7C2;border-radius:8px;padding:9px 8px;text-align:center;cursor:pointer;font-size:15px;font-weight:500;color:var(--encre);min-height:42px;display:flex;align-items:center;justify-content:center}.radios input{position:absolute;opacity:0;width:0;height:0}.radios label.on{background:var(--bleu);color:#fff;border-color:var(--bleu)}.radios label:focus-within{outline:2px solid var(--bleu);outline-offset:1px}',
  '.tabs{display:flex;flex-wrap:wrap;gap:8px;margin:0}.tabs button{font:inherit;font-size:15px;font-weight:600;padding:10px 16px;border-radius:999px;border:1px solid #AEB7C2;background:#fff;color:var(--encre);cursor:pointer;min-height:42px}.tabs button.on{background:var(--bleu);border-color:var(--bleu);color:#fff}.tabs button:focus-visible{outline:2px solid var(--bleu);outline-offset:2px}',
  # légende
  '.legend-title{font-size:15px;font-weight:700;margin:16px 0 6px;color:var(--encre)}.map-legend{list-style:none;margin:0;padding:0}.map-legend li{display:flex;align-items:center;gap:10px;font-size:15px;margin:6px 0;color:var(--encre)}.map-legend li.sep{border-top:1px solid var(--filet);margin-top:10px;padding-top:10px}',
  sprintf('.map-legend .sw{width:26px;height:16px;border-radius:3px;border:1px solid %s;flex:none}.map-legend .sw.sans{border:1.5px dashed %s}', S$couleur, S$sans_couleur),
  '.compte{font-size:15px;color:var(--encre);margin:10px 0 0}.compte b{font-weight:700}.note{font-size:13.5px;color:var(--texte);margin:10px 0 0;line-height:1.45}',
  # bandeau « version statique » : visible tant que le JavaScript ne l'a pas masqué (scripts retirés par une messagerie)
  '.sans-js{margin:12px 0 0;padding:10px 14px;border:1px solid #e0b96a;border-left-width:4px;background:#fff8e6;color:#5b4300;font-size:15px;border-radius:0 8px 8px 0}',
  # carte
  '.carte{position:relative;min-width:0}.carte svg{width:100%;height:auto;display:block}',
  # contours : STYLE_CONTOURS_CARTE (source unique) ; non-scaling-stroke = épaisseur en pixels écran, desktop comme mobile
  sprintf('path.t{stroke:%s;stroke-width:%s;vector-effect:non-scaling-stroke;cursor:pointer;outline:none}path.t.sans{stroke:%s;stroke-width:%s;stroke-dasharray:3 2}',
          S$couleur, S$largeur, S$sans_couleur, S$sans_largeur),
  sprintf('.limites-reg path{fill:none;stroke:%s;stroke-width:%s;vector-effect:non-scaling-stroke;stroke-linejoin:round;pointer-events:none}',
          S$region_couleur, S$region_largeur),
  sprintf('#survol{fill:none;stroke:%s;stroke-width:%s;vector-effect:non-scaling-stroke;stroke-linejoin:round;pointer-events:none;display:none}#survol.on{display:inline}',
          S$survol_couleur, S$survol_largeur),
  # petite carte (mobile) : mêmes couleurs, traits réduits d'un même facteur pour ne pas quadriller la carte
  sprintf('@media (max-width:640px){path.t{stroke-width:%s}path.t.sans{stroke-width:%s}.limites-reg path{stroke-width:%s}#survol{stroke-width:%s}}',
          S$largeur * S$mobile_facteur, S$sans_largeur * S$mobile_facteur, S$region_largeur * S$mobile_facteur, S$survol_largeur * S$mobile_facteur),
  '.encart{fill:none;stroke:#c9ced6;stroke-width:1.2}.encart-lib{font-size:17px;font-weight:600;fill:#3E4C59}',
  # infobulle
  '.bulle{position:absolute;pointer-events:none;background:#1F2933;color:#fff;border-radius:10px;padding:14px 16px;font-size:15px;line-height:1.5;min-width:240px;max-width:320px;box-shadow:0 8px 24px rgba(0,0,0,.22);display:none;z-index:2}',
  '.tooltip-title{font-size:17px;font-weight:700;margin:0 0 2px}.tooltip-cat{color:#cfd6df;margin:0 0 8px;font-weight:500}.tooltip-rows{border-top:1px solid rgba(255,255,255,.25);padding-top:8px}.tooltip-rows .tooltip-value{font-weight:700;padding:3px 0}.tooltip-rows .tooltip-small{color:#cfd6df;font-size:13.5px;padding:0 0 4px}.tooltip-secret{color:#ffd7a8;font-weight:700;margin-top:8px}.tooltip-secret small{display:block;color:#f3f4f6;font-weight:400}.tooltip-sans{color:#e5e7eb;font-weight:600;margin-top:8px}',
  # tableau de bord national
  '.dash{display:grid;grid-template-columns:320px minmax(0,1fr);gap:24px;align-items:start}@media(max-width:900px){.dash{grid-template-columns:1fr}}',
  '.cat-affichee{margin:16px 0 0;padding-top:14px;border-top:1px solid var(--filet)}.cat-affichee .cat-nom{font-size:20px}.cat-affichee .cat-qui{font-size:15px}',
  '.kpis{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:16px}@media(max-width:760px){.kpis{grid-template-columns:1fr}}',
  '.kpi{border:1px solid var(--filet);border-radius:12px;padding:18px 20px;background:#fff}.kpi.primaire{background:var(--fond);border-color:#c9d6e5}',
  '.kpi-value{font-size:40px;font-weight:700;line-height:1.1;letter-spacing:-.02em;color:var(--encre)}.kpi-value.secret{font-size:24px;color:#4b5563}',
  '.kpi-label{font-size:16px;font-weight:600;margin-top:6px;color:var(--encre)}.kpi-help{font-size:14px;color:var(--texte);margin-top:4px;line-height:1.4}',
  '.bloc{border:1px solid var(--filet);border-radius:12px;padding:18px 20px;margin-top:16px}.bloc h2{font-size:17px;margin:0 0 12px;font-weight:700}',
  '.fourchette{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:12px;text-align:center}.fourchette .v{font-size:26px;font-weight:600;color:var(--texte)}.fourchette .centrale .v{font-size:36px;font-weight:700;color:var(--encre)}.fourchette .l{font-size:14px;color:var(--texte);margin-top:2px}.fourchette .centrale .l{color:var(--encre);font-weight:600}',
  '.barre{display:flex;height:22px;border-radius:6px;overflow:hidden;background:#eef1f5;margin:6px 0 10px}.barre span{height:100%}',
  '.causes{list-style:none;margin:0;padding:0}.causes li{display:flex;align-items:center;gap:10px;font-size:15px;padding:5px 0;border-bottom:1px solid var(--filet)}.causes li:last-child{border:0}.causes .sw{width:14px;height:14px;border-radius:3px;flex:none}.causes .v{margin-left:auto;font-weight:700;white-space:nowrap}.causes .p{color:var(--texte);font-weight:500;margin-left:8px;min-width:48px;text-align:right}',
  '.masque-national{border:1px solid var(--filet);border-radius:12px;padding:28px 24px;background:var(--fond)}.masque-national .t{font-size:26px;font-weight:700}.masque-national p{font-size:16px;margin:8px 0 0;color:var(--encre)}',
  'footer{margin-top:28px;color:var(--texte);font-size:13.5px;line-height:1.55;border-top:1px solid var(--filet);padding-top:14px}footer b{color:var(--encre)}',
  sep = "\n")

# --- JavaScript (natif, inline ; carte ET tableau de bord) ---------------------
js_cartes <- function() paste(
  '(function(){',
  # scripts présents : le bandeau « version statique » disparaît (il reste visible si une messagerie a retiré les scripts)
  'var sj=document.getElementById("sans-js");if(sj)sj.style.display="none";',
  'var D=JSON.parse(document.getElementById("donnees").textContent),L=D.libelles;var FR=D.niveau==="france";',
  'var $=function(id){return document.getElementById(id)};',
  'var nf=function(d){return new Intl.NumberFormat("fr-FR",{minimumFractionDigits:d,maximumFractionDigits:d})};',
  'function arrondiPers(x){return x==null?null:Math.floor(x+0.5)}',
  'var fmt=function(x,d){return (x==null||isNaN(x))?"n.d.":nf(d).format(d===0?arrondiPers(x):x)};var pct=function(x,d){return fmt(x,d)+" %"};',
  'var noms={};D.territoires.forEach(function(t){noms[t.code]=t.nom});',
  'var etat={vue:"resultats",cat:D.categories.length?D.categories[0].code:null,ind:"taux",scen:"c"};',
  'var dimPl=D.dimension==="pcs"?"PCS":"catégories",dimUne=D.dimension==="pcs"?"PCS":"catégorie";',
  'var nivLib={france:"France entière",region:"par région",departement:"par département"}[D.niveau];',
  'var nivPl={france:"",region:"régions",departement:"départements"}[D.niveau];',
  # catégorie : libellé officiel de la PCS quand la nomenclature le fournit (k.lib), sinon code + grande catégorie
  'var catLib=function(c){var o=D.categories.filter(function(k){return k.code===c})[0];if(!o)return c;if(D.dimension!=="pcs")return D.libelles_cs[o.code]||o.code;return o.lib?(o.lib+" · PCS "+o.code):("PCS "+o.code+(o.cs1?" · "+(D.libelles_cs[o.cs1]||o.cs1):""))};',
  'var catOpt=function(k){if(D.dimension!=="pcs")return D.libelles_cs[k.code]||k.code;return k.code+" — "+(k.lib?k.lib:(k.cs1?(D.libelles_cs[k.cs1]||k.cs1):""))};',
  'function remplirCats(filtre){var s=$("cat");if(!s)return;var f=(filtre||"").toLowerCase();s.innerHTML="";D.categories.forEach(function(k){var lib=catOpt(k);if(f&&lib.toLowerCase().indexOf(f)<0)return;var o=document.createElement("option");o.value=k.code;o.textContent=lib;s.appendChild(o)});if(s.options.length){var ok=false;for(var i=0;i<s.options.length;i++)if(s.options[i].value===etat.cat){ok=true;break}if(!ok)etat.cat=s.options[0].value;s.value=etat.cat}}',
  'function cle(){return etat.ind==="departs"?etat.scen:(etat.ind==="effectif"?"e":"t")}',
  'var dec={t:1,c:0,b:0,h:0,e:0,r:0,i:0,d:0};',   # personnes -> entier (convention 00g), part -> 1 décimale
  'function valeur(code){var c=D.cellules[code]&&D.cellules[code][etat.cat];if(!c)return{st:"sans"};if(c.s==="m")return{st:"masque"};var v=c[cle()];return{st:"diffuse",v:v,c:c}}',
  'function plusFortsRestes(comp,total){var cible=arrondiPers(total),base=comp.map(Math.floor),reste=cible-base.reduce(function(a,b){return a+b},0);var idx=comp.map(function(v,i){return i}).sort(function(a,b){var fa=Math.round((comp[a]-base[a])*1e9),fb=Math.round((comp[b]-base[b])*1e9);return fb-fa||comp[b]-comp[a]||a-b});for(var i=0;i<reste&&i<idx.length;i++)base[idx[i]]++;var s=base.reduce(function(a,b){return a+b},0);while(s<cible){base[idx[0]]++;s++}while(s>cible){var m=base.indexOf(Math.max.apply(null,base));base[m]--;s--}return base}',
  'function fmtInd(v,legende){var k=cle();if(k==="t")return pct(v,legende?0:1);return fmt(v,legende?0:dec[k])}',
  # classes à bornes « rondes » : pas de 1, 2, 2,5, 5 x 10^k ; 3 à 5 classes ; échelle commune à la catégorie affichée
  'function pasRond(x){if(x<=0)return 1;var p=Math.pow(10,Math.floor(Math.log(x)/Math.LN10)),r=x/p;var c=r<=1?1:(r<=2?2:(r<=2.5?2.5:(r<=5?5:10)));return c*p}',
  'function classesRondes(vals){var dist=vals.filter(function(v,i,a){return a.indexOf(v)===i});if(dist.length===0)return null;var mn=Math.min.apply(null,vals),mx=Math.max.apply(null,vals);if(mn===mx||dist.length<3)return{unique:true,min:mn,max:mx,vals:dist.sort(function(a,b){return a-b})};',
  ' var pas=pasRond((mx-mn)/5),lo,hi,n,tours=0;do{lo=Math.floor(mn/pas)*pas;hi=Math.ceil(mx/pas)*pas;if(hi<=mx)hi+=pas;n=Math.round((hi-lo)/pas);if(n>5)pas=pasRond(pas*1.01);tours++}while(n>5&&tours<8);',
  ' var b=[];for(var k=1;k<n;k++)b.push(+(lo+k*pas).toFixed(6));return{unique:false,bornes:b,n:b.length+1,lo:lo,hi:hi}}',
  'function classeDe(v,cl){if(cl.unique)return cl.vals.indexOf(v);var k=0;while(k<cl.bornes.length&&v>=cl.bornes[k])k++;return k}',
  'function couleur(k,n){var pal=D.couleurs.classes;if(n<=1)return pal[2];return pal[Math.round(k*(pal.length-1)/(n-1))]}',
  'function classeCouv(p){if(p<=0)return -1;var b=L.couverture.bornes;for(var i=b.length-1;i>=0;i--)if(p>=b[i])return i;return 0}',
  'var li=function(leg,col,txt,sans,sep){var e=document.createElement("li");if(sep)e.className="sep";e.innerHTML="<span class=\\"sw"+(sans?" sans":"")+"\\" style=\\"background:"+col+"\\"></span><span></span>";e.lastChild.textContent=txt;leg.appendChild(e)};',
  'function entete(titre,cat,qui){var t=$("titre");t.innerHTML="";t.appendChild(document.createTextNode(titre));t.appendChild(Object.assign(document.createElement("small"),{textContent:nivLib}));$("sous-cat").textContent=cat;$("sous-qui").textContent=qui}',
  'function rendreCarte(){var paths=document.querySelectorAll("path.t"),leg=$("legende");if(!leg)return;leg.innerHTML="";var nD=0,nM=0,nS=0,cl=null;',
  ' if(etat.vue==="resultats"){var vals=[];Object.keys(D.couverture).forEach(function(code){var r=valeur(code);if(r.st==="diffuse"&&r.v!=null)vals.push(r.v)});cl=classesRondes(vals)}',
  ' paths.forEach(function(p){var code=p.getAttribute("data-code"),fill,st;p.classList.remove("sans");',
  '  if(etat.vue==="affichables"){var cv=D.couverture[code];if(!cv){st="sans";fill=D.couleurs.sans_donnee}else{var k=classeCouv(cv.p);if(k<0){st="masque";fill=D.couleurs.secret}else{st="diffuse";fill=couleur(k,L.couverture.bornes.length)}}}',
  '  else{var r2=valeur(code);st=r2.st;fill=st==="masque"?D.couleurs.secret:(st==="sans"?D.couleurs.sans_donnee:(cl?couleur(classeDe(r2.v,cl),cl.unique?cl.vals.length:cl.n):D.couleurs.classes[2]))}',
  '  if(st==="sans")p.classList.add("sans");p.setAttribute("fill",fill);p.setAttribute("data-st",st);if(st==="diffuse")nD++;else if(st==="masque")nM++;else nS++});',
  ' var titreLeg=$("legende-titre");',
  ' if(etat.vue==="affichables"){titreLeg.textContent="Part des "+dimPl+" avec un résultat affichable";var LB=L.couverture.libelles;for(var i=LB.length-1;i>=0;i--)li(leg,couleur(i,LB.length),LB[i]);li(leg,D.couleurs.secret,L.couverture.aucune,false,true);$("legende-note").textContent="Pour chaque territoire : part des "+dimPl+" observées pour lesquelles un résultat peut être affiché. Cette vue ne révèle aucun chiffre non diffusé.";',
  '  entete(L.couverture.titre,"Part des "+dimPl+" pour lesquelles un résultat peut être affiché",L.salaries_age+" · "+nivLib)}',
  ' else{titreLeg.textContent=L.legende[etat.ind]+(etat.ind==="departs"?" ("+L.scenarios[etat.scen].toLowerCase()+")":"");',
  '  if(!cl)li(leg,D.couleurs.classes[2],"Aucun résultat affichable pour cette "+dimUne);',
  '  else if(cl.unique){cl.vals.forEach(function(v,i){li(leg,couleur(i,cl.vals.length),fmtInd(v,false))})}',
  '  else{for(var k=0;k<cl.n;k++){var txt=k===0?"Moins de "+fmtInd(cl.bornes[0],true):(k===cl.n-1?fmtInd(cl.bornes[k-1],true)+" et plus":fmtInd(cl.bornes[k-1],true)+" à "+fmtInd(cl.bornes[k],true));li(leg,couleur(k,cl.n),txt)}}',
  '  li(leg,D.couleurs.secret,L.secret,false,true);',
  '  $("legende-note").textContent=(etat.ind==="taux"?L.note_taux+" ":"")+"Même échelle pour tous les territoires ; elle est recalculée quand vous changez de "+dimUne+" ou d’indicateur.";',
  '  entete("Départs attendus d’ici 2030",catLib(etat.cat),L.salaries_age+" · "+L.legende[etat.ind]+(etat.ind==="departs"?" ("+L.scenarios[etat.scen].toLowerCase()+")":""))}',
  ' li(leg,D.couleurs.sans_donnee,L.sans_donnee,true);',
  ' $("compte").innerHTML="<b>"+nD+"</b> "+nivPl+" avec un résultat · <b>"+nM+"</b> avec un résultat non diffusé (secret statistique) · <b>"+nS+"</b> sans salarié observé";',
  ' var cat=etat.vue==="resultats";$("b-cat").style.display=cat?"":"none";$("b-ind").style.display=cat?"":"none";$("b-scen").style.display=(cat&&etat.ind==="departs")?"":"none";}',
  # infobulle : des phrases, pas des libellés techniques
  'function ligne(el,txt,cls){var d=document.createElement("div");if(cls)d.className=cls;d.textContent=txt;el.appendChild(d);return d}',
  'function bulle(code){var el=document.createElement("div");ligne(el,noms[code]+" · "+code,"tooltip-title");',
  ' if(etat.vue==="affichables"){var cv=D.couverture[code];if(!cv)ligne(el,L.sans_donnee,"tooltip-sans");else{var b=document.createElement("div");b.className="tooltip-rows";ligne(b,fmt(cv.o,0)+" "+dimPl+" observées");ligne(b,fmt(cv.d,0)+(cv.d>1?" résultats affichables":" résultat affichable"));ligne(b,fmt(cv.m,0)+(cv.m>1?" résultats non diffusés":" résultat non diffusé"));el.appendChild(b);if(cv.d===0)ligne(el,L.couverture.aucune,"tooltip-secret")}}',
  ' else{ligne(el,catLib(etat.cat),"tooltip-cat");var r=valeur(code);',
  '  if(r.st==="sans")ligne(el,L.sans_donnee,"tooltip-sans");',
  '  else if(r.st==="masque"){var s4=ligne(el,L.secret_court,"tooltip-secret");var sm=document.createElement("small");sm.textContent=L.secret_sous;s4.appendChild(sm)}',
  '  else{var c=r.c,b2=document.createElement("div");b2.className="tooltip-rows";if(c.e!=null)ligne(b2,fmt(c.e,0)+" "+L.salaries_age_min,"tooltip-value");if(c.c!=null)ligne(b2,fmt(c.c,0)+" "+L.kpi.departs,"tooltip-value");if(c.b!=null&&c.h!=null)ligne(b2,"entre "+fmt(c.b,0)+" et "+fmt(c.h,0)+" selon l’hypothèse","tooltip-small");if(c.t!=null)ligne(b2,pct(c.t,1)+" "+L.kpi.taux,"tooltip-value");el.appendChild(b2)}}',
  ' return el.innerHTML}',
  # tableau de bord national
  'function rendreDash(){var c=D.cellules["FR"]&&D.cellules["FR"][etat.cat];$("sous-cat").textContent=catLib(etat.cat);$("sous-qui").textContent=L.salaries_age+" · France entière";$("cat-affichee").textContent=catLib(etat.cat);$("cat-qui").textContent=L.salaries_age;',
  ' var z=$("zone-kpi"),m=$("zone-masque");',
  ' if(!c||c.s==="m"){z.style.display="none";m.style.display="";$("masque-cat").textContent=catLib(etat.cat);$("masque-txt").textContent=c?L.secret_explication:"Aucun salarié de cette "+dimUne+" n’est observé.";$("masque-titre").textContent=c?L.secret_court:L.sans_donnee;return}',
  ' z.style.display="";m.style.display="none";$("kpi-e").textContent=fmt(c.e,0);$("kpi-c").textContent=fmt(c.c,0);$("kpi-t").textContent=c.t!=null?pct(c.t,1):"n.d.";',
  ' $("f-b").textContent=fmt(c.b,0);$("f-c").textContent=fmt(c.c,0);$("f-h").textContent=fmt(c.h,0);',
  ' var tot=(c.r||0)+(c.i||0)+(c.d||0),bar=$("barre"),ul=$("causes");bar.innerHTML="";ul.innerHTML="";var cols={r:D.couleurs.classes[3],i:D.couleurs.classes[1],d:"#6b7280"};var ent=plusFortsRestes([c.r||0,c.i||0,c.d||0],c.c!=null?c.c:tot);',
  ' if(c.r==null&&c.i==null&&c.d==null){$("bloc-causes").style.display="none"}else{$("bloc-causes").style.display="";["r","i","d"].forEach(function(k){var v=c[k]||0,p=tot>0?100*v/tot:0;var s=document.createElement("span");s.style.width=p+"%";s.style.background=cols[k];s.title=L.causes[k];bar.appendChild(s);var l=document.createElement("li");l.innerHTML="<span class=\\"sw\\" style=\\"background:"+cols[k]+"\\"></span><span></span><span class=\\"v\\"></span><span class=\\"p\\"></span>";l.children[1].textContent=L.causes[k];l.children[2].textContent=fmt(ent[["r","i","d"].indexOf(k)],0);l.children[3].textContent=pct(p,0);ul.appendChild(l)})}}',
  'function rendre(){if(FR)rendreDash();else rendreCarte()}',
  # événements
  # survol : le tracé du territoire est recopié dans <path id="survol">, dessiné AU-DESSUS de ses voisins (sinon ils en couvrent la moitié)
  'var B=$("bulle"),C=$("carte"),U=$("survol");function surligner(code){if(!U)return;var p=code&&$("t-"+code);if(p){U.setAttribute("d",p.getAttribute("d"));U.classList.add("on")}else{U.classList.remove("on");U.setAttribute("d","")}}',
  'if(C){document.querySelectorAll("path.t").forEach(function(p){var code=p.getAttribute("data-code");',
  ' p.addEventListener("mousemove",function(ev){B.innerHTML=bulle(code);B.style.display="block";surligner(code);var r=C.getBoundingClientRect();var x=ev.clientX-r.left+16,y=ev.clientY-r.top+16;if(x+330>r.width)x-=346;if(y+180>r.height)y-=190;B.style.left=Math.max(0,x)+"px";B.style.top=Math.max(0,y)+"px"});',
  ' p.addEventListener("mouseleave",function(){B.style.display="none";surligner(null)});',
  ' p.addEventListener("focus",function(){B.innerHTML=bulle(code);B.style.display="block";surligner(code);B.style.left="12px";B.style.top="12px"});p.addEventListener("blur",function(){B.style.display="none";surligner(null)})})}',
  'document.querySelectorAll(".radios input").forEach(function(r){r.addEventListener("change",function(){etat.vue=r.value;document.querySelectorAll(".radios label").forEach(function(l){l.classList.toggle("on",l.querySelector("input").checked)});rendre()})});',
  'var rc=document.querySelector(".radios input:checked");if(rc){etat.vue=rc.value;document.querySelectorAll(".radios label").forEach(function(l){l.classList.toggle("on",l.querySelector("input").checked)})}',
  'var rech=$("rech");if(rech)rech.addEventListener("input",function(){remplirCats(rech.value);rendre()});',
  'var sel=$("cat");if(sel)sel.addEventListener("change",function(){etat.cat=sel.value;rendre()});',
  'document.querySelectorAll(".tabs button").forEach(function(b){b.addEventListener("click",function(){etat.cat=b.getAttribute("data-cat");document.querySelectorAll(".tabs button").forEach(function(x){x.classList.toggle("on",x===b);x.setAttribute("aria-pressed",x===b?"true":"false")});rendre()})});',
  'var ind=$("ind");if(ind)ind.addEventListener("change",function(){etat.ind=ind.value;rendre()});var sc=$("scen");if(sc)sc.addEventListener("change",function(){etat.scen=sc.value;rendre()});',
  'remplirCats("");rendre();',
  '})();', sep = "\n")

echap_html_carte <- function(s) { s <- gsub("&", "&amp;", s, fixed = TRUE); s <- gsub("<", "&lt;", s, fixed = TRUE); gsub(">", "&gt;", s, fixed = TRUE) }

# --- VUE INITIALE STATIQUE (sans JavaScript) -----------------------------------
# Les passerelles de messagerie retirent souvent <script> (et parfois <style>)
# des pièces jointes HTML : sans attribut fill, un tracé SVG est NOIR. La vue
# initiale (vue « Résultats », première catégorie, part des salariés, estimation
# centrale ; tableau de bord : première catégorie) est donc CALCULÉE EN R et
# écrite dans le HTML : couleurs des territoires (attributs fill / stroke),
# légende, en-tête, liste des catégories, chiffres du tableau de bord. Le
# JavaScript, quand il est présent, recalcule exactement la même vue puis
# masque le bandeau d'avertissement. Mêmes règles que le JS (pasRond,
# classesRondes, couleur) : ports fidèles ci-dessous. AUCUNE valeur masquée :
# tout part de prep (mesures déjà effacées pour les cellules masquées).
LIBELLES_CS_CARTE <- c("Cadres" = "Cadres", "Prof. intermediaires" = "Professions intermédiaires", "Employes" = "Employés", "Ouvriers" = "Ouvriers")
TEXTE_SANS_JS <- "Version statique : cette page a été ouverte sans ses scripts (souvent retirés par les messageries). Vous voyez la première vue, sans les menus ni le survol. Pour la version interactive, enregistrez la pièce jointe puis ouvrez-la dans un navigateur, ou demandez l’archive compressée."
fmt_fr_carte <- function(x, d = 0) ifelse(is.na(x), "n.d.", formatC(if (d == 0) arrondir_nombre_personnes(x) else x, format = "f", digits = d, big.mark = " ", decimal.mark = ","))
pas_rond <- function(x) { if (x <= 0) return(1); p <- 10^floor(log(x) / log(10)); r <- x / p
  c <- if (r <= 1) 1 else if (r <= 2) 2 else if (r <= 2.5) 2.5 else if (r <= 5) 5 else 10; c * p }
classes_rondes <- function(vals) {
  dist <- unique(vals); if (length(dist) == 0) return(NULL)
  mn <- min(vals); mx <- max(vals)
  if (mn == mx || length(dist) < 3) return(list(unique = TRUE, vals = sort(dist)))
  pas <- pas_rond((mx - mn) / 5); tours <- 0
  repeat {
    lo <- floor(mn / pas) * pas; hi <- ceiling(mx / pas) * pas; if (hi <= mx) hi <- hi + pas
    n <- round((hi - lo) / pas); tours <- tours + 1
    if (n > 5 && tours < 8) pas <- pas_rond(pas * 1.01) else break
  }
  b <- round(lo + seq_len(n - 1) * pas, 6); list(unique = FALSE, bornes = b, n = length(b) + 1)
}
classe_de <- function(v, cl) if (isTRUE(cl$unique)) match(v, cl$vals) - 1 else sum(v >= cl$bornes)
couleur_classe <- function(k, n) { pal <- COULEURS_CARTE$classes; if (n <= 1) pal[3] else pal[floor(k * (length(pal) - 1) / (n - 1) + 0.5) + 1] }
libelle_categorie <- function(prep, code, forme = c("titre", "option")) {
  forme <- match.arg(forme); r <- prep$categories[match(code, prep$categories$code), ]
  if (prep$dimension != "pcs") return(unname(ifelse(is.na(LIBELLES_CS_CARTE[code]), code, LIBELLES_CS_CARTE[code])))
  lib <- if (!is.null(r$libelle) && !is.na(r$libelle)) r$libelle else NA
  cs <- if (!is.na(r$cs1)) unname(ifelse(is.na(LIBELLES_CS_CARTE[r$cs1]), r$cs1, LIBELLES_CS_CARTE[r$cs1])) else ""
  if (forme == "titre") { if (!is.na(lib)) paste0(lib, " · PCS ", code) else paste0("PCS ", code, if (nzchar(cs)) paste0(" · ", cs) else "") }
  else paste0(code, " — ", if (!is.na(lib)) lib else cs)
}
# Vue initiale d'une carte territoriale : statut, couleur et légende (vue Résultats, 1re catégorie, part, centrale)
vue_initiale_carte <- function(prep, codes_fond, champ = construire_libelle_champ(), cat = NULL) {
  P <- construire_libelles_public(champ); S <- STYLE_CONTOURS_CARTE
  cat0 <- if (!is.null(cat)) cat else if (nrow(prep$categories)) prep$categories$code[1] else NA_character_
  cel <- prep$cellules[prep$cellules$cat %in% cat0, ]
  vals <- cel$taux_depart_central_pct[cel$statut == "diffuse" & !is.na(cel$taux_depart_central_pct)]
  cl <- classes_rondes(vals)
  terr <- lapply(codes_fond, function(code) {
    r <- cel[cel$code == code, ]
    if (nrow(r) == 0) list(st = "sans", fill = COULEURS_CARTE$sans_donnee)
    else if (r$statut[1] == "masque") list(st = "masque", fill = COULEURS_CARTE$secret)
    else { v <- r$taux_depart_central_pct[1]
      list(st = "diffuse", fill = if (is.null(cl) || is.na(v)) COULEURS_CARTE$classes[3]
                                  else couleur_classe(classe_de(v, cl), if (cl$unique) length(cl$vals) else cl$n)) }
  })
  names(terr) <- codes_fond
  st <- vapply(terr, `[[`, "", "st")
  pct0 <- function(x) paste0(fmt_fr_carte(x, 0), " %"); pct1 <- function(x) paste0(fmt_fr_carte(x, 1), " %")
  leg <- if (is.null(cl)) list(list(col = COULEURS_CARTE$classes[3], txt = paste0("Aucun résultat affichable pour cette ", if (prep$dimension == "pcs") "PCS" else "catégorie")))
    else if (cl$unique) lapply(seq_along(cl$vals), function(i) list(col = couleur_classe(i - 1, length(cl$vals)), txt = pct1(cl$vals[i])))
    else lapply(seq_len(cl$n), function(k) list(col = couleur_classe(k - 1, cl$n),
      txt = if (k == 1) paste0("Moins de ", pct0(cl$bornes[1])) else if (k == cl$n) paste0(pct0(cl$bornes[k - 1]), " et plus") else paste0(pct0(cl$bornes[k - 1]), " à ", pct0(cl$bornes[k]))))
  leg <- c(leg, list(list(col = COULEURS_CARTE$secret, txt = LIBELLES_UI$secret, sep = TRUE)),
                list(list(col = COULEURS_CARTE$sans_donnee, txt = LIBELLES_UI$sans_donnee, sans = TRUE)))
  nivPl <- c(region = "régions", departement = "départements")[[prep$niveau]]
  list(cat = cat0, territoires = terr,
       sous_cat = if (is.na(cat0)) "" else libelle_categorie(prep, cat0, "titre"),
       sous_qui = paste0(P$salaries_age, " · ", P$legende$taux),
       legende_titre = P$legende$taux, legende = leg,
       legende_note = paste0(P$note_taux, " Même échelle pour tous les territoires ; elle est recalculée quand vous changez de ",
                             if (prep$dimension == "pcs") "PCS" else "catégorie", " ou d’indicateur."),
       compte = sprintf("<b>%d</b> %s avec un résultat · <b>%d</b> avec un résultat non diffusé (secret statistique) · <b>%d</b> sans salarié observé",
                        sum(st == "diffuse"), nivPl, sum(st == "masque"), sum(st == "sans")))
}
html_legende_statique <- function(leg) vapply(leg, function(e)
  sprintf('<li%s><span class="sw%s" style="background:%s"></span><span>%s</span></li>', if (isTRUE(e$sep)) ' class="sep"' else "",
          if (isTRUE(e$sans)) " sans" else "", e$col, echap_html_carte(e$txt)), "")
html_bandeau_sans_js <- function() sprintf('<div class="sans-js" id="sans-js" role="note">%s</div>', echap_html_carte(TEXTE_SANS_JS))

# --- Briques HTML communes : en-tête « Qui est concerné ? », sélecteur, pied ----
html_entete_champ <- function(champ, niveau, dimension, sous_cat = "", sous_qui = "") {
  P <- construire_libelles_public(champ)
  c('<header class="page-header">',
    '<p class="kicker">Départs attendus d’ici 2030 · résultats diffusables</p>',
    sprintf('<h1 class="page-title" id="titre">Départs attendus d’ici 2030<small>%s</small></h1>',
            c(france = "France entière", region = "par région", departement = "par département")[[niveau]]),
    sprintf('<p class="page-subtitle" id="sous"><span class="cat-nom" id="sous-cat">%s</span> <span class="cat-qui" id="sous-qui">%s</span></p>',
            echap_html_carte(sous_cat), echap_html_carte(sous_qui)),
    html_bandeau_sans_js(),
    '<div class="study-scope" role="note" aria-label="Qui est concerné">',
    sprintf('<span class="scope-text"><b>%s</b> · %s</span>', echap_html_carte(P$salaries_age), "entreprises du périmètre BITD"),
    sprintf('<details><summary>ⓘ %s</summary></details>', echap_html_carte(P$qui_titre)),
    sprintf('<div class="study-scope-detail" id="scope-detail" hidden>%s</div>',
            paste(sprintf("<p>%s</p>", echap_html_carte(champ$detaille)), collapse = "")),
    '</div>',
    sprintf('<p class="context">%s</p>', if (niveau == "france") sprintf("Choisissez une %s pour consulter les résultats nationaux.", if (dimension == "pcs") "PCS" else "catégorie")
            else sprintf("Choisissez une %s pour comparer les territoires. Les zones grisées correspondent à des résultats non diffusés (secret statistique) ; les zones blanches n’ont aucun salarié observé.", if (dimension == "pcs") "PCS" else "catégorie")),
    '</header>')
}
html_selecteur_categorie <- function(prep) {
  if (prep$dimension == "cs1" && prep$niveau == "france") {
    btn <- vapply(seq_len(nrow(prep$categories)), function(i) {
      code <- prep$categories$code[i]
      sprintf('<button type="button" data-cat="%s" class="%s" aria-pressed="%s">%s</button>', echap_html_carte(code),
              if (i == 1) "on" else "", if (i == 1) "true" else "false", echap_html_carte(libelle_categorie(prep, code, "option")))
    }, "")
    c('<div id="b-cat"><span class="control-label">Quelle catégorie ?</span><div class="tabs" role="group" aria-label="Catégorie socioprofessionnelle">', btn, '</div></div>')
  } else {
    # options écrites en dur (vue statique) ; le JS les régénère à l'identique et les filtre à la recherche
    opts <- vapply(seq_len(nrow(prep$categories)), function(i) {
      code <- prep$categories$code[i]
      sprintf('<option value="%s"%s>%s</option>', echap_html_carte(code), if (i == 1) " selected" else "", echap_html_carte(libelle_categorie(prep, code, "option")))
    }, "")
    c(sprintf('<div id="b-cat"><label class="control-label" for="cat">%s</label>%s<select id="cat" size="1">%s</select></div>',
              if (prep$dimension == "pcs") "Quelle PCS ?" else "Quelle catégorie ?",
              if (prep$dimension == "pcs") '<input type="search" id="rech" placeholder="Rechercher un code PCS ou un libellé" aria-label="Rechercher une PCS">' else "",
              paste(opts, collapse = "")))
  }
}
html_pied <- function(champ, source_note) {
  sprintf(paste0('<footer><b>Qui est concerné ?</b> %s ',
                 '<b>Secret statistique.</b> Certains résultats ne sont pas affichés parce qu’ils concernent trop peu de salariés ou d’entreprises, ou qu’une seule entreprise y pèserait trop ; ces chiffres ne figurent ni dans cette page ni dans ses données. ',
                 'Gris = résultat non diffusé ; blanc pointillé = aucun salarié observé ; un résultat nul est affiché comme tout autre. ',
                 '<b>Source.</b> %s. Fond de carte : IGN Admin Express (COG 2018) via france-geojson, licence ouverte.</footer>'),
          echap_html_carte(paste(champ$detaille, collapse = " ")), echap_html_carte(source_note))
}
js_details_champ <- '<script>(function(){var d=document.querySelector(".study-scope details"),p=document.getElementById("scope-detail");if(d&&p)d.addEventListener("toggle",function(){p.hidden=!d.open})})();</script>'


# --- Génération : carte territoriale OU tableau de bord national ----------------
generer_carte_departs <- function(prep, fond = NULL, fichier_html, fichier_png = NULL, source_note = "calculs propres",
                                  champ = construire_libelle_champ(), fond_regions = NULL) {
  # fond_regions : fond des régions, pour tracer les limites régionales sur une carte départementale (optionnel)
  if (!is.null(fond_regions) && prep$niveau != "departement") fond_regions <- NULL
  titre_page <- sprintf("Départs attendus d’ici 2030 — %s — %s",
                        c(france = "France entière", region = "par région", departement = "par département")[[prep$niveau]],
                        if (prep$dimension == "pcs") "PCS fine" else "grande catégorie socioprofessionnelle")
  tete <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">',
            sprintf('<title>%s</title>', echap_html_carte(titre_page)),
            '<meta name="viewport" content="width=device-width, initial-scale=1">',
            '<style>', css_cartes(), '</style></head><body><div class="page">')
  # vue initiale calculée en R (page lisible même sans JavaScript ni CSS : messageries)
  if (prep$niveau == "france") {
    P <- construire_libelles_public(champ); cat0 <- if (nrow(prep$categories)) prep$categories$code[1] else NA_character_
    tete <- c(tete, html_entete_champ(champ, prep$niveau, prep$dimension,
                                      sous_cat = if (is.na(cat0)) "" else libelle_categorie(prep, cat0, "titre"),
                                      sous_qui = paste0(P$salaries_age, " · France entière")))
    corps <- html_dashboard_national(prep, champ)
  } else {
    if (is.null(fond)) stop("generer_carte_departs : fond requis pour le niveau ", prep$niveau, ".")
    vue <- vue_initiale_carte(prep, names(fond), champ)
    tete <- c(tete, html_entete_champ(champ, prep$niveau, prep$dimension, sous_cat = vue$sous_cat, sous_qui = vue$sous_qui))
    corps <- html_carte_territoriale(prep, fond, titre_page, fond_regions, vue)
  }
  html <- c(tete, corps, html_pied(champ, source_note),
            '<script type="application/json" id="donnees">', donnees_json_carte(prep, champ), '</script>',
            js_details_champ, '<script>', js_cartes(), '</script>', '</div></body></html>')
  dir.create(dirname(fichier_html), showWarnings = FALSE, recursive = TRUE)
  writeLines(enc2utf8(html), fichier_html, useBytes = TRUE)
  if (!is.null(fichier_png) && prep$niveau != "france")
    tryCatch(png_carte_departs(prep, projeter_fond(fond), fichier_png, titre_page, champ,
                               lay_regions = if (!is.null(fond_regions)) projeter_fond(fond_regions) else NULL),
             error = function(e) message("08e : PNG non produit (", conditionMessage(e), ") — le HTML reste la restitution de référence."))
  invisible(fichier_html)
}

html_carte_territoriale <- function(prep, fond, titre_page, fond_regions = NULL, vue = vue_initiale_carte(prep, names(fond))) {
  if (is.null(fond)) stop("html_carte_territoriale : fond requis.")
  lay <- projeter_fond(fond); terr <- prep$territoires; S <- STYLE_CONTOURS_CARTE
  # Attributs fill / stroke de la vue initiale : la carte reste lisible sans JavaScript ni CSS (pièce jointe
  # épurée par une messagerie) ; la CSS et le JS, quand ils sont présents, prennent le dessus.
  paths <- vapply(names(fond), function(code) {
    v <- vue$territoires[[code]]; sans <- identical(v$st, "sans")
    sprintf('<path class="t%s" id="t-%s" data-code="%s" data-st="%s" fill="%s" stroke="%s" stroke-width="%s"%s d="%s" tabindex="0" aria-label="%s"></path>',
            if (sans) " sans" else "", echap_html_carte(code), echap_html_carte(code), v$st, v$fill,
            if (sans) S$sans_couleur else S$couleur, if (sans) S$sans_largeur else S$largeur, if (sans) ' stroke-dasharray="3 2"' else "",
            lay$chemins[[code]], echap_html_carte(terr$nom[match(code, terr$code)])) }, "")
  # Carte départementale : limites régionales (métropole) dessinées au-dessus, sans interaction ;
  # les DROM ne sont pas repris (encart = un seul département = sa région).
  limites_reg <- if (!is.null(fond_regions)) {
    lr <- projeter_fond(fond_regions); metro <- setdiff(names(fond_regions), names(lr$encarts))
    c('<g class="limites-reg" aria-hidden="true">',
      sprintf('<path fill="none" stroke="%s" stroke-width="%s" d="%s"></path>', S$region_couleur, S$region_largeur, lr$chemins[metro]), '</g>')
  } else ""
  survol <- '<path id="survol" fill="none" d="" aria-hidden="true"></path>'   # contour du territoire survolé (même tracé), au-dessus de ses voisins
  encarts <- vapply(lay$encarts, function(e) sprintf('<rect class="encart" fill="none" stroke="#c9ced6" x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="6"></rect><text class="encart-lib" x="%.1f" y="%.1f">%s</text>',
                                                      e$cadre[1] + 1, e$cadre[2] + 1, e$cadre[3] - e$cadre[1] - 2, e$cadre[4] - e$cadre[2] - 2, e$x_lib, e$y_lib, echap_html_carte(e$nom)), "")
  opts_ind <- paste(sprintf('<option value="%s"%s>%s</option>', names(LIBELLES_UI$indicateurs),
                            ifelse(names(LIBELLES_UI$indicateurs) == "taux", " selected", ""), LIBELLES_UI$indicateurs), collapse = "")
  opts_sc <- paste(sprintf('<option value="%s"%s>%s</option>', c("c", "b", "h"), c(" selected", "", ""), c("Centrale", "Basse", "Haute")), collapse = "")
  c('<div class="grille"><aside class="panneau">',
    '<span class="control-label">Vue</span>',
    sprintf('<div class="radios"><label class="on"><input type="radio" name="vue" value="resultats" checked>%s</label><label><input type="radio" name="vue" value="affichables">%s</label></div>',
            LIBELLES_UI$vues[["resultats"]], LIBELLES_UI$vues[["affichables"]]),
    html_selecteur_categorie(prep),
    sprintf('<div id="b-ind"><label class="control-label" for="ind">Que voulez-vous voir ?</label><select id="ind">%s</select></div>', opts_ind),
    sprintf('<div id="b-scen"><label class="control-label" for="scen">Estimation</label><select id="scen">%s</select></div>', opts_sc),
    sprintf('<div class="legend-title" id="legende-titre">%s</div><ul class="map-legend" id="legende">%s</ul>',
            echap_html_carte(vue$legende_titre), paste(html_legende_statique(vue$legende), collapse = "")),
    sprintf('<p class="compte" id="compte">%s</p><p class="note" id="legende-note">%s</p>', vue$compte, echap_html_carte(vue$legende_note)),
    if (length(lay$encarts) > 0) '<p class="note">Outre-mer en encarts, chacun à sa propre échelle.</p>' else "",
    '</aside><div class="carte" id="carte">',
    sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">', lay$largeur, lay$hauteur, echap_html_carte(titre_page)),
    encarts, paths, limites_reg, survol, '</svg><div class="bulle" id="bulle" role="status"></div></div></div>')
}

html_dashboard_national <- function(prep, champ = construire_libelle_champ()) {
  P <- construire_libelles_public(champ)
  # vue initiale statique : chiffres de la première catégorie écrits dans le HTML (lisible sans JavaScript)
  cat0 <- if (nrow(prep$categories)) prep$categories$code[1] else NA_character_
  cat_lib <- if (is.na(cat0)) "" else libelle_categorie(prep, cat0, "titre")
  c('<div class="dash"><aside class="panneau">',
    html_selecteur_categorie(prep),
    sprintf('<p class="cat-affichee"><span class="cat-nom" id="cat-affichee">%s</span><span class="cat-qui" id="cat-qui">%s</span></p>', echap_html_carte(cat_lib), echap_html_carte(P$salaries_age)),
    '</aside><div>', html_bloc_dashboard(prep, cat0, P, ids = TRUE), '</div></div>')
}
# Bloc de chiffres d'une catégorie (KPI, fourchette, causes ; ou bloc « non diffusé ») — ids = TRUE : la page
# interactive (le JS met à jour ces ids) ; FALSE : version courriel (un bloc par catégorie, aucun id).
html_bloc_dashboard <- function(prep, cat0, P, ids = TRUE) {
  id <- function(x) if (ids) sprintf(' id="%s"', x) else ""
  cat_lib <- if (is.na(cat0)) "" else libelle_categorie(prep, cat0, "titre")
  c0 <- prep$cellules[prep$cellules$code == "FR" & prep$cellules$cat %in% cat0, ]
  masque0 <- nrow(c0) == 0 || c0$statut[1] == "masque"
  v <- function(col) if (masque0) NA_real_ else c0[[col]][1]
  fmt0 <- function(x) fmt_fr_carte(x, 0)
  causes <- c(r = v("dep_retraite"), i = v("dep_invalidite"), d = v("dep_deces")); tot <- sum(causes, na.rm = TRUE)
  cols_causes <- c(r = COULEURS_CARTE$classes[4], i = COULEURS_CARTE$classes[2], d = "#6b7280")
  ent <- if (all(is.na(causes))) NULL else arrondir_composantes_avec_total(ifelse(is.na(causes), 0, causes), if (!is.na(v("departs_central"))) v("departs_central") else tot)
  barre <- if (is.null(ent)) "" else paste(sprintf('<span style="width:%s%%;background:%s" title="%s"></span>', formatC(if (tot > 0) 100 * ifelse(is.na(causes), 0, causes) / tot else 0, format = "f", digits = 2, decimal.mark = "."),
                                                   cols_causes, LIBELLES_UI$causes[names(causes)]), collapse = "")
  lis <- if (is.null(ent)) "" else paste(sprintf('<li><span class="sw" style="background:%s"></span><span>%s</span><span class="v">%s</span><span class="p">%s %%</span></li>',
                                                cols_causes, LIBELLES_UI$causes[names(causes)], fmt0(ent),
                                                fmt_fr_carte(if (tot > 0) 100 * ifelse(is.na(causes), 0, causes) / tot else 0, 0)), collapse = "")
  c(sprintf('<div%s%s>', id("zone-kpi"), if (masque0) ' style="display:none"' else ""),
    '<div class="kpis">',
    sprintf('<div class="kpi primaire"><div class="kpi-value"%s>%s</div><div class="kpi-label">%s</div><div class="kpi-help">%s</div></div>', id("kpi-c"), fmt0(v("departs_central")), P$kpi$departs, P$kpi_aide$departs),
    sprintf('<div class="kpi"><div class="kpi-value"%s>%s</div><div class="kpi-label">%s</div><div class="kpi-help">%s</div></div>', id("kpi-t"), if (is.na(v("taux_depart_central_pct"))) "n.d." else paste0(fmt_fr_carte(v("taux_depart_central_pct"), 1), " %"), P$kpi$taux, P$kpi_aide$taux),
    sprintf('<div class="kpi"><div class="kpi-value"%s>%s</div><div class="kpi-label">%s</div><div class="kpi-help">%s</div></div>', id("kpi-e"), fmt0(v("effectif_champ")), P$kpi$salaries, P$kpi_aide$salaries),
    '</div>',
    '<div class="bloc"><h2>Combien de départs, selon l’hypothèse retenue ?</h2><div class="fourchette">',
    sprintf('<div><div class="v"%s>%s</div><div class="l">%s</div></div><div class="centrale"><div class="v"%s>%s</div><div class="l">%s</div></div><div><div class="v"%s>%s</div><div class="l">%s</div></div>',
            id("f-b"), fmt0(v("departs_bas")), LIBELLES_UI$scenarios[["b"]], id("f-c"), fmt0(v("departs_central")), LIBELLES_UI$scenarios[["c"]], id("f-h"), fmt0(v("departs_haut")), LIBELLES_UI$scenarios[["h"]]),
    '</div><p class="note">L’estimation centrale est encadrée par deux hypothèses, basse et haute, sur l’âge de départ en retraite.</p></div>',
    sprintf('<div class="bloc"%s%s><h2>Pourquoi ces salariés partiraient-ils ?</h2><div class="barre"%s aria-hidden="true">%s</div><ul class="causes"%s>%s</ul>',
            id("bloc-causes"), if (is.null(ent)) ' style="display:none"' else "", id("barre"), barre, id("causes"), lis),
    '<p class="note">Répartition des départs estimés (estimation centrale) par motif.</p></div>',
    '</div>',
    sprintf('<div class="masque-national"%s%s><div class="t"%s>%s</div><p><b%s>%s</b> <span%s>%s</span></p><p>Les autres catégories restent consultables ci-contre.</p></div>',
            id("zone-masque"), if (masque0) "" else ' style="display:none"', id("masque-cat"), echap_html_carte(cat_lib),
            id("masque-titre"), if (nrow(c0) == 0) LIBELLES_UI$sans_donnee else LIBELLES_UI$secret_court,
            id("masque-txt"), if (nrow(c0) == 0) paste0("Aucun salarié de cette ", if (prep$dimension == "pcs") "PCS" else "catégorie", " n’est observé.") else LIBELLES_UI$secret_explication))
}

# --- VERSION COURRIEL : aucun script, aucun contenu actif --------------------------
# Les passerelles de messagerie (SISMEL…) retirent ou bloquent tout « contenu actif »
# d'une pièce jointe HTML, y compris un bloc de données JSON dans <script>. Cette
# version n'en contient AUCUN : toutes les catégories sont précalculées en R
# (vue_initiale_carte par catégorie), le changement de catégorie est fait en CSS
# pur (boutons radio + sélecteur :checked), les infobulles sont les <title> natifs
# du SVG. Géométrie écrite une fois (<defs>) et réutilisée par <use>. Un seul
# indicateur (part des salariés susceptibles de partir, estimation centrale) ; les
# infobulles portent salariés, départs, fourchette et part. Même source que la
# page interactive : aucune valeur masquée n'y figure.
css_courriel <- function(n_cat) paste(
  '.courriel>input{position:absolute;opacity:0;width:0;height:0}',
  '.courriel .vue{display:none}.courriel .cats{list-style:none;margin:0;padding:0;max-height:440px;overflow:auto;border:1px solid #AEB7C2;border-radius:8px}',
  '.courriel .cats label{display:block;padding:8px 10px;cursor:pointer;font-size:15px;border-bottom:1px solid var(--filet)}.courriel .cats li:last-child label{border-bottom:0}',
  '.courriel svg{width:100%;height:auto;display:block}.courriel .vue-titre{font-size:20px;font-weight:700;margin:0 0 4px}.courriel .vue-qui{color:var(--texte);margin:0 0 12px}',
  paste(sprintf('#cat-%d:checked~.grille .vue-%d,#cat-%d:checked~.dash .vue-%d{display:block}#cat-%d:checked~.grille label[for=cat-%d],#cat-%d:checked~.dash label[for=cat-%d]{background:var(--bleu);color:#fff}',
                seq_len(n_cat), seq_len(n_cat), seq_len(n_cat), seq_len(n_cat), seq_len(n_cat), seq_len(n_cat), seq_len(n_cat), seq_len(n_cat)), collapse = ""),
  sep = "\n")
infobulle_courriel <- function(prep, code, nom, cat, P) {
  r <- prep$cellules[prep$cellules$code == code & prep$cellules$cat == cat, ]
  tete <- paste0(nom, " · ", code, "\n", libelle_categorie(prep, cat, "titre"), "\n")
  if (nrow(r) == 0) return(paste0(tete, LIBELLES_UI$sans_donnee))
  if (r$statut[1] == "masque") return(paste0(tete, LIBELLES_UI$secret))
  l <- character(0)
  if (!is.na(r$effectif_champ[1])) l <- c(l, paste(fmt_fr_carte(r$effectif_champ[1]), P$salaries_age_min))
  if (!is.na(r$departs_central[1])) l <- c(l, paste(fmt_fr_carte(r$departs_central[1]), P$kpi$departs))
  if (!is.na(r$departs_bas[1]) && !is.na(r$departs_haut[1])) l <- c(l, sprintf("entre %s et %s selon l’hypothèse", fmt_fr_carte(r$departs_bas[1]), fmt_fr_carte(r$departs_haut[1])))
  if (!is.na(r$taux_depart_central_pct[1])) l <- c(l, paste0(fmt_fr_carte(r$taux_depart_central_pct[1], 1), " % ", P$kpi$taux))
  paste0(tete, paste(l, collapse = "\n"))
}
html_radios_courriel <- function(prep) {
  cats <- prep$categories$code
  list(inputs = sprintf('<input type="radio" name="cat" id="cat-%d"%s>', seq_along(cats), ifelse(seq_along(cats) == 1, " checked", "")),
       labels = c(sprintf('<span class="control-label">%s</span><ul class="cats">', if (prep$dimension == "pcs") "Quelle PCS ?" else "Quelle catégorie ?"),
                  sprintf('<li><label for="cat-%d">%s</label></li>', seq_along(cats), echap_html_carte(vapply(cats, function(k) libelle_categorie(prep, k, "option"), ""))), '</ul>'))
}
html_carte_courriel <- function(prep, fond, titre_page, champ, fond_regions = NULL) {
  lay <- projeter_fond(fond); terr <- prep$territoires; S <- STYLE_CONTOURS_CARTE; P <- construire_libelles_public(champ)
  noms <- setNames(terr$nom[match(names(fond), terr$code)], names(fond))
  defs <- c('<svg width="0" height="0" style="position:absolute" aria-hidden="true"><defs>',
            sprintf('<path id="g-%s" d="%s"></path>', echap_html_carte(names(fond)), lay$chemins),
            if (!is.null(fond_regions)) { lr <- projeter_fond(fond_regions); metro <- setdiff(names(fond_regions), names(lr$encarts))
              c('<g id="g-regions">', sprintf('<path fill="none" stroke="%s" stroke-width="%s" d="%s"></path>', S$region_couleur, S$region_largeur, lr$chemins[metro]), '</g>') } else "",
            '</defs></svg>')
  encarts <- vapply(lay$encarts, function(e) sprintf('<rect fill="none" stroke="#c9ced6" x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="6"></rect><text class="encart-lib" fill="#3E4C59" x="%.1f" y="%.1f">%s</text>',
                                                      e$cadre[1] + 1, e$cadre[2] + 1, e$cadre[3] - e$cadre[1] - 2, e$cadre[4] - e$cadre[2] - 2, e$x_lib, e$y_lib, echap_html_carte(e$nom)), "")
  radios <- html_radios_courriel(prep)
  vues <- unlist(lapply(seq_len(nrow(prep$categories)), function(i) {
    cat <- prep$categories$code[i]; v <- vue_initiale_carte(prep, names(fond), champ, cat = cat)
    uses <- vapply(names(fond), function(code) { t <- v$territoires[[code]]; sans <- identical(t$st, "sans")
      sprintf('<use href="#g-%s" fill="%s" stroke="%s" stroke-width="%s"%s><title>%s</title></use>', echap_html_carte(code), t$fill,
              if (sans) S$sans_couleur else S$couleur, if (sans) S$sans_largeur else S$largeur, if (sans) ' stroke-dasharray="3 2"' else "",
              echap_html_carte(infobulle_courriel(prep, code, noms[[code]], cat, P))) }, "")
    c(sprintf('<section class="vue vue-%d">', i),
      sprintf('<p class="vue-titre">%s</p><p class="vue-qui">%s</p>', echap_html_carte(v$sous_cat), echap_html_carte(v$sous_qui)),
      sprintf('<div class="legend-title">%s</div><ul class="map-legend">%s</ul><p class="compte">%s</p>', echap_html_carte(v$legende_titre), paste(html_legende_statique(v$legende), collapse = ""), v$compte),
      sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">', lay$largeur, lay$hauteur, echap_html_carte(paste(titre_page, v$sous_cat, sep = " — "))),
      encarts, uses, if (!is.null(fond_regions)) '<use href="#g-regions"></use>' else "", '</svg>',
      sprintf('<p class="note">%s Survolez un territoire pour lire ses chiffres.</p>', echap_html_carte(v$legende_note)), '</section>') }))
  c('<div class="courriel">', radios$inputs, defs,
    '<div class="grille"><aside class="panneau">', radios$labels,
    if (length(lay$encarts) > 0) '<p class="note">Outre-mer en encarts, chacun à sa propre échelle.</p>' else "", '</aside>',
    '<div class="carte">', vues, '</div></div></div>')
}
html_dashboard_courriel <- function(prep, champ) {
  P <- construire_libelles_public(champ); radios <- html_radios_courriel(prep)
  vues <- unlist(lapply(seq_len(nrow(prep$categories)), function(i) {
    cat <- prep$categories$code[i]
    c(sprintf('<section class="vue vue-%d">', i),
      sprintf('<p class="vue-titre">%s</p><p class="vue-qui">%s · France entière</p>', echap_html_carte(libelle_categorie(prep, cat, "titre")), echap_html_carte(P$salaries_age)),
      html_bloc_dashboard(prep, cat, P, ids = FALSE), '</section>') }))
  c('<div class="courriel">', radios$inputs, '<div class="dash"><aside class="panneau">', radios$labels, '</aside><div>', vues, '</div></div></div>')
}
generer_carte_courriel <- function(prep, fond = NULL, fichier_html, champ = construire_libelle_champ(), fond_regions = NULL, source_note = "calculs propres") {
  if (!is.null(fond_regions) && prep$niveau != "departement") fond_regions <- NULL
  titre_page <- sprintf("Départs attendus d’ici 2030 — %s — %s", c(france = "France entière", region = "par région", departement = "par département")[[prep$niveau]],
                        if (prep$dimension == "pcs") "PCS fine" else "grande catégorie socioprofessionnelle")
  P <- construire_libelles_public(champ); cat0 <- if (nrow(prep$categories)) prep$categories$code[1] else NA_character_
  tete <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">', sprintf('<title>%s</title>', echap_html_carte(titre_page)),
            '<meta name="viewport" content="width=device-width, initial-scale=1">',
            '<style>', css_cartes(), css_courriel(nrow(prep$categories)), '</style></head><body><div class="page">')
  entete <- html_entete_champ(champ, prep$niveau, prep$dimension, sous_cat = "Version pour envoi par courriel",
                              sous_qui = "Sans script : changez de catégorie dans la liste, survolez un territoire pour lire ses chiffres.")
  entete <- entete[!grepl('id="sans-js"', entete, fixed = TRUE)]            # pas de bandeau : rien à masquer, c'est la version prévue
  corps <- if (prep$niveau == "france") html_dashboard_courriel(prep, champ) else {
    if (is.null(fond)) stop("generer_carte_courriel : fond requis pour le niveau ", prep$niveau, ".")
    html_carte_courriel(prep, fond, titre_page, champ, fond_regions) }
  html <- c(tete, entete, corps, html_pied(champ, source_note), '</div></body></html>')
  if (any(grepl("<script", html, fixed = TRUE)) || any(grepl(" on[a-z]+=", html)) || any(grepl("javascript:", html, fixed = TRUE)))
    stop("generer_carte_courriel : contenu actif détecté dans la version courriel.")
  dir.create(dirname(fichier_html), showWarnings = FALSE, recursive = TRUE)
  writeLines(enc2utf8(html), fichier_html, useBytes = TRUE)
  invisible(fichier_html)
}


# --- PNG de la vue initiale d'une carte territoriale (vue Diffusabilité) --------
png_carte_departs <- function(prep, lay, fichier_png, titre = "", champ = construire_libelle_champ(), lay_regions = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 absent")
  S <- STYLE_CONTOURS_CARTE
  long <- lay$long; cv <- prep$couverture; LB <- LIBELLES_UI$couverture
  k <- function(p) if (is.na(p)) LIBELLES_UI$sans_donnee else if (p <= 0) LB$aucune else LB$libelles[max(which(p >= LB$bornes))]
  long$classe <- factor(vapply(cv$part_diff_pct[match(long$code, cv$code)], k, ""),
                        levels = c(rev(LB$libelles), LB$aucune, LIBELLES_UI$sans_donnee))
  cols <- c(rev(COULEURS_CARTE$classes[round(seq(0, 3) * 4 / 3) + 1]), COULEURS_CARTE$secret, COULEURS_CARTE$sans_donnee)
  enc <- if (length(lay$encarts)) bind_rows(lapply(lay$encarts, function(e) tibble(x1 = e$cadre[1], x2 = e$cadre[3], y1 = -e$cadre[2], y2 = -e$cadre[4], lib = e$nom, xl = e$x_lib, yl = -e$y_lib))) else NULL
  g <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = long, ggplot2::aes(x = x, y = y, group = groupe, fill = classe), colour = S$png_couleur, linewidth = S$png_largeur) +
    ggplot2::scale_fill_manual(values = setNames(cols, levels(long$classe)), drop = FALSE, name = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = S$png_couleur))) +
    ggplot2::coord_equal(expand = FALSE) + ggplot2::theme_void(base_size = 12) +
    ggplot2::labs(title = paste(LIBELLES_UI$couverture$titre, sub("^.*— ", "", titre)),
                  subtitle = paste0("Part des ", if (prep$dimension == "pcs") "PCS" else "catégories", " pour lesquelles un résultat peut être affiché\n",
                                    toupper(substr(champ$court, 1, 1)), substr(champ$court, 2, nchar(champ$court))),
                  caption = "Gris : résultat non diffusé (secret statistique) · blanc : aucun salarié observé · aucun chiffre non diffusé n’est représenté. Fond IGN Admin Express via france-geojson.") +
    ggplot2::theme(legend.position = "bottom", legend.direction = "vertical", legend.text = ggplot2::element_text(size = 11),
                   plot.title = ggplot2::element_text(face = "bold", size = 15, colour = COULEURS_CARTE$encre),
                   plot.subtitle = ggplot2::element_text(colour = COULEURS_CARTE$texte, size = 11),
                   plot.caption = ggplot2::element_text(colour = COULEURS_CARTE$texte, size = 9), plot.margin = ggplot2::margin(10, 10, 10, 10))
  if (!is.null(lay_regions)) {                       # limites régionales (métropole) au-dessus des départements
    reg <- lay_regions$long[!(lay_regions$long$code %in% names(lay_regions$encarts)), ]
    g <- g + ggplot2::geom_polygon(data = reg, ggplot2::aes(x = x, y = y, group = groupe), fill = NA,
                                   colour = S$region_couleur, linewidth = S$png_region_largeur)
  }
  if (!is.null(enc)) g <- g + ggplot2::geom_rect(data = enc, ggplot2::aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1), fill = NA, colour = "#c9ced6") +
    ggplot2::geom_text(data = enc, ggplot2::aes(x = xl, y = yl, label = lib), hjust = 0, size = 3.6, colour = COULEURS_CARTE$texte)
  ggplot2::ggsave(fichier_png, g, width = 8, height = 10.8, dpi = 130, bg = "white")
  invisible(fichier_png)
}
