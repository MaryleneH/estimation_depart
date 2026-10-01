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
# Libellés utilisateur : AUCUN nom de colonne R n'atteint l'interface.
LIBELLES_UI <- list(
  indicateurs = c(taux = "Taux de départ estimé", departs = "Départs estimés d’ici 2030", effectif = "Salariés dans le champ étudié"),
  scenarios   = c(b = "Estimation basse", c = "Estimation centrale", h = "Estimation haute"),
  causes      = c(r = "Retraite / fin de carrière", i = "Invalidité", d = "Décès"),
  secret      = "Non diffusé — secret statistique",
  sans_donnee = "Pas de donnée observée",
  couverture  = list(bornes = c(1, 25, 75, 100),
                     libelles = c("Peu diffusables (1 à 24 %)", "Partiellement diffusables (25 à 74 %)",
                                  "Majoritairement diffusables (75 à 99 %)", "Toutes diffusables (100 %)"),
                     aucune = "Aucune diffusable — secret statistique"),
  denominateur = "Part des salariés de la catégorie appartenant au champ étudié.")
COLONNES_MESURES_CARTE <- c(e = "effectif_champ", c = "departs_central", b = "departs_bas", h = "departs_haut",
                            t = "taux_depart_central_pct", r = "dep_retraite", i = "dep_invalidite", d = "dep_deces")
DROM_CARTE <- c("971" = "Guadeloupe", "972" = "Martinique", "973" = "Guyane", "974" = "La Réunion", "976" = "Mayotte")
DROM_REGION_CARTE <- c("01" = "971", "02" = "972", "03" = "973", "04" = "974", "06" = "976")   # région DROM -> département

# --- CHAMP DE L'ÉTUDE : source unique, construite depuis la configuration ------
# Lu au moment de l'appel (jamais figé) : un changement d'AGE_MIN_BTS ou de
# PCS_VERS_CS1 change toutes les pages. Retourne court / titre / detaille.
construire_libelle_champ <- function(age_min = get0("AGE_MIN_BTS", ifnotfound = NA),
                                     cs = get0("PCS_VERS_CS1", ifnotfound = NULL),
                                     hors = get0("PCS_HORS_CHAMP", ifnotfound = character(0)),
                                     annee = get0("ANNEE_REF_GRAPHIQUE", ifnotfound = 2024),
                                     horizon = get0("HORIZON", ifnotfound = 6)) {
  if (is.na(age_min)) stop("construire_libelle_champ : AGE_MIN_BTS introuvable (00_config.R).")
  noms_cs <- c("Cadres" = "cadres", "Prof. intermediaires" = "professions intermédiaires", "Employes" = "employés", "Ouvriers" = "ouvriers")
  cats <- if (is.null(cs)) character(0) else unname(ifelse(cs %in% names(noms_cs), noms_cs[cs], tolower(cs)))
  hors_lib <- c("1" = "agriculteurs exploitants", "2" = "artisans, commerçants et chefs d’entreprise")
  hors_txt <- paste(c(unname(hors_lib[intersect(as.character(hors), names(hors_lib))]), "code PCS non renseigné ou non rattachable"), collapse = ", ")
  court <- sprintf("salariés du périmètre BITD âgés de %d ans ou plus", as.integer(age_min))
  detaille <- c(
    sprintf("Population étudiée : salariés présents en %d dans la Base Tous salariés, employés par une entreprise (SIREN) du périmètre BITD et âgés de %d ans ou plus.", as.integer(annee), as.integer(age_min)),
    if (length(cats)) sprintf("Catégories couvertes par le modèle : %s (grandes catégories socioprofessionnelles %s de la PCS).",
                              paste(cats, collapse = ", "), paste(names(cs), collapse = ", ")),
    sprintf("Hors champ : les salariés dont le code PCS ne peut pas être rattaché à ces catégories (%s) sont exclus de l’estimation.", hors_txt),
    sprintf("Départ : sortie définitive de l’emploi d’ici %d (retraite ou fin de carrière, invalidité, décès) ; les mobilités vers un autre employeur ne sont pas comptées.", as.integer(annee + horizon)),
    paste0("Taux de départ : ", tolower(substr(LIBELLES_UI$denominateur, 1, 1)), substr(LIBELLES_UI$denominateur, 2, nchar(LIBELLES_UI$denominateur))))
  list(court = court, titre = paste("Champ ·", court), detaille = detaille, age_min = as.integer(age_min),
       categories = cats, annee = as.integer(annee), horizon = as.integer(annee + horizon))
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

# --- Préparation : table de diffusion -> structure de carte (sans aucune fuite) --
preparer_carte_departs <- function(donnees, dimension = c("pcs", "cs1"),
                                   niveau = c("france", "region", "departement"), fond = NULL) {
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
    r <- prep$categories[i, ]; if (is.na(r$cs1)) list(code = r$code) else list(code = r$code, cs1 = r$cs1) })
  terr <- lapply(seq_len(nrow(prep$territoires)), function(i) list(code = prep$territoires$code[i], nom = prep$territoires$nom[i]))
  j <- jsonlite::toJSON(list(dimension = prep$dimension, niveau = prep$niveau,
                             categories = cats, territoires = terr, cellules = cel, couverture = cov,
                             couleurs = COULEURS_CARTE[c("classes", "secret", "sans_donnee")],
                             libelles = rapply(LIBELLES_UI, function(x) if (!is.null(names(x))) as.list(x) else x, how = "replace"),
                             champ = champ[c("court", "titre")],
                             libelles_cs = as.list(c("Cadres" = "Cadres", "Prof. intermediaires" = "Professions intermédiaires",
                                                     "Employes" = "Employés", "Ouvriers" = "Ouvriers"))),
                       auto_unbox = TRUE, digits = NA, na = "null", null = "null")
  gsub("</", "<\\/", j, fixed = TRUE)
}

# --- CSS (centralisé ; classes dédiées au champ, aux KPI, à la légende) ---------
css_cartes <- function() paste(
  sprintf(':root{--encre:%s;--texte:%s;--filet:%s;--fond:%s;--bleu:%s;--secret:%s}', COULEURS_CARTE$encre, COULEURS_CARTE$texte,
          COULEURS_CARTE$filet, COULEURS_CARTE$fond, COULEURS_CARTE$bleu, COULEURS_CARTE$secret),
  '*{box-sizing:border-box}html{-webkit-text-size-adjust:100%}body{margin:0;background:#fff;color:var(--encre);font-family:-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif;font-size:16px;line-height:1.5}',
  '.page{max-width:1360px;margin:0 auto;padding:28px 32px 40px}@media(max-width:700px){.page{padding:18px 16px 32px}}',
  # en-tête : vraie zone, jamais tronquée
  '.page-header{border-bottom:1px solid var(--filet);padding-bottom:18px;margin-bottom:22px}',
  '.kicker{font-size:13px;letter-spacing:.12em;text-transform:uppercase;color:var(--texte);font-weight:600;margin:0 0 8px}',
  '.page-title{font-size:32px;line-height:1.15;font-weight:700;margin:0;letter-spacing:-.01em}.page-title small{display:block;font-size:22px;font-weight:500;color:var(--texte);margin-top:2px}',
  '.page-subtitle{font-size:19px;color:var(--encre);margin:12px 0 0;font-weight:500}.page-subtitle .sep{color:var(--texte);margin:0 8px}',
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
  '.legend-title{font-size:15px;font-weight:700;margin:16px 0 6px;color:var(--encre)}.map-legend{list-style:none;margin:0;padding:0}.map-legend li{display:flex;align-items:center;gap:10px;font-size:15px;margin:6px 0;color:var(--encre)}.map-legend .sw{width:26px;height:16px;border-radius:3px;border:1px solid #c9ced6;flex:none}.map-legend .sw.sans{border:1.5px dashed #7b8794}.map-legend li.sep{border-top:1px solid var(--filet);margin-top:10px;padding-top:10px}',
  '.compte{font-size:15px;color:var(--encre);margin:10px 0 0}.compte b{font-weight:700}.note{font-size:13.5px;color:var(--texte);margin:10px 0 0;line-height:1.45}',
  # carte
  '.carte{position:relative;min-width:0}.carte svg{width:100%;height:auto;display:block}',
  'path.t{stroke:#fff;stroke-width:.9;cursor:pointer}path.t:hover,path.t:focus{stroke:#1F2933;stroke-width:1.8;outline:none}path.t.sans{stroke:#8e99a4;stroke-dasharray:3 2;stroke-width:.8}',
  '.encart{fill:none;stroke:#c9ced6;stroke-width:1.2}.encart-lib{font-size:17px;font-weight:600;fill:#3E4C59}',
  # infobulle
  '.bulle{position:absolute;pointer-events:none;background:#1F2933;color:#fff;border-radius:10px;padding:14px 16px;font-size:15px;line-height:1.5;min-width:240px;max-width:320px;box-shadow:0 8px 24px rgba(0,0,0,.22);display:none;z-index:2}',
  '.tooltip-title{font-size:17px;font-weight:700;margin:0 0 2px}.tooltip-cat{color:#cfd6df;margin:0 0 8px;font-weight:500}.tooltip-rows{border-top:1px solid rgba(255,255,255,.25);padding-top:8px;width:100%;border-collapse:collapse}.tooltip-rows td{padding:3px 0;vertical-align:top}.tooltip-rows td.tooltip-value{text-align:right;font-weight:700;padding-left:16px;white-space:nowrap}.tooltip-secret{color:#ffd7a8;font-weight:700;margin-top:8px}.tooltip-secret small{display:block;color:#f3f4f6;font-weight:400}.tooltip-sans{color:#e5e7eb;font-weight:600;margin-top:8px}',
  # tableau de bord national
  '.dash{display:grid;grid-template-columns:320px minmax(0,1fr);gap:24px;align-items:start}@media(max-width:900px){.dash{grid-template-columns:1fr}}',
  '.cat-affichee{font-size:17px;margin:14px 0 0;color:var(--encre)}.cat-affichee b{font-weight:700}',
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
  'var D=JSON.parse(document.getElementById("donnees").textContent),L=D.libelles;var FR=D.niveau==="france";',
  'var $=function(id){return document.getElementById(id)};',
  'var nf=function(d){return new Intl.NumberFormat("fr-FR",{minimumFractionDigits:d,maximumFractionDigits:d})};',
  'var fmt=function(x,d){return (x==null||isNaN(x))?"n.d.":nf(d).format(x)};var pct=function(x,d){return fmt(x,d)+" %"};',
  'var noms={};D.territoires.forEach(function(t){noms[t.code]=t.nom});',
  'var etat={vue:"resultats",cat:D.categories.length?D.categories[0].code:null,ind:"taux",scen:"c"};',
  'var dimLib=D.dimension==="pcs"?"PCS":"Catégorie",dimPl=D.dimension==="pcs"?"PCS":"catégories";',
  'var nivLib={france:"France entière",region:"par région",departement:"par département"}[D.niveau];',
  'var catLib=function(c){var o=D.categories.filter(function(k){return k.code===c})[0];if(!o)return c;return D.dimension==="pcs"?("PCS "+o.code+(o.cs1?" · "+(D.libelles_cs[o.cs1]||o.cs1):"")):(D.libelles_cs[o.code]||o.code)};',
  'var catOpt=function(k){return D.dimension==="pcs"?(k.code+(k.cs1?" — "+(D.libelles_cs[k.cs1]||k.cs1):"")):(D.libelles_cs[k.code]||k.code)};',
  'function remplirCats(filtre){var s=$("cat");if(!s)return;var f=(filtre||"").toLowerCase();s.innerHTML="";D.categories.forEach(function(k){var lib=catOpt(k);if(f&&lib.toLowerCase().indexOf(f)<0)return;var o=document.createElement("option");o.value=k.code;o.textContent=lib;s.appendChild(o)});if(s.options.length){var ok=false;for(var i=0;i<s.options.length;i++)if(s.options[i].value===etat.cat){ok=true;break}if(!ok)etat.cat=s.options[0].value;s.value=etat.cat}}',
  'function cle(){return etat.ind==="departs"?etat.scen:(etat.ind==="effectif"?"e":"t")}',
  'var dec={t:1,c:1,b:1,h:1,e:0,r:1,i:1,d:1};',
  'function valeur(code){var c=D.cellules[code]&&D.cellules[code][etat.cat];if(!c)return{st:"sans"};if(c.s==="m")return{st:"masque"};var v=c[cle()];return{st:"diffuse",v:v,c:c}}',
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
  'function rendreCarte(){var paths=document.querySelectorAll("path.t"),leg=$("legende");if(!leg)return;leg.innerHTML="";var nD=0,nM=0,nS=0,cl=null;',
  ' if(etat.vue==="resultats"){var vals=[];Object.keys(D.couverture).forEach(function(code){var r=valeur(code);if(r.st==="diffuse"&&r.v!=null)vals.push(r.v)});cl=classesRondes(vals)}',
  ' paths.forEach(function(p){var code=p.getAttribute("data-code"),fill,st;p.classList.remove("sans");',
  '  if(etat.vue==="diffusabilite"){var cv=D.couverture[code];if(!cv){st="sans";fill=D.couleurs.sans_donnee}else{var k=classeCouv(cv.p);if(k<0){st="masque";fill=D.couleurs.secret}else{st="diffuse";fill=couleur(k,L.couverture.bornes.length)}}}',
  '  else{var r2=valeur(code);st=r2.st;fill=st==="masque"?D.couleurs.secret:(st==="sans"?D.couleurs.sans_donnee:(cl?couleur(classeDe(r2.v,cl),cl.unique?cl.vals.length:cl.n):D.couleurs.classes[2]))}',
  '  if(st==="sans")p.classList.add("sans");p.setAttribute("fill",fill);p.setAttribute("data-st",st);if(st==="diffuse")nD++;else if(st==="masque")nM++;else nS++});',
  ' var titreLeg=$("legende-titre");',
  ' if(etat.vue==="diffusabilite"){titreLeg.textContent="Part des "+dimPl+" diffusables";var LB=L.couverture.libelles;for(var i=LB.length-1;i>=0;i--)li(leg,couleur(i,LB.length),LB[i]);li(leg,D.couleurs.secret,L.couverture.aucune,false,true);$("legende-note").textContent="Part des "+dimPl+" observées dans le territoire dont le résultat est diffusable. Le statut des cellules n’indique aucune valeur masquée."}',
  ' else{titreLeg.textContent=L.indicateurs[etat.ind]+(etat.ind==="departs"?" — "+L.scenarios[etat.scen].toLowerCase():"");',
  '  if(!cl)li(leg,D.couleurs.classes[2],"Aucune valeur diffusée pour cette catégorie");',
  '  else if(cl.unique){cl.vals.forEach(function(v,i){li(leg,couleur(i,cl.vals.length),fmtInd(v,false))})}',
  '  else{for(var k=0;k<cl.n;k++){var txt=k===0?"Moins de "+fmtInd(cl.bornes[0],true):(k===cl.n-1?fmtInd(cl.bornes[k-1],true)+" et plus":fmtInd(cl.bornes[k-1],true)+" à "+fmtInd(cl.bornes[k],true));li(leg,couleur(k,cl.n),txt)}}',
  '  li(leg,D.couleurs.secret,L.secret,false,true);',
  '  $("legende-note").textContent=(etat.ind==="taux"?L.denominateur+" ":"")+"Même échelle pour tous les territoires de la catégorie affichée ; les classes sont recalculées quand vous changez de catégorie ou d’indicateur."}',
  ' li(leg,D.couleurs.sans_donnee,L.sans_donnee,true);',
  ' $("compte").innerHTML="<b>"+nD+"</b> territoire(s) avec résultat · <b>"+nM+"</b> non diffusé(s) (secret statistique) · <b>"+nS+"</b> sans donnée";',
  ' if(etat.vue==="diffusabilite"){$("titre").innerHTML="";$("titre").appendChild(document.createTextNode("Où les résultats peuvent-ils être diffusés ?"));$("titre").appendChild(Object.assign(document.createElement("small"),{textContent:nivLib}));$("sous").textContent="Part des "+dimPl+" observées dont les résultats sont diffusables"}',
  ' else{$("titre").innerHTML="";$("titre").appendChild(document.createTextNode("Départs attendus d’ici 2030"));$("titre").appendChild(Object.assign(document.createElement("small"),{textContent:nivLib}));$("sous").textContent=catLib(etat.cat)+" · "+L.indicateurs[etat.ind]+(etat.ind==="departs"?" ("+L.scenarios[etat.scen].toLowerCase()+")":"")}',
  ' var cat=etat.vue==="resultats";$("b-cat").style.display=cat?"":"none";$("b-ind").style.display=cat?"":"none";$("b-scen").style.display=(cat&&etat.ind==="departs")?"":"none";}',
  # infobulle éditoriale
  'function row(t,lab,val){var tr=document.createElement("tr"),a=document.createElement("td"),b=document.createElement("td");a.textContent=lab;b.textContent=val;b.className="tooltip-value";tr.appendChild(a);tr.appendChild(b);t.appendChild(tr)}',
  'function bulle(code){var el=document.createElement("div");var h=document.createElement("div");h.className="tooltip-title";h.textContent=noms[code]+" · "+code;el.appendChild(h);',
  ' if(etat.vue==="diffusabilite"){var cv=D.couverture[code];var t=document.createElement("table");t.className="tooltip-rows";if(!cv){var s=document.createElement("div");s.className="tooltip-sans";s.textContent=L.sans_donnee;el.appendChild(s)}else{row(t,(D.dimension==="pcs"?"PCS":"Catégories")+" observées",fmt(cv.o,0));row(t,"Diffusables",fmt(cv.d,0));row(t,"Non diffusées",fmt(cv.m,0));row(t,"Part diffusable",pct(cv.p,0));el.appendChild(t);if(cv.d===0){var s2=document.createElement("div");s2.className="tooltip-secret";s2.textContent=L.couverture.aucune;el.appendChild(s2)}}}',
  ' else{var c0=document.createElement("div");c0.className="tooltip-cat";c0.textContent=catLib(etat.cat);el.appendChild(c0);var r=valeur(code);',
  '  if(r.st==="sans"){var s3=document.createElement("div");s3.className="tooltip-sans";s3.textContent=L.sans_donnee;el.appendChild(s3)}',
  '  else if(r.st==="masque"){var s4=document.createElement("div");s4.className="tooltip-secret";s4.textContent="Non diffusé";var sm=document.createElement("small");sm.textContent="Secret statistique";s4.appendChild(sm);el.appendChild(s4)}',
  '  else{var c=r.c,t2=document.createElement("table");t2.className="tooltip-rows";if(c.e!=null)row(t2,"Salariés dans le champ",fmt(c.e,0));if(c.c!=null)row(t2,"Départs estimés",fmt(c.c,1));if(c.b!=null&&c.h!=null)row(t2,"Fourchette basse – haute",fmt(c.b,1)+" – "+fmt(c.h,1));if(c.t!=null)row(t2,"Taux de départ",pct(c.t,1));el.appendChild(t2)}}',
  ' return el.innerHTML}',
  # tableau de bord national
  'function rendreDash(){var c=D.cellules["FR"]&&D.cellules["FR"][etat.cat];$("cat-affichee").innerHTML="";$("cat-affichee").appendChild(document.createTextNode((D.dimension==="pcs"?"PCS affichée · ":"Catégorie affichée · ")));$("cat-affichee").appendChild(Object.assign(document.createElement("b"),{textContent:catLib(etat.cat)}));',
  ' $("sous").textContent=catLib(etat.cat);var z=$("zone-kpi"),m=$("zone-masque");',
  ' if(!c||c.s==="m"){z.style.display="none";m.style.display="";$("masque-cat").textContent=catLib(etat.cat);$("masque-txt").textContent=c?"Cette information est masquée en application du secret statistique.":"Aucune donnée observée pour cette catégorie dans le champ étudié.";return}',
  ' z.style.display="";m.style.display="none";$("kpi-e").textContent=fmt(c.e,0);$("kpi-c").textContent=fmt(c.c,1);$("kpi-t").textContent=c.t!=null?pct(c.t,1):"n.d.";',
  ' $("f-b").textContent=fmt(c.b,1);$("f-c").textContent=fmt(c.c,1);$("f-h").textContent=fmt(c.h,1);',
  ' var tot=(c.r||0)+(c.i||0)+(c.d||0),bar=$("barre"),ul=$("causes");bar.innerHTML="";ul.innerHTML="";var cols={r:D.couleurs.classes[3],i:D.couleurs.classes[1],d:"#6b7280"};',
  ' if(c.r==null&&c.i==null&&c.d==null){$("bloc-causes").style.display="none"}else{$("bloc-causes").style.display="";["r","i","d"].forEach(function(k){var v=c[k]||0,p=tot>0?100*v/tot:0;var s=document.createElement("span");s.style.width=p+"%";s.style.background=cols[k];s.title=L.causes[k];bar.appendChild(s);var l=document.createElement("li");l.innerHTML="<span class=\\"sw\\" style=\\"background:"+cols[k]+"\\"></span><span></span><span class=\\"v\\"></span><span class=\\"p\\"></span>";l.children[1].textContent=L.causes[k];l.children[2].textContent=fmt(v,1);l.children[3].textContent=pct(p,0);ul.appendChild(l)})}}',
  'function rendre(){if(FR)rendreDash();else rendreCarte()}',
  # événements
  'var B=$("bulle"),C=$("carte");if(C){document.querySelectorAll("path.t").forEach(function(p){',
  ' p.addEventListener("mousemove",function(ev){B.innerHTML=bulle(p.getAttribute("data-code"));B.style.display="block";var r=C.getBoundingClientRect();var x=ev.clientX-r.left+16,y=ev.clientY-r.top+16;if(x+330>r.width)x-=346;if(y+180>r.height)y-=190;B.style.left=Math.max(0,x)+"px";B.style.top=Math.max(0,y)+"px"});',
  ' p.addEventListener("mouseleave",function(){B.style.display="none"});',
  ' p.addEventListener("focus",function(){B.innerHTML=bulle(p.getAttribute("data-code"));B.style.display="block";B.style.left="12px";B.style.top="12px"});p.addEventListener("blur",function(){B.style.display="none"})})}',
  'document.querySelectorAll(".radios input").forEach(function(r){r.addEventListener("change",function(){etat.vue=r.value;document.querySelectorAll(".radios label").forEach(function(l){l.classList.toggle("on",l.querySelector("input").checked)});rendre()})});',
  'var rc=document.querySelector(".radios input:checked");if(rc){etat.vue=rc.value;document.querySelectorAll(".radios label").forEach(function(l){l.classList.toggle("on",l.querySelector("input").checked)})}',
  'var rech=$("rech");if(rech)rech.addEventListener("input",function(){remplirCats(rech.value);rendre()});',
  'var sel=$("cat");if(sel)sel.addEventListener("change",function(){etat.cat=sel.value;rendre()});',
  'document.querySelectorAll(".tabs button").forEach(function(b){b.addEventListener("click",function(){etat.cat=b.getAttribute("data-cat");document.querySelectorAll(".tabs button").forEach(function(x){x.classList.toggle("on",x===b);x.setAttribute("aria-pressed",x===b?"true":"false")});rendre()})});',
  'var ind=$("ind");if(ind)ind.addEventListener("change",function(){etat.ind=ind.value;rendre()});var sc=$("scen");if(sc)sc.addEventListener("change",function(){etat.scen=sc.value;rendre()});',
  'remplirCats("");rendre();',
  '})();', sep = "\n")

echap_html_carte <- function(s) { s <- gsub("&", "&amp;", s, fixed = TRUE); s <- gsub("<", "&lt;", s, fixed = TRUE); gsub(">", "&gt;", s, fixed = TRUE) }

# --- Briques HTML communes : en-tête avec champ, sélecteur de catégorie, pied ---
html_entete_champ <- function(champ, niveau, dimension, sous_defaut = "") {
  c('<header class="page-header">',
    '<p class="kicker">Départs attendus d’ici 2030 · résultats diffusables</p>',
    sprintf('<h1 class="page-title" id="titre">Départs attendus d’ici 2030<small>%s</small></h1>',
            c(france = "France entière", region = "par région", departement = "par département")[[niveau]]),
    sprintf('<p class="page-subtitle" id="sous">%s</p>', echap_html_carte(sous_defaut)),
    '<div class="study-scope" role="note" aria-label="Champ de l’étude">',
    sprintf('<span class="scope-text"><b>Champ</b> · %s</span>', echap_html_carte(champ$court)),
    '<details><summary>ⓘ Comprendre le champ</summary></details>',
    sprintf('<div class="study-scope-detail" id="scope-detail" hidden>%s</div>',
            paste(sprintf("<p><b>%s</b>%s</p>", echap_html_carte(sub(" :.*$", "", champ$detaille)),
                          echap_html_carte(sub("^[^:]+ :", " :", champ$detaille))), collapse = "")),
    '</div>',
    sprintf('<p class="context">%s</p>', if (niveau == "france") "Sélectionnez une catégorie pour consulter les résultats nationaux."
            else "Sélectionnez une catégorie pour comparer les territoires. Les zones grisées ne sont pas diffusées en application du secret statistique."),
    '</header>')
}
html_selecteur_categorie <- function(prep) {
  if (prep$dimension == "cs1" && prep$niveau == "france") {
    btn <- vapply(seq_len(nrow(prep$categories)), function(i) {
      code <- prep$categories$code[i]
      lib <- c("Cadres" = "Cadres", "Prof. intermediaires" = "Professions intermédiaires", "Employes" = "Employés", "Ouvriers" = "Ouvriers")[code]
      sprintf('<button type="button" data-cat="%s" class="%s" aria-pressed="%s">%s</button>', echap_html_carte(code),
              if (i == 1) "on" else "", if (i == 1) "true" else "false", echap_html_carte(ifelse(is.na(lib), code, lib)))
    }, "")
    c('<div id="b-cat"><span class="control-label">Catégorie</span><div class="tabs" role="group" aria-label="Catégorie socioprofessionnelle">', btn, '</div></div>')
  } else {
    c(sprintf('<div id="b-cat"><label class="control-label" for="cat">%s</label>%s<select id="cat" size="1"></select></div>',
              if (prep$dimension == "pcs") "PCS" else "Catégorie",
              if (prep$dimension == "pcs") '<input type="search" id="rech" placeholder="Filtrer par code PCS ou grande catégorie" aria-label="Filtrer les PCS">' else ""))
  }
}
html_pied <- function(champ, source_note) {
  sprintf(paste0('<footer><b>Champ de l’étude.</b> %s ',
                 '<b>Secret statistique.</b> Les cellules masquées ne figurent pas dans cette page ni dans ses données embarquées : seule leur existence est connue. ',
                 'Gris = non diffusé (secret statistique) ; blanc pointillé = pas de donnée observée ; une valeur nulle diffusée est affichée comme toute valeur. ',
                 '<b>Source.</b> %s. Fond de carte : IGN Admin Express (COG 2018) via france-geojson, licence ouverte.</footer>'),
          echap_html_carte(paste(champ$detaille, collapse = " ")), echap_html_carte(source_note))
}
js_details_champ <- '<script>(function(){var d=document.querySelector(".study-scope details"),p=document.getElementById("scope-detail");if(d&&p)d.addEventListener("toggle",function(){p.hidden=!d.open})})();</script>'

# --- Génération : carte territoriale OU tableau de bord national ----------------
generer_carte_departs <- function(prep, fond = NULL, fichier_html, fichier_png = NULL, source_note = "calculs propres",
                                  champ = construire_libelle_champ()) {
  titre_page <- sprintf("Départs attendus d’ici 2030 — %s — %s",
                        c(france = "France entière", region = "par région", departement = "par département")[[prep$niveau]],
                        if (prep$dimension == "pcs") "PCS fine" else "grande catégorie socioprofessionnelle")
  tete <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">',
            sprintf('<title>%s</title>', echap_html_carte(titre_page)),
            '<meta name="viewport" content="width=device-width, initial-scale=1">',
            '<style>', css_cartes(), '</style></head><body><div class="page">',
            html_entete_champ(champ, prep$niveau, prep$dimension))
  corps <- if (prep$niveau == "france") html_dashboard_national(prep) else html_carte_territoriale(prep, fond, titre_page)
  html <- c(tete, corps, html_pied(champ, source_note),
            '<script type="application/json" id="donnees">', donnees_json_carte(prep, champ), '</script>',
            js_details_champ, '<script>', js_cartes(), '</script>', '</div></body></html>')
  dir.create(dirname(fichier_html), showWarnings = FALSE, recursive = TRUE)
  writeLines(enc2utf8(html), fichier_html, useBytes = TRUE)
  if (!is.null(fichier_png) && prep$niveau != "france")
    tryCatch(png_carte_departs(prep, projeter_fond(fond), fichier_png, titre_page, champ),
             error = function(e) message("08e : PNG non produit (", conditionMessage(e), ") — le HTML reste la restitution de référence."))
  invisible(fichier_html)
}

html_carte_territoriale <- function(prep, fond, titre_page) {
  if (is.null(fond)) stop("html_carte_territoriale : fond requis.")
  lay <- projeter_fond(fond); terr <- prep$territoires
  paths <- vapply(names(fond), function(code)
    sprintf('<path class="t" data-code="%s" d="%s" tabindex="0" aria-label="%s"></path>',
            echap_html_carte(code), lay$chemins[[code]], echap_html_carte(terr$nom[match(code, terr$code)])), "")
  encarts <- vapply(lay$encarts, function(e) sprintf('<rect class="encart" x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="6"></rect><text class="encart-lib" x="%.1f" y="%.1f">%s</text>',
                                                      e$cadre[1] + 1, e$cadre[2] + 1, e$cadre[3] - e$cadre[1] - 2, e$cadre[4] - e$cadre[2] - 2, e$x_lib, e$y_lib, echap_html_carte(e$nom)), "")
  opts_ind <- paste(sprintf('<option value="%s"%s>%s</option>', names(LIBELLES_UI$indicateurs),
                            ifelse(names(LIBELLES_UI$indicateurs) == "taux", " selected", ""), LIBELLES_UI$indicateurs), collapse = "")
  opts_sc <- paste(sprintf('<option value="%s"%s>%s</option>', c("c", "b", "h"), c(" selected", "", ""), c("Central", "Bas", "Haut")), collapse = "")
  c('<div class="grille"><aside class="panneau">',
    '<span class="control-label">Vue</span>',
    '<div class="radios"><label class="on"><input type="radio" name="vue" value="resultats" checked>Résultats</label><label><input type="radio" name="vue" value="diffusabilite">Diffusabilité</label></div>',
    html_selecteur_categorie(prep),
    sprintf('<div id="b-ind"><label class="control-label" for="ind">Indicateur</label><select id="ind">%s</select></div>', opts_ind),
    sprintf('<div id="b-scen"><label class="control-label" for="scen">Scénario</label><select id="scen">%s</select></div>', opts_sc),
    '<div class="legend-title" id="legende-titre"></div><ul class="map-legend" id="legende"></ul>',
    '<p class="compte" id="compte"></p><p class="note" id="legende-note"></p>',
    if (length(lay$encarts) > 0) '<p class="note">Départements et régions d’outre-mer en encarts, chacun à sa propre échelle.</p>' else "",
    '</aside><div class="carte" id="carte">',
    sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">', lay$largeur, lay$hauteur, echap_html_carte(titre_page)),
    encarts, paths, '</svg><div class="bulle" id="bulle" role="status"></div></div></div>')
}

html_dashboard_national <- function(prep) {
  c('<div class="dash"><aside class="panneau">',
    html_selecteur_categorie(prep),
    '<p class="cat-affichee" id="cat-affichee"></p>',
    '</aside><div>',
    '<div id="zone-kpi">',
    '<div class="kpis">',
    sprintf('<div class="kpi primaire"><div class="kpi-value" id="kpi-c"></div><div class="kpi-label">%s</div><div class="kpi-help">Scénario central, somme des probabilités individuelles de départ.</div></div>', LIBELLES_UI$indicateurs[["departs"]]),
    sprintf('<div class="kpi"><div class="kpi-value" id="kpi-t"></div><div class="kpi-label">%s</div><div class="kpi-help">%s</div></div>', LIBELLES_UI$indicateurs[["taux"]], LIBELLES_UI$denominateur),
    '<div class="kpi"><div class="kpi-value" id="kpi-e"></div><div class="kpi-label">Salariés dans le champ</div><div class="kpi-help">Salariés de la catégorie affichée appartenant au champ étudié (voir « Champ » ci-dessus), pas l’ensemble des salariés de la BITD.</div></div>',
    '</div>',
    sprintf('<div class="bloc"><h2>%s — fourchette d’estimation</h2><div class="fourchette">', LIBELLES_UI$indicateurs[["departs"]]),
    sprintf('<div><div class="v" id="f-b"></div><div class="l">%s</div></div><div class="centrale"><div class="v" id="f-c"></div><div class="l">%s</div></div><div><div class="v" id="f-h"></div><div class="l">%s</div></div>',
            LIBELLES_UI$scenarios[["b"]], LIBELLES_UI$scenarios[["c"]], LIBELLES_UI$scenarios[["h"]]),
    '</div><p class="note">Hypothèses réglementaires basse et haute autour du scénario central.</p></div>',
    '<div class="bloc" id="bloc-causes"><h2>Composition des départs estimés</h2><div class="barre" id="barre" aria-hidden="true"></div><ul class="causes" id="causes"></ul>',
    '<p class="note">Répartition des départs du scénario central par cause (risques concurrents).</p></div>',
    '</div>',
    '<div class="masque-national" id="zone-masque" style="display:none"><div class="t" id="masque-cat"></div><p><b>Non diffusé.</b> <span id="masque-txt"></span></p><p>Le champ de l’étude reste celui indiqué en tête de page.</p></div>',
    '</div></div>')
}

# --- PNG de la vue initiale d'une carte territoriale (vue Diffusabilité) --------
png_carte_departs <- function(prep, lay, fichier_png, titre = "", champ = construire_libelle_champ()) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 absent")
  long <- lay$long; cv <- prep$couverture; LB <- LIBELLES_UI$couverture
  k <- function(p) if (is.na(p)) LIBELLES_UI$sans_donnee else if (p <= 0) LB$aucune else LB$libelles[max(which(p >= LB$bornes))]
  long$classe <- factor(vapply(cv$part_diff_pct[match(long$code, cv$code)], k, ""),
                        levels = c(rev(LB$libelles), LB$aucune, LIBELLES_UI$sans_donnee))
  cols <- c(rev(COULEURS_CARTE$classes[round(seq(0, 3) * 4 / 3) + 1]), COULEURS_CARTE$secret, COULEURS_CARTE$sans_donnee)
  enc <- if (length(lay$encarts)) bind_rows(lapply(lay$encarts, function(e) tibble(x1 = e$cadre[1], x2 = e$cadre[3], y1 = -e$cadre[2], y2 = -e$cadre[4], lib = e$nom, xl = e$x_lib, yl = -e$y_lib))) else NULL
  g <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = long, ggplot2::aes(x = x, y = y, group = groupe, fill = classe), colour = "#c3c9d1", linewidth = .22) +
    ggplot2::scale_fill_manual(values = setNames(cols, levels(long$classe)), drop = FALSE, name = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = "#9a9a9a"))) +
    ggplot2::coord_equal(expand = FALSE) + ggplot2::theme_void(base_size = 12) +
    ggplot2::labs(title = paste("Où les résultats peuvent-ils être diffusés ?", sub("^.*— ", "", titre)),
                  subtitle = paste0("Part des ", if (prep$dimension == "pcs") "PCS" else "catégories", " observées dont les résultats sont diffusables\n", champ$titre),
                  caption = "Gris : secret statistique · blanc : pas de donnée observée · aucune valeur masquée n’est représentée. Fond IGN Admin Express via france-geojson.") +
    ggplot2::theme(legend.position = "bottom", legend.direction = "vertical", legend.text = ggplot2::element_text(size = 11),
                   plot.title = ggplot2::element_text(face = "bold", size = 15, colour = COULEURS_CARTE$encre),
                   plot.subtitle = ggplot2::element_text(colour = COULEURS_CARTE$texte, size = 11),
                   plot.caption = ggplot2::element_text(colour = COULEURS_CARTE$texte, size = 9), plot.margin = ggplot2::margin(10, 10, 10, 10))
  if (!is.null(enc)) g <- g + ggplot2::geom_rect(data = enc, ggplot2::aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1), fill = NA, colour = "#c9ced6") +
    ggplot2::geom_text(data = enc, ggplot2::aes(x = xl, y = yl, label = lib), hjust = 0, size = 3.6, colour = COULEURS_CARTE$texte)
  ggplot2::ggsave(fichier_png, g, width = 8, height = 10.8, dpi = 130, bg = "white")
  invisible(fichier_png)
}
