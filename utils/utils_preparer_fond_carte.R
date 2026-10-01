# ==============================================================================
# utils/utils_preparer_fond_carte.R — Fabrication UNIQUE du fond cartographique
#                                      local (data/cartographie/), hors chaîne
# ------------------------------------------------------------------------------
# À lancer UNE fois, à la main, sur un poste connecté : la chaîne (08e) ne
# télécharge rien et ne dépend pas de sf. Produit deux GeoJSON WGS84 légers :
#   data/cartographie/departements.geojson  (101 : 96 métropole + 2A/2B inclus + 5 DROM)
#   data/cartographie/regions.geojson       (18 : 13 métropole + 5 DROM)
# Source : france-geojson (G. David), tracés IGN Admin Express COG édition 2018,
#          codes/noms Insee COG 2018, licence ouverte Etalab ; fichiers
#          « version simplifiée » pour la métropole (Visvalingam 25 %, 5 décimales),
#          fichiers régionaux pour les DROM.
# Traitement ici : fusion métropole + DROM, simplification supplémentaire des
# DROM (st_simplify, tolérance ~150 m, Mayotte très détaillée à l'origine),
# arrondi des coordonnées à 4 décimales (~11 m), propriétés réduites à code/nom.
# Dépendances (préparation seulement) : sf, jsonlite, curl ou réseau R.
# ==============================================================================
suppressPackageStartupMessages({ library(sf); library(jsonlite) })
BASE <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master"
DROM <- c(guadeloupe = "971", martinique = "972", guyane = "973", `la-reunion` = "974", mayotte = "976")
DEST <- "data/cartographie"; dir.create(DEST, showWarnings = FALSE, recursive = TRUE)
tmp <- tempfile(); dir.create(tmp)
telecharger <- function(rel) { f <- file.path(tmp, gsub("/", "_", rel)); download.file(file.path(BASE, rel), f, quiet = TRUE, mode = "wb"); f }

lire <- function(f) { x <- st_read(f, quiet = TRUE); st_crs(x) <- 4326; x[, c("code", "nom")] }
simplifier_drom <- function(x) {
  # simplification en projection métrique locale (UTM auto) puis retour WGS84
  utm <- 32600 + floor((mean(st_bbox(x)[c("xmin", "xmax")]) + 180) / 6) + 1
  if (mean(st_bbox(x)[c("ymin", "ymax")]) < 0) utm <- utm + 100
  s <- st_simplify(st_transform(x, utm), dTolerance = 150, preserveTopology = TRUE)
  # la simplification peut produire des GEOMETRYCOLLECTION (polygones + débris
  # linéaires) : on ne garde que les polygones, en MULTIPOLYGON homogène
  geom <- st_geometry(s)
  geom <- st_sfc(lapply(geom, function(g) {
    if (inherits(g, "GEOMETRYCOLLECTION")) g <- st_union(st_collection_extract(st_sfc(g, crs = st_crs(s)), "POLYGON"))[[1]]
    st_cast(g, "MULTIPOLYGON")
  }), crs = st_crs(s))
  st_geometry(s) <- geom
  st_transform(s, 4326)
}
arrondir <- function(x, dec = 4) { st_geometry(x) <- st_sfc(lapply(st_geometry(x), function(g) { m <- round(unclass(g[[1]])[[1]], dec); g }), crs = 4326); x }

construire <- function(niveau) {
  metro <- lire(telecharger(sprintf("%s-version-simplifiee.geojson", niveau)))
  drom <- do.call(rbind, lapply(names(DROM), function(r)
    simplifier_drom(lire(telecharger(sprintf("regions/%s/%s-%s.geojson", r, if (niveau == "regions") "region" else "departements", r))))))
  x <- rbind(metro, drom)
  # la source métropole contient aussi une GEOMETRYCOLLECTION (54, polygone +
  # débris linéaire) : même homogénéisation en polygones pour toutes les entités
  st_geometry(x) <- st_sfc(lapply(st_geometry(x), function(g) {
    if (inherits(g, "GEOMETRYCOLLECTION")) g <- st_union(st_collection_extract(st_sfc(g, crs = 4326), "POLYGON"))[[1]]
    st_cast(g, "MULTIPOLYGON")
  }), crs = 4326)
  x$code <- as.character(x$code); x$nom <- as.character(x$nom)
  x <- x[order(x$code), ]
  stopifnot(anyDuplicated(x$code) == 0, all(nchar(x$code) > 0))
  x
}
# Écriture GeoJSON maîtrisée (jsonlite) : MultiPolygon pour toutes les entités,
# 4 décimales, propriétés code/nom — indépendante des options du pilote GDAL.
ecrire_geojson <- function(x, f) {
  feats <- lapply(seq_len(nrow(x)), function(i) {
    g <- st_geometry(x)[[i]]                       # MULTIPOLYGON : liste de polygones (liste d'anneaux, matrices)
    coords <- lapply(unclass(g), function(poly) lapply(poly, function(ring) round(unname(ring), 4)))
    list(type = "Feature", properties = list(code = x$code[i], nom = x$nom[i]),
         geometry = list(type = "MultiPolygon", coordinates = coords))
  })
  writeLines(enc2utf8(jsonlite::toJSON(list(type = "FeatureCollection", features = feats),
                                       auto_unbox = TRUE, digits = NA)), f, useBytes = TRUE)
}
for (niveau in c("departements", "regions")) {
  x <- construire(niveau)
  f <- file.path(DEST, paste0(niveau, ".geojson"))
  ecrire_geojson(x, f)
  cat(sprintf("%s : %d entités, %.0f Ko -> %s\n", niveau, nrow(x), file.size(f) / 1024, f))
  cat("  codes :", paste(x$code, collapse = " "), "\n")
}
