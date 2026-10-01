# ==============================================================================
# 00f_fonctions_cartes.R — Cartographie des résultats DIFFUSABLES (fonctions pures)
# ------------------------------------------------------------------------------
# Rôle : transformer une table de DIFFUSION (departs_pcs / departs_cs1, niveau
#        france / région / département) en une carte HTML autonome (SVG inline,
#        JavaScript natif, aucune ressource externe, aucune connexion) et en
#        une image PNG de la vue initiale (ggplot2, sans sf).
# SOURCE ABSOLUE : la table de diffusion. Une cellule masquée (masque = TRUE)
#        n'embarque que son statut ; toute valeur en est retirée AVANT
#        sérialisation, quelle que soit la table reçue (une table sans colonne
#        masque est refusée : jamais une table interne par erreur).
# Trois statuts, trois rendus :
#   diffuse      -> couleur de classe (échelle commune à tous les territoires
#                   pour la catégorie et l'indicateur choisis ; recalculée quand
#                   on change de catégorie — documenté dans la légende) ;
#   masque       -> gris COULEURS_CARTE$secret  « Non diffusé — secret statistique » ;
#   sans_donnee  -> blanc, contour pointillé  « Pas de donnée observée »
#                   (jamais assimilé à zéro ni au secret).
# Fond : data/cartographie/{departements,regions}.geojson (local, jsonlite),
#        métropole en Lambert-93 (formules RGF93), Corse incluse, DROM en
#        encarts (projection locale, échelle propre à chaque encart).
# Une seule logique, six configurations : dimension x niveau.
# Dépendances : jsonlite, ggplot2 (PNG), dplyr. Ni sf, ni htmlwidgets, ni CDN.
# ==============================================================================
library(dplyr)

# --- Constantes (couleurs centralisées, modifiables ici) -----------------------
COULEURS_CARTE <- list(
  classes      = c("#deebf7", "#9ecae1", "#4292c6", "#2171b5", "#08306b"),  # séquentielle bleue (ColorBrewer Blues), lisible daltonisme
  secret       = "#b3b3b3",   # gris RÉSERVÉ au secret statistique
  sans_donnee  = "#ffffff",   # blanc + contour pointillé : pas de donnée observée
  contour      = "#ffffff", contour_sans = "#9a9a9a", fond_support = "#dfe7ef",  # fond uniforme des cartes « France entière »
  encre = "#1a1f2b", texte = "#4a5260", gris = "#8a919c", filet = "#e4e7ec", bleu = "#1e3a5f")
CLASSES_COUVERTURE <- list(bornes = c(1, 25, 50, 75, 100),           # part des catégories diffusables (%)
                           libelles = c("1 à 24 %", "25 à 49 %", "50 à 74 %", "75 à 99 %", "100 %"))
INDICATEURS_CARTE <- c(taux_depart_central_pct = "Taux de départ (%)", departs = "Départs estimés d’ici 2030",
                       effectif_champ = "Effectif du champ")
DROM_CARTE <- c("971" = "Guadeloupe", "972" = "Martinique", "973" = "Guyane", "974" = "La Réunion", "976" = "Mayotte")
DROM_REGION_CARTE <- c("01" = "971", "02" = "972", "03" = "973", "04" = "974", "06" = "976")   # région DROM -> département

# --- Lecture d'une table de diffusion (CSV write.csv2, avec ou sans BOM) -------
lire_csv_diffusion <- function(fichier) {
  if (!file.exists(fichier)) stop("Table de diffusion introuvable : ", fichier)
  d <- read.csv2(fichier, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE,
                 colClasses = "character", na.strings = "NA")
  names(d)[1] <- sub("^\ufeff", "", names(d)[1])            # BOM jamais dans un nom de colonne
  num <- intersect(c("effectif_champ", "departs_central", "departs_bas", "departs_haut", "dep_retraite",
                     "dep_invalidite", "dep_deces", "taux_depart_central_pct"), names(d))
  for (v in num) d[[v]] <- as.numeric(sub(",", ".", d[[v]], fixed = TRUE))
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
# Retourne, par entité : chemin SVG `d` (y vers le bas) et table longue (x, y,
# groupe) pour ggplot2 ; plus la liste des encarts (cadres + libellés).
# Largeur 1000 ; métropole dans [0, 1000] x [0, 1000] ; rangée d'encarts dessous.
projeter_fond <- function(fond, largeur = 1000, hauteur_encart = 200) {
  niveau <- attr(fond, "niveau")
  est_drom <- function(code) code %in% names(DROM_CARTE) || (niveau == "region" && code %in% names(DROM_REGION_CARTE))
  dep_drom <- function(code) if (code %in% names(DROM_CARTE)) code else DROM_REGION_CARTE[[code]]
  metro <- fond[!vapply(names(fond), est_drom, logical(1))]
  drom  <- fond[vapply(names(fond), est_drom, logical(1))]
  # métropole : Lambert-93, ajustée dans le carré [marge, largeur - marge]
  pm <- lapply(metro, function(e) lapply(e$anneaux, function(r) projeter_lambert93(r[, 1], r[, 2])))
  tous <- do.call(rbind, unlist(pm, recursive = FALSE))
  bb <- c(min(tous[, 1]), max(tous[, 1]), min(tous[, 2]), max(tous[, 2]))
  marge <- 20; ech <- (largeur - 2 * marge) / max(bb[2] - bb[1], bb[4] - bb[3])
  decal_y <- (largeur - 2 * marge - (bb[4] - bb[3]) * ech) / 2
  svg_metro <- function(m) cbind(marge + (m[, 1] - bb[1]) * ech, marge + decal_y + (bb[4] - m[, 2]) * ech)
  coords <- lapply(pm, function(rings) lapply(rings, svg_metro))
  # encarts DROM : ordre fixe 971, 972, 973, 974, 976 ; projection locale
  # équirectangulaire, échelle propre à chaque encart (indiquée en légende)
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
      ech_e <- (min(l_enc, hauteur_encart) - 40) / etendue
      cx <- x0 + l_enc / 2; cy <- y_enc + hauteur_encart / 2 + 8
      coords[[code]] <- lapply(loc, function(m) cbind(cx + (m[, 1] - mean(range(tl[, 1]))) * ech_e,
                                                       cy - (m[, 2] - mean(range(tl[, 2]))) * ech_e))
      encarts[[code]] <- list(code = code, nom = DROM_CARTE[[ordre_drom[i]]], cadre = cadre, x_lib = x0 + 8, y_lib = y_enc + 16)
    }
  }
  chemins <- vapply(names(fond), function(code) {
    paste(vapply(coords[[code]], function(m) paste0("M", paste(sprintf("%.1f %.1f", m[, 1], m[, 2]), collapse = "L"), "Z"), ""), collapse = "")
  }, "")
  long <- bind_rows(lapply(names(fond), function(code) bind_rows(lapply(seq_along(coords[[code]]), function(k) {
    m <- coords[[code]]; tibble(code = code, groupe = paste(code, k), x = m[[k]][, 1], y = -m[[k]][, 2]) }))))
  list(chemins = chemins, long = long, encarts = encarts, largeur = largeur,
       hauteur = largeur + if (n_enc > 0) hauteur_encart else 0,
       noms = vapply(fond, `[[`, "", "nom"))
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
  mesures <- intersect(c("effectif_champ", "departs_central", "departs_bas", "departs_haut", "taux_depart_central_pct"), names(donnees))
  d <- donnees
  d$masque <- as.logical(d$masque); d$masque[is.na(d$masque)] <- TRUE   # doute = masqué
  d$.code <- if (is.null(cle)) "FR" else as.character(d[[cle]])
  d$.nom  <- if (is.null(cle_nom) || !cle_nom %in% names(d)) NA_character_ else as.character(d[[cle_nom]])
  d$.cat  <- as.character(d[[dimension]])
  # SÉCURITÉ : toute mesure d'une ligne masquée est retirée ici, avant tout usage
  for (v in mesures) d[[v]][d$masque] <- NA_real_
  if (anyDuplicated(d[, c(".code", ".cat")]) > 0) stop("preparer_carte_departs : doublons territoire x catégorie.")
  # territoires non localisables (« inconnu ») : hors carte, comptés
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
                             effectif_champ = if ("effectif_champ" %in% mesures) effectif_champ else NA_real_,
                             departs_central = if ("departs_central" %in% mesures) departs_central else NA_real_,
                             departs_bas = if ("departs_bas" %in% mesures) departs_bas else NA_real_,
                             departs_haut = if ("departs_haut" %in% mesures) departs_haut else NA_real_,
                             taux_depart_central_pct = if ("taux_depart_central_pct" %in% mesures) taux_depart_central_pct else NA_real_)
  couverture <- cellules |> group_by(code) |>
    summarise(n_obs = n(), n_diff = sum(statut == "diffuse"), n_masq = sum(statut == "masque"), .groups = "drop") |>
    mutate(part_diff_pct = 100 * n_diff / n_obs,
           statut = ifelse(n_diff > 0, "diffusable", "masque"))
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
  if (nrow(prep$cellules) > 0 && any(!is.na(prep$cellules$departs_central[prep$cellules$statut == "masque"])))
    stop(prefixe, " : une valeur masquée subsiste dans la structure de carte.")
  cat(sprintf("  %s x %-12s : territoires CSV %3d | joints %3d | masqués %3d | diffusables %3d | sans donnée %3d | catégories %d%s\n",
              prep$dimension, prep$niveau, n[["csv"]], n[["joints"]], n[["masques"]], n[["diffusables"]], n[["sans"]],
              nrow(prep$categories), if (prep$n_non_localises > 0) sprintf(" | non localisables %d", prep$n_non_localises) else ""))
  invisible(n)
}

# --- Sérialisation JSON : cellules masquées = statut seul ----------------------
donnees_json_carte <- function(prep) {
  cel <- list()
  for (i in seq_len(nrow(prep$cellules))) {
    r <- prep$cellules[i, ]
    cel[[r$code]][[r$cat]] <- if (r$statut == "masque") list(s = "m") else
      list(s = "d", e = r$effectif_champ, c = r$departs_central, b = r$departs_bas, h = r$departs_haut, t = r$taux_depart_central_pct)
  }
  cel <- lapply(cel, function(x) lapply(x, function(v) v[!vapply(v, function(z) length(z) == 1 && is.na(z), logical(1))]))
  cov <- setNames(lapply(seq_len(nrow(prep$couverture)), function(i) {
    r <- prep$couverture[i, ]; list(o = r$n_obs, d = r$n_diff, m = r$n_masq, p = round(r$part_diff_pct, 1)) }), prep$couverture$code)
  cats <- lapply(seq_len(nrow(prep$categories)), function(i) {
    r <- prep$categories[i, ]; if (is.na(r$cs1)) list(code = r$code) else list(code = r$code, cs1 = r$cs1) })
  terr <- lapply(seq_len(nrow(prep$territoires)), function(i) list(code = prep$territoires$code[i], nom = prep$territoires$nom[i]))
  j <- jsonlite::toJSON(list(dimension = prep$dimension, niveau = prep$niveau, mesures = prep$mesures,
                             categories = cats, territoires = terr, cellules = cel, couverture = cov,
                             couleurs = COULEURS_CARTE[c("classes", "secret", "sans_donnee", "fond_support")],
                             classes_couverture = CLASSES_COUVERTURE, indicateurs = as.list(INDICATEURS_CARTE)),
                       auto_unbox = TRUE, digits = NA, na = "null", null = "null")
  gsub("</", "<\\/", j, fixed = TRUE)
}

# --- CSS et JavaScript (natifs, inline) ----------------------------------------
css_carte <- function() paste(
  sprintf(':root{--encre:%s;--texte:%s;--gris:%s;--filet:%s;--bleu:%s;--secret:%s}', COULEURS_CARTE$encre, COULEURS_CARTE$texte,
          COULEURS_CARTE$gris, COULEURS_CARTE$filet, COULEURS_CARTE$bleu, COULEURS_CARTE$secret),
  '*{box-sizing:border-box}body{margin:0;background:#fff;color:var(--encre);font-family:-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif;font-size:15px;line-height:1.45}',
  '.page{max-width:1180px;margin:0 auto;padding:32px 28px 40px}',
  '.kicker{font-size:12px;letter-spacing:.14em;text-transform:uppercase;color:var(--gris);margin:0 0 8px}',
  'h1{font-size:26px;font-weight:650;margin:0 0 6px;letter-spacing:-.01em}.sous{color:var(--texte);margin:0 0 18px;font-size:15px}',
  '.bandeau{display:inline-block;background:#eef3f8;color:var(--bleu);border-radius:6px;padding:6px 12px;font-weight:600;margin:0 0 14px}',
  '.grille{display:grid;grid-template-columns:300px 1fr;gap:28px;align-items:start}@media(max-width:860px){.grille{grid-template-columns:1fr}}',
  '.panneau{border:1px solid var(--filet);border-radius:10px;padding:16px 18px;position:sticky;top:12px}',
  '.panneau h2{font-size:12px;letter-spacing:.12em;text-transform:uppercase;color:var(--gris);margin:14px 0 8px}.panneau h2:first-child{margin-top:0}',
  'label{display:block;font-size:13px;color:var(--texte);margin:8px 0 3px}select,input[type=search]{width:100%;font:inherit;font-size:14px;padding:7px 9px;border:1px solid #c9d0d9;border-radius:6px;background:#fff;color:var(--encre)}',
  '.radios{display:flex;gap:6px;margin:4px 0 2px}.radios label{flex:1;margin:0;border:1px solid #c9d0d9;border-radius:6px;padding:7px 8px;text-align:center;cursor:pointer;font-size:13px;color:var(--encre)}.radios input{display:none}.radios label.on{background:var(--bleu);color:#fff;border-color:var(--bleu)}',
  '.legende{list-style:none;margin:6px 0 0;padding:0}.legende li{display:flex;align-items:center;gap:10px;font-size:13px;margin:5px 0;color:var(--texte)}.legende .sw{width:22px;height:14px;border-radius:3px;border:1px solid #d5d9df;flex:none}.legende .sw.sans{border:1px dashed #9a9a9a}',
  '.note{font-size:12.5px;color:var(--gris);margin:10px 0 0;line-height:1.4}.compte{font-size:13px;color:var(--texte);margin:6px 0 0}.compte b{color:var(--encre)}',
  '.carte{position:relative}.carte svg{width:100%;height:auto;display:block}',
  'path.t{stroke:#fff;stroke-width:.8;cursor:pointer;transition:opacity .1s}path.t:hover,path.t:focus{opacity:.78;outline:none;stroke:#1a1f2b;stroke-width:1.4}path.t.sans{stroke:#9a9a9a;stroke-dasharray:3 2}',
  '.encart{fill:none;stroke:#d5d9df;stroke-width:1}.encart-lib{font-size:13px;fill:#6b7280}',
  '.bulle{position:absolute;pointer-events:none;background:#1a1f2b;color:#fff;border-radius:8px;padding:10px 12px;font-size:13px;line-height:1.4;max-width:280px;box-shadow:0 6px 20px rgba(0,0,0,.18);display:none;z-index:2}.bulle b{font-size:14px}.bulle .s{color:#cfd6df}.bulle .secret{color:#ffd7a8}',
  '.national{border:1px solid var(--filet);border-radius:10px;padding:14px 18px;margin:0 0 14px;background:#fafbfc}.national .val{font-size:30px;font-weight:650;letter-spacing:-.01em}.national .lib{font-size:13px;color:var(--gris)}.national .ligne{display:flex;gap:28px;flex-wrap:wrap}',
  'footer{margin-top:22px;color:var(--gris);font-size:12.5px;line-height:1.5;border-top:1px solid var(--filet);padding-top:12px}',
  sep = "\n")

js_carte <- function() paste(
  '(function(){',
  'var D=JSON.parse(document.getElementById("donnees").textContent);',
  'var fmt=function(x,d){return (x==null||isNaN(x))?"n.d.":new Intl.NumberFormat("fr-FR",{minimumFractionDigits:d,maximumFractionDigits:d}).format(x)};',
  'var $=function(id){return document.getElementById(id)};var FR=D.niveau==="france";',
  'var noms={};D.territoires.forEach(function(t){noms[t.code]=t.nom});',
  'var etat={vue:"couverture",cat:D.categories.length?D.categories[0].code:null,ind:"taux_depart_central_pct",scen:"c"};',
  'var dimLib=D.dimension==="pcs"?"PCS":"Catégorie",nivLib={france:"France entière",region:"par région",departement:"par département"}[D.niveau];',
  'var catLabel=function(c){var o=D.categories.filter(function(k){return k.code===c})[0];return o?(D.dimension==="pcs"?("PCS "+o.code+(o.cs1?" · "+o.cs1:"")):o.code):c};',
  'function remplirCats(filtre){var s=$("cat"),f=(filtre||"").toLowerCase();s.innerHTML="";D.categories.forEach(function(k){var lib=D.dimension==="pcs"?(k.code+(k.cs1?" — "+k.cs1:"")):k.code;if(f&&lib.toLowerCase().indexOf(f)<0)return;var o=document.createElement("option");o.value=k.code;o.textContent=lib;s.appendChild(o)});if(s.options.length){var ok=false;for(var i=0;i<s.options.length;i++)if(s.options[i].value===etat.cat){ok=true;break}if(!ok)etat.cat=s.options[0].value;s.value=etat.cat}}',
  'function cle(ind,scen){return ind==="departs"?({c:"c",b:"b",h:"h"}[scen]):(ind==="effectif_champ"?"e":"t")}',
  'function valeur(code){var c=D.cellules[code]&&D.cellules[code][etat.cat];if(!c)return{st:"sans"};if(c.s==="m")return{st:"masque"};var v=c[cle(etat.ind,etat.scen)];return{st:"diffuse",v:v,c:c}}',
  'function quantiles(vals,n){var s=vals.slice().sort(function(a,b){return a-b});var q=[];for(var i=1;i<n;i++){var p=(s.length-1)*i/n,lo=Math.floor(p),hi=Math.ceil(p);q.push(s[lo]+(s[hi]-s[lo])*(p-lo))}return q}',
  'function classesValeur(){var vals=[];Object.keys(D.couverture).forEach(function(code){var r=valeur(code);if(r.st==="diffuse"&&r.v!=null)vals.push(r.v)});var dist=vals.filter(function(v,i,a){return a.indexOf(v)===i});var n=Math.min(D.couleurs.classes.length,dist.length);if(n<=1)return{bornes:[],n:n,min:dist[0],max:dist[0]};var b=quantiles(vals,n);var bb=b.filter(function(v,i,a){return a.indexOf(v)===i});return{bornes:bb,n:bb.length+1,min:Math.min.apply(null,vals),max:Math.max.apply(null,vals)}}',
  'function classeDe(v,cl){if(cl.n<=1)return 0;var k=0;while(k<cl.bornes.length&&v>cl.bornes[k])k++;return k}',
  'function couleurClasse(k,n){var pal=D.couleurs.classes;if(n<=1)return pal[Math.floor(pal.length/2)];var idx=Math.round(k*(pal.length-1)/(n-1));return pal[idx]}',
  'function classeCouv(p){if(p<=0)return -1;var b=D.classes_couverture.bornes;for(var i=b.length-1;i>=0;i--)if(p>=b[i])return i;return 0}',
  'var dec={t:1,c:1,b:1,h:1,e:0};',
  'function rendre(){var paths=document.querySelectorAll("path.t"),leg=$("legende"),cl=null;leg.innerHTML="";var nD=0,nM=0,nS=0;',
  ' if(etat.vue==="categorie"&&!FR)cl=classesValeur();',
  ' paths.forEach(function(p){var code=p.getAttribute("data-code"),fill,st;p.classList.remove("sans");',
  '  if(FR){var r=valeur("FR");st=r.st;fill=st==="masque"?D.couleurs.secret:(st==="sans"?D.couleurs.sans_donnee:D.couleurs.fond_support)}',
  '  else if(etat.vue==="couverture"){var cv=D.couverture[code];if(!cv){st="sans";fill=D.couleurs.sans_donnee}else{var k=classeCouv(cv.p);if(k<0){st="masque";fill=D.couleurs.secret}else{st="diffuse";fill=couleurClasse(k,D.classes_couverture.bornes.length)}}}',
  '  else{var r2=valeur(code);st=r2.st;fill=st==="masque"?D.couleurs.secret:(st==="sans"?D.couleurs.sans_donnee:couleurClasse(classeDe(r2.v,cl),cl.n))}',
  '  if(st==="sans")p.classList.add("sans");p.setAttribute("fill",fill);p.setAttribute("data-st",st);if(st==="diffuse")nD++;else if(st==="masque")nM++;else nS++;});',
  ' var li=function(col,txt,sans){var e=document.createElement("li");e.innerHTML="<span class=\\"sw"+(sans?" sans":"")+"\\" style=\\"background:"+col+"\\"></span><span></span>";e.lastChild.textContent=txt;leg.appendChild(e)};',
  ' if(FR){li(D.couleurs.fond_support,"Résultat national (pas de variation territoriale)")}',
  ' else if(etat.vue==="couverture"){var L=D.classes_couverture.libelles;for(var i=L.length-1;i>=0;i--)li(couleurClasse(i,L.length),L[i]);}',
  ' else if(cl){if(cl.n<=1){li(couleurClasse(0,1),cl.min==null?"aucune valeur diffusée":"valeur unique : "+fmt(cl.min,dec[cle(etat.ind,etat.scen)]))}else{var d=dec[cle(etat.ind,etat.scen)],prev=cl.min;for(var k=0;k<cl.n;k++){var hi=k<cl.bornes.length?cl.bornes[k]:cl.max;li(couleurClasse(k,cl.n),(k===0?"de ":"plus de ")+fmt(prev,d)+" à "+fmt(hi,d));prev=hi}}}',
  ' li(D.couleurs.secret,etat.vue==="couverture"&&!FR?"Aucune catégorie diffusable — secret statistique":"Non diffusé — secret statistique");li(D.couleurs.sans_donnee,"Pas de donnée observée",true);',
  ' $("compte").innerHTML=FR?"":"Territoires : <b>"+nD+"</b> diffusables · <b>"+nM+"</b> masqués (secret) · <b>"+nS+"</b> sans donnée";',
  ' var indLib=D.indicateurs[etat.ind]+(etat.ind==="departs"?" ("+{c:"central",b:"bas",h:"haut"}[etat.scen]+")":"");',
  ' $("titre").textContent=etat.vue==="couverture"&&!FR?"Diffusabilité des résultats par "+dimLib:(etat.ind==="taux_depart_central_pct"?"Taux de départ d’ici 2030":(etat.ind==="effectif_champ"?"Effectif du champ":"Départs estimés d’ici 2030"));',
  ' $("sous").textContent=etat.vue==="couverture"&&!FR?"Part des "+dimLib+" observées pouvant être diffusées dans chaque "+(D.niveau==="region"?"région":"département")+". Les territoires grisés n’ont aucune catégorie diffusable, en application du secret statistique.":(catLabel(etat.cat)+" — "+nivLib+". Les territoires grisés ne sont pas diffusés en application du secret statistique.");',
  ' $("echelle").textContent=FR?"":(etat.vue==="couverture"?"Classes fixes de la part diffusable.":"Classes par quantiles des valeurs diffusées ("+indLib+"), échelle commune à tous les territoires affichés, recalculée à chaque changement de catégorie ou d’indicateur.");',
  ' $("b-cat").style.display=(etat.vue==="categorie"||FR)?"":"none";$("b-ind").style.display=(etat.vue==="categorie"||FR)?"":"none";$("b-scen").style.display=((etat.vue==="categorie"||FR)&&etat.ind==="departs")?"":"none";',
  ' if(FR)rendreNational();}',
  'function rendreNational(){var n=$("national");if(!n)return;var cv=D.couverture["FR"],c=D.cellules["FR"]&&D.cellules["FR"][etat.cat];var h="<div class=\\"lib\\">France entière — résultat national · "+catLabel(etat.cat)+"</div>";',
  ' if(!c)h+="<div class=\\"val\\">Pas de donnée observée</div>";else if(c.s==="m")h+="<div class=\\"val\\" style=\\"color:#6b7280\\">Non diffusé — secret statistique</div>";',
  ' else h+="<div class=\\"ligne\\"><div><div class=\\"val\\">"+fmt(c.e,0)+"</div><div class=\\"lib\\">Effectif du champ</div></div><div><div class=\\"val\\">"+fmt(c.c,1)+"</div><div class=\\"lib\\">Départs estimés (bas "+fmt(c.b,1)+" · haut "+fmt(c.h,1)+")</div></div><div><div class=\\"val\\">"+fmt(c.t,1)+" %</div><div class=\\"lib\\">Taux de départ</div></div></div>";',
  ' if(cv)h+="<div class=\\"lib\\" style=\\"margin-top:8px\\">"+(D.dimension==="pcs"?"PCS":"Catégories")+" observées : "+cv.o+" · diffusables : "+cv.d+" · masquées : "+cv.m+"</div>";n.innerHTML=h;}',
  'function bulle(code){var h="<b></b>";var el=document.createElement("div");var b=document.createElement("b");b.textContent=noms[code]+(FR?"":" ("+code+")");el.appendChild(b);',
  ' if(!FR&&etat.vue==="couverture"){var cv=D.couverture[code];if(!cv)el.appendChild(ligne("Pas de donnée observée","s"));else{el.appendChild(ligne((D.dimension==="pcs"?"PCS":"Catégories")+" observées : "+cv.o));el.appendChild(ligne("Diffusables : "+cv.d));el.appendChild(ligne("Masquées : "+cv.m));el.appendChild(ligne(cv.d>0?"Part diffusable : "+fmt(cv.p,0)+" %":"Aucune catégorie diffusable — secret statistique",cv.d>0?"":"secret"))}}',
  ' else{var r=valeur(FR?"FR":code);el.appendChild(ligne(catLabel(etat.cat),"s"));if(r.st==="sans")el.appendChild(ligne("Pas de donnée observée"));else if(r.st==="masque")el.appendChild(ligne("Non diffusé — secret statistique","secret"));else{var c=r.c;if(c.e!=null)el.appendChild(ligne("Effectif du champ : "+fmt(c.e,0)));if(c.c!=null)el.appendChild(ligne("Départs estimés : "+fmt(c.c,1)+(c.b!=null?" (bas "+fmt(c.b,1)+" · haut "+fmt(c.h,1)+")":"")));if(c.t!=null)el.appendChild(ligne("Taux de départ : "+fmt(c.t,1)+" %"))}}',
  ' return el.innerHTML;}',
  'function ligne(t,cls){var d=document.createElement("div");if(cls)d.className=cls;d.textContent=t;return d}',
  'var B=$("bulle"),C=$("carte");document.querySelectorAll("path.t").forEach(function(p){',
  ' p.addEventListener("mousemove",function(ev){B.innerHTML=bulle(p.getAttribute("data-code"));B.style.display="block";var r=C.getBoundingClientRect();var x=ev.clientX-r.left+14,y=ev.clientY-r.top+14;if(x+290>r.width)x-=304;B.style.left=x+"px";B.style.top=y+"px"});',
  ' p.addEventListener("mouseleave",function(){B.style.display="none"});',
  ' p.addEventListener("focus",function(){B.innerHTML=bulle(p.getAttribute("data-code"));B.style.display="block";B.style.left="12px";B.style.top="12px"});p.addEventListener("blur",function(){B.style.display="none"});});',
  'document.querySelectorAll(".radios input").forEach(function(r){r.addEventListener("change",function(){etat.vue=r.value;document.querySelectorAll(".radios label").forEach(function(l){l.classList.toggle("on",l.querySelector("input").checked)});rendre()})});',
  'var rech=$("rech");if(rech)rech.addEventListener("input",function(){remplirCats(rech.value);rendre()});',
  '$("cat").addEventListener("change",function(){etat.cat=$("cat").value;rendre()});$("ind").addEventListener("change",function(){etat.ind=$("ind").value;rendre()});$("scen").addEventListener("change",function(){etat.scen=$("scen").value;rendre()});',
  'var rc=document.querySelector(".radios input:checked");if(rc){etat.vue=rc.value;document.querySelectorAll(".radios label").forEach(function(l){l.classList.toggle("on",l.querySelector("input").checked)})}',
  'remplirCats("");if(FR){etat.vue="categorie"}rendre();',
  '})();', sep = "\n")

echap_html_carte <- function(s) { s <- gsub("&", "&amp;", s, fixed = TRUE); s <- gsub("<", "&lt;", s, fixed = TRUE); gsub(">", "&gt;", s, fixed = TRUE) }

# --- Génération : HTML autonome (+ PNG de la vue initiale) ---------------------
generer_carte_departs <- function(prep, fond, fichier_html, fichier_png = NULL, source_note = "calculs propres",
                                  age_min = get0("AGE_MIN_BTS", ifnotfound = NA)) {
  lay <- projeter_fond(fond)
  terr <- prep$territoires
  dimLib <- if (prep$dimension == "pcs") "PCS fine" else "grande catégorie socioprofessionnelle"
  nivLib <- c(france = "France entière", region = "par région administrative", departement = "par département")[[prep$niveau]]
  # chemins SVG : un <path> par entité du fond (toutes, même sans donnée)
  paths <- vapply(names(fond), function(code) {
    nom <- if (prep$niveau == "france") "France entière" else terr$nom[match(code, terr$code)]
    sprintf('<path class="t" data-code="%s" d="%s" tabindex="0" aria-label="%s"></path>',
            echap_html_carte(if (prep$niveau == "france") "FR" else code), lay$chemins[[code]], echap_html_carte(nom))
  }, "")
  encarts <- vapply(lay$encarts, function(e) sprintf('<rect class="encart" x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="4"></rect><text class="encart-lib" x="%.1f" y="%.1f">%s</text>',
                                                      e$cadre[1] + 1, e$cadre[2] + 1, e$cadre[3] - e$cadre[1] - 2, e$cadre[4] - e$cadre[2] - 2, e$x_lib, e$y_lib, echap_html_carte(e$nom)), "")
  opts_ind <- paste(sprintf('<option value="%s"%s>%s</option>', names(INDICATEURS_CARTE), ifelse(names(INDICATEURS_CARTE) == "taux_depart_central_pct", " selected", ""), INDICATEURS_CARTE), collapse = "")
  titre_page <- sprintf("Départs d’ici 2030 par %s — %s", dimLib, nivLib)
  html <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">',
            sprintf('<title>%s</title>', echap_html_carte(titre_page)),
            '<meta name="viewport" content="width=device-width, initial-scale=1">',
            '<style>', css_carte(), '</style></head><body><div class="page">',
            '<header><p class="kicker">Départs attendus d’ici 2030 · résultats diffusables</p>',
            '<h1 id="titre"></h1><p class="sous" id="sous"></p>',
            if (prep$niveau == "france") '<div class="bandeau">France entière — résultat national</div><div class="national" id="national"></div>' else "",
            '</header><div class="grille"><aside class="panneau">',
            '<h2>Vue</h2>',
            if (prep$niveau == "france") '<p class="note">Un seul résultat national : la carte sert de support, sans variation territoriale.</p>' else
              '<div class="radios"><label class="on"><input type="radio" name="vue" value="couverture" checked>Couverture de diffusion</label><label><input type="radio" name="vue" value="categorie">Une catégorie</label></div>',
            sprintf('<div id="b-cat"><h2>%s</h2>%s<select id="cat" size="1" aria-label="Catégorie"></select></div>',
                    if (prep$dimension == "pcs") "PCS" else "Catégorie",
                    if (prep$dimension == "pcs") '<input type="search" id="rech" placeholder="Filtrer : code PCS ou grande CS" aria-label="Filtrer les PCS">' else ""),
            sprintf('<div id="b-ind"><h2>Indicateur</h2><select id="ind">%s</select></div>', opts_ind),
            '<div id="b-scen"><label for="scen">Scénario (départs)</label><select id="scen"><option value="c" selected>Central</option><option value="b">Bas</option><option value="h">Haut</option></select></div>',
            '<h2>Légende</h2><ul class="legende" id="legende"></ul><p class="compte" id="compte"></p><p class="note" id="echelle"></p>',
            if (length(lay$encarts) > 0) '<p class="note">DROM en encarts, chaque encart à sa propre échelle.</p>' else "",
            '</aside><div class="carte" id="carte">',
            sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">', lay$largeur, lay$hauteur, echap_html_carte(titre_page)),
            encarts, paths, '</svg><div class="bulle" id="bulle" role="status"></div></div></div>',
            sprintf('<footer>Source : %s%s. Les cellules masquées (secret statistique) ne figurent pas dans cette page, ni dans ses données embarquées : seule leur existence est connue. Gris = non diffusé (secret statistique) ; blanc pointillé = pas de donnée observée ; une valeur nulle diffusée est colorée comme toute valeur. Fond : IGN Admin Express (COG 2018) via france-geojson, licence ouverte.</footer>',
                    echap_html_carte(source_note), if (!is.na(age_min)) sprintf(" · champ : salariés de %d ans et + en 2024, entreprises du périmètre BITD", as.integer(age_min)) else ""),
            '<script type="application/json" id="donnees">', donnees_json_carte(prep), '</script>',
            '<script>', js_carte(), '</script>',
            '</div></body></html>')
  dir.create(dirname(fichier_html), showWarnings = FALSE, recursive = TRUE)
  writeLines(enc2utf8(html), fichier_html, useBytes = TRUE)
  if (!is.null(fichier_png)) tryCatch(png_carte_departs(prep, lay, fichier_png, titre_page),
                                      error = function(e) message("08e : PNG non produit (", conditionMessage(e), ") — le HTML reste la restitution de référence."))
  invisible(fichier_html)
}

# --- PNG de la vue initiale (couverture ; France : résultat national) ----------
png_carte_departs <- function(prep, lay, fichier_png, titre = "") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 absent")
  long <- lay$long
  if (prep$niveau == "france") {
    st <- prep$territoires$statut[prep$territoires$code == "FR"]
    long$classe <- if (st == "masque") "Non diffusé — secret statistique" else "Résultat national"
    niveaux <- c("Résultat national", "Non diffusé — secret statistique")
    cols <- c(COULEURS_CARTE$fond_support, COULEURS_CARTE$secret)
    cv <- prep$couverture[prep$couverture$code == "FR", ]
    sous <- if (nrow(cv)) sprintf("France entière — résultat national : %d catégorie(s) observée(s), %d diffusable(s), %d masquée(s)", cv$n_obs, cv$n_diff, cv$n_masq) else "France entière"
  } else {
    cv <- prep$couverture
    k <- function(p) if (is.na(p)) "Pas de donnée observée" else if (p <= 0) "Aucune catégorie diffusable — secret statistique" else
      CLASSES_COUVERTURE$libelles[max(which(p >= CLASSES_COUVERTURE$bornes))]
    long$classe <- vapply(cv$part_diff_pct[match(long$code, cv$code)], k, "")
    niveaux <- c(rev(CLASSES_COUVERTURE$libelles), "Aucune catégorie diffusable — secret statistique", "Pas de donnée observée")
    cols <- c(rev(COULEURS_CARTE$classes), COULEURS_CARTE$secret, COULEURS_CARTE$sans_donnee)
    sous <- sprintf("Part des %s observées pouvant être diffusées — gris : secret statistique ; blanc : pas de donnée",
                    if (prep$dimension == "pcs") "PCS" else "catégories")
  }
  long$classe <- factor(long$classe, levels = niveaux)
  enc <- if (length(lay$encarts)) bind_rows(lapply(lay$encarts, function(e) tibble(x1 = e$cadre[1], x2 = e$cadre[3], y1 = -e$cadre[2], y2 = -e$cadre[4], lib = e$nom, xl = e$x_lib, yl = -e$y_lib))) else NULL
  g <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = long, ggplot2::aes(x = x, y = y, group = groupe, fill = classe), colour = "#c3c9d1", linewidth = .22) +
    ggplot2::scale_fill_manual(values = setNames(cols, niveaux), drop = FALSE, name = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = "#9a9a9a"))) +
    ggplot2::coord_equal(expand = FALSE) + ggplot2::theme_void(base_size = 11) +
    ggplot2::labs(title = titre, subtitle = sous, caption = "Les cellules masquées ne sont pas représentées par leur valeur. Fond IGN Admin Express via france-geojson.") +
    ggplot2::theme(legend.position = "bottom", legend.direction = "vertical", plot.title = ggplot2::element_text(face = "bold", size = 14),
                   plot.subtitle = ggplot2::element_text(colour = "#4a5260", size = 10), plot.caption = ggplot2::element_text(colour = "#8a919c", size = 8),
                   plot.margin = ggplot2::margin(10, 10, 10, 10))
  if (!is.null(enc)) g <- g + ggplot2::geom_rect(data = enc, ggplot2::aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1), fill = NA, colour = "#d5d9df") +
    ggplot2::geom_text(data = enc, ggplot2::aes(x = xl, y = yl, label = lib), hjust = 0, size = 3, colour = "#6b7280")
  ggplot2::ggsave(fichier_png, g, width = 8, height = 10.5, dpi = 130, bg = "white")
  invisible(fichier_png)
}
