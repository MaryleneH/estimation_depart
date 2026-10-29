# ==============================================================================
# 00k_fonctions_tension.R — Exposition au recrutement difficile
#                           (fonctions pures, maille DÉPARTEMENT ; specs/10 §13)
# ------------------------------------------------------------------------------
# IDÉE     : la typologie dit combien de salariés partent et à quel rythme ; on
#            ajoute une question : ces départs portent-ils sur des métiers
#            difficiles à recruter ? Indice d'EXPOSITION PAR COMPOSITION :
#              exposition = départs des PCS stratégiques en tension
#                           / départs des PCS stratégiques de tension connue.
# PRINCIPE : la tension est une propriété NATIONALE du métier (rangs de la FAP,
#            France entière), jamais du département (SPEC-TYPO-020 à 023, 101) ;
#            le pont est la PCS (classeur PCS -> FAP), jamais FAP -> grande CS.
# ENTRÉES  : classeur PCS -> FAP (hors dépôt), table EXACTE département x PCS du
#            08c, bts_projete (secret). Aucun départ recalculé, aucun paramètre du
#            modèle touché. Textes par gabarits fixes, jamais générés.
# ==============================================================================
library(dplyr)

CLASSES_EXPOSITION <- c("Faible", "Modérée", "Forte")
# Quatre situations de métier, libellés FACTUELS (ils décrivent, ils ne jugent pas ; SPEC-TYPO-104)
QUADRANTS_TENSION <- c(critique = "Départs rapides et métier en tension", releve = "Départs rapides, métier hors tension",
                       difficile = "Départs modérés, métier en tension", controle = "Départs modérés, métier hors tension")
COULEURS_SITUATIONS <- c(critique = "#fbe4e1", releve = "#fff1dc", difficile = "#e3eef9", controle = "#eef1f4")   # fonds (tableau, classeur)
COULEURS_POINTS_SITUATIONS <- c(critique = "#b2182b", releve = "#ef8a62", difficile = "#67a9cf", controle = "#9aa5b1")   # points (PNG, SVG)
COULEURS_CLASSES_EXPOSITION <- c(Faible = "#eef1f4", "Modérée" = "#fff1dc", Forte = "#fbe4e1")   # fonds de lignes du classeur
LIBELLES_EXPOSITION <- c(long = "part des remplacements difficiles", court = "remplacements difficiles")   # libellé PUBLIC de l’indicateur « exposition » (code, CSV)
TITRES_MESSAGES_TENSION <- c("Renouvellement rapide et métiers en tension", "Métiers à départs rapides et en tension", "Le levier")
CAUSES_FRAGILITE <- c(hypothese_bas = "hypothèse de départ", hypothese_haut = "hypothèse de départ",
                      seuil_plus = "seuil de tension", seuil_moins = "seuil de tension", confiance = "rattachement PCS → FAP")

# --- Paramètres (source unique : 00_config.R) --------------------------------------
parametres_tension <- function() list(
  fichier = FICHIER_PCS_FAP_TENSION, feuille = TENSION_FEUILLE, cols = TENSION_COLS, composantes = TENSION_COMPOSANTES,
  rang_max = TENSION_RANG_MAX, sous_niveaux = get0("TENSION_SOUS_NIVEAUX", ifnotfound = NULL), seuil_rang = TENSION_SEUIL_RANG, confiance_min = get0("TENSION_CONFIANCE_MIN", ifnotfound = NULL),
  methode = TENSION_METHODE_CLASSES, expo_faible = TENSION_SEUIL_EXPO_FAIBLE, expo_forte = TENSION_SEUIL_EXPO_FORTE,
  couverture_min = TENSION_COUVERTURE_MIN_PCT, effectif_min = TYPO_SEUIL_EFFECTIF_MIN,
  classeur_excel = isTRUE(get0("TENSION_CLASSEUR_EXCEL", ifnotfound = TRUE)),
  n_suivis = as.integer(get0("TENSION_N_METIERS_SUIVIS", ifnotfound = NA_integer_)),   # N saisi à la main (00_config)
  libelle_liste = lib_liste(), ecart_pts = as.numeric(get0("TENSION_ECART_POINTS", ifnotfound = 3)))

# --- Libellé public de la liste de métiers (00_config TENSION_LIBELLE_LISTE) -----------
# Les textes du code sont écrits avec « métiers stratégiques » ; texte_liste() y substitue le libellé
# paramétré au moment de la restitution (comme texte_perimetre() pour le périmètre) : pluriel,
# singulier (« métier … ») et « PCS … ». Appliqué aux textes, fiches, classeurs, cartes et console.
lib_liste <- function() { x <- get0("TENSION_LIBELLE_LISTE", ifnotfound = "métiers de la première sous-liste"); if (is.character(x) && length(x) == 1 && nzchar(x)) x else "métiers de la première sous-liste" }
texte_liste <- function(x) {
  if (is.null(x) || !is.character(x)) return(x)
  pl <- lib_liste(); if (identical(pl, "métiers stratégiques")) return(x)               # libellé d'origine : rien à substituer
  sg <- sub("^métiers ", "métier ", pl); pcs <- sub("^métiers? ", "PCS ", pl)
  x <- gsub("métiers stratégiques (suivis|de la liste)", pl, x); x <- gsub("métiers stratégiques", pl, x)
  x <- gsub("métier stratégique", sg, x); x <- gsub("PCS stratégiques?", pcs, x)
  x
}

# --- Lecture contrôlée du classeur (SPEC-TYPO-100) ----------------------------------
# Une ligne par PCS (une seule FAP) ; rangs entiers de 1 à rang_max, ou niveau maximal
# avec un sous-niveau admis (Dares 2023 : 5.1, 5.2), valeurs ramenées à UNE décimale
# (5,0999 -> 5.1, compté) ; rang manquant = tension inconnue (jamais 0). Produit
# rang_<k> (niveau ENTIER, pour le seuil) et rang_<k>_fin (valeur fine, affichage).
# Les colonnes d'emploi éventuelles ne sont pas lues.
lire_pcs_fap_tension <- function(fichier = params$fichier, params = parametres_tension()) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("lire_pcs_fap_tension : le paquet readxl est requis.")
  if (!file.exists(fichier)) stop("lire_pcs_fap_tension : classeur introuvable : ", fichier)
  feuilles <- readxl::excel_sheets(fichier)
  if (!params$feuille %in% feuilles)
    stop(sprintf("lire_pcs_fap_tension : feuille « %s » absente du classeur (feuilles : %s) — réglez TENSION_FEUILLE.", params$feuille, paste(feuilles, collapse = ", ")))
  x <- readxl::read_excel(fichier, sheet = params$feuille, col_types = "text")
  names(x) <- trimws(names(x))
  interdites <- grep("^cs1$|grande_?cs|tension_departement|departement_en_tension", names(x), value = TRUE, ignore.case = TRUE)
  if (length(interdites)) stop("lire_pcs_fap_tension : colonne(s) interdite(s) (aucune correspondance FAP -> grande CS, aucune tension départementale) : ", paste(interdites, collapse = ", "))
  col_comp <- vapply(params$composantes, function(k) unname(k[["colonne"]]), "")
  manque <- setdiff(c(unname(params$cols), unname(col_comp)), names(x))
  if (length(manque)) stop("lire_pcs_fap_tension : colonne(s) absente(s) : ", paste(manque, collapse = ", "), " — présentes : ", paste(names(x), collapse = ", "),
                           ". Réglez TENSION_COLS / TENSION_COMPOSANTES.")
  txt <- function(col) { v <- trimws(as.character(x[[col]])); v[!is.na(v) & v == ""] <- NA_character_; v }
  pcs_src <- txt(params$cols[["pcs"]])
  if (all(is.na(pcs_src))) stop("lire_pcs_fap_tension : aucune PCS dans la feuille « ", params$feuille, " ».")
  if (anyNA(pcs_src)) { message("lire_pcs_fap_tension : ", sum(is.na(pcs_src)), " ligne(s) sans code PCS ignorée(s)."); x <- x[!is.na(pcs_src), , drop = FALSE]; pcs_src <- pcs_src[!is.na(pcs_src)] }
  num <- function(col) suppressWarnings(as.numeric(sub(",", ".", txt(col), fixed = TRUE)))
  d <- tibble(pcs = normaliser_code_pcs(pcs_src), pcs_source = pcs_src, pcs_libelle = txt(params$cols[["pcs_libelle"]]),
              fap_code = txt(params$cols[["fap_code"]]), fap_libelle = txt(params$cols[["fap_libelle"]]),
              poids_pcs = num(params$cols[["poids_pcs"]]), indice_confiance = num(params$cols[["indice_confiance"]]))
  dup <- unique(d$pcs_source[duplicated(d$pcs)])
  if (length(dup)) stop("lire_pcs_fap_tension : PCS en double (une seule ligne, donc une seule FAP, par PCS) : ", paste(dup, collapse = ", "))
  admis <- c(seq_len(params$rang_max), if (length(params$sous_niveaux)) params$rang_max + params$sous_niveaux / 10)
  n_ramenees <- 0L
  rang <- function(col) {
    s <- txt(col); brut <- suppressWarnings(as.numeric(sub(",", ".", s, fixed = TRUE))); r <- round(brut, 1)
    bad <- !is.na(s) & (is.na(r) | !vapply(r, function(v) !is.na(v) && any(abs(admis - v) < 1e-9), TRUE))
    if (any(bad)) stop(sprintf("lire_pcs_fap_tension : colonne « %s » : %d valeur(s) hors de l'échelle admise (%s) (PCS %s ; valeurs %s) — réglez TENSION_RANG_MAX (10 pour des déciles) ou TENSION_SOUS_NIVEAUX.",
                               col, sum(bad), paste(format(admis, drop0trailing = TRUE), collapse = ", "), paste(head(d$pcs_source[bad], 8), collapse = ", "), paste(head(s[bad], 8), collapse = ", ")))
    n_ramenees <<- n_ramenees + sum(!is.na(brut) & abs(brut - r) > 1e-9)
    r
  }
  fin <- rang(params$cols[["tension"]]); d$rang_tension <- as.integer(floor(fin + 1e-9)); d$rang_tension_fin <- fin
  for (k in names(params$composantes)) { fin <- rang(params$composantes[[k]][["colonne"]]); d[[paste0("rang_", k)]] <- as.integer(floor(fin + 1e-9)); d[[paste0("rang_", k, "_fin")]] <- fin }
  if (n_ramenees > 0) message("lire_pcs_fap_tension : ", n_ramenees, " valeur(s) de rang ramenée(s) à une décimale (imprécision de virgule flottante, ex. 5,0999 -> 5.1).")
  d
}

# --- Tension du métier : en tension, composante dominante, levier, confiance ---------
# Composante dominante = rang le plus élevé, retenue seulement s'il atteint le seuil ;
# ex aequo départagés par l'ordre de TENSION_COMPOSANTES (signalés). Faible confiance :
# indice < confiance_min ; NULL = plus bas niveau observé (si >= 2 niveaux) ; NA = faible.
tension_metiers <- function(d, params = parametres_tension(), seuil = params$seuil_rang) {
  cles <- names(params$composantes)
  m <- as.matrix(d[, paste0("rang_", cles), drop = FALSE]); mf <- as.matrix(d[, paste0("rang_", cles, "_fin"), drop = FALSE])
  dom <- character(nrow(d)); exa <- logical(nrow(d))
  for (i in seq_len(nrow(d))) {
    r <- m[i, ]; f <- mf[i, ]; dom[i] <- NA_character_
    if (all(is.na(r))) next
    mx <- max(r, na.rm = TRUE); if (mx < seuil) next
    # ex aequo sur le niveau entier : la valeur fine (5.2 > 5.1) tranche d'abord, puis l'ordre de la configuration
    cand <- which(!is.na(r) & r == mx); mxf <- max(f[cand]); gagnants <- cand[abs(f[cand] - mxf) < 1e-9]
    dom[i] <- cles[gagnants[1]]; exa[i] <- length(gagnants) > 1
  }
  lib <- vapply(params$composantes, function(k) unname(k[["libelle"]]), ""); lev <- vapply(params$composantes, function(k) unname(k[["levier"]]), "")
  conf <- d$indice_confiance
  seuil_conf <- if (is.null(params$confiance_min)) { lv <- sort(unique(conf[!is.na(conf)])); if (length(lv) >= 2) lv[2] else NA_real_ } else params$confiance_min
  res <- d |> mutate(tension_connue = !is.na(rang_tension), en_tension = ifelse(is.na(rang_tension), NA, rang_tension >= seuil),
                     composante_dominante = dom, composante_ex_aequo = exa, composante_libelle = unname(lib[dom]), levier = unname(lev[dom]),
                     faible_confiance = is.na(conf) | (!is.na(seuil_conf) & conf < seuil_conf))
  attr(res, "seuil_confiance") <- seuil_conf
  res
}

# --- Exposition par département (SPEC-TYPO-102 / 103) ------------------------------
# dep_pcs : table EXACTE département x PCS du 08c ; metiers : tension_metiers().
# Parts : A = stratégiques en tension, B = stratégiques hors tension, C = le reste.
exposition_departements <- function(dep_pcs, metiers, params = parametres_tension()) {
  requis <- c("geo_code", "pcs", "effectif_champ", "departs_central", "departs_bas", "departs_haut")
  manque <- setdiff(requis, names(dep_pcs)); if (length(manque)) stop("exposition_departements : colonnes absentes : ", paste(manque, collapse = ", "))
  if (!is.character(dep_pcs$geo_code)) stop("exposition_departements : geo_code doit être un texte.")
  t <- dep_pcs |> mutate(pcs_n = normaliser_code_pcs(pcs)) |>
    left_join(metiers |> select(pcs_n = pcs, rang_tension, rang_tension_fin, composante_dominante, faible_confiance, pcs_libelle), by = "pcs_n")
  t$strategique <- !is.na(t$rang_tension)
  t$part <- ifelse(t$strategique & t$rang_tension >= params$seuil_rang, "A", ifelse(t$strategique, "B", "C"))
  calc <- function(col = "departs_central", seuil = params$seuil_rang, sans_faible = FALSE) {
    garder <- if (sans_faible) !(t$faible_confiance %in% TRUE) else rep(TRUE, nrow(t))
    a <- ifelse(t$strategique & t$rang_tension >= seuil & garder, t[[col]], 0); s <- ifelse(t$strategique & garder, t[[col]], 0)
    tibble(geo_code = t$geo_code, a = a, s = s) |> group_by(geo_code) |> summarise(a = sum(a), s = sum(s), .groups = "drop") |>
      mutate(e = ifelse(s > 0, 100 * a / s, NA_real_))
  }
  c0 <- calc()
  d <- t |> group_by(geo_code) |>
    summarise(geo_nom = if ("geo_nom" %in% names(t)) first(geo_nom) else NA_character_, effectif_champ_total = sum(effectif_champ),
              departs_total = sum(departs_central), n_metiers_strategiques = sum(strategique), .groups = "drop") |>
    left_join(c0 |> select(geo_code, departs_strategiques_tension = a, departs_strategiques = s, exposition_pct = e), by = "geo_code") |>
    mutate(couverture_pct = ifelse(departs_total > 0, 100 * departs_strategiques / departs_total, NA_real_))
  fr <- list(exposition_pct = if (sum(c0$s) > 0) 100 * sum(c0$a) / sum(c0$s) else NA_real_,
             couverture_pct = if (sum(d$departs_total) > 0) 100 * sum(c0$s) / sum(d$departs_total) else NA_real_)
  d$exposition_france_pct <- fr$exposition_pct
  # classes : bornes sur les départements retenus (couverture suffisante, effectif du champ >= plancher)
  interpretable <- !is.na(d$couverture_pct) & d$couverture_pct >= params$couverture_min & !is.na(d$exposition_pct)
  retenus <- interpretable & d$effectif_champ_total >= params$effectif_min
  bornes <- seuils_classes(d$exposition_pct[retenus], params$methode, c(params$expo_faible, params$expo_forte))
  classer <- function(e) ifelse(interpretable, classer_par_seuils(e, bornes, CLASSES_EXPOSITION), NA_character_)
  d$classe_exposition <- classer(d$exposition_pct)
  # robustesse : cinq variantes, MÊMES bornes
  presents_faibles <- any(metiers$faible_confiance & metiers$pcs %in% t$pcs_n)
  variantes <- list(hypothese_bas = calc("departs_bas"), hypothese_haut = calc("departs_haut"),
                    seuil_plus = if (params$seuil_rang < params$rang_max) calc(seuil = params$seuil_rang + 1) else NULL,
                    seuil_moins = calc(seuil = params$seuil_rang - 1),
                    confiance = if (presents_faibles) calc(sans_faible = TRUE) else NULL)
  variantes <- variantes[!vapply(variantes, is.null, TRUE)]
  change <- vapply(names(variantes), function(v) { cl <- classer(variantes[[v]]$e[match(d$geo_code, variantes[[v]]$geo_code)])
    !is.na(d$classe_exposition) & (is.na(cl) | cl != d$classe_exposition) }, logical(nrow(d)))
  if (!is.matrix(change)) change <- matrix(change, nrow = nrow(d), dimnames = list(NULL, names(variantes)))
  d$causes_fragilite <- vapply(seq_len(nrow(d)), function(i) { cs <- unique(unname(CAUSES_FRAGILITE[colnames(change)[change[i, ]]])); if (length(cs)) paste(cs, collapse = ", ") else NA_character_ }, "")
  d$robustesse <- ifelse(is.na(d$classe_exposition), NA_character_, ifelse(is.na(d$causes_fragilite), "robuste", "à confirmer"))
  d$exposition_bas_pct <- variantes$hypothese_bas$e[match(d$geo_code, variantes$hypothese_bas$geo_code)]
  d$exposition_haut_pct <- variantes$hypothese_haut$e[match(d$geo_code, variantes$hypothese_haut$geo_code)]
  # PLANCHERS pour la carte (SPEC-TYPO-109) : départs de la part A (hypothèses basse / haute) et part de A dans TOUS les départs
  d$departs_tension_bas <- variantes$hypothese_bas$a[match(d$geo_code, variantes$hypothese_bas$geo_code)]
  d$departs_tension_haut <- variantes$hypothese_haut$a[match(d$geo_code, variantes$hypothese_haut$geo_code)]
  d$part_departs_tension_pct <- ifelse(d$departs_total > 0, 100 * d$departs_strategiques_tension / d$departs_total, NA_real_)
  # levier du département : composante dominante la plus fréquente parmi les départs de A (pondérée)
  ordre <- names(params$composantes)
  lev <- t |> filter(part == "A", !is.na(composante_dominante)) |> group_by(geo_code, composante_dominante) |> summarise(dep = sum(departs_central), .groups = "drop") |>
    arrange(geo_code, desc(dep), match(composante_dominante, ordre)) |> group_by(geo_code) |> slice(1) |> ungroup()
  d$levier_cle <- lev$composante_dominante[match(d$geo_code, lev$geo_code)]
  d$levier <- unname(vapply(params$composantes, function(k) unname(k[["levier"]]), "")[d$levier_cle])
  d$composante_levier <- unname(vapply(params$composantes, function(k) unname(k[["libelle"]]), "")[d$levier_cle])
  top <- t |> filter(part == "A") |> arrange(geo_code, desc(departs_central), pcs) |> group_by(geo_code) |> slice_head(n = 3) |>
    summarise(metiers = paste(ifelse(is.na(pcs_libelle), paste0("PCS ", pcs), pcs_libelle), collapse = " ; "), .groups = "drop")
  d$metiers_tension_top <- top$metiers[match(d$geo_code, top$geo_code)]
  list(departements = d |> arrange(geo_code), cellules = t, bornes = bornes, france = fr, n_retenus = sum(retenus), params = params,
       variantes = names(variantes), seuil_confiance = attr(metiers, "seuil_confiance"))
}

# Cumul : renouvellement rapide (profils à intensité élevée) ET exposition Forte
cumul_renouvellement_tension <- function(profil, classe_exposition) {
  profil %in% PROFILS_TYPOLOGIE[c("enjeu", "emergent")] & classe_exposition %in% CLASSES_EXPOSITION[3]
}

# --- Texte d'un département (gabarit fixe, SPEC-TYPO-105) ---------------------------
rediger_exposition_departement <- function(d, params = parametres_tension(), territoire = c("département", "région")) texte_liste(rediger_exposition_departement_brut(d, params, territoire))
rediger_exposition_departement_brut <- function(d, params = parametres_tension(), territoire = c("département", "région")) {
  territoire <- match.arg(territoire); du <- if (territoire == "région") "de la région" else "du département"; ce <- if (territoire == "région") "cette région" else "ce département"
  f0 <- function(x) formatC(round(x), format = "d"); f1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
  vapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    if (is.na(r$couverture_pct)) return(NA_character_)
    if (r$couverture_pct < params$couverture_min || is.na(r$exposition_pct))
      return(sprintf("Les métiers stratégiques ne représentent que %s %% des départs attendus d’ici 2030 dans %s : sa part des remplacements difficiles n’est pas interprétée.", f1(r$couverture_pct), ce))
    sprintf("Sur 100 départs attendus d’ici 2030 dans les métiers stratégiques, %s concernent des métiers en tension en France (France : %s)%s. Ces métiers représentent %s %% des départs %s ; les autres départs (%s %%) concernent des métiers dont la tension n’est pas observée.%s",
            f0(r$exposition_pct), f0(r$exposition_france_pct),
            if (is.na(r$classe_exposition)) "" else sprintf(" : %s %s, %s", LIBELLES_EXPOSITION[["long"]], tolower(r$classe_exposition),
                                                           if (identical(r$robustesse, "robuste")) "résultat robuste" else sprintf("résultat à confirmer (sensible : %s)", r$causes_fragilite)),
            f0(r$couverture_pct), du, f0(100 - r$couverture_pct), if (!is.na(r$levier)) sprintf(" Levier le plus fréquent : %s (%s).", r$levier, r$composante_levier) else "")
  }, "")
}

# --- Secret (SPEC-TYPO-106, SPEC-SEC-034) ----------------------------------------------
# Publication TOUT OU RIEN par département : chaque part non vide (A, B, C) satisfait la
# règle primaire (indicateurs sur les lignes de bts_projete) ; aucune part ne contient
# exactement une cellule PCS masquée par le 08c ; département non masqué par la typologie.
controler_parts_secret <- function(base, cles, dif_pcs, regles) {
  sec <- base |> count(across(all_of(c(cles, "part"))), name = "n") |>
    left_join(indicateurs_secret(base, c(cles, "part")), by = c(cles, "part")) |>
    mutate(primaire = secret_primaire(n, n_entreprises, part_dominante_pct, regles) & n > 0)
  bloq_p <- sec |> group_by(across(all_of(cles))) |> summarise(bloque_primaire = any(primaire), .groups = "drop")
  diffe <- dif_pcs |> group_by(across(all_of(c(cles, "part")))) |> summarise(n_masquees = sum(masque %in% TRUE), .groups = "drop") |>
    group_by(across(all_of(cles))) |> summarise(bloque_difference = any(n_masquees == 1), .groups = "drop")
  full_join(bloq_p, diffe, by = cles) |> mutate(bloque_primaire = coalesce(bloque_primaire, FALSE), bloque_difference = coalesce(bloque_difference, FALSE))
}
appliquer_secret_exposition <- function(expo, metiers, base, dif_pcs_dep, typo_dif = NULL, dif_pcs_fr = NULL, regles = regles_secret(), params = expo$params, territoire = "département", univers = NULL) {
  partie <- function(pcs_n) ifelse(pcs_n %in% metiers$pcs[metiers$en_tension %in% TRUE], "A", ifelse(pcs_n %in% metiers$pcs[metiers$en_tension %in% FALSE], "B", "C"))
  b <- base |> mutate(geo_code = ifelse(is.na(geo_code), "inconnu", as.character(geo_code)), part = partie(normaliser_code_pcs(pcs)))
  if (!"masque" %in% names(dif_pcs_dep)) stop("appliquer_secret_exposition : table de diffusion du 08c requise (colonne masque).")
  dp <- dif_pcs_dep |> mutate(geo_code = as.character(geo_code), part = partie(normaliser_code_pcs(pcs)))
  ctl <- controler_parts_secret(b, "geo_code", dp, regles)
  # repère France : même contrôle sur la France entière (masques du 08c France)
  fr_ok <- TRUE
  if (!is.null(dif_pcs_fr)) {
    bf <- b |> mutate(.fr = "FR"); df <- dif_pcs_fr |> mutate(.fr = "FR", part = partie(normaliser_code_pcs(pcs)))
    cf <- controler_parts_secret(bf, ".fr", df, regles); fr_ok <- !(cf$bloque_primaire | cf$bloque_difference)
  }
  d <- expo$departements |> left_join(ctl, by = "geo_code") |>
    mutate(bloque_primaire = coalesce(bloque_primaire, TRUE), bloque_difference = coalesce(bloque_difference, FALSE))
  masque_typo <- if (is.null(typo_dif)) rep(FALSE, nrow(d)) else d$geo_code %in% typo_dif$geo_code[typo_dif$masque %in% TRUE]
  profil <- if (is.null(typo_dif)) rep(NA_character_, nrow(d)) else typo_dif$profil_typologie[match(d$geo_code, typo_dif$geo_code)]
  d$tension_diffusee <- !(d$bloque_primaire | d$bloque_difference | masque_typo)
  if (!fr_ok) d$exposition_france_pct <- NA_real_
  # métiers cités : cellules PCS DIFFUSABLES du département, dans la part A
  top <- dp |> filter(part == "A", masque %in% FALSE) |> mutate(pcs_n = normaliser_code_pcs(pcs)) |>
    left_join(metiers |> select(pcs_n = pcs, pcs_libelle), by = "pcs_n") |> arrange(geo_code, desc(departs_central), pcs) |>
    group_by(geo_code) |> slice_head(n = 3) |> summarise(m = paste(ifelse(is.na(pcs_libelle), paste0("PCS ", pcs), pcs_libelle), collapse = " ; "), .groups = "drop")
  d$texte <- rediger_exposition_departement(d, params, territoire)   # (texte_liste appliqué dans rediger_exposition_departement)
  out <- tibble(geo_code = d$geo_code, geo_nom = d$geo_nom, tension_diffusee = d$tension_diffusee,
                tension_exposition_pct = d$exposition_pct, tension_exposition_france_pct = d$exposition_france_pct, tension_couverture_pct = d$couverture_pct,
                tension_classe = d$classe_exposition, tension_robustesse = d$robustesse, tension_causes = d$causes_fragilite,
                tension_levier = d$levier, tension_composante = d$composante_levier, tension_metiers = top$m[match(d$geo_code, top$geo_code)],
                tension_cumul = cumul_renouvellement_tension(profil, d$classe_exposition), tension_texte = d$texte,
                # planchers pour la carte des remplacements difficiles (SPEC-TYPO-109) : départs de la part A et leur part dans tous les départs
                tension_departs_min = d$departs_strategiques_tension, tension_departs_min_bas = d$departs_tension_bas, tension_departs_min_haut = d$departs_tension_haut,
                tension_part_departs_min_pct = d$part_departs_tension_pct)
  for (v in setdiff(names(out), c("geo_code", "geo_nom", "tension_diffusee"))) out[[v]][!out$tension_diffusee] <- NA
  # univers des territoires (SPEC-SEC-035) : territoire sans salarié observé = ligne non diffusée, sans valeur
  if (!is.null(univers)) out <- completer_univers_diffusion(out, univers, col_masque = "tension_diffusee", valeur_masque = FALSE) |> arrange(geo_code)
  out
}

# --- Matrice nationale des métiers stratégiques (SPEC-TYPO-104) ----------------------
# x = part des salariés du champ susceptibles de partir (08c France, cellule diffusable) ;
# repère x = même part pour tout le champ en France ; repère y = seuil de rang.
matrice_metiers_tension <- function(metiers, fr_analytique, fr_diffusion = NULL, params = parametres_tension()) {
  ref_x <- 100 * sum(fr_analytique$departs_central) / sum(fr_analytique$effectif_champ)
  fr <- (if (is.null(fr_diffusion)) fr_analytique |> mutate(masque = FALSE) else fr_diffusion) |>
    mutate(pcs = normaliser_code_pcs(pcs)) |> select(pcs, masque, departs_central, any_of(c("departs_bas", "departs_haut")), taux_depart_central_pct)
  m <- metiers |> left_join(fr, by = "pcs") |>
    mutate(statut = ifelse(is.na(masque), texte_perimetre("absente de la Base"), ifelse(masque, "non diffusé (secret statistique)", ifelse(is.na(en_tension), "tension inconnue", "diffusé"))),
           quadrant = ifelse(statut != "diffusé", NA_character_,
                             ifelse(taux_depart_central_pct >= ref_x, ifelse(en_tension, QUADRANTS_TENSION[["critique"]], QUADRANTS_TENSION[["releve"]]),
                                    ifelse(en_tension, QUADRANTS_TENSION[["difficile"]], QUADRANTS_TENSION[["controle"]]))))
  for (v in c("departs_bas", "departs_haut")) if (!v %in% names(m)) m[[v]] <- NA_real_
  for (v in c("departs_central", "departs_bas", "departs_haut", "taux_depart_central_pct")) m[[v]][m$statut != "diffusé"] <- NA
  list(metiers = m |> select(-masque), ref_x = ref_x, seuil_rang = params$seuil_rang, rang_max = params$rang_max, sous_niveaux = params$sous_niveaux)
}

# --- Trois messages pour la fiche (gabarits fixes) ------------------------------------
messages_fiche_tension <- function(dif, typo_dif, mat, params = parametres_tension()) texte_liste(messages_fiche_tension_brut(dif, typo_dif, mat, params))
messages_fiche_tension_brut <- function(dif, typo_dif, mat, params = parametres_tension()) {
  f0 <- function(x) formatC(round(x), format = "d"); pl <- function(n, s = "s") if (n > 1) s else ""
  noms <- function(v) if (length(v) > 6) paste0(paste(head(v, 6), collapse = ", "), "…") else paste(v, collapse = ", ")
  dh <- dif |> filter(tension_cumul %in% TRUE)
  ok <- typo_dif[!(typo_dif$masque %in% TRUE) & !is.na(typo_dif$departs_central), ]
  n_suivis <- params$n_suivis                         # N : saisi dans 00_config (TENSION_N_METIERS_SUIVIS), jamais déduit du classeur
  m1 <- if (nrow(dh) == 0) sprintf("Parmi les %d métiers stratégiques suivis, aucun département ne cumule un renouvellement rapide d’ici 2030 et une forte part de remplacements difficiles. La tension des autres métiers n’est pas observée.", n_suivis) else
    sprintf("%d département%s cumule%s un renouvellement rapide d’ici 2030 et une forte part de remplacements difficiles parmi les %d métiers stratégiques suivis : %s. %s %s %s %% des départs attendus. La tension des autres métiers n’est pas observée.",
            nrow(dh), pl(nrow(dh)), if (nrow(dh) > 1) "nt" else "", n_suivis, noms(paste0(dh$geo_nom, " (", dh$geo_code, ")")), if (nrow(dh) > 1) "Ils" else "Il", if (nrow(dh) > 1) "concentrent" else "concentre",
            f0(100 * sum(ok$departs_central[ok$geo_code %in% dh$geo_code]) / sum(ok$departs_central)))
  mm <- mat$metiers |> filter(statut == "diffusé"); cr <- mm |> filter(quadrant == QUADRANTS_TENSION[["critique"]]) |> arrange(desc(departs_central))
  m2 <- if (nrow(cr) == 0) sprintf("Parmi les %d métiers stratégiques suivis, aucun ne cumule des départs plus rapides que la moyenne et une tension forte en France.", n_suivis) else
    sprintf("Parmi les %d métiers stratégiques suivis, %d cumule%s des départs plus rapides que la moyenne et une tension forte en France : %s. %s %s %s %% des départs des métiers stratégiques.",
            n_suivis, nrow(cr), if (nrow(cr) > 1) "nt" else "",
            noms(ifelse(is.na(cr$pcs_libelle), paste0("PCS ", cr$pcs_source), cr$pcs_libelle)), if (nrow(cr) > 1) "Ils" else "Il", if (nrow(cr) > 1) "portent" else "porte",
            f0(100 * sum(cr$departs_central) / sum(mm$departs_central)))
  base_lev <- if (nrow(cr) > 0) cr else mm |> filter(en_tension %in% TRUE)
  lv <- base_lev |> filter(!is.na(levier)) |> count(levier, composante_libelle, sort = TRUE)
  m3 <- if (nrow(lv) == 0) "Aucune composante de la tension n’atteint le seuil pour ces métiers : pas de levier dominant." else
    sprintf("Levier le plus fréquent %s : %s (%s), pour %d métier%s sur %d.", if (nrow(cr) > 0) "pour ces métiers" else "pour les métiers en tension",
            lv$levier[1], lv$composante_libelle[1], lv$n[1], pl(lv$n[1]), nrow(base_lev))
  c(m1, m2, m3)
}

# --- Matrice en image (ggplot2, sans ggrepel) -------------------------------------------
png_matrice_metiers_tension <- function(mat, fichier) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 absent")
  d <- mat$metiers |> filter(statut == "diffusé")
  if (nrow(d) == 0) stop("aucun métier stratégique diffusable")
  # y = valeur fine ; l'axe est ORDINAL (des rangs) : chaque sous-niveau (5.1, 5.2) est écarté d'un demi-cran
  # au-dessus de son niveau (5 -> 5, 5.1 -> 5,5, 5.2 -> 6) pour rester lisible ; points d'un même niveau légèrement dispersés
  fin <- if ("rang_tension_fin" %in% names(d)) d$rang_tension_fin else d$rang_tension
  pos <- function(v) floor(v + 1e-9) + round((v - floor(v + 1e-9)) * 10) * 0.5
  d$y <- pos(fin) + ave(seq_len(nrow(d)), round(fin, 1), FUN = function(i) { k <- order(order(d$taux_depart_central_pct[i])); pmin(0.2, pmax(-0.2, (k - (length(i) + 1) / 2) * 0.1)) })
  d$quadrant <- factor(d$quadrant, levels = unname(QUADRANTS_TENSION)); d$confiance <- ifelse(d$faible_confiance, "faible", "bonne")
  d$etiquette <- ifelse(d$quadrant == QUADRANTS_TENSION[["critique"]], ifelse(is.na(d$pcs_libelle), d$pcs_source, d$pcs_libelle), NA_character_)
  pal <- setNames(unname(COULEURS_POINTS_SITUATIONS), unname(QUADRANTS_TENSION))
  xr <- mat$ref_x; ys <- mat$seuil_rang - 0.5; xmax <- max(d$taux_depart_central_pct, xr) * 1.08
  sn <- if (length(mat$sous_niveaux)) mat$rang_max + mat$sous_niveaux / 10 else numeric(0); sn <- sn[sn %in% round(fin, 1)]
  br_y <- c(seq_len(mat$rang_max), pos(sn))
  lib_y <- c("1 (la plus faible)", if (mat$rang_max > 2) as.character(2:(mat$rang_max - 1)), if (length(sn)) as.character(mat$rang_max) else paste0(mat$rang_max, " (la plus forte)"),
             if (length(sn)) paste0(format(sn, nsmall = 1), ifelse(sn == max(sn), " (la plus forte)", "")))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = taux_depart_central_pct, y = y)) +
    ggplot2::annotate("rect", xmin = xr, xmax = Inf, ymin = ys, ymax = Inf, fill = "#fde0dc", alpha = .6) +
    ggplot2::geom_vline(xintercept = xr, linetype = "42", colour = "#555555") + ggplot2::geom_hline(yintercept = ys, linetype = "42", colour = "#555555") +
    ggplot2::geom_point(ggplot2::aes(colour = quadrant, shape = confiance), size = 2.8, stroke = 1.1) +
    ggplot2::geom_text(ggplot2::aes(label = etiquette), size = 2.7, hjust = 0, nudge_x = xmax * 0.008, check_overlap = TRUE, colour = "#1A1A1A", na.rm = TRUE) +
    ggplot2::annotate("text", x = Inf, y = Inf, label = QUADRANTS_TENSION[["critique"]], hjust = 1.05, vjust = 1.5, fontface = "bold", colour = "#b2182b", size = 3.6) +
    ggplot2::annotate("text", x = Inf, y = -Inf, label = QUADRANTS_TENSION[["releve"]], hjust = 1.05, vjust = -1, fontface = "bold", colour = "#c4602c", size = 3.6) +
    ggplot2::annotate("text", x = -Inf, y = Inf, label = QUADRANTS_TENSION[["difficile"]], hjust = -0.05, vjust = 1.5, fontface = "bold", colour = "#2b7bb9", size = 3.6) +
    ggplot2::annotate("text", x = -Inf, y = -Inf, label = QUADRANTS_TENSION[["controle"]], hjust = -0.05, vjust = -1, fontface = "bold", colour = "#5b6470", size = 3.6) +
    ggplot2::scale_colour_manual(values = pal, drop = FALSE, name = NULL) +
    ggplot2::scale_shape_manual(values = c(bonne = 16, faible = 1), name = "Rattachement PCS → FAP", labels = c(bonne = "fiable", faible = "faible confiance")) +
    ggplot2::scale_y_continuous(breaks = br_y, labels = lib_y, limits = c(0.5, max(br_y) + 0.5)) +
    ggplot2::scale_x_continuous(limits = c(0, xmax), labels = function(x) paste0(format(x, decimal.mark = ","), " %")) +
    ggplot2::labs(title = texte_liste("Les métiers stratégiques : vont-ils partir, et se recrutent-ils facilement ?"),
                  subtitle = sprintf("Pointillés : départs moyens de l’ensemble des salariés concernés (%s %%) et seuil « en tension » (rang %d sur %d)",
                                     formatC(xr, format = "f", digits = 1, decimal.mark = ","), mat$seuil_rang, mat$rang_max),
                  x = "Part des salariés du métier susceptibles de partir d’ici 2030", y = "Tension du métier en France (rang)",
                  caption = texte_liste("Un point = un métier stratégique (PCS) diffusable. La tension est celle de sa famille professionnelle en France : elle ne mesure pas la tension d’un territoire.")) +
    ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(legend.position = "bottom", legend.box = "vertical", panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"))
  ggplot2::ggsave(fichier, g, width = 11, height = 7.5, dpi = 130, bg = "white")
  invisible(fichier)
}

# --- Matrice en SVG pour la fiche : sans script, libellés au survol (SPEC-TYPO-104) ----
# Même géométrie que le PNG (axe y ordinal, demi-crans 5.1 / 5.2, repères en pointillés,
# zone « départs rapides et métier en tension » teintée). AUCUN libellé de métier imprimé :
# chaque point porte un <title> natif, affiché par le navigateur au survol de la souris.
# Métiers DIFFUSABLES seulement ; rend une chaîne <svg> (vide s'il n'y a aucun point).
svg_matrice_metiers_tension <- function(mat, params = parametres_tension()) {
  e <- function(x) { x <- gsub("&", "&amp;", as.character(x), fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
  f0 <- function(x) ifelse(is.na(x), "n.d.", formatC(round(x), format = "d", big.mark = " ")); f1 <- function(x) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = 1, decimal.mark = ","))
  n <- function(x) formatC(x, format = "f", digits = 1)
  d <- mat$metiers[mat$metiers$statut == "diffusé", ]
  if (nrow(d) == 0) return("")
  fin <- if ("rang_tension_fin" %in% names(d)) d$rang_tension_fin else d$rang_tension
  pos <- function(v) floor(v + 1e-9) + round((v - floor(v + 1e-9)) * 10) * 0.5
  y <- pos(fin) + ave(seq_len(nrow(d)), round(fin, 1), FUN = function(i) { k <- order(order(d$taux_depart_central_pct[i])); pmin(0.2, pmax(-0.2, (k - (length(i) + 1) / 2) * 0.1)) })
  xr <- mat$ref_x; ys_seuil <- mat$seuil_rang - 0.5; xmax <- max(d$taux_depart_central_pct, xr) * 1.08
  sn <- if (length(mat$sous_niveaux)) mat$rang_max + mat$sous_niveaux / 10 else numeric(0); sn <- sn[sn %in% round(fin, 1)]
  br_y <- c(seq_len(mat$rang_max), pos(sn)); ymax <- max(br_y)
  lib_y <- c("1 (la plus faible)", if (mat$rang_max > 2) as.character(2:(mat$rang_max - 1)), if (length(sn)) as.character(mat$rang_max) else paste0(mat$rang_max, " (la plus forte)"),
             if (length(sn)) paste0(format(sn, nsmall = 1, decimal.mark = ","), ifelse(sn == max(sn), " (la plus forte)", "")))
  # géométrie (unités du viewBox ; largeur 100 % de la page)
  W <- 900; H <- 540; L <- 150; R <- 24; T <- 26; B <- 76; pw <- W - L - R; ph <- H - T - B
  sx <- function(v) L + pw * v / xmax; sy <- function(v) T + ph * ((ymax + 0.5) - v) / ymax
  br_x <- pretty(c(0, xmax)); br_x <- br_x[br_x <= xmax]
  cle <- names(QUADRANTS_TENSION)[match(d$quadrant, QUADRANTS_TENSION)]; coul <- unname(COULEURS_POINTS_SITUATIONS[cle])
  bulle <- sprintf("%s (PCS %s)\nFamille professionnelle : %s%s\nDéparts attendus d’ici 2030 : %s%s\nPart des salariés du métier susceptibles de partir : %s %% (France, tous métiers : %s %%)\nTension du métier en France : %s sur %d%s\nSituation : %s\nRattachement PCS → FAP : %s%s",
                   ifelse(is.na(d$pcs_libelle), paste0("PCS ", d$pcs_source), d$pcs_libelle), d$pcs_source, ifelse(is.na(d$fap_libelle), "n.d.", d$fap_libelle), ifelse(is.na(d$fap_code), "", paste0(" (", d$fap_code, ")")),
                   f0(d$departs_central), ifelse(is.na(d$departs_bas) | is.na(d$departs_haut), "", paste0(" (de ", f0(d$departs_bas), " à ", f0(d$departs_haut), ")")), f1(d$taux_depart_central_pct), f1(xr),
                   format(fin, decimal.mark = ","), mat$rang_max, ifelse(d$en_tension %in% TRUE, " : en tension", " : hors tension"), d$quadrant,
                   ifelse(d$faible_confiance, "à confirmer (faible confiance)", "fiable"),
                   ifelse(is.na(d$composante_libelle), "", paste0("\nCe qui pèse le plus : ", d$composante_libelle, " · levier : ", d$levier)))
  points <- sprintf('<circle cx="%s" cy="%s" r="7" fill="%s" stroke="%s" stroke-width="2"><title>%s</title></circle>',
                    n(sx(d$taux_depart_central_pct)), n(sy(y)), ifelse(d$faible_confiance, "#ffffff", coul), coul, e(bulle))
  c(sprintf('<svg class="mat" viewBox="0 0 %d %d" role="img" aria-label="Matrice des métiers stratégiques : part susceptible de partir et tension en France ; survolez un point pour lire le métier">', W, H),
    sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="#fde0dc" opacity="0.6"/>', n(sx(xr)), n(T), n(sx(xmax) - sx(xr)), n(sy(ys_seuil) - T)),
    paste(sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#e5e9ee" stroke-width="1"/>', n(L), n(sy(br_y)), n(sx(xmax)), n(sy(br_y))), collapse = ""),
    paste(sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#e5e9ee" stroke-width="1"/>', n(sx(br_x)), n(T), n(sx(br_x)), n(T + ph)), collapse = ""),
    paste(sprintf('<text x="%s" y="%s" font-size="13" fill="#52606D" text-anchor="middle">%s %%</text>', n(sx(br_x)), n(T + ph + 20), format(br_x, decimal.mark = ",")), collapse = ""),
    paste(sprintf('<text x="%s" y="%s" font-size="13" fill="#52606D" text-anchor="end" dominant-baseline="middle">%s</text>', n(L - 8), n(sy(br_y)), e(lib_y)), collapse = ""),
    sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#555555" stroke-width="1.5" stroke-dasharray="6 4"/>', n(sx(xr)), n(T), n(sx(xr)), n(T + ph)),
    sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#555555" stroke-width="1.5" stroke-dasharray="6 4"/>', n(L), n(sy(ys_seuil)), n(sx(xmax)), n(sy(ys_seuil))),
    sprintf('<text x="%s" y="%s" font-size="12" fill="#555555">France : %s %%</text>', n(sx(xr) + 5), n(T + 14), f1(xr)),
    sprintf('<text x="%s" y="%s" font-size="12" fill="#555555" text-anchor="end">seuil « en tension » : rang %d</text>', n(sx(xmax) - 4), n(sy(ys_seuil) - 6), mat$seuil_rang),
    sprintf('<text x="%s" y="%s" font-size="14" fill="#1F2933" text-anchor="middle">Part des salariés du métier susceptibles de partir d’ici 2030</text>', n(L + pw / 2), n(H - 30)),
    sprintf('<text x="18" y="%s" font-size="14" fill="#1F2933" text-anchor="middle" transform="rotate(-90 18 %s)">Tension du métier en France (rang)</text>', n(T + ph / 2), n(T + ph / 2)),
    paste(points, collapse = ""), "</svg>")
}

# --- Tableau des métiers stratégiques, groupés par situation (SPEC-TYPO-105, sans script) ----
# Une ligne par métier DIFFUSABLE ; les autres (secret, tension inconnue, absents) ne sont que des comptes.
html_tableau_metiers_tension <- function(mat, params = parametres_tension()) {
  e <- function(x) { x <- gsub("&", "&amp;", as.character(x), fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
  f0 <- function(x) ifelse(is.na(x), "n.d.", formatC(round(x), format = "d", big.mark = " ")); f1 <- function(x) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = 1, decimal.mark = ","))
  m <- mat$metiers; d <- m[m$statut == "diffusé", ]
  reglette <- function(ent, fin) sprintf('<span class="reg" aria-label="tension %s sur %d">%s</span><span class="regv">%s</span>', format(fin, decimal.mark = ","), mat$rang_max,
                                         paste(ifelse(seq_len(mat$rang_max) <= ent, '<i class="on"></i>', "<i></i>"), collapse = ""), format(fin, decimal.mark = ","))
  entete <- sprintf('<thead><tr><th>Métier (PCS)</th><th>Famille professionnelle</th><th class="num">Départs d’ici 2030</th><th class="num">Part susceptible de partir<br><small>France : %s %%</small></th><th>Tension en France<br><small>1 = la plus faible, %d = la plus forte</small></th><th>Ce qui pèse le plus</th><th>Levier</th><th>Rattachement</th></tr></thead>',
                    f1(mat$ref_x), mat$rang_max)
  corps <- unlist(lapply(names(QUADRANTS_TENSION), function(k) {
    g <- d[d$quadrant %in% QUADRANTS_TENSION[[k]], ]; g <- g[order(-g$departs_central, g$pcs), ]
    c(sprintf('<tr class="sit sit-%s"><th colspan="8" style="background:%s">%s <span class="n">%d métier%s</span></th></tr>', k, COULEURS_SITUATIONS[[k]], e(QUADRANTS_TENSION[[k]]), nrow(g), if (nrow(g) > 1) "s" else ""),
      if (nrow(g) == 0) '<tr><td colspan="8" class="vide">Aucun métier dans cette situation parmi les métiers stratégiques suivis et diffusables.</td></tr>' else
        sprintf('<tr><td><b>%s</b><br><small>PCS %s</small></td><td>%s<br><small>%s</small></td><td class="num">%s<br><small>%s à %s</small></td><td class="num">%s %%</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
                e(ifelse(is.na(g$pcs_libelle), paste0("PCS ", g$pcs_source), g$pcs_libelle)), e(g$pcs_source), e(ifelse(is.na(g$fap_libelle), "", g$fap_libelle)), e(ifelse(is.na(g$fap_code), "", g$fap_code)),
                f0(g$departs_central), f0(g$departs_bas), f0(g$departs_haut), f1(g$taux_depart_central_pct),
                mapply(reglette, g$rang_tension, if ("rang_tension_fin" %in% names(g)) g$rang_tension_fin else g$rang_tension),
                e(ifelse(is.na(g$composante_libelle), "aucune composante au-dessus du seuil", g$composante_libelle)), e(ifelse(is.na(g$levier), "—", g$levier)),
                ifelse(g$faible_confiance, '<span class="conf">à confirmer</span>', "fiable"))) }))
  autres <- table(factor(m$statut[m$statut != "diffusé"], levels = c("non diffusé (secret statistique)", "tension inconnue", texte_perimetre("absente de la Base"))))
  pied <- if (sum(autres) > 0) sprintf('<p class="note">Hors tableau : %s.</p>', paste(sprintf("%d métier%s %s", autres, ifelse(autres > 1, "s", ""), c("non diffusé(s) (secret statistique)", "de tension inconnue (rang manquant)", texte_perimetre("sans salarié du champ dans la Base")))[autres > 0], collapse = " ; ")) else ""
  c('<table class="tab-met">', entete, "<tbody>", corps, "</tbody></table>", pied)
}

# --- Fiche d'une page, sans script (SPEC-TYPO-105) --------------------------------------
# `png` : conservé pour la compatibilité des appels ; le PNG reste un fichier à part et n'est
# plus embarqué (illisible) : l'annexe repliée porte la matrice SVG, libellés au survol.
generer_fiche_exposition_tension <- function(messages, fichier, png = NULL, champ = construire_libelle_champ(), params = parametres_tension(),
                                             bornes = c(bas = NA, haut = NA), n_forte = NA_integer_, source_note = "calculs propres", mat = NULL) {
  e <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
  f1 <- function(x) if (is.na(x)) "n.d." else formatC(x, format = "f", digits = 1, decimal.mark = ",")
  svg <- if (is.null(mat)) "" else svg_matrice_metiers_tension(mat, params)
  annexe <- if (!nzchar(svg[1])) "" else
    c('<details class="annexe"><summary>Version graphique (annexe)</summary>',
      '<p class="note">Un point = un métier stratégique diffusable. Aucun nom n’est imprimé pour que le graphique reste lisible : <b>survolez un point avec la souris</b> pour lire le métier, ses départs, sa part susceptible de partir, sa tension et sa situation.</p>',
      svg,
      '<ul class="leg-mat">', sprintf('<li><i style="background:%s;border-color:%s"></i>%s</li>', COULEURS_POINTS_SITUATIONS, COULEURS_POINTS_SITUATIONS, e(QUADRANTS_TENSION[names(COULEURS_POINTS_SITUATIONS)])),
      '<li><i style="background:#fff;border-color:#52606D"></i>point creux : rattachement PCS → FAP à confirmer</li></ul>',
      '<p class="note">Pointillés : part susceptible de partir de l’ensemble des salariés concernés en France (verticale) et seuil « en tension » (horizontale) ; la zone teintée est celle des départs rapides et des métiers en tension. La tension est celle de la famille professionnelle en France : elle ne mesure pas la tension d’un territoire.</p></details>')
  tableau <- if (is.null(mat)) '<p class="note">Tableau des métiers non disponible.</p>' else html_tableau_metiers_tension(mat, params)
  n_suivis <- params$n_suivis; n_presents <- if (is.null(mat)) NA_integer_ else sum(mat$metiers$statut != texte_perimetre("absente de la Base"))
  univers <- if (is.na(n_suivis)) "métiers stratégiques" else if (is.na(n_presents)) sprintf("%d métiers stratégiques suivis", n_suivis) else sprintf(texte_perimetre("%d métiers stratégiques suivis, dont %d présents dans la Base"), n_suivis, n_presents)
  liste <- if (is.na(n_suivis)) "les métiers stratégiques de la liste" else sprintf("les %d métiers stratégiques de la liste", n_suivis)
  css <- paste('body{margin:0;background:#fff;color:#1F2933;font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;font-size:16px;line-height:1.5}',
               '.page{max-width:1100px;margin:0 auto;padding:28px 32px 40px}h1{font-size:28px;margin:0 0 4px}.sous{color:#52606D;margin:0 0 18px}',
               '.msgs{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px;margin:0 0 18px}@media(max-width:860px){.msgs{grid-template-columns:1fr}}',
               '.msg{border:1px solid #DDE3EA;border-left:5px solid #1D4E89;border-radius:10px;padding:12px 14px;background:#F5F7FA}.msg b{display:block;font-size:13px;letter-spacing:.06em;text-transform:uppercase;color:#1D4E89;margin-bottom:4px}',
               'img{width:100%;height:auto;border:1px solid #DDE3EA;border-radius:8px}h2{font-size:18px;margin:20px 0 6px}ul{margin:4px 0 0;padding-left:20px}',
               '.cadre{border:1px solid #e0b96a;border-left-width:5px;background:#fff8e6;border-radius:8px;padding:10px 14px;margin-top:16px}.note{font-size:13.5px;color:#52606D}',
               '.tab-met{width:100%;border-collapse:collapse;font-size:14px;margin:6px 0 4px}.tab-met th,.tab-met td{text-align:left;vertical-align:top;padding:7px 8px;border-top:1px solid #DDE3EA;line-height:1.4}',
               '.tab-met thead th{font-size:12px;letter-spacing:.04em;text-transform:uppercase;color:#52606D;border-top:0;border-bottom:2px solid #c9ced6}.tab-met thead small,.tab-met td small{display:block;font-size:12px;color:#52606D;font-weight:400;text-transform:none;letter-spacing:0}',
               '.tab-met td:first-child{min-width:190px}.tab-met td:nth-child(2){min-width:120px}.tab-met .num{text-align:right;white-space:nowrap}.tab-met tbody tr:nth-child(even):not(.sit) td{background:#fafbfc}.tab-met tr.sit th{font-size:14px;font-weight:700;color:#1F2933;padding:9px 8px;border-top:2px solid #fff}.tab-met tr.sit .n{font-weight:500;color:#52606D;margin-left:8px}.tab-met td.vide{color:#52606D;font-style:italic}',
               '.reg{display:inline-flex;gap:2px;vertical-align:middle;margin-right:6px}.reg i{display:inline-block;width:10px;height:14px;border:1px solid #9aa5b1;border-radius:2px;background:#fff}.reg i.on{background:#1D4E89;border-color:#1D4E89}.regv{font-weight:700}.conf{color:#8a5a00;background:#fff3d6;padding:1px 6px;border-radius:999px;font-size:12.5px}',
               'details.annexe{margin-top:18px;border:1px solid #DDE3EA;border-radius:8px;padding:0 14px 6px}details.annexe summary{cursor:pointer;font-weight:600;color:#1D4E89;padding:10px 0}',
               'svg.mat{display:block;width:100%;height:auto;max-height:640px;margin:6px 0 4px;border:1px solid #DDE3EA;border-radius:8px;background:#fff}svg.mat circle{cursor:help}svg.mat circle:hover{stroke-width:4;r:9}',
               '.leg-mat{list-style:none;display:flex;flex-wrap:wrap;gap:6px 18px;padding:0;margin:6px 0 8px;font-size:13.5px}.leg-mat i{display:inline-block;width:12px;height:12px;border-radius:50%;border:2px solid;vertical-align:-2px;margin-right:6px}',
               '@media(max-width:700px){.tab-met th:nth-child(2),.tab-met td:nth-child(2),.tab-met th:nth-child(7),.tab-met td:nth-child(7),.tab-met th:nth-child(8),.tab-met td:nth-child(8){display:none}.tab-met tr.sit th{display:table-cell}.page{padding:18px 14px 30px}}', sep = "\n")
  titres <- TITRES_MESSAGES_TENSION
  html <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
            '<title>Départs attendus et difficultés de recrutement</title><style>', css, '</style></head><body><div class="page">',
            '<h1>Départs attendus et difficultés de recrutement</h1>',
            sprintf(texte_perimetre('<p class="sous">%s · entreprises du périmètre Base · départs attendus d’ici 2030 · %s</p>'), e(construire_libelles_public(champ)$salaries_age), univers),
            '<div class="msgs">', sprintf('<div class="msg"><b>%s</b>%s</div>', titres, e(messages)), '</div>',
            '<h2>Les métiers stratégiques, situation par situation</h2>',
            sprintf('<p class="note">Un métier est « à départs rapides » quand la part de ses salariés susceptibles de partir d’ici 2030 atteint celle de l’ensemble des salariés concernés en France (%s %%) ; il est « en tension » quand sa famille professionnelle atteint le rang %d sur %d en France.</p>', f1(if (is.null(mat)) NA else mat$ref_x), params$seuil_rang, params$rang_max),
            tableau,
            '<h2>Comment lire</h2><ul>',
            '<li><b>Ce que dit la carte des profils</b> : sa couleur indique combien de salariés vont partir d’ici 2030, et à quel rythme.</li>',
            sprintf('<li><b>Ce qu’ajoute la part des remplacements difficiles</b> : sur 100 salariés à remplacer dans les métiers stratégiques, combien partent de métiers en tension en France (rang %d ou plus sur %d) ; un département hachuré a une part forte.</li>', params$seuil_rang, params$rang_max),
            '<li><b>Le croisement des deux</b> : un département à renouvellement rapide <i>et</i> à forte part de remplacements difficiles cumule les deux difficultés ; le tableau dit quels métiers sont concernés, et le levier indique la nature de la tension.</li>',
            '<li><b>Le tableau</b> : chaque métier est classé selon ses départs (rapides ou modérés) et la tension de sa famille professionnelle en France ; la réglette donne le rang de tension, « à confirmer » signale un rattachement PCS → famille professionnelle de faible confiance.</li></ul>',
            sprintf('<div class="cadre"><b>Ce que cette page ne dit pas.</b> La tension n’est connue que pour %s : un zéro ou une situation vide veut dire « aucun parmi les métiers suivis », pas « aucun » ; d’autres métiers peuvent être en tension sans que nous le sachions. Ce n’est pas une prédiction de pénurie. La tension est mesurée pour le métier en France entière, pas dans le département : une forte part de remplacements difficiles veut dire que les départs du département portent sur des métiers difficiles à recruter, pas que le département manque de candidats.</div>', liste),
            '<h2>Méthode en bref</h2><ul>',
            '<li>Part des remplacements difficiles = départs attendus des métiers stratégiques en tension ÷ départs attendus des métiers stratégiques (estimation centrale) ; repère : la même part en France.</li>',
            sprintf('<li>Classes : faible sous %s %%, forte à partir de %s %% (%s) ; %s département(s) à part forte. Non interprétée si les métiers stratégiques portent moins de %s %% des départs du département.</li>',
                    f1(bornes[["bas"]]), f1(bornes[["haut"]]), if (identical(params$methode, "fixes")) "seuils fixes" else "terciles, règle provisoire", if (is.na(n_forte)) "n.d." else n_forte, f1(params$couverture_min)),
            '<li>Robustesse : la classe est recalculée avec les hypothèses de départ basse et haute, un seuil de tension un cran plus strict et un cran plus large, et sans les rattachements PCS → FAP de faible confiance ; « robuste » si elle ne change jamais.</li>',
            '<li>Secret statistique : un département n’est montré que si ses parts respectent la règle Insee et ne permettent de retrouver aucune valeur masquée ; seuls des pourcentages et des classes sont diffusés.</li></ul>',
            sprintf('<p class="note">Carte associée : <a href="../cartes/carte_typologie.html">carte des profils</a>  (hachures = forte part de remplacements difficiles) ; <a href="fiche_metiers_liste.html">métiers de la liste, territoire par territoire</a>. Sources : %s ; tension par famille professionnelle (FAP) en France, rattachée aux PCS stratégiques.</p>', e(source_note)),
            annexe, '</div></body></html>')
  if (any(grepl("<script", html, fixed = TRUE)) || any(grepl(" on[a-z]+=", html)) || any(grepl("javascript:", html, fixed = TRUE)))
    stop("generer_fiche_exposition_tension : contenu actif détecté.")
  dir.create(dirname(fichier), showWarnings = FALSE, recursive = TRUE)
  writeLines(enc2utf8(texte_liste(html)), fichier, useBytes = TRUE)   # libellé public de la liste (TENSION_LIBELLE_LISTE)
  invisible(fichier)
}

# --- Classeur Excel pour les commanditaires (SPEC-TYPO-108) ------------------------------
# Quatre feuilles (Lisez-moi, Départements, Métiers, Paramètres) construites à partir des
# SEULES tables de diffusion (secret déjà appliqué) : aucun calcul nouveau, aucun effectif.
# tables_classeur_tension() rend des data.frames aux colonnes en clair ; ecrire_classeur_tension()
# les écrit (openxlsx avec mise en forme, sinon writexl sans mise en forme, sinon message).
tables_classeur_tension <- function(dif, mat, messages, parametres, params = parametres_tension(), bornes = c(bas = NA, haut = NA), n_forte = NA_integer_,
                                    champ = construire_libelle_champ(), source_note = "calculs propres", date = Sys.Date()) {
  if (any(grepl("^departs_|^effectif_|n_entreprises|part_dominante", names(dif)))) stop("tables_classeur_tension : effectif ou indicateur de secret dans la table des départements.")
  oui_non <- function(x) ifelse(is.na(x), NA_character_, ifelse(x, "oui", "non"))
  f1 <- function(x) if (is.na(x)) "n.d." else formatC(x, format = "f", digits = 1, decimal.mark = ",")
  # Départements : une ligne par département ; non diffusé = aucune valeur
  dep <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
    Code = as.character(dif$geo_code), `Département` = dif$geo_nom, `Diffusé` = ifelse(dif$tension_diffusee, "oui", "non diffusé (secret statistique)"),
    `Part des remplacements difficiles (%)` = arrondir_taux(dif$tension_exposition_pct), `Repère France (%)` = arrondir_taux(dif$tension_exposition_france_pct), `Couverture (%)` = arrondir_taux(dif$tension_couverture_pct),
    Classe = ifelse(dif$tension_diffusee & is.na(dif$tension_classe) & !is.na(dif$tension_couverture_pct), "non interprétée (couverture insuffisante)", dif$tension_classe),
    Robustesse = dif$tension_robustesse, `Sensible à` = dif$tension_causes, Levier = dif$tension_levier, Composante = dif$tension_composante,
    `Métiers en tension les plus concernés` = dif$tension_metiers, `Renouvellement rapide et forte part de remplacements difficiles` = oui_non(dif$tension_cumul),
    `Départs dans les métiers suivis en tension (au moins)` = if ("tension_departs_min" %in% names(dif)) arrondir_nombre_personnes(dif$tension_departs_min) else NA_real_,
    `Part de ces départs dans tous les départs (%, au moins)` = if ("tension_part_departs_min_pct" %in% names(dif)) arrondir_taux(dif$tension_part_departs_min_pct) else NA_real_, Texte = dif$tension_texte)
  if (any(!is.na(as.matrix(dep[!dif$tension_diffusee, -(1:3), drop = FALSE])))) stop("tables_classeur_tension : valeur présente pour un département non diffusé.")
  dep <- dep[order(dep$Code), , drop = FALSE]; rownames(dep) <- NULL
  # Métiers : une ligne par PCS stratégique (niveau France), situations dans l'ordre, départs décroissants
  m <- mat$metiers; fin <- if ("rang_tension_fin" %in% names(m)) m$rang_tension_fin else m$rang_tension
  met <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
    Situation = ifelse(m$statut == "diffusé", m$quadrant, NA_character_), `Métier` = ifelse(is.na(m$pcs_libelle), paste0("PCS ", m$pcs_source), m$pcs_libelle), `Code PCS` = m$pcs_source,
    `Famille professionnelle` = m$fap_libelle, `Code FAP` = m$fap_code,
    `Départs d’ici 2030` = arrondir_nombre_personnes(m$departs_central), `Fourchette basse` = arrondir_nombre_personnes(m$departs_bas), `Fourchette haute` = arrondir_nombre_personnes(m$departs_haut),
    `Part susceptible de partir (%)` = arrondir_taux(m$taux_depart_central_pct), `Repère France (%)` = ifelse(m$statut == "diffusé", arrondir_taux(mat$ref_x), NA_real_),
    `Tension (rang)` = m$rang_tension, `Tension (valeur fine)` = fin, `En tension` = oui_non(m$en_tension),
    `Composante qui pèse le plus` = m$composante_libelle, Levier = m$levier, `Rattachement PCS → FAP` = ifelse(m$faible_confiance, "à confirmer", "fiable"), Statut = m$statut)
  if (any(!is.na(as.matrix(met[met$Statut != "diffusé", c("Départs d’ici 2030", "Fourchette basse", "Fourchette haute", "Part susceptible de partir (%)"), drop = FALSE]))))
    stop("tables_classeur_tension : départ présent pour un métier non diffusé.")
  met <- met[order(match(met$Situation, unname(QUADRANTS_TENSION), nomatch = length(QUADRANTS_TENSION) + 1), -ifelse(is.na(m$departs_central), -1, m$departs_central), met$`Code PCS`), , drop = FALSE]
  rownames(met) <- NULL
  # Lisez-moi : le texte de la fiche, en cellules (une colonne) ; attr "gras" = titres
  titre <- function(x) structure(x, gras = TRUE)
  lignes <- list(titre("Départs attendus et difficultés de recrutement"),
    paste0(construire_libelles_public(champ)$salaries_age, texte_perimetre(" · entreprises du périmètre Base · départs attendus d’ici 2030 · métiers stratégiques")),
    paste0("Classeur produit le ", format(date, "%d/%m/%Y"), ". Sources : ", source_note, " ; tension par famille professionnelle (FAP) en France, rattachée aux PCS stratégiques."), "",
    titre("Trois messages"), paste0(TITRES_MESSAGES_TENSION, " : ", messages), "",
    titre("Comment lire"),
    "Ce que dit la carte des profils : sa couleur indique combien de salariés vont partir d’ici 2030, et à quel rythme.",
    sprintf("Ce qu’ajoute la part des remplacements difficiles : sur 100 salariés à remplacer dans les métiers stratégiques, combien partent de métiers en tension en France (rang %d ou plus sur %d) ; un département hachuré sur la carte a une part forte.", params$seuil_rang, params$rang_max),
    "Le croisement des deux : un département à renouvellement rapide et à forte part de remplacements difficiles cumule les deux difficultés ; la feuille Métiers dit quels métiers sont concernés, et le levier indique la nature de la tension.",
    sprintf("Un métier est « à départs rapides » quand la part de ses salariés susceptibles de partir d’ici 2030 atteint celle de l’ensemble des salariés concernés en France (%s %%) ; il est « en tension » quand sa famille professionnelle atteint le rang %d sur %d en France.", f1(mat$ref_x), params$seuil_rang, params$rang_max), "",
    titre("Ce que ce classeur ne dit pas"),
    sprintf("La tension n’est connue que pour les %d métiers stratégiques de la liste : un zéro ou une situation vide veut dire « aucun parmi les métiers suivis », pas « aucun » ; d’autres métiers peuvent être en tension sans que nous le sachions.", params$n_suivis),
    "Ce n’est pas une prédiction de pénurie. La tension est mesurée pour le métier en France entière, pas dans le département : une forte part de remplacements difficiles veut dire que les départs du département portent sur des métiers difficiles à recruter, pas que le département manque de candidats.", "",
    titre("Méthode en bref"),
    "Part des remplacements difficiles = départs attendus des métiers stratégiques en tension ÷ départs attendus des métiers stratégiques (estimation centrale) ; repère : la même part en France.",
    sprintf("Classes : faible sous %s %%, forte à partir de %s %% (%s) ; %s département(s) à part forte. Non interprétée si les métiers stratégiques portent moins de %s %% des départs du département.",
            f1(bornes[["bas"]]), f1(bornes[["haut"]]), if (identical(params$methode, "fixes")) "seuils fixes" else "terciles, règle provisoire", if (is.na(n_forte)) "n.d." else n_forte, f1(params$couverture_min)),
    "Robustesse : la classe est recalculée avec les hypothèses de départ basse et haute, un seuil de tension un cran plus strict et un cran plus large, et sans les rattachements PCS → FAP de faible confiance ; « robuste » si elle ne change jamais.",
    "Secret statistique : un département n’est montré que si ses parts respectent la règle Insee et ne permettent de retrouver aucune valeur masquée ; seuls des pourcentages, des classes et des textes sont diffusés. Une cellule vide est une valeur non diffusée, jamais un zéro.", "",
    titre("Les feuilles"),
    "Départements : une ligne par département. Part des remplacements difficiles = sur 100 départs attendus des métiers stratégiques, part concernant des métiers en tension en France ; Repère France = la même part en France ; Couverture = part des départs du département portés par les métiers stratégiques ; Classe (faible, modérée, forte) ; Robustesse et Sensible à (variantes qui changent la classe) ; Levier et Composante (nature de la tension la plus fréquente) ; Métiers en tension les plus concernés (cellules diffusables) ; Renouvellement rapide et forte part de remplacements difficiles ; Départs dans les métiers suivis en tension et leur part dans tous les départs du département (minimums : les métiers non suivis ne sont pas comptés) ; Texte.",
    sprintf(texte_perimetre("Métiers : une ligne par PCS stratégique de la liste, au niveau France. Situation ; départs attendus d’ici 2030 et fourchette (estimation centrale, basse, haute) ; part susceptible de partir et repère France ; tension en France (rang de 1 à %d, valeur fine%s) ; composante qui pèse le plus ; levier ; rattachement PCS → FAP (fiable ou à confirmer) ; Statut (diffusé, non diffusé pour secret statistique, tension inconnue, absente de la Base)."),
            params$rang_max, if (length(params$sous_niveaux)) paste0(" : ", paste(params$rang_max + params$sous_niveaux / 10, collapse = " et "), " affinent le niveau le plus fort") else ""),
    "Paramètres : seuils et bornes effectifs de ce calcul.")
  gras <- vapply(lignes, function(x) isTRUE(attr(x, "gras")), TRUE); gras <- rep(gras, lengths(lignes)); lignes <- unlist(lapply(lignes, as.character))
  lisez <- data.frame(texte = lignes, stringsAsFactors = FALSE); attr(lisez, "gras") <- gras
  tables_texte_liste(list("Lisez-moi" = lisez, "Départements" = dep, "Métiers" = met, "Paramètres" = as.data.frame(parametres, stringsAsFactors = FALSE)))
}
# libellé public de la liste dans un jeu de tables de classeur : noms de colonnes et colonnes texte (attributs conservés)
tables_texte_liste <- function(tables) lapply(tables, function(t) {
  g <- attr(t, "gras"); names(t) <- texte_liste(names(t))
  for (v in names(t)) if (is.character(t[[v]])) t[[v]] <- texte_liste(t[[v]])
  if (!is.null(g)) attr(t, "gras") <- g; t
})

# Écrit le classeur ; rend le chemin (invisible) ou NA_character_ si aucun paquet ne le permet.
# moteur : "auto" (openxlsx s'il est installé, sinon writexl), "openxlsx" ou "writexl" (tests).
# L'appelant (08i) entoure l'appel d'un tryCatch : un classeur n'arrête jamais la chaîne.
ecrire_classeur_tension <- function(tables, fichier, moteur = c("auto", "openxlsx", "writexl")) {
  moteur <- match.arg(moteur)
  dir.create(dirname(fichier), showWarnings = FALSE, recursive = TRUE)
  if (moteur != "writexl" && requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    st_ent <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#1D4E89", textDecoration = "bold", valign = "center", wrapText = TRUE)
    st_titre <- openxlsx::createStyle(fontSize = 16, textDecoration = "bold", fontColour = "#1D4E89"); st_gras <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold")
    st_wrap <- openxlsx::createStyle(wrapText = TRUE, valign = "top"); st_pct <- openxlsx::createStyle(numFmt = "0.0"); st_pers <- openxlsx::createStyle(numFmt = "#,##0")
    largeur <- function(t) pmin(60, pmax(10, vapply(seq_along(t), function(j) max(nchar(c(names(t)[j], as.character(t[[j]]))), na.rm = TRUE) * 0.9 + 2, 0)))
    for (nom in names(tables)) {
      t <- tables[[nom]]; openxlsx::addWorksheet(wb, nom, gridLines = nom != "Lisez-moi")
      if (nom == "Lisez-moi") {
        openxlsx::writeData(wb, nom, t, colNames = FALSE); openxlsx::setColWidths(wb, nom, 1, 125)
        openxlsx::addStyle(wb, nom, st_wrap, rows = seq_len(nrow(t)), cols = 1)
        g <- which(attr(t, "gras") %in% TRUE); if (length(g)) openxlsx::addStyle(wb, nom, st_gras, rows = g, cols = 1, stack = TRUE)
        openxlsx::addStyle(wb, nom, st_titre, rows = 1, cols = 1, stack = TRUE)
      } else {
        openxlsx::writeData(wb, nom, t, headerStyle = st_ent, withFilter = nrow(t) > 0)
        openxlsx::freezePane(wb, nom, firstRow = TRUE); openxlsx::setRowHeights(wb, nom, 1, 34)
        l <- largeur(t); l[names(t) == "Texte"] <- 90; openxlsx::setColWidths(wb, nom, seq_along(t), l)
        if (nrow(t) > 0) {
          lignes <- 1 + seq_len(nrow(t))
          j <- grep("\\(%\\)$", names(t)); if (length(j)) openxlsx::addStyle(wb, nom, st_pct, rows = lignes, cols = j, gridExpand = TRUE, stack = TRUE)
          j <- grep("^Départs d’ici|^Fourchette", names(t)); if (length(j)) openxlsx::addStyle(wb, nom, st_pers, rows = lignes, cols = j, gridExpand = TRUE, stack = TRUE)
          teinte <- if (nom == "Départements") unname(COULEURS_CLASSES_EXPOSITION[t$Classe]) else if (nom == "Métiers") unname(COULEURS_SITUATIONS[names(QUADRANTS_TENSION)[match(t$Situation, QUADRANTS_TENSION)]]) else NULL
          for (coul in unique(teinte[!is.na(teinte)])) openxlsx::addStyle(wb, nom, openxlsx::createStyle(fgFill = coul), rows = lignes[teinte %in% coul], cols = seq_along(t), gridExpand = TRUE, stack = TRUE)
          j <- which(names(t) == "Texte"); if (length(j)) openxlsx::addStyle(wb, nom, st_wrap, rows = lignes, cols = j, gridExpand = TRUE, stack = TRUE)
        }
      }
    }
    openxlsx::saveWorkbook(wb, fichier, overwrite = TRUE)
  } else if (moteur != "openxlsx" && requireNamespace("writexl", quietly = TRUE)) {
    message("ecrire_classeur_tension : classeur écrit sans mise en forme (writexl", if (moteur == "auto") ", paquet openxlsx absent" else "", ").")
    writexl::write_xlsx(lapply(tables, function(t) { attr(t, "gras") <- NULL; as.data.frame(t, stringsAsFactors = FALSE) }), fichier)
  } else {
    message("ecrire_classeur_tension : ni openxlsx ni writexl : classeur non produit (les CSV de diffusion restent la référence).")
    return(invisible(NA_character_))
  }
  invisible(fichier)
}
