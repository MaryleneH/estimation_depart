# ==============================================================================
# 00h_fonctions_typologie.R — Typologie nationale des territoires de la BITD
#                             (fonctions pures, maille DÉPARTEMENT × grande CS)
# ------------------------------------------------------------------------------
# RÔLE : couche d'ANALYSE EN AVAL des départs par grande CS (08d). Aucun départ
#        n'est recalculé ici : les mesures (effectif_champ, departs_central /
#        bas / haut, taux_depart_central_pct) viennent de calculer_departs()
#        (00e) ; le stock tous âges vient du 01c. Spécification : specs/10.
# RÈGLES : typologie à règles métier lisibles (aucun score composite, aucune
#        pondération cachée, aucun clustering) ; tous les seuils viennent de
#        la configuration (TYPO_*, 00) et sont écrits dans la sortie
#        parametres_typologie.csv ; chaque département porte une justification
#        textuelle déterministe construite par règles.
# MAILLE : département uniquement (SPEC-TYPO-001). Les tensions DGA / France
#        Travail, observées par FAP × bassin, sont une couche SÉPARÉE
#        (SPEC-TYPO-020 à 023) : jamais attribuées au département, jamais
#        alimentées par les départs départementaux, jamais de FAP -> grande CS.
# ==============================================================================
library(dplyr)

PROFILS_TYPOLOGIE <- c(enjeu = "Fort enjeu de renouvellement", emergent = "Risque de renouvellement émergent",
                       concentre = "Renouvellement concentré", stable = "Pôle BITD relativement stable",
                       diffus = "Implantation BITD diffuse", expertiser = "Cas à expertiser")
CLASSES_TYPOLOGIE <- list(poids = c("Faible", "Moyen", "Fort"), intensite = c("Faible", "Modérée", "Élevée"),
                          volume = c("Faible", "Modéré", "Élevé"))

# --- Paramètres (source unique : 00_config.R) -----------------------------------
parametres_typologie <- function() list(
  seuil_dominance_cs        = TYPO_SEUIL_DOMINANCE_CS,
  seuil_concentration       = TYPO_SEUIL_CONCENTRATION_DEPARTS,
  effectif_min              = TYPO_SEUIL_EFFECTIF_MIN,
  methode_classes           = TYPO_METHODE_CLASSES,
  poids_faible              = TYPO_SEUIL_POIDS_FAIBLE,
  poids_fort                = TYPO_SEUIL_POIDS_FORT,
  intensite_faible          = TYPO_SEUIL_INTENSITE_FAIBLE,
  intensite_elevee          = TYPO_SEUIL_RENOUVELLEMENT_ELEVE,
  volume_faible             = TYPO_SEUIL_VOLUME_FAIBLE,
  volume_eleve              = TYPO_SEUIL_VOLUME_ELEVE,
  marge_frontiere_pct       = TYPO_MARGE_FRONTIERE_PCT,
  nb_frontiere_expertise    = TYPO_NB_CRITERES_FRONTIERE,
  facteur_intensite_extreme = TYPO_FACTEUR_INTENSITE_EXTREME,
  indicateur_intensite      = TYPO_INDICATEUR_INTENSITE)

# --- Seuils d'une classe à trois niveaux ---------------------------------------
# methode "terciles" = RÈGLE DE CLASSEMENT STATISTIQUE PROVISOIRE : bornes =
# terciles observés sur les départements retenus (effectif >= effectif_min) ;
# methode "fixes" = seuils métier de la configuration. Les bornes effectives
# sont toujours retournées, pour être écrites dans parametres_typologie.csv.
seuils_classes <- function(x, methode = c("terciles", "fixes"), fixes = c(NA, NA)) {
  methode <- match.arg(methode)
  if (methode == "fixes") { if (anyNA(fixes)) stop("seuils_classes : seuils fixes manquants."); return(c(bas = fixes[1], haut = fixes[2])) }
  x <- x[is.finite(x)]
  if (length(x) < 3) return(c(bas = NA_real_, haut = NA_real_))
  q <- unname(quantile(x, probs = c(1 / 3, 2 / 3), type = 7, names = FALSE))
  c(bas = q[1], haut = q[2])
}
# Classe : < bas -> 1er libellé ; [bas, haut[ -> 2e ; >= haut -> 3e (borne INCLUSE dans la classe supérieure).
classer_par_seuils <- function(x, seuils, libelles) {
  out <- ifelse(is.na(x) | is.na(seuils[["bas"]]), NA_character_,
                ifelse(x < seuils[["bas"]], libelles[1], ifelse(x < seuils[["haut"]], libelles[2], libelles[3])))
  unname(out)
}
# Poids BITD : classe sur la part nationale, mais un effectif sous le plancher absolu est toujours « Faible ».
classer_poids_bitd <- function(part_nat_pct, effectif, seuils, effectif_min) {
  cl <- classer_par_seuils(part_nat_pct, seuils, CLASSES_TYPOLOGIE$poids)
  ifelse(!is.na(effectif) & effectif < effectif_min, CLASSES_TYPOLOGIE$poids[1], cl)
}

# --- Structure de l'emploi par grande CS ---------------------------------------
# cs_dep : département x cs1 avec effectif de base (colonne `effectif`) ; retourne part_pct par cellule.
calculer_structure_cs <- function(cs_dep, effectif = "effectif") {
  x <- cs_dep[[effectif]]
  tot <- ave(x, cs_dep$geo_code, FUN = function(v) sum(v, na.rm = TRUE))
  cs_dep$part_pct <- ifelse(tot > 0, 100 * x / tot, NA_real_)
  cs_dep
}
# Dominance : la CS la plus représentée ; « Dominante » si sa part >= seuil, sinon « Mixte ».
# Égalité de parts : première CS dans l'ordre métier (ordre_cs), signalée par egalite = TRUE.
identifier_dominance_cs <- function(cs, part_pct, seuil, ordre_cs = unname(PCS_VERS_CS1)) {
  ok <- !is.na(part_pct)
  if (!any(ok)) return(list(cs_principale = NA_character_, part_pct = NA_real_, structure = NA_character_, egalite = FALSE))
  o <- order(-part_pct[ok], match(cs[ok], ordre_cs)); i <- which(ok)[o[1]]
  egal <- sum(part_pct[ok] == part_pct[i]) > 1
  list(cs_principale = cs[i], part_pct = part_pct[i],
       structure = if (part_pct[i] >= seuil) paste("Dominante", libelle_cs_typo(cs[i])) else "Mixte", egalite = egal)
}
libelle_cs_typo <- function(cs) {
  lib <- c("Cadres" = "cadres", "Prof. intermediaires" = "professions intermédiaires", "Employes" = "employés", "Ouvriers" = "ouvriers")
  unname(ifelse(is.na(cs), NA_character_, ifelse(cs %in% names(lib), lib[cs], tolower(cs))))
}

# --- Concentration des départs sur une grande CS --------------------------------
# part de chaque CS dans les départs du département ; « Concentré » si la part
# maximale >= seuil (borne incluse), sinon « Diffus ».
calculer_concentration_departs <- function(cs, departs, seuil, ordre_cs = unname(PCS_VERS_CS1)) {
  tot <- sum(departs, na.rm = TRUE)
  if (!is.finite(tot) || tot <= 0 || all(is.na(departs)))
    return(list(cs_max = NA_character_, part_max_pct = NA_real_, type = NA_character_))
  part <- 100 * departs / tot
  o <- order(-part, match(cs, ordre_cs)); i <- o[1]
  list(cs_max = cs[i], part_max_pct = part[i], type = if (part[i] >= seuil) "Concentré" else "Diffus")
}
identifier_cs_volume_max <- function(cs, departs, ordre_cs = unname(PCS_VERS_CS1)) {
  ok <- !is.na(departs); if (!any(ok)) return(NA_character_)
  cs[which(ok)[order(-departs[ok], match(cs[ok], ordre_cs))[1]]]
}
identifier_cs_taux_max <- function(cs, taux, ordre_cs = unname(PCS_VERS_CS1)) identifier_cs_volume_max(cs, taux, ordre_cs)

# --- Attribution du profil (règles ordonnées, specs/10 §6) ----------------------
# Entrées : classes de poids / volume / intensité, type de concentration, nombre
# de critères à la frontière d'un seuil, intensité extrême (poids faible).
# Ordre d'examen : 1 frontière -> expertiser ; 2 poids faible -> diffuse (ou
# expertiser si intensité extrême) ; 3 volume élevé sans intensité élevée ->
# expertiser ; 4 fort enjeu ; 5 émergent ; 6 concentré ; 7 stable.
attribuer_profil_typologie <- function(classe_poids, classe_volume, classe_intensite, type_concentration,
                                       n_frontiere, intensite_extreme, nb_frontiere_expertise) {
  P <- PROFILS_TYPOLOGIE; CP <- CLASSES_TYPOLOGIE
  n <- length(classe_poids); out <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    po <- classe_poids[i]; vo <- classe_volume[i]; it <- classe_intensite[i]; co <- type_concentration[i]
    if (is.na(po) || is.na(vo) || is.na(it)) { out[i] <- P[["expertiser"]]; next }
    if (po == CP$poids[1]) { out[i] <- if (isTRUE(intensite_extreme[i])) P[["expertiser"]] else P[["diffus"]]; next }
    if (!is.na(n_frontiere[i]) && n_frontiere[i] >= nb_frontiere_expertise) { out[i] <- P[["expertiser"]]; next }
    if (vo == CP$volume[3] && it != CP$intensite[3]) { out[i] <- P[["expertiser"]]; next }
    if (po == CP$poids[3] && vo == CP$volume[3] && it == CP$intensite[3]) { out[i] <- P[["enjeu"]]; next }
    if (it == CP$intensite[3]) { out[i] <- P[["emergent"]]; next }
    if (!is.na(co) && co == "Concentré") { out[i] <- P[["concentre"]]; next }
    out[i] <- P[["stable"]]
  }
  out
}

# --- Justification textuelle déterministe (règles, jamais de texte généré) ------
construire_justification_profil <- function(d, params) {
  fmt1 <- function(x) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = 1, decimal.mark = ","))
  fmt0 <- function(x) ifelse(is.na(x), "n.d.", formatC(round(x), format = "d", big.mark = " "))
  lib_int <- if (params$indicateur_intensite == "part_a_remplacer_pct") "part de l’emploi actuel à remplacer" else "part des salariés du champ susceptibles de partir"
  vapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    morceaux <- c(
      sprintf("Poids BITD %s (%s %% de l’emploi BITD national, %s salariés)", tolower(r$classe_poids_bitd), fmt1(r$part_emploi_bitd_national_pct), fmt0(r$effectif_bitd_total)),
      sprintf("volume de départs %s (%s départs estimés)", tolower(r$classe_volume_departs), fmt0(r$departs_central)),
      sprintf("intensité de renouvellement %s (%s : %s %%)", tolower(r$classe_intensite), lib_int, fmt1(r$intensite_renouvellement_pct)),
      if (!is.na(r$structure_emploi)) sprintf("structure %s (%s : %s %%)", tolower(r$structure_emploi), libelle_cs_typo(r$cs_principale), fmt1(r$part_cs_principale_pct)) else NULL,
      if (!is.na(r$type_concentration)) sprintf("départs %s (%s : %s %% des départs)", tolower(r$type_concentration), libelle_cs_typo(r$cs_volume_departs_max), fmt1(r$part_departs_cs_max_pct)) else NULL,
      if (isTRUE(r$intensite_extreme_faible_stock)) "intensité extrême sur un stock faible" else NULL,
      if (!is.na(r$n_criteres_frontiere) && r$n_criteres_frontiere > 0) sprintf("%d critère(s) à la frontière d’un seuil", r$n_criteres_frontiere) else NULL)
    paste0(paste(morceaux, collapse = " ; "), ".")
  }, "")
}

# --- Construction complète ---------------------------------------------------------
# cs_dep : table département x cs1 EXACTE du 08d (departs_cs1$analytique$departement) ;
# stock  : stock_tous_ages (geo_code, cs1, effectif_tous_ages) ou NULL ;
# params : parametres_typologie().
# Retourne list(departements, departement_cs, parametres, base_poids).
construire_typologie_departements <- function(cs_dep, stock = NULL, params = parametres_typologie(), ordre_cs = unname(PCS_VERS_CS1)) {
  requis <- c("geo_code", "geo_nom", "cs1", "effectif_champ", "departs_central", "departs_bas", "departs_haut")
  manque <- setdiff(requis, names(cs_dep)); if (length(manque)) stop("construire_typologie_departements : colonnes absentes : ", paste(manque, collapse = ", "))
  if (anyDuplicated(cs_dep[, c("geo_code", "cs1")]) > 0) stop("construire_typologie_departements : doublons département x cs1.")
  if (!is.character(cs_dep$geo_code)) stop("construire_typologie_departements : geo_code doit être un texte.")
  avec_stock <- !is.null(stock) && all(c("geo_code", "cs1", "effectif_tous_ages") %in% names(stock))
  long <- cs_dep |> select(all_of(requis), any_of(c("region_code", "region_nom", "taux_depart_central_pct"))) |>
    mutate(taux_depart_central_pct = 100 * departs_central / effectif_champ)
  if (avec_stock) {
    long <- long |> left_join(stock |> select(geo_code, cs1, effectif_tous_ages) |> mutate(geo_code = as.character(geo_code)), by = c("geo_code", "cs1")) |>
      mutate(part_a_remplacer_pct = ifelse(!is.na(effectif_tous_ages) & effectif_tous_ages > 0, 100 * departs_central / effectif_tous_ages, NA_real_))
  } else long$effectif_tous_ages <- NA_integer_
  # Base du poids et de la structure : emploi actuel tous âges si disponible, sinon le champ (SPEC-TYPO-002)
  base_poids <- if (avec_stock && !anyNA(long$effectif_tous_ages)) "effectif_tous_ages" else "effectif_champ"
  indicateur <- if (params$indicateur_intensite == "part_a_remplacer_pct" && base_poids == "effectif_tous_ages") "part_a_remplacer_pct" else "taux_depart_central_pct"
  if (indicateur != params$indicateur_intensite) message("Typologie : stock tous âges indisponible -> intensité = taux_depart_central_pct (dénominateur : champ).")
  long <- long |> mutate(effectif_base = .data[[base_poids]]) |> calculer_structure_cs("effectif_base") |>
    mutate(intensite_cs_pct = .data[[indicateur]]) |>
    group_by(geo_code) |>
    mutate(part_departs_pct = if (sum(departs_central) > 0) 100 * departs_central / sum(departs_central) else rep(NA_real_, n()),
           rang_volume = rank(-departs_central, ties.method = "first"), rang_intensite = rank(-intensite_cs_pct, ties.method = "first", na.last = "keep")) |>
    ungroup() |> arrange(geo_code, match(cs1, ordre_cs))
  cs_order <- ordre_cs[ordre_cs %in% unique(long$cs1)]
  # Par département : totaux, structure, concentration, CS identifiées
  dep <- long |> group_by(across(any_of(c("region_code", "region_nom", "geo_code", "geo_nom")))) |>
    summarise(effectif_champ_total = sum(effectif_champ), effectif_tous_ages_total = if (avec_stock) sum(effectif_tous_ages) else NA_integer_,
              effectif_bitd_total = sum(effectif_base),
              departs_central = sum(departs_central), departs_bas = sum(departs_bas), departs_haut = sum(departs_haut),
              n_cs = n(), .groups = "drop") |>
    mutate(part_emploi_bitd_national_pct = 100 * effectif_bitd_total / sum(effectif_bitd_total),
           taux_depart_central_pct = 100 * departs_central / effectif_champ_total,
           part_a_remplacer_pct = if (avec_stock) 100 * departs_central / effectif_tous_ages_total else NA_real_,
           intensite_renouvellement_pct = if (indicateur == "part_a_remplacer_pct") part_a_remplacer_pct else taux_depart_central_pct,
           indicateur_intensite = indicateur, base_poids = base_poids)
  par_dep <- lapply(split(long, long$geo_code), function(g) {
    dom <- identifier_dominance_cs(g$cs1, g$part_pct, params$seuil_dominance_cs, ordre_cs)
    con <- calculer_concentration_departs(g$cs1, g$departs_central, params$seuil_concentration, ordre_cs)
    tibble(geo_code = g$geo_code[1], cs_principale = dom$cs_principale, part_cs_principale_pct = dom$part_pct, structure_emploi = dom$structure,
           egalite_structure = dom$egalite, cs_volume_departs_max = identifier_cs_volume_max(g$cs1, g$departs_central, ordre_cs),
           part_departs_cs_max_pct = con$part_max_pct, cs_taux_renouvellement_max = identifier_cs_taux_max(g$cs1, g$intensite_cs_pct, ordre_cs),
           type_concentration = con$type)
  }) |> bind_rows()
  # Effectifs et parts par CS en colonnes (audit)
  larges <- long |> select(geo_code, cs1, effectif_base, part_pct, departs_central, intensite_cs_pct) |>
    mutate(cs1 = gsub("[^A-Za-z]", "_", cs1)) |>
    tidyr_pivot(cs_order)
  dep <- dep |> left_join(par_dep, by = "geo_code") |> left_join(larges, by = "geo_code")
  # Classes : seuils (terciles provisoires ou fixes) sur les départements au-dessus du plancher
  retenus <- dep$effectif_bitd_total >= params$effectif_min
  s_poids <- seuils_classes(dep$part_emploi_bitd_national_pct[retenus], params$methode_classes, c(params$poids_faible, params$poids_fort))
  s_int   <- seuils_classes(dep$intensite_renouvellement_pct[retenus], params$methode_classes, c(params$intensite_faible, params$intensite_elevee))
  s_vol   <- seuils_classes(dep$departs_central[retenus], params$methode_classes, c(params$volume_faible, params$volume_eleve))
  frontiere <- function(x, s) { m <- params$marge_frontiere_pct / 100
    (!is.na(x) & !is.na(s[["bas"]]) & abs(x - s[["bas"]]) <= m * s[["bas"]]) | (!is.na(x) & !is.na(s[["haut"]]) & abs(x - s[["haut"]]) <= m * s[["haut"]]) }
  dep <- dep |> mutate(
    classe_poids_bitd = classer_poids_bitd(part_emploi_bitd_national_pct, effectif_bitd_total, s_poids, params$effectif_min),
    classe_intensite  = classer_par_seuils(intensite_renouvellement_pct, s_int, CLASSES_TYPOLOGIE$intensite),
    classe_volume_departs = classer_par_seuils(departs_central, s_vol, CLASSES_TYPOLOGIE$volume),
    frontiere_poids = frontiere(part_emploi_bitd_national_pct, s_poids), frontiere_intensite = frontiere(intensite_renouvellement_pct, s_int),
    frontiere_volume = frontiere(departs_central, s_vol),
    frontiere_dominance = !is.na(part_cs_principale_pct) & abs(part_cs_principale_pct - params$seuil_dominance_cs) <= params$marge_frontiere_pct / 100 * params$seuil_dominance_cs,
    frontiere_concentration = !is.na(part_departs_cs_max_pct) & abs(part_departs_cs_max_pct - params$seuil_concentration) <= params$marge_frontiere_pct / 100 * params$seuil_concentration,
    n_criteres_frontiere = frontiere_poids + frontiere_intensite + frontiere_volume + frontiere_dominance + frontiere_concentration,
    intensite_extreme_faible_stock = classe_poids_bitd == CLASSES_TYPOLOGIE$poids[1] & !is.na(s_int[["haut"]]) &
      intensite_renouvellement_pct >= params$facteur_intensite_extreme * s_int[["haut"]],
    profil_typologie = attribuer_profil_typologie(classe_poids_bitd, classe_volume_departs, classe_intensite, type_concentration,
                                                  n_criteres_frontiere, intensite_extreme_faible_stock, params$nb_frontiere_expertise))
  dep$justification_profil <- construire_justification_profil(dep, params)
  dep <- dep |> arrange(geo_code)
  parametres <- tibble(parametre = c("methode_classes", "indicateur_intensite", "base_poids", "seuil_dominance_cs_pct", "seuil_concentration_departs_pct",
                                     "effectif_min", "poids_bas_pct", "poids_haut_pct", "intensite_bas_pct", "intensite_haut_pct", "volume_bas", "volume_haut",
                                     "marge_frontiere_pct", "nb_criteres_frontiere_expertise", "facteur_intensite_extreme", "n_departements", "n_departements_retenus_seuils"),
                       valeur = c(params$methode_classes, indicateur, base_poids, params$seuil_dominance_cs, params$seuil_concentration, params$effectif_min,
                                  s_poids[["bas"]], s_poids[["haut"]], s_int[["bas"]], s_int[["haut"]], s_vol[["bas"]], s_vol[["haut"]],
                                  params$marge_frontiere_pct, params$nb_frontiere_expertise, params$facteur_intensite_extreme, nrow(dep), sum(retenus)))
  list(departements = dep, departement_cs = long |> select(-effectif_base), parametres = parametres, base_poids = base_poids, indicateur = indicateur)
}
# pivot large sans tidyr : une colonne par mesure x CS (effectif_<cs>, part_<cs>_pct, departs_<cs>, intensite_<cs>_pct)
tidyr_pivot <- function(long, cs_order) {
  out <- distinct(long, geo_code)
  for (cs in gsub("[^A-Za-z]", "_", cs_order)) {
    g <- long[long$cs1 == cs, ]
    out <- out |> left_join(tibble(geo_code = g$geo_code, a = g$effectif_base, b = g$part_pct, c = g$departs_central, d = g$intensite_cs_pct) |>
                              setNames(c("geo_code", paste0("effectif_", cs), paste0("part_", cs, "_pct"), paste0("departs_", cs), paste0("intensite_", cs, "_pct"))), by = "geo_code")
  }
  out
}

# --- Contrôles (bloquants) -----------------------------------------------------------
controler_typologie <- function(typo, cs_dep, tol = 1e-6) {
  d <- typo$departements; l <- typo$departement_cs
  verifier <- function(ok, quoi) if (!isTRUE(ok)) stop("08f : contrôle ÉCHOUÉ — ", quoi)
  verifier(anyDuplicated(d$geo_code) == 0, "doublons de département.")
  verifier(setequal(d$geo_code, unique(cs_dep$geo_code)), "départements de la typologie différents de la table cs1.")
  s <- l |> group_by(geo_code) |> summarise(e = sum(effectif_champ), c = sum(departs_central), b = sum(departs_bas), h = sum(departs_haut), .groups = "drop") |>
    inner_join(d |> select(geo_code, effectif_champ_total, departs_central, departs_bas, departs_haut), by = "geo_code")
  verifier(all(s$e == s$effectif_champ_total) && max(abs(s$c - s$departs_central), abs(s$b - s$departs_bas), abs(s$h - s$departs_haut)) < tol,
           "Σ grandes CS ≠ total du département.")
  verifier(abs(sum(d$departs_central) - sum(cs_dep$departs_central)) < tol && sum(d$effectif_champ_total) == sum(cs_dep$effectif_champ),
           "total national de la typologie ≠ table cs1 du 08d.")
  verifier(abs(sum(d$part_emploi_bitd_national_pct) - 100) < 1e-6, "Σ parts nationales ≠ 100.")
  verifier(all(d$departs_bas <= d$departs_central + tol & d$departs_central <= d$departs_haut + tol), "fourchette non ordonnée.")
  verifier(all(d$profil_typologie %in% PROFILS_TYPOLOGIE), "profil inconnu.")
  verifier(!any(grepl("tension_departement|departement_en_tension", names(d))), "colonne interdite (tension attribuée au département).")
  invisible(TRUE)
}

# --- Secret statistique de la typologie (specs/10 § Secret) -------------------------
# diffusion_cs_dep : table département x cs1 DIFFUSÉE du 08d (colonne masque) ;
# base : bts_projete au niveau département (indicateurs de secret du département entier).
# Règles : (1) département non diffusable (règle Insee sur le département entier,
# puis secondaire sur le bloc national) -> toutes les mesures NA ; (2) toute
# colonne par CS suit le masque de sa cellule ; (3) dès qu'UNE cellule CS du
# département est masquée, les variables dérivées par CS (cs_principale,
# part_cs_principale_pct, structure_emploi, cs_volume_departs_max,
# cs_taux_renouvellement_max, part_departs_cs_max_pct, type_concentration) sont
# NA et les profils qui en dépendent (« Renouvellement concentré », « Pôle BITD
# relativement stable ») deviennent « Non diffusé (secret statistique) ».
appliquer_secret_typologie <- function(typo, diffusion_cs_dep, base, regles = regles_secret()) {
  d <- typo$departements
  if (!"masque" %in% names(diffusion_cs_dep)) stop("appliquer_secret_typologie : table de diffusion du 08d requise (colonne masque).")
  sec <- indicateurs_secret(base |> mutate(geo_code = as.character(geo_code)), "geo_code")
  d <- d |> left_join(sec, by = "geo_code") |>
    mutate(n_entreprises = coalesce(n_entreprises, 0L), part_dominante_pct = coalesce(part_dominante_pct, 100),
           masque = masquer_cellules(effectif_champ_total, n_entreprises, part_dominante_pct, regles)) |>
    select(-n_entreprises, -part_dominante_pct)
  masq_cs <- diffusion_cs_dep |> group_by(geo_code) |> summarise(cs_masquee = any(masque), .groups = "drop")
  d <- d |> left_join(masq_cs, by = "geo_code") |> mutate(cs_masquee = coalesce(cs_masquee, FALSE))
  cols_cs <- grep("^(effectif|part|departs|intensite)_[A-Za-z_]+(_pct)?$", names(d), value = TRUE)
  cols_cs <- setdiff(cols_cs, c("part_emploi_bitd_national_pct", "part_cs_principale_pct", "part_departs_cs_max_pct", "part_a_remplacer_pct", "departs_central", "departs_bas", "departs_haut", "intensite_renouvellement_pct"))
  for (v in cols_cs) {                      # colonne par CS : masque de la cellule
    cs <- sub("^(effectif|part|departs|intensite)_", "", sub("_pct$", "", v))
    m <- diffusion_cs_dep |> mutate(k = gsub("[^A-Za-z]", "_", cs1)) |> filter(k == cs) |> select(geo_code, masque)
    mm <- d$geo_code %in% m$geo_code[m$masque]
    d[[v]][mm] <- NA
  }
  derivees <- c("cs_principale", "part_cs_principale_pct", "structure_emploi", "cs_volume_departs_max", "cs_taux_renouvellement_max",
                "part_departs_cs_max_pct", "type_concentration", "egalite_structure")
  for (v in derivees) d[[v]][d$cs_masquee] <- NA
  dep_cs_profil <- d$cs_masquee & d$profil_typologie %in% PROFILS_TYPOLOGIE[c("concentre", "stable")]
  d$profil_typologie[dep_cs_profil] <- "Non diffusé (secret statistique)"
  mesures <- setdiff(names(d), c("region_code", "region_nom", "geo_code", "geo_nom", "masque", "cs_masquee", "indicateur_intensite", "base_poids"))
  for (v in mesures) d[[v]][d$masque] <- NA
  d$profil_typologie[d$masque] <- "Non diffusé (secret statistique)"
  d$justification_profil[d$masque | dep_cs_profil] <- NA_character_
  d |> select(-starts_with("frontiere_"), -n_criteres_frontiere, -intensite_extreme_faible_stock, -egalite_structure, -cs_masquee) |>
    mutate(motif_masque = ifelse(masque, "territoire", ifelse(dep_cs_profil, "cellule cs masquée", NA_character_)))
}

# --- AIDE AU RÉGLAGE : proposer des seuils fixes à partir des données ----------------
# Calcule, sur la table interne des départements (typologie_departements$departements
# ou le CSV interne relu), les indicateurs statistiques de chaque axe (min, terciles,
# médiane, max, sur les départements au-dessus du plancher), en déduit des bornes
# ARRONDIES à un pas lisible (pas_poids en points de %, pas_intensite en points de %,
# pas_volume en départs), compte les départements par classe avec ces bornes et
# imprime un bloc prêt à coller dans 00_config.R (TYPO_METHODE_CLASSES = "fixes").
# Elle PROPOSE : rien n'est appliqué, rien n'est écrit. Les seuils métier
# (dominance, concentration, plancher) ne sont pas proposés : seule leur
# distribution observée est affichée pour éclairer le choix.
arrondir_pas <- function(x, pas) ifelse(is.na(x), NA_real_, round(x / pas) * pas)
proposer_seuils_typologie <- function(departements, effectif_min = TYPO_SEUIL_EFFECTIF_MIN,
                                      pas = c(poids = 0.5, intensite = 1, volume = 10), afficher = TRUE) {
  requis <- c("effectif_bitd_total", "part_emploi_bitd_national_pct", "intensite_renouvellement_pct", "departs_central")
  manque <- setdiff(requis, names(departements)); if (length(manque)) stop("proposer_seuils_typologie : colonnes absentes : ", paste(manque, collapse = ", "))
  if (!all(c("poids", "intensite", "volume") %in% names(pas)) || any(pas <= 0)) stop("proposer_seuils_typologie : pas = c(poids, intensite, volume) strictement positifs.")
  d <- departements[!is.na(departements$effectif_bitd_total) & departements$effectif_bitd_total >= effectif_min, ]
  if (nrow(d) < 3) stop("proposer_seuils_typologie : moins de 3 départements au-dessus du plancher (", effectif_min, ") — aucun tercile calculable.")
  axes <- list(poids = list(col = "part_emploi_bitd_national_pct", bas = "TYPO_SEUIL_POIDS_FAIBLE", haut = "TYPO_SEUIL_POIDS_FORT", libs = CLASSES_TYPOLOGIE$poids, unite = "% de l'emploi BITD national"),
               intensite = list(col = "intensite_renouvellement_pct", bas = "TYPO_SEUIL_INTENSITE_FAIBLE", haut = "TYPO_SEUIL_RENOUVELLEMENT_ELEVE", libs = CLASSES_TYPOLOGIE$intensite, unite = "%"),
               volume = list(col = "departs_central", bas = "TYPO_SEUIL_VOLUME_FAIBLE", haut = "TYPO_SEUIL_VOLUME_ELEVE", libs = CLASSES_TYPOLOGIE$volume, unite = "départs estimés"))
  tableau <- bind_rows(lapply(names(axes), function(a) {
    x <- d[[axes[[a]]$col]]; x <- x[is.finite(x)]
    t <- seuils_classes(x, "terciles"); q <- quantile(x, c(0, .25, .5, .75, 1), names = FALSE)
    prop <- c(bas = arrondir_pas(t[["bas"]], pas[[a]]), haut = arrondir_pas(t[["haut"]], pas[[a]]))
    if (prop[["haut"]] <= prop[["bas"]]) prop[["haut"]] <- prop[["bas"]] + pas[[a]]       # bornes confondues après arrondi : on écarte d'un pas
    cl <- classer_par_seuils(x, prop, axes[[a]]$libs)
    tibble(axe = a, indicateur = axes[[a]]$col, unite = axes[[a]]$unite, n = length(x), min = q[1], q1 = q[2], mediane = q[3], q3 = q[4], max = q[5],
           tercile_bas = t[["bas"]], tercile_haut = t[["haut"]], pas = pas[[a]],
           parametre_bas = axes[[a]]$bas, proposition_bas = prop[["bas"]], parametre_haut = axes[[a]]$haut, proposition_haut = prop[["haut"]],
           n_classe_1 = sum(cl == axes[[a]]$libs[1]), n_classe_2 = sum(cl == axes[[a]]$libs[2]), n_classe_3 = sum(cl == axes[[a]]$libs[3]))
  }))
  # distributions observées des seuils métier (pour information, aucune proposition)
  obs <- function(col) if (col %in% names(departements)) { x <- departements[[col]]; x <- x[is.finite(x)]; if (length(x)) quantile(x, c(0, .5, 1), names = FALSE) else rep(NA_real_, 3) } else rep(NA_real_, 3)
  metier <- tibble(parametre = c("TYPO_SEUIL_DOMINANCE_CS", "TYPO_SEUIL_CONCENTRATION_DEPARTS", "TYPO_SEUIL_EFFECTIF_MIN"),
                   indicateur_observe = c("part_cs_principale_pct", "part_departs_cs_max_pct", "effectif_bitd_total"),
                   min = c(obs("part_cs_principale_pct")[1], obs("part_departs_cs_max_pct")[1], obs("effectif_bitd_total")[1]),
                   mediane = c(obs("part_cs_principale_pct")[2], obs("part_departs_cs_max_pct")[2], obs("effectif_bitd_total")[2]),
                   max = c(obs("part_cs_principale_pct")[3], obs("part_departs_cs_max_pct")[3], obs("effectif_bitd_total")[3]),
                   n_sous_plancher = c(NA, NA, sum(departements$effectif_bitd_total < effectif_min, na.rm = TRUE)))
  fmt <- function(x) trimws(formatC(x, format = "fg", digits = 4, decimal.mark = "."))
  bloc <- c("# --- Proposition de seuils fixes (proposer_seuils_typologie) : à relire, puis à coller dans 00_config.R ---",
            sprintf("# %d département(s) au-dessus du plancher TYPO_SEUIL_EFFECTIF_MIN = %s ; bornes = terciles arrondis au pas (poids %s, intensité %s, volume %s)",
                    nrow(d), fmt(effectif_min), fmt(pas[["poids"]]), fmt(pas[["intensite"]]), fmt(pas[["volume"]])),
            'TYPO_METHODE_CLASSES <- "fixes"',
            unlist(lapply(seq_len(nrow(tableau)), function(i) c(
              sprintf("%-33s <- %-6s # %s : tercile %s -> %s ; classes %s / %s / %s = %d / %d / %d", tableau$parametre_bas[i], fmt(tableau$proposition_bas[i]), tableau$unite[i],
                      fmt(tableau$tercile_bas[i]), fmt(tableau$proposition_bas[i]), axes[[tableau$axe[i]]]$libs[1], axes[[tableau$axe[i]]]$libs[2], axes[[tableau$axe[i]]]$libs[3],
                      tableau$n_classe_1[i], tableau$n_classe_2[i], tableau$n_classe_3[i]),
              sprintf("%-33s <- %-6s # tercile %s -> %s ; médiane %s, min %s, max %s", tableau$parametre_haut[i], fmt(tableau$proposition_haut[i]),
                      fmt(tableau$tercile_haut[i]), fmt(tableau$proposition_haut[i]), fmt(tableau$mediane[i]), fmt(tableau$min[i]), fmt(tableau$max[i]))))),
            "# Seuils métier (non proposés, distribution observée pour éclairer le choix) :",
            sprintf("#   %-32s %s : min %s, médiane %s, max %s%s", metier$parametre, metier$indicateur_observe, fmt(metier$min), fmt(metier$mediane), fmt(metier$max),
                    ifelse(is.na(metier$n_sous_plancher), "", sprintf(" ; %d département(s) sous le plancher actuel", metier$n_sous_plancher))))
  if (afficher) cat(paste(bloc, collapse = "\n"), "\n")
  invisible(list(tableau = tableau, metier = metier, bloc = bloc))
}

# --- Synthèse par profil ------------------------------------------------------------
synthese_profils <- function(typo) {
  d <- typo$departements; tot_e <- sum(d$effectif_bitd_total); tot_d <- sum(d$departs_central)
  d |> group_by(profil_typologie) |>
    summarise(n_departements = n(), effectif_bitd_total = sum(effectif_bitd_total), departs_central = sum(departs_central),
              poids_national_emploi_pct = 100 * effectif_bitd_total / tot_e, poids_national_departs_pct = 100 * departs_central / tot_d,
              intensite_mediane_pct = median(intensite_renouvellement_pct, na.rm = TRUE), part_nationale_mediane_pct = median(part_emploi_bitd_national_pct),
              n_concentres = sum(type_concentration %in% "Concentré"), n_mixtes = sum(structure_emploi %in% "Mixte"), .groups = "drop") |>
    mutate(profil_typologie = factor(profil_typologie, levels = PROFILS_TYPOLOGIE)) |> arrange(profil_typologie) |> mutate(profil_typologie = as.character(profil_typologie))
}

# --- Tensions DGA / France Travail : couche SÉPARÉE, maille différente --------------
# tensions : table au format data/templates/tensions_fap_territoires_template.csv
#            (territoire_code, type_territoire, fap_code, niveau_tension, ...) ;
# passage  : table bassin -> département (code_source = bassin, code_cible = département).
# RÈGLE (SPEC-TYPO-020 à 023) : on ne produit QUE des indicateurs de PRÉSENCE
# (nb_bassins_signales, presence_signal_tension_localise). Jamais de colonne
# « tension_departement » ; jamais de départs attribués au bassin ; jamais de
# correspondance FAP -> grande CS. La tension n'entre pas dans le profil.
joindre_signal_tension_localise <- function(typo_dep, tensions = NULL, passage = NULL, niveaux_signales = c("forte", "tres_forte")) {
  d <- typo_dep |> mutate(nb_bassins_signales = NA_integer_, presence_signal_tension_localise = NA)
  if (is.null(tensions) || is.null(passage)) return(d)
  requis <- c("territoire_code", "type_territoire", "fap_code", "niveau_tension")
  if (!all(requis %in% names(tensions))) stop("joindre_signal_tension_localise : colonnes absentes : ", paste(setdiff(requis, names(tensions)), collapse = ", "))
  if (any(grepl("tension_departement|departement_en_tension", names(tensions)))) stop("joindre_signal_tension_localise : colonne interdite dans les tensions.")
  if (any(grepl("^cs1$|grande_cs", names(tensions)))) stop("joindre_signal_tension_localise : aucune correspondance FAP -> grande CS n'est admise.")
  bassins <- tensions |> filter(type_territoire == "bassin", niveau_tension %in% niveaux_signales) |>
    distinct(territoire_code) |> inner_join(passage |> mutate(code_source = as.character(code_source), code_cible = as.character(code_cible)),
                                            by = c(territoire_code = "code_source")) |>
    count(geo_code = code_cible, name = "nb_bassins_signales")
  d |> select(-nb_bassins_signales, -presence_signal_tension_localise) |> left_join(bassins, by = "geo_code") |>
    mutate(nb_bassins_signales = coalesce(nb_bassins_signales, 0L), presence_signal_tension_localise = nb_bassins_signales > 0)
}

# --- Matrice pédagogique (PNG, ggplot2, sans ggrepel) ------------------------------
png_matrice_typologie <- function(typo, fichier, titre = "Typologie des départements BITD") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 absent")
  d <- typo$departements |> filter(is.finite(intensite_renouvellement_pct), is.finite(part_emploi_bitd_national_pct))
  p <- typo$parametres; val <- function(k) as.numeric(p$valeur[p$parametre == k])
  lib_y <- if (typo$indicateur == "part_a_remplacer_pct") "Intensité : part de l’emploi actuel à remplacer d’ici 2030 (%)" else "Intensité : part des salariés du champ susceptibles de partir d’ici 2030 (%)"
  pal <- c("Mixte" = "#6b7280", "Dominante cadres" = "#08306b", "Dominante professions intermédiaires" = "#2171b5",
           "Dominante employés" = "#4292c6", "Dominante ouvriers" = "#9ecae1")
  d$structure_emploi <- ifelse(is.na(d$structure_emploi), "Mixte", d$structure_emploi)
  g <- ggplot2::ggplot(d, ggplot2::aes(x = part_emploi_bitd_national_pct, y = intensite_renouvellement_pct)) +
    ggplot2::geom_vline(xintercept = c(val("poids_bas_pct"), val("poids_haut_pct")), linetype = "42", colour = "#666666", linewidth = .4) +
    ggplot2::geom_hline(yintercept = c(val("intensite_bas_pct"), val("intensite_haut_pct")), linetype = "42", colour = "#666666", linewidth = .4) +
    ggplot2::geom_point(ggplot2::aes(size = departs_central, colour = structure_emploi), alpha = .85) +
    ggplot2::geom_text(ggplot2::aes(label = geo_nom), size = 2.6, vjust = -1.1, colour = "#1A1A1A") +
    ggplot2::scale_colour_manual(values = pal, name = "Structure de l’emploi", drop = TRUE) +
    ggplot2::scale_size_area(max_size = 12, name = "Départs estimés (centrale)") +
    ggplot2::labs(title = titre, subtitle = "Lignes pointillées : seuils des classes (faible / moyen / fort) — règle de classement statistique provisoire",
                  x = "Poids : part du département dans l’emploi BITD national (%)", y = lib_y,
                  caption = "Une position = un département ; la taille du point = volume de départs. Aucune tension DGA / France Travail n’est représentée (maille différente).") +
    ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(legend.position = "bottom", legend.box = "vertical", panel.grid.minor = ggplot2::element_blank())
  ggplot2::ggsave(fichier, g, width = 11, height = 8.5, dpi = 130, bg = "white")
  invisible(fichier)
}
