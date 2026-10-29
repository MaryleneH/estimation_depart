# ==============================================================================
# 09a_fonctions_analyse_hypotheses.R — Analyse « hypothèses de départ » (specs/11)
# ------------------------------------------------------------------------------
# QUESTION  : combien de salariés du champ auront quitté l'emploi sur la plage de
#             projection (année BTS -> année BTS + HORIZON), sous quelles hypothèses,
#             dans quels métiers et dans quels territoires, et où les départs touchent-ils
#             les métiers de la liste difficiles à remplacer ?
# ENTRÉES   : objets de la chaîne : bts_projete (volume national, par âge, seniors),
#             departs_cs1$diffusion (catégorie sociale : France, région, département),
#             departs_pcs$diffusion$france (métiers), exposition_tension (liste, matrice,
#             exposition départements / régions), metiers_liste_territoires$table (08j),
#             typologie_departements$diffusion (profils), synthese_geo_diffusable (08).
# RÈGLES    : seules des tables de DIFFUSION ou des agrégats passés par la règle de secret
#             (masquer_cellules, 00d) ; une cellule masquée = NA + « non diffusé (secret
#             statistique) », jamais zéro ; gabarits fixes ; libellé public de la liste
#             (texte_liste, 00k) ; rien n'est recalculé par le modèle.
# SORTIES   : des data.frame aux colonnes en clair (prêtes pour Word) et
#             ecrire_classeur_analyse() (openxlsx : titre, note, tableau par feuille ;
#             writexl en repli ; jamais d'arrêt).
# ==============================================================================
library(dplyr)

.nd_analyse <- "non diffusé (secret statistique)"
.n_ana <- function(x) ifelse(is.na(x), NA_real_, arrondir_nombre_personnes(x))
.t_ana <- function(x) ifelse(is.na(x), NA_real_, arrondir_taux(x))
.f1_ana <- function(x) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = 1, decimal.mark = ","))
.f2_ana <- function(x) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = 2, decimal.mark = ","))
.oui_non_ana <- function(x, inconnu = "inconnue") ifelse(is.na(x), inconnu, ifelse(x, "oui", "non"))
.df_ana <- function(...) data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
.colc_ana <- function(d, v) if (v %in% names(d)) as.character(d[[v]]) else rep(NA_character_, nrow(d))
.rang_ana <- function(x) { r <- rep(NA_integer_, length(x)); ok <- !is.na(x); r[ok] <- as.integer(rank(-x[ok], ties.method = "min")); r }

# --- Paramètres (00_config, source unique) ----------------------------------------------------
parametres_analyse <- function() {
  annee <- as.integer(get0("ANNEE_REF_GRAPHIQUE", ifnotfound = 2024L)); h <- as.integer(get0("HORIZON", ifnotfound = 6L))
  list(annee_bts = annee, horizon = h, fin = annee + h,
       top_n = as.integer(get0("ANALYSE_TOP_N", ifnotfound = 15L)),
       effectif_min_part = as.integer(get0("ANALYSE_EFFECTIF_MIN_PART", ifnotfound = 50L)),
       n_regions = as.integer(get0("ANALYSE_N_REGIONS_PRIORITES", ifnotfound = 3L)),
       age_senior = as.integer(get0("AGE_SENIOR", ifnotfound = 55L)))
}

# --- 1. Hypothèses et critères (SPEC-ANA-002) : tout vient de la configuration et du modèle ----
table_hypotheses_analyse <- function(deltas = NULL, params = parametres_analyse(), champ = construire_libelle_champ()) {
  P <- construire_libelles_public(champ); scen <- get0("SCENARIO", ifnotfound = "B")
  lig <- function(h, v, s, e, sc) .df_ana(`Hypothèse` = h, `Valeur retenue` = v, Source = s, `Effet sur le résultat` = e, `Ce qui la fait varier` = sc)
  rows <- list(
    lig("Champ", sprintf(texte_perimetre("%s en %d, entreprises du périmètre Base"), P$salaries_age, params$annee_bts), sprintf("BTS %d", params$annee_bts),
        "Fixe le dénominateur : les salariés plus jeunes ne sont pas comptés.", "AGE_MIN_BTS"),
    lig("Plage de projection", sprintf("%d → %d (%d ans)", params$annee_bts, params$fin, params$horizon), "HORIZON (00_config)",
        "Un horizon plus long cumule davantage de départs ; les départs par an sont le total divisé par la durée.", "HORIZON"),
    lig("Définition du départ", "Sortie définitive de l’emploi : retraite ou fin de carrière, invalidité, décès. Les mobilités vers un autre employeur ne sont pas comptées.",
        "Méthode", "Le total est un plancher du besoin de remplacement.", "aucun"),
    lig("Calendrier de fin de carrière",
        if (!is.null(deltas)) sprintf("Âge de sortie par catégorie sociale, décalé de δ : central %s an, bas %s, haut %s", .f2_ana(deltas$central), .f2_ana(deltas$bas), .f2_ana(deltas$haut))
        else "Âge de sortie par catégorie sociale, décalé de δ (réforme des retraites 2023)",
        "DREES, réforme des retraites 2023", "Un décalage plus grand retarde les départs : l’hypothèse basse les retarde, l’hypothèse haute les avance.", "hypothèses basse et haute"),
    lig("Sas de fin de carrière", if (identical(scen, "B")) "Pris en compte : sortie de l’emploi avant la liquidation (scénario B)" else "Non pris en compte : liquidation seule (scénario A)",
        "DREES (âge de sortie de l’emploi)", "Avance les départs par rapport à l’âge légal de liquidation.", "SCENARIO"),
    lig("Invalidité", "Flux d’entrée en invalidité par catégorie sociale, âge et sexe, cumulé sur la plage", sprintf("EACR %s", get0("INVAL_ANNEE", ifnotfound = "")),
        "Ajoute des sorties hors retraite, surtout avant l’âge de départ.", "aucun"),
    lig("Décès", "Quotients de mortalité par âge et sexe, cumulés sur la plage", "Insee", "Ajoute des sorties hors retraite.", "aucun"),
    lig("Dispersion des âges de sortie", sprintf("σ = %s an(s) autour de l’âge de sortie", .f1_ana(get0("SIGMA_SORTIE", ifnotfound = NA_real_))), "Méthode",
        "Lisse les départs autour de l’âge de sortie : une partie part avant, une partie après.", "SIGMA_SORTIE"),
    lig("Secret statistique", sprintf("Cellule publiée si au moins %d salariés et %d entreprises, aucune entreprise au-delà de %d %% ; secret secondaire", SECRET_MIN_SALARIES, SECRET_MIN_ENTREPRISES, SECRET_DOMINANCE_PCT),
        "Règle Insee", "Une cellule non diffusée est vide, jamais zéro ; un territoire non diffusé n’est pas un territoire sans départ.", "aucun"))
  if (exists("TENSION_SEUIL_RANG") && exists("lib_liste")) {
    n <- get0("TENSION_N_METIERS_SUIVIS", ifnotfound = NA)
    rows <- c(rows, list(
      lig("Métiers suivis", sprintf("%s %s (classeur PCS → FAP) ; information partielle : les autres métiers ne sont pas observés", if (is.na(n)) "les" else n, lib_liste()),
          "Liste des métiers suivis ; France Travail (tension par famille professionnelle)", "Tout ce qui est dit de la tension ne vaut que pour ces métiers.", "TENSION_N_METIERS_SUIVIS"),
      lig("Tension d’un métier", sprintf("Métier « en tension » à partir du rang %d sur %d ; tension nationale de la famille professionnelle, jamais celle d’un territoire", TENSION_SEUIL_RANG, TENSION_RANG_MAX),
          "France Travail / Dares", "Un seuil plus bas classe davantage de métiers en tension.", "TENSION_SEUIL_RANG"),
      lig("Écart à la France", sprintf("« plus forte » ou « plus faible » qu’en France à partir de %s points d’écart", .f1_ana(get0("TENSION_ECART_POINTS", ifnotfound = NA_real_))),
          "Méthode", "Un seuil plus grand réduit le nombre de territoires lus comme différents de la France.", "TENSION_ECART_POINTS")))
  }
  do.call(rbind, rows)
}

# --- 2. Volume (SPEC-ANA-003) ---------------------------------------------------------------
# Total national : une seule cellule, passée par la règle de secret (règle Insee sur n, entreprises, dominance).
table_volume_total <- function(bts_projete, regles = regles_secret(), params = parametres_analyse()) {
  d <- ajouter_parts_causes(bts_projete); n <- nrow(d)
  ind <- indicateurs_secret(d)
  masque <- isTRUE(masquer_cellules(n, ind$n_entreprises, ind$part_dominante_pct, regles))
  v <- function(x) if (masque) rep(NA_real_, length(x)) else x
  tot <- c(central = sum(d$p_central), bas = sum(d$p_bas), haut = sum(d$p_haut))
  out <- .df_ana(
    Indicateur = c("Salariés du champ", sprintf("Départs attendus entre %d et %d", params$annee_bts, params$fin), "Part susceptible de partir (%)",
                   "Départs par an en moyenne", "dont retraite ou fin de carrière", "dont invalidité", "dont décès"),
    `Scénario central` = v(c(n, .n_ana(tot[["central"]]), .t_ana(100 * tot[["central"]] / n), .n_ana(tot[["central"]] / params$horizon), .n_ana(sum(d$part_ret)), .n_ana(sum(d$part_inv)), .n_ana(sum(d$part_dec)))),
    `Hypothèse basse (départs retardés)` = v(c(n, .n_ana(tot[["bas"]]), .t_ana(100 * tot[["bas"]] / n), .n_ana(tot[["bas"]] / params$horizon), NA, NA, NA)),
    `Hypothèse haute (départs avancés)` = v(c(n, .n_ana(tot[["haut"]]), .t_ana(100 * tot[["haut"]] / n), .n_ana(tot[["haut"]] / params$horizon), NA, NA, NA)))
  attr(out, "masque") <- masque; attr(out, "total_central") <- if (masque) NA_real_ else tot[["central"]]
  out
}

# Volume par groupe (tranche d'âge, seniors...) : `groupe` = vecteur aligné sur bts_projete ; `niveaux` = ordre (et
# complétion : un niveau sans salarié est une cellule à zéro, masquée comme une autre, SPEC-PUB-007).
table_volume_groupe <- function(bts_projete, groupe, libelle = "Groupe", niveaux = NULL, regles = regles_secret(), total_central = NA_real_) {
  if (length(groupe) != nrow(bts_projete)) stop("table_volume_groupe : `groupe` doit être aligné sur bts_projete.")
  d <- ajouter_parts_causes(bts_projete) |> mutate(.g = as.character(groupe))
  agg <- d |> group_by(.g) |> summarise(n = n(), central = sum(p_central), bas = sum(p_bas), haut = sum(p_haut), ret = sum(part_ret), inv = sum(part_inv), dec = sum(part_dec), .groups = "drop") |>
    left_join(indicateurs_secret(d, ".g"), by = ".g")
  if (!is.null(niveaux)) {
    manquants <- setdiff(as.character(niveaux), agg$.g)
    if (length(manquants)) agg <- bind_rows(agg, tibble::tibble(.g = manquants, n = 0L, central = 0, bas = 0, haut = 0, ret = 0, inv = 0, dec = 0, n_entreprises = 0L, part_dominante_pct = 100))
    agg <- agg[order(match(agg$.g, as.character(niveaux))), ]
  }
  m <- masquer_cellules(agg$n, agg$n_entreprises, agg$part_dominante_pct, regles)
  out <- .df_ana(g = agg$.g, `Salariés` = ifelse(m, NA_integer_, agg$n),
                 `Départs attendus (central)` = ifelse(m, NA, .n_ana(agg$central)), `Hypothèse basse (départs retardés)` = ifelse(m, NA, .n_ana(agg$bas)), `Hypothèse haute (départs avancés)` = ifelse(m, NA, .n_ana(agg$haut)),
                 `Part susceptible de partir (%)` = ifelse(m | agg$n == 0, NA, .t_ana(100 * agg$central / pmax(agg$n, 1))),
                 `dont retraite ou fin de carrière` = ifelse(m, NA, .n_ana(agg$ret)), `dont invalidité` = ifelse(m, NA, .n_ana(agg$inv)), `dont décès` = ifelse(m, NA, .n_ana(agg$dec)),
                 `Poids dans l’ensemble des départs (%)` = ifelse(m | is.na(total_central) | total_central <= 0, NA, .t_ana(100 * agg$central / total_central)),
                 `Diffusé` = ifelse(m, .nd_analyse, "oui"))
  names(out)[1] <- libelle; rownames(out) <- NULL
  out
}

# Volume par grande catégorie sociale : table de DIFFUSION France du 08d (secret déjà appliqué)
table_volume_cs1 <- function(dif_fr, total_central = NA_real_, libelles = get0("LIBELLES_CS_CARTE", ifnotfound = NULL), ordre = unname(get0("PCS_VERS_CS1", ifnotfound = NULL))) {
  if (!"masque" %in% names(dif_fr)) stop("table_volume_cs1 : table sans colonne masque — seule une table de DIFFUSION est admise.")
  d <- dif_fr |> mutate(cs1 = as.character(cs1)); if (length(ordre)) d <- d[order(match(d$cs1, ordre)), ]
  m <- d$masque %in% TRUE; lib <- if (is.null(libelles)) d$cs1 else ifelse(is.na(libelles[d$cs1]), d$cs1, unname(libelles[d$cs1]))
  col <- function(v) if (v %in% names(d)) d[[v]] else rep(NA_real_, nrow(d))
  .df_ana(`Catégorie sociale` = lib, `Salariés` = ifelse(m, NA, as.integer(d$effectif_champ)),
          `Départs attendus (central)` = ifelse(m, NA, .n_ana(d$departs_central)), `Hypothèse basse (départs retardés)` = ifelse(m, NA, .n_ana(d$departs_bas)), `Hypothèse haute (départs avancés)` = ifelse(m, NA, .n_ana(d$departs_haut)),
          `Part susceptible de partir (%)` = ifelse(m, NA, .t_ana(d$taux_depart_central_pct)),
          `dont retraite ou fin de carrière` = ifelse(m, NA, .n_ana(col("dep_retraite"))), `dont invalidité` = ifelse(m, NA, .n_ana(col("dep_invalidite"))), `dont décès` = ifelse(m, NA, .n_ana(col("dep_deces"))),
          `Poids dans l’ensemble des départs (%)` = ifelse(m | is.na(total_central) | total_central <= 0, NA, .t_ana(100 * d$departs_central / total_central)),
          `Diffusé` = ifelse(m, .nd_analyse, "oui"))
}

# --- 3. Répartition par métier (SPEC-ANA-004) ------------------------------------------------
# Classement des métiers diffusés : par volume (départs) ou par part (métiers d'au moins effectif_min salariés)
table_metiers_classement <- function(dif_fr_pcs, critere = c("volume", "part"), libelles = NULL, top_n = 15L, effectif_min = 50L, total_central = NA_real_,
                                     libelles_cs = get0("LIBELLES_CS_CARTE", ifnotfound = NULL)) {
  critere <- match.arg(critere)
  if (!"masque" %in% names(dif_fr_pcs)) stop("table_metiers_classement : table sans colonne masque — seule une table de DIFFUSION est admise.")
  d <- dif_fr_pcs |> filter(!(masque %in% TRUE)) |> mutate(pcs = as.character(pcs))
  if (!"cs1" %in% names(d)) d$cs1 <- NA_character_
  if (critere == "part") d <- d |> filter(effectif_champ >= effectif_min)
  d <- if (critere == "volume") d |> arrange(desc(departs_central), pcs) else d |> arrange(desc(taux_depart_central_pct), pcs)
  d <- head(d, top_n)
  lib <- if (is.null(libelles) || nrow(d) == 0) rep(NA_character_, nrow(d)) else unname(libelles[normaliser_code_pcs(d$pcs)])
  cs <- as.character(d$cs1); if (!is.null(libelles_cs)) cs <- ifelse(is.na(libelles_cs[cs]), cs, unname(libelles_cs[cs]))
  out <- .df_ana(Rang = seq_len(nrow(d)), `Code PCS` = d$pcs, `Métier` = ifelse(is.na(lib), paste0("PCS ", d$pcs), lib), `Catégorie sociale` = cs,
                 `Salariés` = as.integer(d$effectif_champ), `Départs attendus (central)` = .n_ana(d$departs_central), `Hypothèse basse (départs retardés)` = .n_ana(d$departs_bas), `Hypothèse haute (départs avancés)` = .n_ana(d$departs_haut),
                 `Part susceptible de partir (%)` = .t_ana(d$taux_depart_central_pct),
                 `Poids dans l’ensemble des départs (%)` = if (is.na(total_central) || total_central <= 0) rep(NA_real_, nrow(d)) else .t_ana(100 * d$departs_central / total_central))
  attr(out, "critere") <- critere; attr(out, "effectif_min") <- if (critere == "part") effectif_min else NA
  out
}

# Métiers de la liste : matrice du 08i (matrice_metiers_tension()$metiers) ; situations dans l'ordre des quadrants
table_metiers_liste_analyse <- function(mat_metiers) {
  m <- mat_metiers; ordre <- unname(QUADRANTS_TENSION)
  m <- m |> mutate(.o = ifelse(statut == "diffusé", match(quadrant, ordre), length(ordre) + 1L),
                   .t = ifelse(is.na(taux_depart_central_pct), -Inf, taux_depart_central_pct)) |> arrange(.o, desc(.t), pcs_source)
  .df_ana(Situation = ifelse(m$statut == "diffusé", m$quadrant, m$statut),
          `Métier` = ifelse(is.na(m$pcs_libelle) | m$pcs_libelle == "", paste0("PCS ", m$pcs_source), m$pcs_libelle), `Code PCS` = m$pcs_source,
          `Famille professionnelle` = ifelse(is.na(m$fap_libelle), "n.d.", m$fap_libelle), `En tension en France` = .oui_non_ana(m$en_tension),
          `Levier d’action` = ifelse(m$en_tension %in% TRUE, m$levier, NA_character_),
          `Dimension principale de la tension` = ifelse(m$en_tension %in% TRUE, .colc_ana(m, "composante_libelle"), NA_character_),
          `Confiance du rattachement PCS → FAP` = ifelse(m$faible_confiance %in% TRUE, "faible confiance", "confiance normale"),
          `Départs attendus (central)` = ifelse(m$statut == "diffusé", .n_ana(m$departs_central), NA), `Part susceptible de partir (%)` = ifelse(m$statut == "diffusé", .t_ana(m$taux_depart_central_pct), NA))
}
table_noyau_dur_analyse <- function(tab_liste) tab_liste[tab_liste$Situation == QUADRANTS_TENSION[["critique"]], , drop = FALSE] |> (\(x) { rownames(x) <- NULL; x })()

# --- 4. Répartition géographique (SPEC-ANA-005) ----------------------------------------------
# Un territoire est totalisé sur ses catégories sociales seulement si aucune cellule n'est masquée (règle du 08h).
table_territoires_cs1 <- function(dif, niveau = c("région", "département")) {
  niveau <- match.arg(niveau)
  if (!"masque" %in% names(dif)) stop("table_territoires_cs1 : table sans colonne masque — seule une table de DIFFUSION est admise.")
  code <- if (niveau == "région") "region_code" else "geo_code"; nom <- if (niveau == "région") "region_nom" else "geo_nom"
  if (!all(c(code, nom) %in% names(dif))) stop("table_territoires_cs1 : colonnes ", code, " / ", nom, " absentes.")
  d <- dif |> mutate(.code = as.character(.data[[code]]), .nom = as.character(.data[[nom]])) |> filter(!.code %in% c("inconnu", ""), !is.na(.code))
  agg <- d |> group_by(.code, .nom) |> summarise(n_masque = sum(masque %in% TRUE), eff = sum(effectif_champ, na.rm = TRUE), c = sum(departs_central, na.rm = TRUE),
                                               b = sum(departs_bas, na.rm = TRUE), h = sum(departs_haut, na.rm = TRUE), .groups = "drop")
  ok <- agg$n_masque == 0 & agg$eff > 0
  part <- ifelse(ok, 100 * agg$c / pmax(agg$eff, 1), NA_real_); vol <- ifelse(ok, agg$c, NA_real_)
  out <- .df_ana(Code = agg$.code, Territoire = agg$.nom, `Salariés` = ifelse(ok, as.integer(agg$eff), NA_integer_),
                 `Départs attendus (central)` = ifelse(ok, .n_ana(agg$c), NA), `Hypothèse basse (départs retardés)` = ifelse(ok, .n_ana(agg$b), NA), `Hypothèse haute (départs avancés)` = ifelse(ok, .n_ana(agg$h), NA),
                 `Part susceptible de partir (%)` = .t_ana(part), `Rang par nombre de départs` = .rang_ana(vol), `Rang par part susceptible de partir` = .rang_ana(part),
                 `Diffusé` = ifelse(ok, "oui", .nd_analyse))
  names(out)[2] <- if (niveau == "région") "Région" else "Département"
  out <- out[order(-ifelse(is.na(part), -Inf, part), out$Code), ]; rownames(out) <- NULL
  out
}

# Enrichissement par la typologie (profil) et l'exposition (classe, robustesse, part, plancher) ; `Code` = clé
enrichir_territoires_analyse <- function(tab, typo_dif = NULL, expo_dif = NULL) {
  if (!is.null(typo_dif) && all(c("geo_code", "profil_typologie") %in% names(typo_dif)))
    tab[["Profil (typologie)"]] <- typo_dif$profil_typologie[match(tab$Code, as.character(typo_dif$geo_code))]
  if (!is.null(expo_dif) && all(c("geo_code", "tension_diffusee", "tension_classe") %in% names(expo_dif))) {
    i <- match(tab$Code, as.character(expo_dif$geo_code)); ok <- expo_dif$tension_diffusee[i] %in% TRUE
    tab[["Classe d’exposition aux remplacements difficiles"]] <- ifelse(is.na(i), NA_character_, ifelse(ok, expo_dif$tension_classe[i], .nd_analyse))
    if ("tension_robustesse" %in% names(expo_dif)) tab[["Robustesse de la classe"]] <- ifelse(ok, expo_dif$tension_robustesse[i], NA_character_)
    if ("tension_exposition_pct" %in% names(expo_dif)) tab[["Part des remplacements difficiles (%)"]] <- ifelse(ok, .t_ana(expo_dif$tension_exposition_pct[i]), NA_real_)
    if ("tension_departs_min" %in% names(expo_dif)) tab[["Départs attendus dans les métiers suivis en tension (minimum)"]] <- ifelse(ok, .n_ana(expo_dif$tension_departs_min[i]), NA_real_)
  }
  tab
}

# Exposition aux remplacements difficiles par territoire (diffusion 08i, départements ou régions), par part décroissante
table_exposition_territoires_analyse <- function(expo_dif, niveau = c("région", "département")) {
  niveau <- match.arg(niveau)
  if (!all(c("geo_code", "geo_nom", "tension_diffusee", "tension_classe") %in% names(expo_dif))) stop("table_exposition_territoires_analyse : table d'exposition de DIFFUSION attendue (geo_code, geo_nom, tension_diffusee, tension_classe).")
  d <- expo_dif |> mutate(geo_code = as.character(geo_code)); ok <- d$tension_diffusee %in% TRUE
  col <- function(v) if (v %in% names(d)) d[[v]] else rep(NA_real_, nrow(d)); colc <- function(v) if (v %in% names(d)) as.character(d[[v]]) else rep(NA_character_, nrow(d))
  part <- ifelse(ok, col("tension_exposition_pct"), NA_real_)
  out <- .df_ana(Code = d$geo_code, Territoire = d$geo_nom, `Classe d’exposition aux remplacements difficiles` = ifelse(ok, as.character(d$tension_classe), .nd_analyse),
                 `Robustesse de la classe` = ifelse(ok, colc("tension_robustesse"), NA_character_), `Part des remplacements difficiles (%)` = .t_ana(part),
                 `France (%)` = .t_ana(ifelse(ok, col("tension_exposition_france_pct"), NA_real_)),
                 `Départs attendus dans les métiers suivis en tension (minimum)` = ifelse(ok, .n_ana(col("tension_departs_min")), NA_real_),
                 `Levier d’action` = ifelse(ok, colc("tension_levier"), NA_character_), `Rang par part des remplacements difficiles` = .rang_ana(part), `Diffusé` = ifelse(ok, "oui", .nd_analyse))
  names(out)[2] <- if (niveau == "région") "Région" else "Département"
  out <- out[order(-ifelse(is.na(part), -Inf, part), out$Code), ]; rownames(out) <- NULL
  out
}

# Métiers de la liste x régions « plus forte qu'en France », par écart décroissant (table du 08j)
table_liste_regions_analyse <- function(tab_08j, top_n = 15L) {
  d <- tab_08j |> filter(niveau == "région", diffuse, lecture == "plus forte qu’en France") |> arrange(desc(ecart_pts), ordre_liste, geo_code) |> head(top_n)
  .df_ana(`Métier` = ifelse(is.na(d$pcs_libelle) | d$pcs_libelle == "", paste0("PCS ", d$pcs), d$pcs_libelle), `Code PCS` = d$pcs, `Région` = paste0(d$geo_nom, " (", d$geo_code, ")"),
          `Salariés du métier` = as.integer(d$effectif_champ), `Départs attendus (central)` = .n_ana(d$departs_central),
          `Part susceptible de partir (%)` = .t_ana(d$taux_pct), `France (%)` = .t_ana(d$taux_france_pct), `Écart à la France (points)` = .t_ana(d$ecart_pts), Lecture = d$lecture)
}

# Priorités (SPEC-ANA-006) : noyau dur x premières régions diffusées (par part) x levier
table_priorites_analyse <- function(mat_metiers, tab_08j = NULL, n_regions = 3L) {
  cr <- mat_metiers |> filter(statut == "diffusé", quadrant == QUADRANTS_TENSION[["critique"]]) |> arrange(desc(taux_depart_central_pct), pcs_source)
  regs <- function(p) {
    vide <- rep(NA_character_, n_regions); if (is.null(tab_08j) || nrow(cr) == 0) return(vide)
    x <- tab_08j |> filter(normaliser_code_pcs(pcs) == normaliser_code_pcs(p), niveau == "région", diffuse) |> arrange(desc(taux_pct), geo_code) |> head(n_regions)
    r <- sprintf("%s (%s %%)", x$geo_nom, .f1_ana(x$taux_pct)); c(r, vide)[seq_len(n_regions)] }
  out <- .df_ana(`Métier` = ifelse(is.na(cr$pcs_libelle) | cr$pcs_libelle == "", paste0("PCS ", cr$pcs_source), cr$pcs_libelle), `Code PCS` = cr$pcs_source,
                 `En tension en France` = .oui_non_ana(cr$en_tension), `Dimension principale de la tension` = .colc_ana(cr, "composante_libelle"), `Levier d’action` = cr$levier,
                 `Part susceptible de partir en France (%)` = .t_ana(cr$taux_depart_central_pct), `Départs attendus en France (central)` = .n_ana(cr$departs_central))
  mat <- if (nrow(cr)) t(vapply(cr$pcs_source, regs, character(n_regions))) else matrix(character(0), 0, n_regions)
  for (k in seq_len(n_regions)) out[[sprintf("Région %d (part %%)", k)]] <- if (nrow(cr)) unname(mat[, k]) else character(0)
  out
}

# Seniors par territoire du zonage : synthèse diffusable du 08
table_synthese_seniors_analyse <- function(syn, age_senior = parametres_analyse()$age_senior) {
  if (!all(c("masque", "masque_55plus") %in% names(syn))) stop("table_synthese_seniors_analyse : synthèse sans colonnes masque — seule la synthèse DIFFUSABLE est admise.")
  s <- sprintf("%d ans et plus", age_senior); m <- syn$masque %in% TRUE; m55 <- syn$masque_55plus %in% TRUE
  col <- function(v) if (v %in% names(syn)) syn[[v]] else rep(NA_real_, nrow(syn))
  out <- .df_ana(Code = as.character(syn$geo_code), Territoire = syn$geo_nom, `Salariés` = ifelse(m, NA, as.integer(col("effectif_champ"))),
                 s1 = ifelse(m55, NA, as.integer(col("effectif_55plus"))), s2 = ifelse(m55, NA, .t_ana(col("part_55plus_pct"))),
                 s3 = ifelse(m55, NA, .n_ana(col("departs_55plus"))), s4 = ifelse(m55, NA, .t_ana(col("taux_depart_55plus_pct"))),
                 `Diffusé` = ifelse(m, .nd_analyse, ifelse(m55, sprintf("partiellement (%s non diffusés)", s), "oui")))
  names(out)[4:7] <- c(sprintf("Salariés de %s", s), sprintf("Part des %s (%%)", s), sprintf("Départs des %s (central)", s), sprintf("Part susceptible de partir, %s (%%)", s))
  out
}

# --- Définitions des colonnes (SPEC-ANA-007) : une feuille lue avant les tableaux --------------------------------
table_definitions_analyse <- function(params = parametres_analyse()) {
  s <- sprintf("%d ans et plus", params$age_senior); lib <- if (exists("lib_liste")) lib_liste() else "métiers de la liste"
  seuil <- .f1_ana(get0("TENSION_ECART_POINTS", ifnotfound = NA_real_)); rang <- get0("TENSION_SEUIL_RANG", ifnotfound = NA); rmax <- get0("TENSION_RANG_MAX", ifnotfound = NA)
  d <- function(t, def, f) .df_ana(Terme = t, `Définition` = def, Feuilles = f)
  rbind(
    d("Salariés (du champ, du métier, de la catégorie)", sprintf(texte_perimetre("Nombre de salariés présents à la date de la photo BTS (%d) dans le champ : entreprises du périmètre Base, âge minimum de la configuration. C’est le dénominateur de la part susceptible de partir."), params$annee_bts), "toutes"),
    d("Départs attendus (central)", sprintf("Nombre de salariés qui devraient avoir quitté définitivement l’emploi entre %d et %d (retraite ou fin de carrière, invalidité, décès). C’est une espérance : la somme des probabilités individuelles de départ, pas un décompte de personnes identifiées. Les mobilités vers un autre employeur ne sont pas comptées : le nombre est un minimum.", params$annee_bts, params$fin), "2, 3, 4"),
    d("Hypothèse basse (départs retardés) / Hypothèse haute (départs avancés)", "Même nombre de départs sous deux hypothèses de calendrier : la basse décale les âges de départ plus tard (moins de départs), la haute plus tôt (plus de départs). Le scénario central est entre les deux. Elles encadrent le résultat, elles ne sont pas des bornes d’erreur statistiques.", "2, 3, 4"),
    d("Part susceptible de partir (%)", "Départs attendus divisés par les salariés du groupe (métier, catégorie, territoire), en %. Elle mesure la vitesse d’érosion d’un groupe, indépendamment de sa taille.", "2, 3, 4, 5"),
    d("Départs par an en moyenne", sprintf("Départs attendus divisés par la durée de la plage (%d ans). Un ordre de grandeur à comparer à la capacité annuelle de recrutement.", params$horizon), "2a"),
    d("dont retraite ou fin de carrière / dont invalidité / dont décès", "Répartition des départs attendus entre les trois causes de sortie, au scénario central. La répartition est additive : la somme des trois redonne les départs attendus.", "2"),
    d("Poids dans l’ensemble des départs (%)", "Part des départs attendus du groupe dans tous les départs attendus du champ. Mesure ce que pèse le groupe dans l’effort de remplacement, indépendamment de sa vitesse d’érosion.", "2, 3"),
    d("Rang par nombre de départs / Rang par part susceptible de partir", "Position du territoire parmi les territoires diffusés, 1 = le plus élevé. Deux classements distincts : par volume (combien il faut remplacer) et par part (à quelle vitesse). Un territoire peut être en tête de l’un et loin dans l’autre.", "4a, 4b"),
    d("Diffusé", "« oui » quand la cellule respecte la règle de secret statistique ; « non diffusé (secret statistique) » sinon : ses valeurs sont vides, jamais zéro. Un territoire non diffusé n’est pas un territoire sans départ.", "toutes"),
    d(sprintf("Seniors (%s)", s), sprintf("Salariés âgés de %s à la date de la photo BTS. La part des seniors mesure le vieillissement du groupe ; leurs départs sont les plus proches dans le temps.", s), "2c, 4e"),
    d("Code PCS / Métier", "Métier au sens de la nomenclature des professions et catégories socioprofessionnelles (PCS), à quatre caractères. Le libellé vient de la nomenclature officielle ; à défaut, « PCS <code> ».", "3, 4d, 5"),
    d("Catégorie sociale", "Grande catégorie de la PCS : cadres, professions intermédiaires, employés, ouvriers.", "2d, 3"),
    d("Famille professionnelle (FAP)", "Regroupement de métiers utilisé par France Travail pour mesurer la tension. Chaque métier de la liste est rattaché à une famille ; c’est à ce niveau que la tension est connue.", "3c, 3d"),
    d("En tension en France", sprintf("« oui » si la famille professionnelle du métier a un rang de tension d’au moins %s sur %s en France (indicateur France Travail / Dares). La tension est une propriété nationale du métier : elle ne mesure jamais la tension d’un territoire. « inconnue » quand l’indicateur manque.", rang, rmax), "3c, 3d, 5"),
    d("Dimension principale de la tension", "Parmi les sept composantes de la tension mesurées par France Travail (manque de main-d’œuvre disponible, formation spécifique au métier, conditions de travail contraignantes, faible attractivité salariale, inadéquation géographique, emplois peu durables, forte intensité d’embauche), celle qui pèse le plus pour la famille professionnelle du métier. Renseignée seulement pour un métier en tension.", "3c, 3d, 5"),
    d("Levier d’action", "Traduction opérationnelle de la dimension principale de la tension (par exemple « former tôt » pour une formation spécifique au métier). Il décrit la nature dominante de la difficulté de recrutement ; ce n’est pas une recommandation.", "3c, 3d, 4c, 5"),
    d("Confiance du rattachement PCS → FAP", "Qualité du lien entre le métier (PCS) et sa famille professionnelle (FAP), indiquée dans le classeur de la liste. « faible confiance » signale un rattachement incertain : la tension attribuée au métier est à lire avec réserve.", "3c, 3d"),
    d("Situation", "Croisement de la vitesse des départs du métier (rapides ou modérés, par rapport aux autres métiers de la liste) et de sa tension en France (en tension ou hors tension). Quatre situations ; « départs rapides et métier en tension » forme le noyau dur.", "3c, 3d"),
    d("Noyau dur", "Les métiers de la liste en situation « départs rapides et métier en tension » : ceux où l’anticipation compte le plus, puisque les départs y sont rapides et les candidats difficiles à trouver.", "3d, 5"),
    d("Profil (typologie)", "Profil du département dans la typologie territoriale, qui croise le poids du département dans l’emploi du champ, le volume de ses départs et leur intensité (pôle majeur à renouveler rapidement, pôle majeur au renouvellement modéré, implantation stable, implantation réduite, situation à expertiser).", "4b"),
    d("Classe d’exposition aux remplacements difficiles", "Sur 100 départs attendus dans les métiers de la liste du territoire, part de ceux qui concernent des métiers en tension en France, classée faible, modérée ou forte. « non diffusé » quand le secret ou une couverture insuffisante empêche la lecture.", "4a, 4b, 4c"),
    d("Part des remplacements difficiles (%)", "La même part, en valeur : départs attendus dans les métiers de la liste en tension divisés par les départs attendus dans tous les métiers de la liste du territoire. « France (%) » donne le repère national.", "4a, 4b, 4c"),
    d("Robustesse de la classe", "« robuste » si la classe d’exposition ne change pas quand on fait varier les hypothèses (calendrier de départ, seuil de tension, rattachements de faible confiance) ; « à confirmer » sinon.", "4a, 4b, 4c"),
    d("Départs attendus dans les métiers suivis en tension (minimum)", sprintf("Nombre de départs attendus dans les %s en tension du territoire. C’est un minimum : les métiers hors liste ne sont pas observés sur cette dimension.", lib), "4a, 4b, 4c"),
    d("Écart à la France (points) / Lecture", sprintf("Différence, en points, entre la part susceptible de partir du métier dans le territoire et la même part en France. « plus forte qu’en France » à partir de %s points, « plus faible » en deçà de l’opposé, « proche de la France » entre les deux.", seuil), "4d"),
    d("Région 1, 2, 3 (part %)", "Pour un métier du noyau dur, ses premières régions diffusées classées par part susceptible de partir décroissante, avec cette part entre parenthèses.", "5"))
}

# --- Contrôles (SPEC-ANA-008) : aucun effectif visible sous le seuil ; aucune chaîne « stratégique » -------------
controler_tables_analyse <- function(tables, min_salaries = SECRET_MIN_SALARIES) {
  for (nom in names(tables)) { t <- tables[[nom]]; if (is.null(t) || !is.data.frame(t)) next
    for (v in grep("^Salariés", names(t), value = TRUE)) { x <- suppressWarnings(as.numeric(t[[v]])); if (any(!is.na(x) & x > 0 & x < min_salaries)) stop("analyse : effectif visible sous le seuil de secret dans « ", nom, " » (", v, ").") }
    txt <- c(names(t), unlist(lapply(t, function(c) if (is.character(c)) c else character(0))))
    if (any(grepl("stratégique", txt))) stop("analyse : libellé non public (« stratégique ») dans « ", nom, " ».") }
  invisible(TRUE)
}

# --- Classeur (SPEC-ANA-007) : une feuille par tableau, titre, note, prêt à coller dans Word ---------------------
# feuilles : liste nommée (nom de feuille <= 31 caractères) de list(titre, note, table) ; lisez_moi : data.frame(texte) + attr gras
ecrire_classeur_analyse <- function(feuilles, fichier, lisez_moi = NULL, parametres = NULL, moteur = c("auto", "openxlsx", "writexl"), definitions = NULL) {
  moteur <- match.arg(moteur); dir.create(dirname(fichier), showWarnings = FALSE, recursive = TRUE)
  tab_de <- function(f) if (is.null(f$table) || !is.data.frame(f$table) || nrow(f$table) == 0) .df_ana(Note = if (!is.null(f$raison)) f$raison else "Aucune ligne.") else f$table
  if (moteur != "writexl" && requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    st_ent <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#1D4E89", textDecoration = "bold", valign = "center", halign = "center", wrapText = TRUE, border = "TopBottomLeftRight", borderColour = "#B7C4D6")
    st_cell <- openxlsx::createStyle(border = "TopBottomLeftRight", borderColour = "#D9DEE5", valign = "top", wrapText = TRUE)
    st_titre <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold", fontColour = "#1D4E89"); st_note <- openxlsx::createStyle(fontSize = 10, textDecoration = "italic", fontColour = "#52606D")
    st_pct <- openxlsx::createStyle(numFmt = "0.0", halign = "right"); st_pers <- openxlsx::createStyle(numFmt = "#,##0", halign = "right"); st_gras <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold")
    st_wrap <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
    largeur <- function(t) pmin(55, pmax(9, vapply(seq_along(t), function(j) max(nchar(c(names(t)[j], as.character(t[[j]]))), na.rm = TRUE) * 0.85 + 2, 0)))
    if (!is.null(lisez_moi)) {
      openxlsx::addWorksheet(wb, "Lisez-moi", gridLines = FALSE); openxlsx::writeData(wb, "Lisez-moi", lisez_moi, colNames = FALSE); openxlsx::setColWidths(wb, "Lisez-moi", 1, 120)
      openxlsx::addStyle(wb, "Lisez-moi", st_wrap, rows = seq_len(nrow(lisez_moi)), cols = 1)
      g <- which(attr(lisez_moi, "gras") %in% TRUE); if (length(g)) openxlsx::addStyle(wb, "Lisez-moi", st_gras, rows = g, cols = 1, stack = TRUE)
      openxlsx::addStyle(wb, "Lisez-moi", st_titre, rows = 1, cols = 1, stack = TRUE)
    }
    if (!is.null(definitions)) {
      openxlsx::addWorksheet(wb, "Définitions", gridLines = FALSE); openxlsx::writeData(wb, "Définitions", definitions, headerStyle = st_ent); openxlsx::freezePane(wb, "Définitions", firstRow = TRUE)
      openxlsx::setColWidths(wb, "Définitions", seq_along(definitions), c(38, 100, 30)[seq_along(definitions)]); openxlsx::addStyle(wb, "Définitions", st_wrap, rows = 1 + seq_len(nrow(definitions)), cols = seq_along(definitions), gridExpand = TRUE)
    }
    for (nom in names(feuilles)) {
      f <- feuilles[[nom]]; t <- tab_de(f); openxlsx::addWorksheet(wb, nom, gridLines = FALSE)
      openxlsx::writeData(wb, nom, f$titre, startRow = 1, colNames = FALSE); openxlsx::addStyle(wb, nom, st_titre, rows = 1, cols = 1)
      openxlsx::writeData(wb, nom, if (is.null(f$note)) "" else f$note, startRow = 2, colNames = FALSE); openxlsx::addStyle(wb, nom, st_note, rows = 2, cols = 1)
      openxlsx::writeData(wb, nom, t, startRow = 4, headerStyle = st_ent)
      openxlsx::freezePane(wb, nom, firstActiveRow = 5); openxlsx::setRowHeights(wb, nom, 4, 36)
      l <- largeur(t); l[grepl("^Hypothèse$|^Valeur retenue|^Effet|^Métier$|^Territoire|^Région$|^Département$|^Famille", names(t))] <- pmax(l[grepl("^Hypothèse$|^Valeur retenue|^Effet|^Métier$|^Territoire|^Région$|^Département$|^Famille", names(t))], 26)
      l[names(t) %in% c("Valeur retenue", "Effet sur le résultat")] <- 55; openxlsx::setColWidths(wb, nom, seq_along(t), l)
      lignes <- 4 + seq_len(nrow(t)); openxlsx::addStyle(wb, nom, st_cell, rows = lignes, cols = seq_along(t), gridExpand = TRUE)
      j <- grep("\\(%\\)|\\(points\\)$", names(t)); if (length(j)) openxlsx::addStyle(wb, nom, st_pct, rows = lignes, cols = j, gridExpand = TRUE, stack = TRUE)
      j <- grep("^Salariés|^Départs|^Hypothèse basse|^Hypothèse haute|^dont |minimum\\)$|^Rang", names(t)); if (length(j)) openxlsx::addStyle(wb, nom, st_pers, rows = lignes, cols = j, gridExpand = TRUE, stack = TRUE)
      if (nom == "2a Volume" && ncol(t) >= 4) { j <- which(t$Indicateur == "Part susceptible de partir (%)"); if (length(j)) openxlsx::addStyle(wb, nom, st_pct, rows = 4 + j, cols = 2:4, gridExpand = TRUE, stack = TRUE) }
      if ("Situation" %in% names(t) && exists("COULEURS_SITUATIONS") && exists("QUADRANTS_TENSION")) {
        teinte <- unname(COULEURS_SITUATIONS[names(QUADRANTS_TENSION)[match(t$Situation, QUADRANTS_TENSION)]])
        for (coul in unique(teinte[!is.na(teinte)])) openxlsx::addStyle(wb, nom, openxlsx::createStyle(fgFill = coul), rows = lignes[teinte %in% coul], cols = seq_along(t), gridExpand = TRUE, stack = TRUE) }
      cl <- grep("^Classe d’exposition", names(t)); if (length(cl) && exists("COULEURS_CLASSES_EXPOSITION")) {
        teinte <- unname(COULEURS_CLASSES_EXPOSITION[t[[cl[1]]]])
        for (coul in unique(teinte[!is.na(teinte)])) openxlsx::addStyle(wb, nom, openxlsx::createStyle(fgFill = coul), rows = lignes[teinte %in% coul], cols = cl[1], gridExpand = TRUE, stack = TRUE) }
    }
    if (!is.null(parametres)) { openxlsx::addWorksheet(wb, "Paramètres"); openxlsx::writeData(wb, "Paramètres", parametres, headerStyle = st_ent); openxlsx::setColWidths(wb, "Paramètres", 1:2, c(34, 60)) }
    openxlsx::saveWorkbook(wb, fichier, overwrite = TRUE)
  } else if (moteur != "openxlsx" && requireNamespace("writexl", quietly = TRUE)) {
    message("ecrire_classeur_analyse : classeur écrit sans mise en forme (writexl", if (moteur == "auto") ", paquet openxlsx absent" else "", ").")
    t <- lapply(feuilles, function(f) as.data.frame(tab_de(f), stringsAsFactors = FALSE))
    if (!is.null(definitions)) t <- c(list("Définitions" = as.data.frame(definitions, stringsAsFactors = FALSE)), t)
    if (!is.null(lisez_moi)) t <- c(list("Lisez-moi" = .df_ana(texte = as.character(lisez_moi[[1]]))), t)
    if (!is.null(parametres)) t[["Paramètres"]] <- as.data.frame(parametres, stringsAsFactors = FALSE)
    writexl::write_xlsx(t, fichier)
  } else {
    message("ecrire_classeur_analyse : ni openxlsx ni writexl : classeur non produit (les CSV restent la référence).")
    return(invisible(NA_character_))
  }
  invisible(fichier)
}
