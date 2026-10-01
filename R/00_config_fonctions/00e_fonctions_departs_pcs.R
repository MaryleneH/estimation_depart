# ==============================================================================
# 00e_fonctions_departs_pcs.R — Départs attendus par PCS FINE et par grande CS
#                               (fonctions pures ; nom du fichier historique)
# ------------------------------------------------------------------------------
# Rôle : restituer les résultats EXISTANTS du modèle selon une DIMENSION de
#        restitution (pcs fine pour 08c, cs1 grande CS pour 08d) et trois
#        mailles géographiques, sans aucune nouvelle formule :
#          - départs = somme des probabilités individuelles (comme 05, 07, 08) ;
#          - taux = 100 x départs / effectif (comme 05 : part_departs_pct) ;
#          - retraite / invalidité / décès = ajouter_parts_causes() du 00d
#            (mêmes maths que 06 et 08 : risques concurrents).
#        pcs est une DIMENSION de restitution ; cs1 reste la CLÉ du modèle
#        (une personne en pcs 311D garde les paramètres de sa cs1 « Cadres »
#        et contribue à la ligne 311D).
# Contrat : base = bts_projete (+ region_code / region_nom via ajouter_region()).
#        UNE fonction, trois niveaux, deux dimensions :
#          calculer_departs(base, niveau, dimension = "pcs" | "cs1")
#        (calculer_departs_pcs() = enveloppe historique, dimension "pcs").
#        Une seule logique statistique, une seule logique de secret : la
#        dimension ne change que la colonne de regroupement et, pour le secret
#        secondaire, la liste des blocs dont la marge est publiée (blocs_secret).
# Secret : la table ANALYTIQUE reste complète (avec les indicateurs de secret
#        n_entreprises / part_dominante_pct, usage interne) ; appliquer_secret_pcs()
#        produit la table de DIFFUSION selon la règle Insee (00d : secret_primaire,
#        secret_secondaire ; blocs documentés dans le script 08c).
# Dépendances : dplyr, tibble ; indicateurs_secret(), secret_primaire(),
#        secret_secondaire() et ajouter_parts_causes() (00d).
# ==============================================================================
library(dplyr)

NIVEAUX_DEPARTS_PCS   <- c("france", "region", "departement")
DIMENSIONS_DEPARTS    <- c("pcs", "cs1")          # colonnes de bts_projete admises comme dimension
MESURES_DEPARTS_PCS   <- c("effectif_champ", "departs_central", "departs_bas", "departs_haut",
                           "dep_retraite", "dep_invalidite", "dep_deces")

# --- Base commune de 08c et 08d : TOUT le périmètre national, région rattachée --
# Jamais restreint (GEO_INTERET, FICHES_SELECTION ne s'appliquent pas) ; les
# niveaux région / département exigent geo_type = "departement" (sinon retirés,
# message). Retourne list(base, niveaux) ; France toujours présente.
preparer_base_departs <- function(bts_projete, niveaux, prefixe = "08c") {
  niveaux <- unique(niveaux)
  inconnus <- setdiff(niveaux, NIVEAUX_DEPARTS_PCS)
  if (length(inconnus) > 0)
    stop(prefixe, " : niveau(x) inconnu(s) : ", paste(inconnus, collapse = ", "),
         " (attendus : ", paste(NIVEAUX_DEPARTS_PCS, collapse = ", "), ").")
  base <- bts_projete |>
    mutate(geo_code = ifelse(is.na(geo_code), "inconnu", as.character(geo_code)),
           geo_nom  = ifelse(geo_code == "inconnu", "Territoire inconnu", as.character(geo_nom)))
  if (nrow(base) != nrow(bts_projete))
    stop(prefixe, " : la base France doit contenir TOUT bts_projete (aucune restriction territoriale).")
  geo_departemental <- identical(unique(as.character(bts_projete$geo_type)), "departement")
  if (!geo_departemental && any(niveaux %in% c("region", "departement"))) {
    message(prefixe, " : zonage d'analyse « ", paste(unique(bts_projete$geo_type), collapse = ","),
            " » : les niveaux région et département exigent GEO_ANALYSE = \"departement\" -> ignorés, France seule produite.")
    niveaux <- setdiff(niveaux, c("region", "departement"))
  }
  niveaux <- union("france", niveaux)     # France toujours calculée : référence des contrôles
  if (any(niveaux %in% c("region", "departement")))
    base <- ajouter_region(base, GEO_PASSAGES[["departement->region"]], prefixe = prefixe)
  list(base = base, niveaux = niveaux)
}

# Clés de regroupement d'un niveau. Le niveau département porte AUSSI la région
# quand elle est disponible : lecture plus simple, contrôles hiérarchiques et
# secret secondaire (bloc région x pcs) directs.
cles_niveau_pcs <- function(niveau, base) {
  niveau <- match.arg(niveau, NIVEAUX_DEPARTS_PCS)
  avec_region <- all(c("region_code", "region_nom") %in% names(base))
  switch(niveau,
         france      = character(0),
         region      = c("region_code", "region_nom"),
         departement = c(if (avec_region) c("region_code", "region_nom"), "geo_code", "geo_nom"))
}

# --- Agrégation générique : niveau x dimension (pcs ou cs1) -------------------
# dimension = "pcs" : table niveau x pcs, cs1 portée en clé (une pcs -> une cs1).
# dimension = "cs1" : table niveau x cs1 (grande CS), mêmes mesures, mêmes noms.
calculer_departs <- function(base, niveau = c("france", "region", "departement"),
                             dimension = c("pcs", "cs1")) {
  niveau <- match.arg(niveau); dimension <- match.arg(dimension)
  if (!exists("ajouter_parts_causes")) stop("Exécutez d'abord 00d_fonctions_fiches.R (ajouter_parts_causes).")
  cles <- cles_niveau_pcs(niveau, base)
  requis <- unique(c(cles, dimension, "cs1", "siren", "p_central", "p_bas", "p_haut", "p_cal_central", "p_inval", "p_deces"))
  manque <- setdiff(requis, names(base))
  if (length(manque) > 0)
    stop("calculer_departs(", niveau, ", ", dimension, ") : colonnes absentes : ", paste(manque, collapse = ", "),
         if (niveau == "region") " — appelez ajouter_region() d'abord." else ".")
  if (anyNA(base[[dimension]])) stop("calculer_departs : valeurs manquantes dans la dimension « ", dimension, " ».")
  if (dimension == "pcs") {
    # Une PCS fine appartient à UNE grande CS (règle du 01b) ; sinon la ligne pcs
    # mélangerait deux jeux de paramètres et la table serait ambiguë.
    amb <- base |> distinct(pcs, cs1) |> count(pcs) |> filter(n > 1)
    if (nrow(amb) > 0)
      stop("PCS rattachée(s) à plusieurs grandes CS : ", paste(amb$pcs, collapse = ", "))
  }
  grp <- c(cles, dimension)
  res <- base |>
    ajouter_parts_causes() |>
    group_by(across(all_of(grp))) |>
    summarise(effectif_champ   = n(),
              departs_central  = sum(p_central),
              departs_bas      = sum(p_bas),
              departs_haut     = sum(p_haut),
              dep_retraite     = sum(part_ret),
              dep_invalidite   = sum(part_inv),
              dep_deces        = sum(part_dec),
              .groups = "drop") |>
    mutate(taux_depart_central_pct = 100 * departs_central / effectif_champ) |>
    # indicateurs de secret de la cellule (règle Insee : entreprises, dominance)
    left_join(indicateurs_secret(base, grp), by = grp) |>
    arrange(across(all_of(grp)))
  if (dimension == "pcs")                  # cs1 = clé du modèle, juste après pcs
    res <- res |> left_join(distinct(base, pcs, cs1), by = "pcs") |> relocate(cs1, .after = pcs)
  res
}
# Enveloppe historique (08c, tests) : strictement calculer_departs(..., "pcs").
calculer_departs_pcs <- function(base, niveau = c("france", "region", "departement"))
  calculer_departs(base, match.arg(niveau), dimension = "pcs")

# --- Contrôles arithmétiques (sur les tables ANALYTIQUES, avant arrondi) ------
# Arrêt explicite au premier écart : ces égalités sont la preuve que la feature
# ne change que la granularité.
controler_departs <- function(tables, base, dimension = c("pcs", "cs1"), tol = 1e-6, prefixe = "08c") {
  dimension <- match.arg(dimension)
  b <- ajouter_parts_causes(base)
  attendu <- c(effectif_champ = nrow(b), departs_central = sum(b$p_central), departs_bas = sum(b$p_bas),
               departs_haut = sum(b$p_haut), dep_retraite = sum(b$part_ret),
               dep_invalidite = sum(b$part_inv), dep_deces = sum(b$part_dec))
  sommes <- function(t) colSums(as.matrix(t[, MESURES_DEPARTS_PCS]))
  verifier <- function(ok, quoi) if (!isTRUE(ok)) stop(prefixe, " : contrôle ÉCHOUÉ — ", quoi)
  ecart_max <- function(a, b) max(abs(a - b))
  # 1. chaque niveau retombe sur la base entière (périmètre national identique)
  for (n in names(tables))
    verifier(ecart_max(sommes(tables[[n]])[names(attendu)], attendu) < tol,
             paste0("total ", n, " x ", dimension, " différent de la base"))
  # 2. par dimension : Σ régions = France, Σ départements = France
  par_dim <- function(t) t |> group_by(across(all_of(dimension))) |>
    summarise(across(all_of(MESURES_DEPARTS_PCS), sum), .groups = "drop") |> arrange(across(all_of(dimension)))
  fr <- par_dim(tables$france)
  for (n in intersect(c("region", "departement"), names(tables))) {
    x <- par_dim(tables[[n]])
    verifier(identical(x[[dimension]], fr[[dimension]]) &&
               ecart_max(as.matrix(x[, MESURES_DEPARTS_PCS]), as.matrix(fr[, MESURES_DEPARTS_PCS])) < tol,
             paste0("Σ ", n, " par ", dimension, " ≠ France par ", dimension))
  }
  # 3. par région x dimension : Σ départements de la région = ligne région
  if (all(c("region", "departement") %in% names(tables)) && "region_code" %in% names(tables$departement)) {
    d <- tables$departement |> group_by(across(all_of(c("region_code", dimension)))) |>
      summarise(across(all_of(MESURES_DEPARTS_PCS), sum), .groups = "drop") |> arrange(across(all_of(c("region_code", dimension))))
    r <- tables$region |> select(all_of(c("region_code", dimension, MESURES_DEPARTS_PCS))) |> arrange(across(all_of(c("region_code", dimension))))
    verifier(nrow(d) == nrow(r) && identical(paste(d$region_code, d[[dimension]]), paste(r$region_code, r[[dimension]])) &&
               ecart_max(as.matrix(d[, MESURES_DEPARTS_PCS]), as.matrix(r[, MESURES_DEPARTS_PCS])) < tol,
             paste0("Σ départements d'une région x ", dimension, " ≠ région x ", dimension))
  }
  invisible(TRUE)
}
# Enveloppe historique (08c, tests).
controler_departs_pcs <- function(tables, base, tol = 1e-6) controler_departs(tables, base, "pcs", tol, "08c")

# --- Contrôle FONDAMENTAL cs1 vs pcs, niveau par niveau : la table par grande CS
#     est EXACTEMENT la table PCS fine regroupée par cs1 (7 mesures), avant
#     arrondi et avant secret. Les deux tables sortent de la même fonction : ce
#     contrôle prouve que la dimension ne change que la granularité. ----------
controler_cs1_vs_pcs <- function(tables_cs1, tables_pcs, tol = 1e-9, prefixe = "08d") {
  hors_cles <- c("pcs", "cs1", MESURES_DEPARTS_PCS, "taux_depart_central_pct", "n_entreprises", "part_dominante_pct")
  for (n in intersect(names(tables_cs1), names(tables_pcs))) {
    cles <- setdiff(names(tables_pcs[[n]]), hors_cles)
    grp  <- c(cles, "cs1")
    a <- tables_pcs[[n]] |> group_by(across(all_of(grp))) |>
      summarise(across(all_of(MESURES_DEPARTS_PCS), sum), .groups = "drop") |> arrange(across(all_of(grp)))
    b <- tables_cs1[[n]] |> select(all_of(c(grp, MESURES_DEPARTS_PCS))) |> arrange(across(all_of(grp)))
    ok <- nrow(a) == nrow(b) &&
      identical(do.call(paste, a[grp]), do.call(paste, b[grp])) &&
      max(abs(as.matrix(a[, MESURES_DEPARTS_PCS]) - as.matrix(b[, MESURES_DEPARTS_PCS]))) < tol
    if (!ok) stop(prefixe, " : contrôle ÉCHOUÉ — ", n, " : la table par cs1 n'est pas la table PCS regroupée par cs1.")
  }
  invisible(TRUE)
}

# --- Contrôle FONDAMENTAL PCS -> CS : regrouper les pcs par cs1 redonne EXACTEMENT
#     les résultats par grande CS (effectif, central, bas, haut, causes). -------
controler_pcs_vs_cs <- function(france, base, tol = 1e-9) {
  b <- ajouter_parts_causes(base)
  ref <- b |> group_by(cs1) |>
    summarise(effectif_champ = n(), departs_central = sum(p_central), departs_bas = sum(p_bas),
              departs_haut = sum(p_haut), dep_retraite = sum(part_ret), dep_invalidite = sum(part_inv),
              dep_deces = sum(part_dec), .groups = "drop") |> arrange(cs1)
  agg <- france |> group_by(cs1) |> summarise(across(all_of(MESURES_DEPARTS_PCS), sum), .groups = "drop") |> arrange(cs1)
  ok <- identical(agg$cs1, ref$cs1) &&
    max(abs(as.matrix(agg[, MESURES_DEPARTS_PCS]) - as.matrix(ref[, MESURES_DEPARTS_PCS]))) < tol
  if (!ok) stop("08c : contrôle ÉCHOUÉ — les pcs regroupées par cs1 ne redonnent pas les résultats par grande CS.")
  invisible(TRUE)
}

# --- Secret statistique : table de DIFFUSION --------------------------------
# Primaire : règle Insee (00d secret_primaire : salariés, entreprises, dominance).
# Secondaire (00d secret_secondaire) : SEULEMENT dans les blocs dont la marge
# est effectivement publiée (voir la règle dans 08c), et seulement quand un
# bloc ne contient qu'UNE cellule masquée (sinon rien n'est déductible).
# Les cellules masquées passent à NA (jamais 0) ; masque / motif_masque tracent.
# Les indicateurs n_entreprises / part_dominante_pct sont RETIRÉS de la
# diffusion (ils décriraient la structure des cellules masquées).
# Blocs du secret secondaire = ensembles de cellules dont la MARGE est publiée
# ailleurs. Ils dépendent de la dimension (les marges publiées diffèrent), pas
# la règle : la décision est toujours secret_primaire() puis secret_secondaire().
blocs_secret <- function(niveau, table, dimension = c("pcs", "cs1")) {
  dimension <- match.arg(dimension)
  avec_region <- "region_code" %in% names(table)
  if (dimension == "pcs")
    switch(niveau,
           france      = list(c("cs1")),                                        # marge : 07/07b (total par cs1)
           region      = list(c("pcs"), c("region_code", "cs1")),                # marges : France x pcs ; région x cs1 (Σ 08b)
           departement = list(if (avec_region) c("region_code", "pcs") else c("pcs"),   # marge : région x pcs (ou France x pcs)
                              c("geo_code", "cs1")))                            # marge : département x cs1 (08b)
  else
    switch(niveau,
           france      = list(character(0)),                                    # marge : total national (05, 07) ; bloc = la table
           region      = list(c("cs1"), c("region_code")),                      # marges : France x cs1 ; total régional (Σ 08 par département)
           departement = list(if (avec_region) c("region_code", "cs1") else c("cs1"),  # marge : région x cs1 (ou France x cs1)
                              c("geo_code")))                                   # marge : total départemental (08, fiches) = bloc du 08b
}
# Enveloppe historique (tests) : blocs de la dimension pcs.
blocs_secret_pcs <- function(niveau, table) blocs_secret(niveau, table, "pcs")

appliquer_secret_pcs <- function(table, niveau, regles = regles_secret(), max_iter = 20,
                                 dimension = c("pcs", "cs1")) {
  niveau <- match.arg(niveau, NIVEAUX_DEPARTS_PCS); dimension <- match.arg(dimension)
  if (!all(c("n_entreprises", "part_dominante_pct") %in% names(table)))
    stop("appliquer_secret_pcs : indicateurs de secret absents (table issue de calculer_departs ?).")
  t <- table |> mutate(masque = secret_primaire(effectif_champ, n_entreprises, part_dominante_pct, regles),
                       motif_masque = ifelse(masque, "primaire", NA_character_))
  for (bloc in blocs_secret(niveau, t, dimension)) {
    for (i in seq_len(max_iter)) {
      avant <- t$masque
      t <- t |> group_by(across(all_of(bloc))) |>
        mutate(masque = secret_secondaire(masque, effectif_champ)) |> ungroup()
      if (identical(avant, t$masque)) break
    }
  }
  # itération croisée : un masque secondaire posé dans un bloc peut isoler une
  # cellule dans l'autre ; on repasse tous les blocs jusqu'à stabilité
  for (i in seq_len(max_iter)) {
    avant <- t$masque
    for (bloc in blocs_secret(niveau, t, dimension))
      t <- t |> group_by(across(all_of(bloc))) |>
        mutate(masque = secret_secondaire(masque, effectif_champ)) |> ungroup()
    if (identical(avant, t$masque)) break
  }
  t |>
    mutate(motif_masque = ifelse(masque & is.na(motif_masque), "secondaire", motif_masque),
           across(all_of(c(MESURES_DEPARTS_PCS, "taux_depart_central_pct")), ~ ifelse(masque, NA_real_, .x)),
           effectif_champ = as.integer(effectif_champ)) |>
    select(-n_entreprises, -part_dominante_pct)
}

# --- Export : convention de restitution (00g) — personnes -> entier, causes
#     cohérentes avec departs_central (plus forts restes), taux -> 1 décimale ;
#     la table ANALYTIQUE en mémoire reste exacte. --------------------------------
arrondir_departs_pcs <- function(table) formater_restitution(table)
