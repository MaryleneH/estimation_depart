# ==============================================================================
# 00e_fonctions_departs_pcs.R — Départs attendus par PCS FINE (fonctions pures)
# ------------------------------------------------------------------------------
# Rôle : restituer les résultats EXISTANTS du modèle selon une nouvelle
#        granularité (pcs fine) et trois mailles géographiques, sans aucune
#        nouvelle formule :
#          - départs = somme des probabilités individuelles (comme 05, 07, 08) ;
#          - taux = 100 x départs / effectif (comme 05 : part_departs_pct) ;
#          - retraite / invalidité / décès = ajouter_parts_causes() du 00d
#            (mêmes maths que 06 et 08 : risques concurrents).
#        pcs est une DIMENSION de restitution ; cs1 reste la CLÉ du modèle
#        (une personne en pcs 311D garde les paramètres de sa cs1 « Cadres »
#        et contribue à la ligne 311D).
# Contrat : base = bts_projete (+ region_code / region_nom via ajouter_region()).
#        Une fonction générique, trois niveaux : calculer_departs_pcs(base, niveau).
# Secret : la table ANALYTIQUE reste complète (avec les indicateurs de secret
#        n_entreprises / part_dominante_pct, usage interne) ; appliquer_secret_pcs()
#        produit la table de DIFFUSION selon la règle Insee (00d : secret_primaire,
#        secret_secondaire ; blocs documentés dans le script 08c).
# Dépendances : dplyr, tibble ; indicateurs_secret(), secret_primaire(),
#        secret_secondaire() et ajouter_parts_causes() (00d).
# ==============================================================================
library(dplyr)

NIVEAUX_DEPARTS_PCS <- c("france", "region", "departement")
MESURES_DEPARTS_PCS <- c("effectif_champ", "departs_central", "departs_bas", "departs_haut",
                         "dep_retraite", "dep_invalidite", "dep_deces")

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

# --- Agrégation générique : niveau x pcs --------------------------------------
calculer_departs_pcs <- function(base, niveau = c("france", "region", "departement")) {
  niveau <- match.arg(niveau)
  if (!exists("ajouter_parts_causes")) stop("Exécutez d'abord 00d_fonctions_fiches.R (ajouter_parts_causes).")
  cles <- cles_niveau_pcs(niveau, base)
  requis <- c(cles, "pcs", "cs1", "siren", "p_central", "p_bas", "p_haut", "p_cal_central", "p_inval", "p_deces")
  manque <- setdiff(requis, names(base))
  if (length(manque) > 0)
    stop("calculer_departs_pcs(", niveau, ") : colonnes absentes : ", paste(manque, collapse = ", "),
         if (niveau == "region") " — appelez ajouter_region() d'abord." else ".")
  # Une PCS fine appartient à UNE grande CS (règle du 01b) ; sinon la ligne pcs
  # mélangerait deux jeux de paramètres et la table serait ambiguë.
  amb <- base |> distinct(pcs, cs1) |> count(pcs) |> filter(n > 1)
  if (nrow(amb) > 0)
    stop("PCS rattachée(s) à plusieurs grandes CS : ", paste(amb$pcs, collapse = ", "))
  base |>
    ajouter_parts_causes() |>
    group_by(across(all_of(c(cles, "pcs")))) |>
    summarise(cs1              = first(cs1),
              effectif_champ   = n(),
              departs_central  = sum(p_central),
              departs_bas      = sum(p_bas),
              departs_haut     = sum(p_haut),
              dep_retraite     = sum(part_ret),
              dep_invalidite   = sum(part_inv),
              dep_deces        = sum(part_dec),
              .groups = "drop") |>
    mutate(taux_depart_central_pct = 100 * departs_central / effectif_champ) |>
    # indicateurs de secret de la cellule (règle Insee : entreprises, dominance)
    left_join(indicateurs_secret(base, c(cles, "pcs")), by = c(cles, "pcs")) |>
    arrange(across(all_of(c(cles, "pcs"))))
}

# --- Contrôles arithmétiques (sur les tables ANALYTIQUES, avant arrondi) ------
# Arrêt explicite au premier écart : ces égalités sont la preuve que la feature
# ne change que la granularité.
controler_departs_pcs <- function(tables, base, tol = 1e-6) {
  b <- ajouter_parts_causes(base)
  attendu <- c(effectif_champ = nrow(b), departs_central = sum(b$p_central), departs_bas = sum(b$p_bas),
               departs_haut = sum(b$p_haut), dep_retraite = sum(b$part_ret),
               dep_invalidite = sum(b$part_inv), dep_deces = sum(b$part_dec))
  sommes <- function(t) colSums(as.matrix(t[, MESURES_DEPARTS_PCS]))
  verifier <- function(ok, quoi) if (!isTRUE(ok)) stop("08c : contrôle ÉCHOUÉ — ", quoi)
  ecart_max <- function(a, b) max(abs(a - b))
  # 1. chaque niveau retombe sur la base entière (périmètre national identique)
  for (n in names(tables))
    verifier(ecart_max(sommes(tables[[n]])[names(attendu)], attendu) < tol,
             paste0("total ", n, " x pcs différent de la base"))
  # 2. par pcs : Σ régions = France, Σ départements = France
  par_pcs <- function(t) t |> group_by(pcs) |> summarise(across(all_of(MESURES_DEPARTS_PCS), sum), .groups = "drop") |> arrange(pcs)
  fr <- par_pcs(tables$france)
  for (n in intersect(c("region", "departement"), names(tables))) {
    x <- par_pcs(tables[[n]])
    verifier(identical(x$pcs, fr$pcs) &&
               ecart_max(as.matrix(x[, MESURES_DEPARTS_PCS]), as.matrix(fr[, MESURES_DEPARTS_PCS])) < tol,
             paste0("Σ ", n, " par pcs ≠ France par pcs"))
  }
  # 3. par région x pcs : Σ départements de la région = ligne région
  if (all(c("region", "departement") %in% names(tables)) && "region_code" %in% names(tables$departement)) {
    d <- tables$departement |> group_by(region_code, pcs) |>
      summarise(across(all_of(MESURES_DEPARTS_PCS), sum), .groups = "drop") |> arrange(region_code, pcs)
    r <- tables$region |> select(region_code, pcs, all_of(MESURES_DEPARTS_PCS)) |> arrange(region_code, pcs)
    verifier(nrow(d) == nrow(r) && identical(paste(d$region_code, d$pcs), paste(r$region_code, r$pcs)) &&
               ecart_max(as.matrix(d[, MESURES_DEPARTS_PCS]), as.matrix(r[, MESURES_DEPARTS_PCS])) < tol,
             "Σ départements d'une région x pcs ≠ région x pcs")
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
blocs_secret_pcs <- function(niveau, table) {
  avec_region <- "region_code" %in% names(table)
  switch(niveau,
         france      = list(c("cs1")),                                        # marge : 07/07b (total par cs1)
         region      = list(c("pcs"), c("region_code", "cs1")),                # marges : France x pcs ; région x cs1 (Σ 08b)
         departement = list(if (avec_region) c("region_code", "pcs") else c("pcs"),   # marge : région x pcs (ou France x pcs)
                            c("geo_code", "cs1")))                            # marge : département x cs1 (08b)
}
appliquer_secret_pcs <- function(table, niveau, regles = regles_secret(), max_iter = 20) {
  niveau <- match.arg(niveau, NIVEAUX_DEPARTS_PCS)
  if (!all(c("n_entreprises", "part_dominante_pct") %in% names(table)))
    stop("appliquer_secret_pcs : indicateurs de secret absents (table issue de calculer_departs_pcs ?).")
  t <- table |> mutate(masque = secret_primaire(effectif_champ, n_entreprises, part_dominante_pct, regles),
                       motif_masque = ifelse(masque, "primaire", NA_character_))
  for (bloc in blocs_secret_pcs(niveau, t)) {
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
    for (bloc in blocs_secret_pcs(niveau, t))
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

# --- Export : arrondi 0,1 sur les mesures, conventions des autres CSV ---------
arrondir_departs_pcs <- function(table) {
  table |> mutate(across(any_of(c("departs_central", "departs_bas", "departs_haut", "dep_retraite",
                                  "dep_invalidite", "dep_deces", "taux_depart_central_pct",
                                  "part_dominante_pct")), ~ round(.x, 1)))
}
