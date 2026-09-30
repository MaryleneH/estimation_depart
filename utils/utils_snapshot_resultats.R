# ==============================================================================
# utils/utils_snapshot_resultats.R — Instantané numérique de la chaîne
# ------------------------------------------------------------------------------
# Rôle : figer un petit jeu de résultats (effectifs, SIREN, totaux par grande
#        CS, scénarios central/bas/haut, quelques territoires) pour prouver
#        qu'une réorganisation du code ne change AUCUN résultat.
# Usage :
#   source("utils/utils_snapshot_resultats.R")
#   snap <- snapshot_chaine(environment())     # après main.R, dans la session
#   comparer_snapshots(reference, snap)        # écart max par clé, 0 attendu
# Format : table longue  cle;valeur  (valeurs arrondies à 6 décimales).
# ==============================================================================
snapshot_chaine <- function(env = globalenv(), n_territoires = 10) {
  g <- function(nom) get(nom, envir = env)
  b <- g("bts_projete")
  cle <- character(0); val <- numeric(0)
  add <- function(k, v) { cle <<- c(cle, k); val <<- c(val, round(as.numeric(v), 6)) }
  add("n_bts", nrow(g("bts")))
  add("n_bts_projete", nrow(b))
  add("n_siren", dplyr::n_distinct(b$siren))
  add("n_55plus", sum(b$age_2024 >= g("AGE_SENIOR")))
  add("departs_central", sum(b$p_central)); add("departs_bas", sum(b$p_bas)); add("departs_haut", sum(b$p_haut))
  add("departs_55plus_central", sum(b$p_central[b$age_2024 >= g("AGE_SENIOR")]))
  par_cs <- b |> dplyr::group_by(cs1) |>
    dplyr::summarise(n = dplyr::n(), c = sum(p_central), bas = sum(p_bas), haut = sum(p_haut), .groups = "drop") |>
    dplyr::arrange(cs1)
  for (i in seq_len(nrow(par_cs))) {
    add(paste0("cs1_", par_cs$cs1[i], "_effectif"), par_cs$n[i])
    add(paste0("cs1_", par_cs$cs1[i], "_central"), par_cs$c[i])
    add(paste0("cs1_", par_cs$cs1[i], "_bas"), par_cs$bas[i])
    add(paste0("cs1_", par_cs$cs1[i], "_haut"), par_cs$haut[i])
  }
  if (exists("synthese_geo", envir = env)) {
    sg <- g("synthese_geo") |> dplyr::arrange(geo_code) |> head(n_territoires)
    for (i in seq_len(nrow(sg))) {
      add(paste0("geo_", sg$geo_code[i], "_effectif_champ"), sg$effectif_champ[i])
      add(paste0("geo_", sg$geo_code[i], "_departs_55plus"), sg$departs_55plus[i])
    }
    add("n_territoires", nrow(g("synthese_geo")))
  }
  if (exists("departs_geo_cs", envir = env))
    add("08b_departs_total", sum(g("departs_geo_cs")$departs_2030, na.rm = TRUE))
  if (exists("journal_fiches", envir = env))
    add("09_fiches_ok", sum(g("journal_fiches")$statut == "ok"))
  data.frame(cle = cle, valeur = val, stringsAsFactors = FALSE)
}

comparer_snapshots <- function(reference, nouveau, tolerance = 1e-6) {
  m <- merge(reference, nouveau, by = "cle", all = TRUE, suffixes = c("_ref", "_new"))
  m$ecart <- abs(m$valeur_new - m$valeur_ref)
  manquantes <- m$cle[is.na(m$valeur_new)]; nouvelles <- m$cle[is.na(m$valeur_ref)]
  if (length(manquantes)) cat("Clés absentes du nouveau run :", paste(manquantes, collapse = ", "), "\n")
  if (length(nouvelles))  cat("Clés nouvelles :", paste(nouvelles, collapse = ", "), "\n")
  diff <- m[!is.na(m$ecart) & m$ecart > tolerance, ]
  if (nrow(diff) > 0) { cat("ÉCARTS :\n"); print(diff, row.names = FALSE) } else cat("Aucun écart (", nrow(m), "clés ).\n")
  invisible(m)
}
