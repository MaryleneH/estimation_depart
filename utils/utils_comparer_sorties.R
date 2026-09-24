# ==============================================================================
# utils/utils_comparer_sorties.R — Comparer deux CSV de synthèse du 08
# (avant / après une modification), territoire à territoire.
# ------------------------------------------------------------------------------
# Usage :
#   source("utils/utils_comparer_sorties.R")
#   comparer_sorties("ancien/analyse_55plus_par_ze.csv",
#                    "sorties/analyse_55plus_par_ze.csv")
# Jointure sur le libellé du territoire (colonne `ze` des anciens fichiers,
# `geo_nom` des nouveaux) ; écart maximal absolu par indicateur ; territoires
# présents d'un seul côté listés. Un écart max de 0 partout = non-régression.
# ==============================================================================
comparer_sorties <- function(avant, apres,
                             indicateurs = c("effectif_43plus", "effectif_55plus",
                                             "part_55plus_pct", "departs_55plus",
                                             "departs_55plus_bas", "departs_55plus_haut",
                                             "dep_55_retraite", "dep_55_invalidite",
                                             "dep_55_deces", "taux_depart_55plus_pct")) {
  lire <- function(f) {
    d <- read.csv2(f, check.names = FALSE, stringsAsFactors = FALSE)
    cle <- intersect(c("geo_nom", "ze"), names(d))[1]
    if (is.na(cle)) stop(f, " : aucune colonne territoire (geo_nom ou ze).")
    d$territoire <- d[[cle]]
    d
  }
  a <- lire(avant); b <- lire(apres)
  seul_a <- setdiff(a$territoire, b$territoire)
  seul_b <- setdiff(b$territoire, a$territoire)
  if (length(seul_a)) cat("Présents seulement AVANT :", paste(seul_a, collapse = ", "), "\n")
  if (length(seul_b)) cat("Présents seulement APRÈS :", paste(seul_b, collapse = ", "), "\n")
  m <- merge(a, b, by = "territoire", suffixes = c("_avant", "_apres"))
  ecarts <- vapply(indicateurs, function(v) {
    x <- m[[paste0(v, "_avant")]]; y <- m[[paste0(v, "_apres")]]
    if (is.null(x) || is.null(y)) return(NA_real_)
    max(abs(x - y), na.rm = TRUE)
  }, numeric(1))
  res <- data.frame(indicateur = indicateurs, ecart_max = ecarts, row.names = NULL)
  cat("\n", nrow(m), "territoires communs — écart maximal absolu par indicateur :\n")
  print(res, row.names = FALSE)
  invisible(res)
}
