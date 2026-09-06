# ==============================================================================
# 01b_agreger_pcs.R — Agrégation du code PCS fin -> CS niveau 1 (cs1)
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts` (script 01) ; paramètres d'agrégation (00_config :
#             AGGREGER_PCS, COL_PCS, PCS_VERS_CS1, PCS_HORS_CHAMP)
# PRODUIT   : `bts` enrichi d'une colonne `cs1` (CS niveau 1), les salariés
#             hors champ (indépendants, non renseigné) étant EXCLUS et COMPTÉS.
# Principe  : la CS niveau 1 = premier caractère du code PCS (3/4/5/6). Vaut
#             pour la PCS-ESE à 4 chiffres, la PCS à 2 chiffres, ou la CS à 1.
#             Robuste aux zéros initiaux et aux codes stockés en numérique.
#
# INACTIF par défaut (AGGREGER_PCS = FALSE) : sur la table test, `cs1` est déjà
# fourni par le 01, ce script ne fait rien. Le jour de la vraie BTS, mettre
# AGGREGER_PCS = TRUE et renseigner COL_PCS dans 00_config.R.
# ==============================================================================
if (!exists("bts")) stop("Objet 'bts' introuvable : exécutez R/01 (ou main.R).")

library(dplyr)

if (isTRUE(AGGREGER_PCS)) {
  if (!COL_PCS %in% names(bts))
    stop("Colonne PCS '", COL_PCS, "' absente de la BTS : vérifiez COL_PCS ",
         "dans 00_config.R (colonnes présentes : ",
         paste(names(bts), collapse = ", "), ").")

  n_depart <- nrow(bts)

  bts <- bts |>
    mutate(
      # 1er caractère du code, robuste : on retire un éventuel ".0" (code lu en
      # numérique) et les espaces, puis on prend le premier chiffre.
      .pcs_txt   = trimws(sub("\\.0$", "", as.character(.data[[COL_PCS]]))),
      .grande_cs = substr(.pcs_txt, 1, 1),
      cs1        = unname(PCS_VERS_CS1[.grande_cs])   # NA si hors table
    )

  # Traçabilité de l'exclusion AVANT de filtrer (jamais de perte silencieuse)
  exclus <- bts |> filter(is.na(cs1))
  if (nrow(exclus) > 0) {
    resume <- exclus |> count(.grande_cs, name = "n") |> arrange(desc(n))
    message("01b : exclusion de ", nrow(exclus), " lignes hors champ (",
            round(100 * nrow(exclus) / n_depart, 1), "% de la BTS). ",
            "Répartition par 1er chiffre PCS :")
    print(as.data.frame(resume))
    if (nrow(exclus) / n_depart > 0.15)
      warning("Plus de 15 % de la BTS exclue : vérifiez COL_PCS et le format ",
              "des codes (ex. zéros initiaux, codes indépendants attendus ?).")
  }

  bts <- bts |>
    filter(!is.na(cs1)) |>
    select(-.pcs_txt, -.grande_cs)

  message("01b OK -> cs1 agrégée ; ", nrow(bts), " salariés retenus sur ",
          n_depart, " (", n_distinct(bts$cs1), " catégories).")
} else {
  message("01b : agrégation PCS désactivée (AGGREGER_PCS = FALSE) -> ",
          "cs1 conservée telle quelle.")
}
