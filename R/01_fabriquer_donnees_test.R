# ==============================================================================
# 01_fabriquer_donnees_test.R — Source des données BTS (test OU Parquet réel)
# ------------------------------------------------------------------------------
# PRÉREQUIS : 00_config.R (SOURCE_BTS, GRAINE, N_TEST ; ou FICHIER_BTS,
#             FICHIER_SIREN, AGE_MIN_BTS, COL_BTS)
# PRODUIT   : objet `bts` (id, siren, sexe, age_2024, pcs, generation)
#             -> contrat de colonnes IDENTIQUE quelle que soit la source.
#
# DEUX MODES (SOURCE_BTS dans 00_config.R) :
#   "test"    : table simulée en mémoire (PCS-ESE 4 chiffres, indépendants + NR)
#               -> développement, pas de fichier requis.
#   "parquet" : vraie extraction BTS lue avec Arrow en LAZY. Le filtre (âge, SIREN
#               du périmètre BITD) et la sélection de colonnes sont POUSSÉS au
#               niveau du fichier : Arrow ne matérialise en RAM que le sous-
#               ensemble utile (crucial si peu de mémoire). collect() en dernier.
# ==============================================================================
if (!exists("SOURCE_BTS")) stop("Exécutez d'abord R/00_config.R (ou main.R).")

library(dplyr)

if (SOURCE_BTS == "parquet") {
  # ------------------------------------------------------- lecture Parquet réel
  if (!requireNamespace("arrow", quietly = TRUE))
    stop("Le package 'arrow' est requis pour SOURCE_BTS='parquet' : install.packages('arrow').")
  if (!file.exists(FICHIER_BTS)) stop("Fichier BTS introuvable : ", FICHIER_BTS)
  if (!file.exists(FICHIER_SIREN)) stop("Liste SIREN introuvable : ", FICHIER_SIREN)
  library(arrow)

  sirens_bitd <- readr::read_lines(FICHIER_SIREN)
  sirens_bitd <- trimws(sirens_bitd[sirens_bitd != ""])

  col_siren <- COL_BTS[["siren"]]; col_age <- COL_BTS[["age"]]
  col_sexe  <- COL_BTS[["sexe"]];  col_pcs <- COL_BTS[["pcs"]]

  ds <- open_dataset(FICHIER_BTS)            # LAZY : ne lit rien encore
  # Contrôle de schéma : les colonnes déclarées existent-elles ?
  manquantes <- setdiff(unname(COL_BTS), names(ds))
  if (length(manquantes) > 0)
    stop("Colonnes absentes du Parquet : ", paste(manquantes, collapse = ", "),
         " (schéma réel : ", paste(names(ds), collapse = ", "),
         "). Ajustez COL_BTS dans 00_config.R.")

  bts <- ds |>
    filter(.data[[col_age]] >= AGE_MIN_BTS,          # filtre âge poussé au disque
           .data[[col_siren]] %in% sirens_bitd) |>    # filtre périmètre BITD
    select(all_of(unname(COL_BTS))) |>                # colonnes utiles seulement
    collect() |>                                      # MATÉRIALISATION (RAM) ici
    rename(siren = all_of(col_siren), sexe = all_of(col_sexe),
           age_2024 = all_of(col_age), pcs = all_of(col_pcs)) |>
    mutate(id = sprintf("ID%08d", row_number()),
           generation = 2024 - age_2024)             # exact si AGE = âge ds l'année

  message("01 OK (parquet) -> bts : ", format(nrow(bts), big.mark = " "),
          " salariés (>= ", AGE_MIN_BTS, " ans, périmètre BITD) chargés en RAM.")

} else {
  # ------------------------------------------------------- table test simulée
  set.seed(GRAINE)
  codes_pcs <- list(
    cadres      = c("3812","3753","3822","3719","3892"),
    prof_interm = c("4620","4631","4642","4611","4652"),
    employes    = c("5411","5422","5511","5621","5311"),
    ouvriers    = c("6543","6210","6234","6712","6821"),
    independants= c("1000","2310","2200"),
    nr          = c(NA, "0000", ""))
  tirer_pcs <- function(n) {
    grp <- sample(names(codes_pcs), n, replace = TRUE,
                  prob = c(0.24, 0.24, 0.18, 0.28, 0.04, 0.02))
    vapply(grp, function(g) sample(codes_pcs[[g]], 1), character(1))
  }
  bts <- tibble(
    id       = sprintf("ID%05d", 1:N_TEST),
    siren    = sample(sprintf("ENT_%02d", 1:8), N_TEST, replace = TRUE),
    sexe     = sample(c("Hommes", "Femmes"), N_TEST, replace = TRUE, prob = c(0.55, 0.45)),
    age_2024 = sample(43:66, N_TEST, replace = TRUE, prob = rev(seq_along(43:66))^0.7),
    pcs      = tirer_pcs(N_TEST)
  ) |>
    mutate(generation = 2024 - age_2024)
  print(bts |> mutate(cs = substr(pcs, 1, 1)) |> count(cs))
  message("01 OK (test) -> bts (", nrow(bts), " lignes simulées, PCS 4 chiffres)")
}

# Normalisation commune du sexe -> H/F (le reste de la chaîne attend H/F)
bts <- bts |> mutate(sexe = toupper(substr(sexe, 1, 1)))
