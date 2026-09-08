# ==============================================================================
# 01_fabriquer_donnees_test.R — Source des données BTS (test OU Parquet réel)
# ------------------------------------------------------------------------------
# PRÉREQUIS : 00_config.R (SOURCE_BTS, GRAINE, N_TEST ; ou FICHIER_BTS,
#             FICHIER_SIREN, AGE_MIN_BTS, COL_BTS)
# PRODUIT   : objet `bts` (id, siren, sexe, age_2024, pcs, generation, ze)
#             -> contrat de colonnes IDENTIQUE quelle que soit la source.
#             `ze` = zone d'emploi de l'ÉTABLISSEMENT (analyse 55+, script 08) ;
#             simulée en mode test, lue (ou dérivée du code commune via la
#             table de passage Insee) en mode parquet — voir 00_config.R.
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
  manquantes <- setdiff(c(unname(COL_BTS), COL_GEO_BTS), names(ds))
  if (length(manquantes) > 0)
    stop("Colonnes absentes du Parquet : ", paste(manquantes, collapse = ", "),
         " (schéma réel : ", paste(names(ds), collapse = ", "),
         "). Ajustez COL_BTS / COL_GEO_BTS dans 00_config.R.")

  bts <- ds |>
    filter(.data[[col_age]] >= AGE_MIN_BTS,          # filtre âge poussé au disque
           .data[[col_siren]] %in% sirens_bitd) |>    # filtre périmètre BITD
    select(all_of(c(unname(COL_BTS), COL_GEO_BTS))) |># colonnes utiles seulement
    collect() |>                                      # MATÉRIALISATION (RAM) ici
    rename(siren = all_of(col_siren), sexe = all_of(col_sexe),
           age_2024 = all_of(col_age), pcs = all_of(col_pcs),
           ze = all_of(COL_GEO_BTS)) |>
    mutate(id = sprintf("ID%08d", row_number()),
           generation = 2024 - age_2024)             # exact si AGE = âge ds l'année

  # Géographie en code commune -> zone d'emploi via la table de passage Insee.
  # Les communes sans correspondance restent en NA (tracées ; le script 08 les
  # regroupe sous « ZE inconnue » plutôt que de les perdre en silence).
  if (GEO_NIVEAU == "commune") {
    if (!file.exists(FICHIER_COMMUNE_ZE))
      stop("GEO_NIVEAU='commune' mais table de passage introuvable : ",
           FICHIER_COMMUNE_ZE, " (attendu : codgeo;ze;libze).")
    passage <- readr::read_delim(FICHIER_COMMUNE_ZE, delim = ";",
                                 show_col_types = FALSE) |>
      transmute(codgeo = as.character(codgeo), ze_lib = libze)
    bts <- bts |>
      mutate(codgeo = as.character(ze)) |> select(-ze) |>
      left_join(passage, by = "codgeo") |>
      rename(ze = ze_lib)
    n_na <- sum(is.na(bts$ze))
    if (n_na > 0) message("01 : ", n_na, " salariés sans ZE (commune hors table de passage).")
  }

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
  # Zones d'emploi simulées À L'ÉCHELLE RÉELLE du périmètre (42 zones) pour
  # dimensionner la restitution du script 08 sur la vraie densité de points.
  # Pyramides des âges volontairement CONTRASTÉES (poids senior ~0.55 à ~2.2),
  # sinon toutes les zones s'empilent sur la moyenne.
  zones_test <- c("Toulouse", "Bordeaux", "Brest", "Cherbourg-en-Cotentin",
                  "Bourges", "Toulon", "Rennes", "Saint-Nazaire", "Lorient",
                  "Nantes", "Paris", "Versailles", "Évry", "Créteil",
                  "Marseille", "Aix-en-Provence", "Istres", "Nice", "Lyon",
                  "Grenoble", "Valence", "Clermont-Ferrand", "Limoges",
                  "Tarbes", "Pau", "Angoulême", "Poitiers", "Tours",
                  "Orléans", "Le Mans", "Caen", "Rouen", "Le Havre", "Lille",
                  "Douai", "Valenciennes", "Metz", "Nancy", "Strasbourg",
                  "Mulhouse", "Belfort", "Dijon")
  poids_senior_ze <- runif(length(zones_test), 0.55, 2.2)
  bts <- tibble(
    id       = sprintf("ID%05d", 1:N_TEST),
    siren    = sample(sprintf("ENT_%02d", 1:8), N_TEST, replace = TRUE),
    sexe     = sample(c("Hommes", "Femmes"), N_TEST, replace = TRUE, prob = c(0.55, 0.45)),
    age_2024 = sample(43:66, N_TEST, replace = TRUE, prob = rev(seq_along(43:66))^0.7),
    pcs      = tirer_pcs(N_TEST)
  ) |>
    mutate(generation = 2024 - age_2024,
           ze = ifelse(age_2024 >= 55,
                       sample(zones_test, N_TEST, replace = TRUE,
                              prob = poids_senior_ze),
                       sample(zones_test, N_TEST, replace = TRUE)))
  print(bts |> mutate(cs = substr(pcs, 1, 1)) |> count(cs))
  message("01 OK (test) -> bts (", nrow(bts), " lignes simulées, PCS 4 chiffres)")
}

# Codes ZE -> libellés (FICHIER_LIBELLES_ZE, 00_config), si le fichier est
# présent : les restitutions porteront des NOMS de zones, pas des numéros.
# Un code sans libellé est CONSERVÉ tel quel et compté (jamais perdu).
if (GEO_NIVEAU == "ze" && file.exists(FICHIER_LIBELLES_ZE)) {
  libs_ze <- readr::read_delim(FICHIER_LIBELLES_ZE, delim = ";",
                               show_col_types = FALSE)
  if (!all(c("ze", "libze") %in% names(libs_ze)))
    stop("Fichier ", FICHIER_LIBELLES_ZE, " : colonnes attendues  ze;libze  ",
         "(trouvées : ", paste(names(libs_ze), collapse = ", "), ").")
  bts <- bts |>
    mutate(ze = as.character(ze)) |>
    left_join(libs_ze |> mutate(ze = as.character(ze)) |> distinct(ze, libze),
              by = "ze")
  n_sans_lib <- sum(is.na(bts$libze) & !is.na(bts$ze))
  if (n_sans_lib > 0)
    message("01 : ", n_sans_lib, " salarié(s) avec un code ZE sans libellé ",
            "dans ", basename(FICHIER_LIBELLES_ZE), " (code conservé).")
  bts <- bts |> mutate(ze = dplyr::coalesce(libze, ze)) |> select(-libze)
  message("01 : codes ZE remplacés par les libellés (",
          basename(FICHIER_LIBELLES_ZE), ").")
}

# Normalisation commune du sexe -> H/F (le reste de la chaîne attend H/F)
bts <- bts |> mutate(sexe = toupper(substr(sexe, 1, 1)))
