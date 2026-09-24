# ==============================================================================
# 01_fabriquer_donnees_test.R — Source des données BTS (test OU Parquet réel)
# ------------------------------------------------------------------------------
# PRÉREQUIS : 00_config.R (SOURCE_BTS, GRAINE, N_TEST ; ou FICHIER_BTS,
#             FICHIER_SIREN, AGE_MIN_BTS, COL_BTS) + bloc GEO_* ;
#             00c_fonctions_geo.R (charger_bts_parquet, normaliser_geo)
# PRODUIT   : objet `bts` (id, siren, sexe, age_2024, pcs, generation,
#             geo_code, geo_nom, geo_type)
#             -> contrat de colonnes IDENTIQUE quelle que soit la source ET
#                quel que soit le zonage. Le reste de la chaîne ne connaît ni
#                les noms des colonnes du fichier, ni le zonage (voir 00c).
#
# DEUX MODES (SOURCE_BTS dans 00_config.R) :
#   "test"    : table simulée en mémoire (PCS-ESE 4 chiffres, indépendants + NR),
#               géographie simulée selon GEO_ANALYSE (ze ou departement).
#   "parquet" : vraie extraction BTS lue avec Arrow en LAZY. Le filtre (âge, SIREN
#               du périmètre BITD) et la sélection de colonnes sont POUSSÉS au
#               niveau du fichier : Arrow ne matérialise en RAM que le sous-
#               ensemble utile (crucial si peu de mémoire). collect() en dernier.
# ==============================================================================
if (!exists("SOURCE_BTS")) stop("Exécutez d'abord R/00_config.R (ou main.R).")
if (!exists("normaliser_geo")) stop("Exécutez d'abord R/00c_fonctions_geo.R (ou main.R).")
garde_migration_geo()

library(dplyr)

if (SOURCE_BTS == "parquet") {
  # ------------------------------------------------------- lecture Parquet réel
  if (!file.exists(FICHIER_SIREN)) stop("Liste SIREN introuvable : ", FICHIER_SIREN)
  sirens_bitd <- readr::read_lines(FICHIER_SIREN)
  sirens_bitd <- trimws(sirens_bitd[sirens_bitd != ""])

  # Lecture lazy + renommage générique (COL_BTS -> contrat, COL_GEO -> geo_*).
  bts <- charger_bts_parquet(FICHIER_BTS, COL_BTS, COL_GEO, AGE_MIN_BTS, sirens_bitd)

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
  # Territoires simulés, par zonage (même générateur, seule la liste change).
  # ZE : 42 zones à l'échelle réelle du périmètre — code = nom (identité), ce
  # qui conserve à l'identique les résultats de référence d'avant découplage.
  # Département : 20 codes RÉELS (dont « 01 », « 2A », « 2B ») pour éprouver
  # le traitement des codes en texte.
  zones_test <- c("Toulouse", "Bordeaux", "Brest", "Cherbourg-en-Cotentin",
                  "Bourges", "Toulon", "Rennes", "Saint-Nazaire", "Lorient",
                  "Nantes", "Paris", "Versailles", "Évry", "Créteil",
                  "Marseille", "Aix-en-Provence", "Istres", "Nice", "Lyon",
                  "Grenoble", "Valence", "Clermont-Ferrand", "Limoges",
                  "Tarbes", "Pau", "Angoulême", "Poitiers", "Tours",
                  "Orléans", "Le Mans", "Caen", "Rouen", "Le Havre", "Lille",
                  "Douai", "Valenciennes", "Metz", "Nancy", "Strasbourg",
                  "Mulhouse", "Belfort", "Dijon")
  zonages_test <- list(
    ze = list(code = zones_test, nom = zones_test),
    departement = list(
      code = c("01", "03", "09", "18", "2A", "2B", "33", "35", "44", "56",
               "59", "64", "69", "75", "78", "83", "86", "87", "91", "93"),
      nom  = c("Ain", "Allier", "Ariège", "Cher", "Corse-du-Sud", "Haute-Corse",
               "Gironde", "Ille-et-Vilaine", "Loire-Atlantique", "Morbihan",
               "Nord", "Pyrénées-Atlantiques", "Rhône", "Paris", "Yvelines",
               "Var", "Vienne", "Haute-Vienne", "Essonne", "Seine-Saint-Denis")))
  if (!GEO_ANALYSE %in% names(zonages_test))
    stop("Mode test : zonage simulé pour ", paste(names(zonages_test), collapse = ", "),
         " seulement (GEO_ANALYSE = '", GEO_ANALYSE, "').")
  zt <- zonages_test[[GEO_ANALYSE]]
  # Pyramides des âges volontairement CONTRASTÉES entre territoires (poids
  # senior ~0.55 à ~2.2), sinon tout s'empile sur la moyenne du quadrant.
  poids_senior <- runif(length(zt$code), 0.55, 2.2)
  bts <- tibble(
    id       = sprintf("ID%05d", 1:N_TEST),
    siren    = sample(sprintf("ENT_%02d", 1:8), N_TEST, replace = TRUE),
    sexe     = sample(c("Hommes", "Femmes"), N_TEST, replace = TRUE, prob = c(0.55, 0.45)),
    age_2024 = sample(43:66, N_TEST, replace = TRUE, prob = rev(seq_along(43:66))^0.7),
    pcs      = tirer_pcs(N_TEST)
  ) |>
    mutate(generation = 2024 - age_2024,
           # tirage d'INDICES (mêmes tirages aléatoires qu'un sample() sur les noms)
           .idx = ifelse(age_2024 >= 55,
                         sample(seq_along(zt$code), N_TEST, replace = TRUE,
                                prob = poids_senior),
                         sample(seq_along(zt$code), N_TEST, replace = TRUE)),
           geo_code = zt$code[.idx],
           geo_nom  = zt$nom[.idx]) |>
    select(-.idx)
  print(bts |> mutate(cs = substr(pcs, 1, 1)) |> count(cs))
  message("01 OK (test) -> bts (", nrow(bts), " lignes simulées, PCS 4 chiffres, ",
          "géographie : ", GEO_ANALYSE, ")")
}

# --- Normalisation géographique -> contrat geo_code / geo_nom / geo_type ------
# Codes en TEXTE (zéros à gauche via GEO_CODE_LARGEUR), passage éventuel
# GEO_SOURCE -> GEO_ANALYSE, libellés (référentiel local, GEO_INTERET nommé).
# En mode test la source est déjà au zonage d'analyse.
bts <- normaliser_geo(
  bts,
  geo_source   = if (SOURCE_BTS == "parquet") GEO_SOURCE else GEO_ANALYSE,
  geo_analyse  = GEO_ANALYSE,
  largeur      = GEO_CODE_LARGEUR,
  referentiels = GEO_REFERENTIELS,
  passages     = GEO_PASSAGES,
  geo_interet  = GEO_INTERET,
  prefixe      = "01")

# Normalisation commune du sexe -> H/F (le reste de la chaîne attend H/F)
bts <- bts |> mutate(sexe = toupper(substr(sexe, 1, 1)))
