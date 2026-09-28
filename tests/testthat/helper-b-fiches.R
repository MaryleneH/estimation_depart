# Helper testthat pour les fiches territoriales : paramètres minimaux, couche
# 00d, et une petite base SYNTHÉTIQUE au contrat de bts_projete (geo_code /
# geo_nom / geo_type + probabilités) — rapide et indépendante de la chaîne.
AGE_SENIOR      <- 55
AGE_MIN_BTS     <- 45
SEUIL_DIFFUSION <- 20
BREAKS_TRANCHES <- c(-Inf, 48, 54, 60, Inf)
LABELS_TRANCHES <- c(sprintf("%d-48 ans", AGE_MIN_BTS), "49-54 ans", "55-60 ans", "61 ans et +")
source(file.path(RACINE, "R", "00d_fonctions_fiches.R"), local = TRUE)

# Un territoire = liste(code, nom, effectifs par CS, part de seniors).
# Déterministe (graine fixe) ; p_* plausibles : seniors ~0.9, autres ~0.2.
fabriquer_base_fiches <- function(territoires, seed = 1) {
  set.seed(seed)
  rows <- lapply(territoires, function(t) {
    cs <- rep(names(t$n_cs), t$n_cs)
    n  <- length(cs)
    senior <- runif(n) < t$part_senior
    age <- ifelse(senior, sample(55:66, n, TRUE), sample(AGE_MIN_BTS:54, n, TRUE))
    p   <- ifelse(senior, 0.9, 0.2)
    tibble::tibble(
      geo_code = t$code, geo_nom = t$nom, geo_type = "departement",
      siren = sample(sprintf("E%02d", 1:t$n_entreprises), n, TRUE),
      age_2024 = age, cs1 = cs,
      p_central = p, p_bas = p * 0.95, p_haut = pmin(1, p * 1.05),
      p_cal_central = p * 0.93, p_inval = 0.01, p_deces = 0.005)
  })
  dplyr::bind_rows(rows)
}

CS4 <- c(Cadres = 40, `Prof. intermediaires` = 40, Employes = 30, Ouvriers = 60)
TERRITOIRES_TEST <- list(
  list(code = "01", nom = "Ain",          n_cs = CS4, part_senior = 0.35, n_entreprises = 6),
  list(code = "2A", nom = "Corse-du-Sud", n_cs = CS4, part_senior = 0.25, n_entreprises = 5),
  # 2B : une CS de 5 salariés -> secret statistique (et suppression secondaire)
  list(code = "2B", nom = "Haute-Corse",  n_cs = c(Cadres = 5, `Prof. intermediaires` = 40,
                                                   Employes = 30, Ouvriers = 60),
       part_senior = 0.30, n_entreprises = 4),
  list(code = "33", nom = "Gironde",      n_cs = CS4 * 2, part_senior = 0.40, n_entreprises = 9),
  # 09 : aucun senior -> taux de départ des 55+ indéfini
  list(code = "09", nom = "Ariège",       n_cs = CS4, part_senior = 0, n_entreprises = 3),
  # 48 : territoire entier sous le seuil de diffusion
  list(code = "48", nom = "Lozère",       n_cs = c(Ouvriers = 12), part_senior = 0.5, n_entreprises = 1)
)
BASE_FICHES <- fabriquer_base_fiches(TERRITOIRES_TEST)
ZONAGE_DEP  <- list(libelle = "Département", un = "un département",
                    pluriel = "départements", suffixe = "departement", type = "departement")
