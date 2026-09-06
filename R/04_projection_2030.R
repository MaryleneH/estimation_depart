# ==============================================================================
# 04_projection_2030.R — Probabilités individuelles de sortie d'ici 2030
# ------------------------------------------------------------------------------
# PRÉREQUIS : objets `bts`, `param_cs`, `param_ensemble`, `table_mortalite`
#             (scripts 01-03 + 02b) ; paramètres du modèle (00)
# PRODUIT   : objet `bts_projete` — par individu : p_frott (invalidité + décès),
#             p_cal_* (calendrier retraite/fin de carrière), p_bas/central/haut
#             (combinés), et l'indicatrice seuil.
# ==============================================================================
if (!exists("bts"))             stop("Objet 'bts' introuvable : exécutez R/01 (ou main.R).")
if (!exists("param_cs"))        stop("Objet 'param_cs' introuvable : exécutez R/03 (ou main.R).")
if (!exists("table_mortalite")) stop("Objet 'table_mortalite' introuvable : exécutez R/02b (ou main.R).")

library(dplyr)

# --- A. Correspondance nomenclatures ------------------------------------------
correspondance <- tribble(
  ~cs1,                    ~csp,
  "Cadres",                "3 - Cadres et professions intellectuelles supérieures",
  "Prof. intermediaires",  "4 - Professions intermédiaires",
  "Employes",              "5 - Employés",
  "Ouvriers",              "6 - Ouvriers"
)
manquants <- bts |> distinct(cs1) |> anti_join(correspondance, by = "cs1")
if (nrow(manquants) > 0)
  stop("cs1 sans correspondance CSP : ", paste(manquants$cs1, collapse = ", "))

# --- B. Correctif réglementaire δ ---------------------------------------------
delta_bas     <- AGE_CONJ_TOUS_REGIMES_2023 - param_ensemble$age_conj_ensemble
delta_haut    <- delta_bas + MONTEE_RESIDUELLE_2030
delta_central <- (delta_bas + delta_haut) / 2
message(sprintf("δ : bas = %.2f | central = %.2f | haut = %.2f (années)",
                delta_bas, delta_central, delta_haut))

# --- C. Calendrier retraite / fin de carrière (loi normale autour de μ+δ) -----
p_sortie_horizon <- function(age, mu, sigma = SIGMA_SORTIE, h = HORIZON) {
  Fa  <- pnorm(age,     mu, sigma)
  Fah <- pnorm(age + h, mu, sigma)
  p   <- ifelse(Fa >= 0.999, 1, (Fah - Fa) / (1 - Fa))
  pmin(pmax(p, 0), 1)
}

# --- D. Deux risques hors calendrier retraite, sur TOUT l'horizon :
#        décès (quotients Insee) et invalidité (profil par CSP x âge).
#        Chacun cumulé sur les âges SUCCESSIVEMENT atteints, 1-(1-t)^... .
q_lookup <- function(age, sexe) {
  a <- pmin(pmax(age, min(table_mortalite$age)), max(table_mortalite$age))
  table_mortalite$q[match(paste(a, sexe),
                          paste(table_mortalite$age, table_mortalite$sexe))]
}
# Incidence d'invalidité = base(TRANCHE d'âge, sexe) x coefficient CSP.
# base : taux par tranche calé sur le dénominateur (script 02c, fonction
# taux_invalidite_tranche) ; le coefficient CSP est le seul paramètre expert.
if (!exists("taux_invalidite_tranche"))
  stop("Fonction 'taux_invalidite_tranche' introuvable : exécutez R/02c (ou main.R).")
t_invalidite <- function(age, cs1, sexe) {
  base <- taux_invalidite_tranche(age, sexe)   # taux "en escalier" par tranche
  as.numeric(base * COEF_CSP_INVALIDITE[cs1])
}
# Probabilité de NE PAS mourir sur l'horizon (âges successifs)
survie_deces_h <- function(age, sexe, h = HORIZON) {
  s <- rep(1, length(age))
  for (k in 0:(h - 1)) s <- s * (1 - q_lookup(age + k, sexe))
  s
}
# Probabilité de NE PAS devenir invalide sur l'horizon (âges successifs)
survie_inval_h <- function(age, cs1, sexe, h = HORIZON) {
  s <- rep(1, length(age))
  for (k in 0:(h - 1)) s <- s * (1 - t_invalidite(age + k, cs1, sexe))
  s
}
# Probabilité cumulée d'invalidité seule, et de décès seul (pour le graphique)
p_invalidite_h <- function(age, cs1, sexe, h = HORIZON) 1 - survie_inval_h(age, cs1, sexe, h)
p_deces_h      <- function(age, sexe, h = HORIZON)      1 - survie_deces_h(age, sexe, h)
# Frottement combiné (invalidité OU décès), risques indépendants
frottement_h <- function(age, cs1, sexe, h = HORIZON)
  1 - survie_deces_h(age, sexe, h) * survie_inval_h(age, cs1, sexe, h)

# Retrait anti-double-compte : l'invalidité étant désormais un flux propre,
# on l'enlève de μ. Part du sas imputable à l'invalidité, approchée par
# l'incidence cumulée d'invalidité entre 50 ans et μ (au profil de la CSP) ;
# on en déduit un μ CORRIGÉ, légèrement décalé vers la droite (μ' >= μ).
retrait_mu_invalidite <- function(mu, cs1, sexe) {
  ans <- pmax(0, round(mu - 50))
  s <- rep(1, length(mu))
  for (k in seq_len(max(ans))) {
    actif <- k <= ans
    s[actif] <- s[actif] * (1 - t_invalidite(50 + k - 1, cs1[actif], sexe[actif]))
  }
  mu + (1 - s)      # décale μ de la fraction hors-emploi rendue à l'emploi
}

bts_projete <- bts |>
  left_join(correspondance, by = "cs1") |>
  left_join(param_cs |> select(csp, age_conj, mu_sortie), by = "csp") |>
  mutate(
    age_2030 = age_2024 + HORIZON,

    # ================= NIVEAU 1 — LÉGISLATION SEULE (scénario A) =============
    # Départs par le seul âge conjoncturel de LIQUIDATION (age_conj), sans sas
    # ni événement de vie. μ n'intervient pas ici : socle réglementaire pur.
    pA_bas     = p_sortie_horizon(age_2024, age_conj + delta_haut),   # δ haut = borne basse
    pA_central = p_sortie_horizon(age_2024, age_conj + delta_central),
    pA_haut    = p_sortie_horizon(age_2024, age_conj + delta_bas),

    # ================= NIVEAU 2 — + ÉVÉNEMENTS DE VIE (scénario B) ===========
    # (i) le SAS : il fait passer de l'âge de liquidation (age_conj) à l'âge de
    #     SORTIE DE L'EMPLOI (μ). C'est le premier événement de vie ajouté.
    #     μ corrigé de la part d'invalidité (anti-double-compte, voir plus bas).
    mu_corrige = retrait_mu_invalidite(mu_sortie, cs1, sexe),
    pB_cal_bas     = p_sortie_horizon(age_2024, mu_corrige + delta_haut),
    pB_cal_central = p_sortie_horizon(age_2024, mu_corrige + delta_central),
    pB_cal_haut    = p_sortie_horizon(age_2024, mu_corrige + delta_bas),
    # (ii) INVALIDITÉ (flux propre CSP x âge) et (iii) DÉCÈS (Insee), cumulés
    #      sur l'horizon ; exposés séparément pour la décomposition du graphique.
    p_inval = p_invalidite_h(age_2024, cs1, sexe),
    p_deces = p_deces_h(age_2024, sexe),
    p_frott = frottement_h(age_2024, cs1, sexe),      # invalidité OU décès
    # combinaison finale : sortie d'emploi (sas inclus) OU invalidité OU décès
    pB_bas     = 1 - (1 - pB_cal_bas)     * (1 - p_frott),
    pB_central = 1 - (1 - pB_cal_central) * (1 - p_frott),
    pB_haut    = 1 - (1 - pB_cal_haut)    * (1 - p_frott),

    # ================= Alias du scénario ACTIF (piloté par SCENARIO) =========
    p_bas     = if (SCENARIO == "A") pA_bas     else pB_bas,
    p_central = if (SCENARIO == "A") pA_central else pB_central,
    p_haut    = if (SCENARIO == "A") pA_haut    else pB_haut,
    # p_cal_* = part « départ retraite/fin de carrière » pour le graphique 06
    p_cal_bas     = if (SCENARIO == "A") pA_bas     else pB_cal_bas,
    p_cal_central = if (SCENARIO == "A") pA_central else pB_cal_central,
    p_cal_haut    = if (SCENARIO == "A") pA_haut    else pB_cal_haut,
    seuil_age = if (SCENARIO == "A") age_conj else mu_corrige,
    sortant_seuil = age_2030 >= (seuil_age + delta_central)
  )

# --- Tableau de CONTRIBUTION en pelures (central) : effet de chaque niveau ---
contribution <- bts_projete |>
  summarise(
    `1. Législation seule (âge légal)`        = sum(pA_central),
    `2. + Sas de fin de carrière`             = sum(pB_cal_central),
    `3. + Invalidité et décès (total B)`      = sum(pB_central)
  ) |>
  mutate(across(everything(), round))
apport <- with(contribution,
  tibble::tibble(
    poste  = c("Législation seule", "Apport du sas",
               "Apport invalidité + décès", "TOTAL (scénario B)"),
    depart = round(c(`1. Législation seule (âge légal)`,
              `2. + Sas de fin de carrière` - `1. Législation seule (âge légal)`,
              `3. + Invalidité et décès (total B)` - `2. + Sas de fin de carrière`,
              `3. + Invalidité et décès (total B)`))))
cat("\n--- Décomposition en pelures (nb de départs, central) ---\n")
print(as.data.frame(apport), row.names = FALSE)

cat("\n--- Scénario ACTIF :", SCENARIO, "---\n")
print(bts_projete |>
  summarise(departs_seuil  = sum(sortant_seuil),
            departs_lisse  = round(sum(p_central)),
            fourchette_bas = round(sum(p_bas)),
            fourchette_haut= round(sum(p_haut))))
print(bts_projete |> group_by(cs1) |>
  summarise(part_sortants = round(100 * mean(p_central), 1), .groups = "drop"))
message("04 OK -> objet bts_projete (scénario ", SCENARIO, ")")
