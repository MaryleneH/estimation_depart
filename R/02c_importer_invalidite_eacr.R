# ==============================================================================
# 02c_importer_invalidite_eacr.R — Incidence d'invalidité par TRANCHE d'âge x sexe
# ------------------------------------------------------------------------------
# PRÉREQUIS : 00_config.R (FICHIER_INVALIDITE, INVAL_*, AGE_PLEIN_INVALIDITE,
#             FICHIER_POP_ACTIVE, COHORTE_PAR_SEXE, TAUX_ACTIVITE,
#             TRANCHES_DEFAUT, T_INVALIDITE_BASE)
# MÉTHODE   : taux d'incidence PAR TRANCHE = entrées EACR de la tranche /
#             population active de la MÊME tranche. Pas de lissage ni d'âge fin :
#             on se cale sur la granularité (grossière) du dénominateur — c'est
#             une estimation, la simplicité prime. Taux appliqué "en escalier".
# ENTRÉES   : (1) data/incidence_invalidite.xlsx — table I-Invalidité EACR.
#             (2) DÉNOMINATEUR = population active par tranche x sexe :
#                 data/pop_active_insee.csv, colonnes ';' :
#                     tranche;sexe;actifs   (ex. "50 à 54 ans" ; format PACT00)
#                 sexe H/F, lignes "Ensemble" ignorées. Les TRANCHES DE CE
#                 FICHIER pilotent tout le calcul. À défaut : reconstruction
#                 sur TRANCHES_DEFAUT (cohorte x taux d'activité).
# PRODUIT   : objet `inval_tranches` (borne_inf, borne_sup, sexe, taux) et
#             l'aide `taux_invalidite_tranche(age, sexe)` pour le script 04.
# SECOURS   : EACR absent -> repli T_INVALIDITE_BASE (message fort).
# ==============================================================================
if (!exists("FICHIER_INVALIDITE")) stop("Exécutez d'abord R/00_config.R (ou main.R).")

library(readxl)
library(readr)
library(dplyr)
library(tidyr)

# --- Bornes [inf, sup] d'une étiquette de tranche ("50 à 54 ans", "65 ou +") --
bornes_tranche <- function(lbl) {
  n <- as.integer(unlist(regmatches(lbl, gregexpr("[0-9]+", lbl))))
  if (length(n) >= 2) c(min(n), max(n))
  else if (length(n) == 1 && grepl("moins", lbl, TRUE)) c(43, n)
  else if (length(n) == 1 && grepl("plus|\\+", lbl))    c(n, 72)
  else if (length(n) == 1)                              c(n, n)
  else c(NA, NA)
}

# --- Dénominateur : actifs par tranche x sexe (fichier Insee, sinon reconstruit)
pop_active_tranches <- function() {
  if (file.exists(FICHIER_POP_ACTIVE)) {
    raw <- read_delim(FICHIER_POP_ACTIVE, delim = ";",
                      locale = locale(decimal_mark = "."), show_col_types = FALSE) |>
      rename_with(tolower) |>
      mutate(sexe = toupper(substr(sexe, 1, 1))) |>
      filter(sexe %in% c("H", "F"))
    col_act <- intersect(c("actifs", "actif", "valeur", "nombre"), names(raw))[1]
    if (is.na(col_act)) stop("Colonne d'effectifs introuvable (attendu 'actifs').")
    if (!"tranche" %in% names(raw))
      stop("Colonne 'tranche' attendue dans ", FICHIER_POP_ACTIVE,
           " (format PACT00 : tranche;sexe;actifs).")
    b <- t(vapply(raw$tranche, bornes_tranche, numeric(2)))
    pa <- raw |>
      mutate(borne_inf = b[, 1], borne_sup = b[, 2],
             actifs = parse_number(as.character(.data[[col_act]]),
                                   locale = locale(decimal_mark = "."))) |>
      filter(!is.na(borne_inf)) |>
      group_by(borne_inf, borne_sup, sexe) |>
      summarise(actifs = sum(actifs), .groups = "drop")
    if (max(pa$actifs, na.rm = TRUE) < 10000)      # milliers -> unités
      pa <- pa |> mutate(actifs = actifs * 1000)
    attr(pa, "src") <- "fichier Insee PACT00 (tranches natives)"
  } else {
    ta <- pivot_longer(TAUX_ACTIVITE, cols = starts_with("a_"),
                       names_to = "bande", values_to = "taux_act") |>
      mutate(bande = sub("^a_", "", bande))
    # bandes de TAUX_ACTIVITE -> bornes, alignées sur TRANCHES_DEFAUT
    corr <- tibble(bande = c("43_49","50_54","55_59","60_61","62plus"),
                   borne_inf = c(43,50,55,60,62), borne_sup = c(49,54,59,61,72))
    pa <- ta |> left_join(corr, by = "bande") |>
      mutate(largeur = borne_sup - borne_inf + 1,
             actifs = COHORTE_PAR_SEXE * taux_act * largeur) |>
      group_by(borne_inf, borne_sup, sexe) |>
      summarise(actifs = sum(actifs), .groups = "drop")
    attr(pa, "src") <- "reconstruction (cohorte x taux d'activité) PROVISOIRE"
  }
  pa
}

if (file.exists(FICHIER_INVALIDITE)) {
  denom <- pop_active_tranches()                 # les tranches viennent d'ICI

  brut <- read_excel(FICHIER_INVALIDITE, sheet = "I-Invalidité")
  flux_age <- brut |>
    filter(Année == INVAL_ANNEE, Caisse == INVAL_CAISSE, Champ == INVAL_CHAMP,
           Champ_FluxStock == "flux", Sexe %in% c("Hommes", "Femmes")) |>
    mutate(age = suppressWarnings(as.integer(Age)),
           sexe = if_else(Sexe == "Hommes", "H", "F"),
           entrees = suppressWarnings(as.numeric(effectifs))) |>
    filter(!is.na(age), !is.na(entrees))
  if (nrow(flux_age) == 0)
    stop("Aucune ligne EACR pour Année=", INVAL_ANNEE, " / Caisse=", INVAL_CAISSE,
         " / Champ=", INVAL_CHAMP, " : vérifiez 00_config.R.")

  # Affecter chaque âge EACR à SA tranche du dénominateur, puis sommer
  tr <- denom |> distinct(borne_inf, borne_sup)
  affecte_tranche <- function(age)
    tr$borne_inf[match(TRUE, age >= tr$borne_inf & age <= tr$borne_sup)]
  flux_tr <- flux_age |>
    mutate(borne_inf = vapply(age, affecte_tranche, numeric(1))) |>
    filter(!is.na(borne_inf)) |>
    group_by(borne_inf, sexe) |>
    summarise(entrees = sum(entrees), .groups = "drop")

  # Taux par tranche = entrées / actifs de la tranche
  inval_tranches <- denom |>
    left_join(flux_tr, by = c("borne_inf", "sexe")) |>
    mutate(entrees = coalesce(entrees, 0),
           taux = if_else(actifs > 0, entrees / actifs, NA_real_))

  # Gel au-delà de l'âge légal : la (ou les) tranche(s) dont la borne_inf
  # dépasse AGE_PLEIN_INVALIDITE reçoit le taux de la dernière tranche "pleine".
  ref <- inval_tranches |>
    filter(borne_inf <= AGE_PLEIN_INVALIDITE) |>
    group_by(sexe) |> slice_max(borne_inf, n = 1) |>
    select(sexe, taux_ref = taux) |> ungroup()
  inval_tranches <- inval_tranches |>
    left_join(ref, by = "sexe") |>
    mutate(taux = if_else(borne_inf > AGE_PLEIN_INVALIDITE, taux_ref, taux)) |>
    mutate(taux = pmin(taux, 0.05)) |>
    select(borne_inf, borne_sup, sexe, taux) |>
    arrange(sexe, borne_inf)

  stopifnot(
    "taux d'invalidité invraisemblables (0-6%/an attendu)" =
      all(inval_tranches$taux >= 0 & inval_tranches$taux < 0.06, na.rm = TRUE),
    "tranches manquantes pour un sexe" =
      all(table(inval_tranches$sexe) == nrow(tr))
  )
  message("02c OK -> inval_tranches (EACR ", INVAL_ANNEE, " ", INVAL_CHAMP,
          " / dénominateur : ", attr(denom, "src"), " ; ", nrow(tr), " tranches)")

} else {
  inval_tranches <- T_INVALIDITE_BASE |>
    left_join(TRANCHES_DEFAUT, by = "borne_inf") |>
    select(borne_inf, borne_sup, sexe, taux) |> arrange(sexe, borne_inf)
  message("02c ATTENTION : ", FICHIER_INVALIDITE, " absent -> repli sur ",
          "T_INVALIDITE_BASE (config).")
}

# --- Aide pour le script 04 : taux d'invalidité d'un âge (via sa tranche) ------
# VECTORISÉ : findInterval affecte chaque âge à sa tranche en une passe (pas de
# boucle par ligne), puis lookup par clé. Indispensable pour les gros volumes
# (des centaines de milliers de lignes appelées 6 fois par la boucle du 04).
.inval_bornes <- sort(unique(inval_tranches$borne_inf))       # bornes basses triées
taux_invalidite_tranche <- function(age, sexe) {
  # findInterval : indice de la tranche dont borne_inf <= age (dernière <=)
  idx <- findInterval(age, .inval_bornes)
  idx[idx < 1] <- 1L                                          # âges < 1re borne
  key_inf <- .inval_bornes[idx]
  cle  <- paste(key_inf, sexe)
  cler <- paste(inval_tranches$borne_inf, inval_tranches$sexe)
  inval_tranches$taux[match(cle, cler)]
}
