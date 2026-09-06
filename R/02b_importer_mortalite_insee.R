# ==============================================================================
# 02b_importer_mortalite_insee.R — Quotients de mortalité Insee (sexe x âge)
# ------------------------------------------------------------------------------
# PRÉREQUIS : 00_config.R (FICHIER_MORTALITE, CHAMP_MORTALITE, ANNEE_MORTALITE,
#             Q_DECES_ANNUEL)
# ENTRÉE    : data/3_Quotients_mortalite.xlsx — classeur Insee au format natif :
#             4 onglets {FR,FM}-{Femmes,Hommes}, table LARGE (1 ligne/année,
#             1 colonne/âge "43 ans", ...), quotients POUR 100 000, 2 lignes
#             de titre, suffixe "(p)" sur les années provisoires.
# PRODUIT   : objet `table_mortalite` (age, sexe, q en proportion), 43-72 ans
# SECOURS   : fichier absent -> quotients plats Q_DECES_ANNUEL (message fort).
# ==============================================================================
if (!exists("FICHIER_MORTALITE")) stop("Exécutez d'abord R/00_config.R (ou main.R).")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

if (file.exists(FICHIER_MORTALITE)) {

  # Un onglet (ex. "FR-Hommes") -> table longue (annee, age, q_pour_100000).
  # STRUCTURE réelle du classeur Insee (vérifiée) :
  #   ligne 1 : titre ; ligne 2 : vide ; ligne 3 : "Année" + descriptif ;
  #   ligne 4 : libellés d'âge "0 an","1 an",... décalés d'1 colonne
  #             (1re cellule vide, sous "Année") ; lignes 5+ : données.
  # On lit SANS en-tête et on reconstruit, plus robuste que de deviner skip.
  lire_onglet <- function(champ, sexe_lib) {
    onglet <- paste0(champ, "-", sexe_lib)
    raw <- read_excel(FICHIER_MORTALITE, sheet = onglet, col_names = FALSE)
    ages <- suppressWarnings(parse_number(as.character(unlist(raw[4, ]))))
    ages[1] <- NA                       # colonne "Année" : pas un âge
    donnees <- raw[-(1:4), ]            # lignes de données uniquement
    names(donnees) <- c("annee_txt",
                        paste0("age_", ages[-1]))   # age_43, age_44, ...
    donnees |>
      mutate(annee = suppressWarnings(parse_number(as.character(annee_txt)))) |>
      filter(!is.na(annee), annee >= 1900) |>
      pivot_longer(starts_with("age_"),
                   names_to = "age", names_prefix = "age_",
                   values_to = "q100k") |>
      mutate(age = suppressWarnings(as.integer(age)),
             q100k = suppressWarnings(as.numeric(q100k))) |>
      filter(!is.na(age), !is.na(q100k))
  }

  table_mortalite <- bind_rows(
    lire_onglet(CHAMP_MORTALITE, "Hommes") |> mutate(sexe = "H"),
    lire_onglet(CHAMP_MORTALITE, "Femmes") |> mutate(sexe = "F")
  ) |>
    filter(annee == ANNEE_MORTALITE, age %in% 43:72) |>
    transmute(age, sexe, q = q100k / 1e5) |>      # pour 100 000 -> proportion
    arrange(sexe, age)

  # Garde-fous : millésime présent, couverture complète, plausibilité
  if (nrow(table_mortalite) == 0)
    stop("Année ", ANNEE_MORTALITE, " absente du classeur : vérifiez ANNEE_MORTALITE.")
  stopifnot(
    "couverture d'âges incomplète (43-72 requis pour H et F)" =
      nrow(table_mortalite) == 2 * length(43:72),
    "quotients invraisemblables : vérifiez l'onglet / l'échelle" =
      all(table_mortalite$q > 0 & table_mortalite$q < 0.06)
  )
  message("02b OK -> table_mortalite (Insee ", CHAMP_MORTALITE, " ",
          ANNEE_MORTALITE, ", ", nrow(table_mortalite), " lignes)")

} else {
  table_mortalite <- expand_grid(age = 43:72, sexe = c("H", "F")) |>
    mutate(q = Q_DECES_ANNUEL[sexe]) |> arrange(sexe, age)
  message("02b ATTENTION : ", FICHIER_MORTALITE, " absent -> quotients PLATS ",
          "de secours. Déposez le classeur Insee pour la version finale.")
}
