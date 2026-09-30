# ==============================================================================
# 01c_stock_tous_ages.R — Effectifs ACTUELS tous âges par territoire x grande CS
# ------------------------------------------------------------------------------
# PRÉREQUIS : 00_config.R (STOCK_TOUS_AGES, SOURCE_BTS, FICHIER_BTS, COL_BTS,
#             COL_GEO, PCS_VERS_CS1, bloc GEO_*), 00c (normaliser_geo), objet
#             `bts` du 01/01b (champ AGE_MIN_BTS+, cs1, contrat geo_*).
# PRODUIT   : objet `stock_tous_ages` (geo_code, geo_nom, cs1, effectif_tous_ages)
#             ou NULL si indisponible — jamais bloquant.
# POURQUOI  : le modèle projette les départs des AGE_MIN_BTS ans et + (en dessous,
#             les sorties définitives d'ici 2030 sont marginales), mais
#             le DÉNOMINATEUR qui parle à un décideur est l'effectif actuel de la
#             catégorie, tous âges : « 38 ouvriers partiront sur 400, soit 10 % à
#             remplacer ». Ce script fournit ce dénominateur, rien d'autre.
# COMMENT   : en mode parquet, une AGRÉGATION Arrow (comptage par géographie x
#             PCS, même périmètre SIREN, sans filtre d'âge) : aucun individu
#             supplémentaire n'est chargé en RAM. Mêmes recodages qu'en 01/01b :
#             cs1 = 1er chiffre PCS (hors champ exclus), géographie normalisée.
# GARDE-FOU : si l'extraction est déjà préfiltrée sur l'âge (aucun salarié sous
#             AGE_MIN_BTS), l'effectif « tous âges » serait celui du champ et le
#             taux à remplacer serait faux -> désactivé avec avertissement.
# ==============================================================================
if (!exists("bts")) stop("Objet 'bts' introuvable : exécutez R/01 et R/01b (ou main.R).")
if (!exists("normaliser_geo")) stop("Exécutez d'abord R/00c_fonctions_geo.R (ou main.R).")

library(dplyr)

stock_tous_ages <- NULL

if (!isTRUE(get0("STOCK_TOUS_AGES", ifnotfound = TRUE))) {
  message("01c : effectifs tous âges désactivés (STOCK_TOUS_AGES = FALSE).")

} else if (SOURCE_BTS == "parquet") {
  # ---------------------------------------------------------- agrégation Arrow
  if (!requireNamespace("arrow", quietly = TRUE)) stop("Le package 'arrow' est requis.")
  if (!exists("sirens_bitd")) {
    sirens_bitd <- readr::read_lines(FICHIER_SIREN); sirens_bitd <- trimws(sirens_bitd[sirens_bitd != ""])
  }
  ds <- arrow::open_dataset(FICHIER_BTS)
  col_siren <- COL_BTS[["siren"]]; col_age <- COL_BTS[["age"]]; col_pcs <- COL_BTS[["pcs"]]
  col_geo <- COL_GEO[!is.na(COL_GEO)]
  # Contrat interne AVANT l'agrégation : Arrow traduit rename()/select() sur des
  # noms fixes, mais pas .data[[variable]] dans un summarise().
  contrat <- c(geo_code = unname(col_geo[["code"]]), pcs = unname(col_pcs), age = unname(col_age))
  if ("nom" %in% names(col_geo)) contrat <- c(contrat, geo_nom = unname(col_geo[["nom"]]))
  cles <- setdiff(names(contrat), "age")
  requete <- ds |>
    filter(.data[[col_siren]] %in% sirens_bitd) |>            # même périmètre BITD, SANS filtre d'âge
    select(all_of(unname(contrat))) |>
    rename(all_of(contrat))
  agg <- tryCatch(
    requete |>
      group_by(across(all_of(cles))) |>
      summarise(n = n(), age_min_cellule = min(age, na.rm = TRUE), .groups = "drop") |>
      collect(),                                                # quelques milliers de lignes au plus
    error = function(e) {
      # Repli : agrégation refusée par Arrow -> on rapatrie SEULEMENT les 3 ou 4
      # colonnes clés (entiers / codes courts), puis on compte en R.
      message("01c : agrégation Arrow non supportée (", conditionMessage(e),
              ") -> comptage en R sur les colonnes clés uniquement.")
      requete |> collect() |>
        group_by(across(all_of(cles))) |>
        summarise(n = n(), age_min_cellule = suppressWarnings(min(age, na.rm = TRUE)), .groups = "drop")
    })
  age_min_obs <- suppressWarnings(min(agg$age_min_cellule, na.rm = TRUE))
  if (!is.finite(age_min_obs) || age_min_obs >= AGE_MIN_BTS) {
    warning("01c : l'extraction ne contient aucun salarié de moins de ", AGE_MIN_BTS,
            " ans (âge minimal observé : ", age_min_obs, "). Effectifs tous âges INDISPONIBLES : ",
            "fournissez l'extraction complète pour obtenir la part des effectifs à remplacer.")
  } else {
    stock_tous_ages <- agg |>
      mutate(.pcs_txt = trimws(sub("\\.0$", "", as.character(pcs))),
             cs1 = unname(PCS_VERS_CS1[substr(.pcs_txt, 1, 1)])) |>     # même règle que 01b
      filter(!is.na(cs1)) |>
      normaliser_geo(geo_source = GEO_SOURCE, geo_analyse = GEO_ANALYSE, largeur = GEO_CODE_LARGEUR,
                     referentiels = GEO_REFERENTIELS, passages = GEO_PASSAGES,
                     geo_interet = GEO_INTERET, prefixe = "01c") |>
      mutate(geo_code = ifelse(is.na(geo_code), "inconnu", as.character(geo_code))) |>
      group_by(geo_code, geo_nom, cs1) |>
      summarise(effectif_tous_ages = sum(n), .groups = "drop")
    message("01c OK (parquet) -> stock_tous_ages : ", format(sum(stock_tous_ages$effectif_tous_ages), big.mark = " "),
            " salariés tous âges (âge minimal observé : ", age_min_obs, ") sur ",
            n_distinct(stock_tous_ages$geo_code), " territoires.")
  }

} else {
  # ------------------------------------------------- mode test : stock simulé
  # Les moins de AGE_MIN_BTS ans ne sont pas simulés individuellement : on tire,
  # par territoire x CS, un effectif jeune de 1,5 à 3 fois l'effectif du champ
  # (ordre de grandeur d'une pyramide d'entreprise), de façon reproductible.
  set.seed(GRAINE + 1)
  stock_tous_ages <- bts |>
    count(geo_code, geo_nom, cs1, name = "n_champ") |>
    mutate(effectif_tous_ages = n_champ + round(n_champ * runif(n(), 1.5, 3.0))) |>
    select(-n_champ)
  message("01c OK (test) -> stock_tous_ages simulé (", nrow(stock_tous_ages), " cellules).")
}

# Cohérence : le champ est inclus dans le stock (jamais moins de salariés tous
# âges que de salariés de AGE_MIN_BTS ans et +, à cellule identique).
if (!is.null(stock_tous_ages)) {
  verif <- bts |> count(geo_code, cs1, name = "n_champ") |>
    left_join(stock_tous_ages |> select(geo_code, cs1, effectif_tous_ages), by = c("geo_code", "cs1"))
  incoherent <- verif |> filter(is.na(effectif_tous_ages) | effectif_tous_ages < n_champ)
  if (nrow(incoherent) > 0)
    stop("01c : ", nrow(incoherent), " cellule(s) territoire x CS avec un effectif tous âges ",
         "inférieur au champ (ou absent) : périmètres SIREN / géographie incohérents entre 01 et 01c.")
}
