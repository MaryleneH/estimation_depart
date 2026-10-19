# ==============================================================================
# 01c_stock_tous_ages.R — Effectifs ACTUELS tous âges par territoire x grande CS
# ------------------------------------------------------------------------------
# PRÉREQUIS : 00_config.R (STOCK_TOUS_AGES, SOURCE_BTS, FICHIER_BTS, COL_BTS,
#             COL_GEO, PCS_VERS_CS1, bloc GEO_*, STOCK_TOUS_AGES_TRANCHES), 00c
#             (normaliser_geo), 00h (tranches_stock_ages, pyramide), objet
#             `bts` du 01/01b (champ AGE_MIN_BTS+, cs1, contrat geo_*).
# PRODUIT   : objet `stock_tous_ages` (geo_code, geo_nom, cs1, effectif_tous_ages)
#             ou NULL si indisponible — jamais bloquant ; objet `stock_tranches_ages`
#             (pyramide des âges, specs/02 SPEC-PREP-014, specs/10 §12) : liste
#             cs (geo_code, geo_nom, cs1, tranche_age, effectif) et territoire
#             (geo_code, tranche_age, effectif, n_entreprises, part_dominante_pct),
#             ou NULL (STOCK_TOUS_AGES_TRANCHES = FALSE, stock indisponible).
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
stock_tranches_ages <- NULL
tranches_01c <- if (!isTRUE(get0("STOCK_TOUS_AGES_TRANCHES", ifnotfound = FALSE))) NULL else if (!exists("tranches_stock_ages")) {
  message("01c : fonctions de la pyramide (00h) non chargées -> pyramide des âges non construite (stock_tranches_ages = NULL).")
  NULL
} else tranches_stock_ages()

if (!isTRUE(get0("STOCK_TOUS_AGES", ifnotfound = TRUE))) {
  message("01c : effectifs tous âges désactivés (STOCK_TOUS_AGES = FALSE).")

} else if (SOURCE_BTS == "parquet") {
  # ---------------------------------------------------------- agrégation Arrow
  if (!requireNamespace("arrow", quietly = TRUE)) stop("Le package 'arrow' est requis.")
  if (!exists("sirens_base")) {
    sirens_base <- readr::read_lines(FICHIER_SIREN); sirens_base <- trimws(sirens_base[sirens_base != ""])
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
    filter(.data[[col_siren]] %in% sirens_base) |>            # même périmètre Base, SANS filtre d'âge
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
    if (!is.null(tranches_01c)) {
      # Pyramide : même lecture, même périmètre ; tranche calculée par Arrow sur l'âge
      # (somme d'indicatrices), comptage par cellule x SIREN pour le secret.
      b2 <- tranches_01c$borne_inf[2]; b3 <- tranches_01c$borne_inf[3]; b4 <- tranches_01c$borne_inf[4]
      contrat_t <- c(contrat, siren = unname(col_siren))
      requete_t <- ds |>
        filter(.data[[col_siren]] %in% sirens_base) |>
        select(all_of(unname(contrat_t))) |>
        rename(all_of(contrat_t)) |>
        mutate(tranche_i = 1L + as.integer(age >= b2) + as.integer(age >= b3) + as.integer(age >= b4))
      cles_t <- c(cles, "tranche_i", "siren")
      agg_t <- tryCatch(
        requete_t |> group_by(across(all_of(cles_t))) |> summarise(n = n(), .groups = "drop") |> collect(),
        error = function(e) {
          message("01c : agrégation Arrow par tranche non supportée (", conditionMessage(e), ") -> comptage en R.")
          requete_t |> collect() |> group_by(across(all_of(cles_t))) |> summarise(n = n(), .groups = "drop")
        })
      agg_t <- agg_t |>
        mutate(.pcs_txt = trimws(sub("\\.0$", "", as.character(pcs))),
               cs1 = unname(PCS_VERS_CS1[substr(.pcs_txt, 1, 1)]),
               tranche_age = tranches_01c$cle[tranche_i]) |>
        filter(!is.na(cs1)) |>
        normaliser_geo(geo_source = GEO_SOURCE, geo_analyse = GEO_ANALYSE, largeur = GEO_CODE_LARGEUR,
                       referentiels = GEO_REFERENTIELS, passages = GEO_PASSAGES,
                       geo_interet = GEO_INTERET, prefixe = "01c") |>
        mutate(geo_code = ifelse(is.na(geo_code), "inconnu", as.character(geo_code)))
      n_age_na_01c <- sum(agg_t$n[is.na(agg_t$tranche_age)])
      if (n_age_na_01c > 0)
        message("01c : ", n_age_na_01c, " salarié(s) d'âge manquant comptés dans stock_tous_ages mais dans aucune tranche de la pyramide.")
      agg_t <- agg_t |> filter(!is.na(tranche_age))
      stock_tranches_ages <- list(
        cs = agg_t |> group_by(geo_code, geo_nom, cs1, tranche_age) |> summarise(effectif = sum(n), .groups = "drop"),
        territoire = indicateurs_secret_effectif(agg_t, c("geo_code", "tranche_age")))
      rm(b2, b3, b4, contrat_t, requete_t, cles_t, agg_t)
    }
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
  if (!is.null(tranches_01c)) {
    # Pyramide simulée : tranches du champ EXACTES (bts) ; moins de AGE_MIN_BTS ans
    # (stock simulé ci-dessus) répartis entre jeunes et milieu par un tirage
    # reproductible ; secret des tranches simulées = indicateurs du territoire.
    champ_01c <- bts |> mutate(tranche_age = affecter_tranche_stock(age_2024, tranches_01c))
    set.seed(GRAINE + 2)
    moins_01c <- stock_tous_ages |>
      left_join(bts |> count(geo_code, cs1, name = "n_champ"), by = c("geo_code", "cs1")) |>
      mutate(n_moins = effectif_tous_ages - coalesce(n_champ, 0L), jeunes = round(n_moins * runif(n(), 0.15, 0.6)), milieu = n_moins - jeunes)
    cs_01c <- bind_rows(champ_01c |> count(geo_code, geo_nom, cs1, tranche_age, name = "effectif"),
                        moins_01c |> transmute(geo_code, geo_nom, cs1, tranche_age = "jeunes", effectif = jeunes),
                        moins_01c |> transmute(geo_code, geo_nom, cs1, tranche_age = "milieu", effectif = milieu))
    sec_01c <- indicateurs_secret_effectif(bts, "geo_code") |> select(geo_code, n_entreprises, part_dominante_pct)
    terr_01c <- bind_rows(indicateurs_secret_effectif(champ_01c, c("geo_code", "tranche_age")),
                          cs_01c |> filter(tranche_age %in% c("jeunes", "milieu")) |> group_by(geo_code, tranche_age) |>
                            summarise(effectif = sum(effectif), .groups = "drop") |> left_join(sec_01c, by = "geo_code"))
    stock_tranches_ages <- list(cs = cs_01c |> arrange(geo_code, cs1, match(tranche_age, tranches_01c$cle)),
                                territoire = terr_01c |> arrange(geo_code, match(tranche_age, tranches_01c$cle)))
    rm(champ_01c, moins_01c, cs_01c, sec_01c, terr_01c)
  }
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

# Pyramide : les tranches du champ égalent EXACTEMENT le champ (SPEC-PREP-014) ;
# sans stock tous âges, aucune pyramide complète (repli du 08f sur le champ).
if (is.null(stock_tous_ages)) stock_tranches_ages <- NULL
if (!is.null(stock_tranches_ages)) {
  champ_t <- stock_tranches_ages$cs |> filter(tranche_age %in% tranches_01c$cle[tranches_01c$dans_champ]) |>
    group_by(geo_code, cs1) |> summarise(n_tranches = sum(effectif), .groups = "drop")
  verif_t <- bts |> count(geo_code, cs1, name = "n_champ") |> full_join(champ_t, by = c("geo_code", "cs1")) |>
    filter(is.na(n_champ) | is.na(n_tranches) | n_champ != n_tranches)
  if (nrow(verif_t) > 0)
    stop("01c : ", nrow(verif_t), " cellule(s) territoire x CS où les tranches ", tranches_01c$libelle[3], " et ", tranches_01c$libelle[4],
         " de la pyramide diffèrent du champ : périmètres SIREN / âge / géographie incohérents entre 01 et 01c ",
         "(STOCK_TOUS_AGES_TRANCHES = FALSE pour désactiver la pyramide complète).")
  message("01c OK -> stock_tranches_ages : pyramide des âges (", paste(tranches_01c$libelle, collapse = ", "), ") sur ",
          n_distinct(stock_tranches_ages$territoire$geo_code), " territoires ; tranches du champ = champ.")
  rm(champ_t, verif_t)
}
