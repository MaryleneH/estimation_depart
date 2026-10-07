# ==============================================================================
# 01d_recoder_geo_hors_passage.R — Départements absents de la table de passage
#                                  département -> région : recodés « inconnu »
# ------------------------------------------------------------------------------
# PROBLÈME : la BTS réelle contient des codes de département qui ne sont dans
#            aucune région administrative (ex. « 99 » = hors France ou non
#            localisé). ajouter_region() (00c) s'arrête alors dans 08c / 08d :
#            « N salarié(s) dans K département(s) sans région dans la table de
#            passage ». Ce script règle le cas EN AMONT, sans toucher aux scripts
#            existants : les salariés concernés sont rattachés au territoire
#            « inconnu », que toute la chaîne sait déjà traiter (08, 08b, 08c,
#            08d, 09 : « Territoire inconnu », hors cartes, compté à part).
# PRÉREQUIS : objets `bts` (01 / 01b) et, s'il existe, `stock_tous_ages` (01c) ;
#            fonctions 00c (lire_passage_geo) ; GEO_ANALYSE, GEO_PASSAGES,
#            RECODER_GEO_HORS_PASSAGE (00).
# PRODUIT  : `bts` et `stock_tous_ages` recodés ; objet `geo_hors_passage`
#            (table des codes recodés) ; sorties/geo_hors_passage_<zonage>.csv.
# RÈGLES   : aucune perte silencieuse (chaque code recodé est compté, tracé,
#            affiché) ; aucune donnée inventée (pas de région attribuée) ; sans
#            objet hors zonage département ; ne fait rien si rien n'est à recoder.
# ==============================================================================
if (!isTRUE(get0("RECODER_GEO_HORS_PASSAGE", ifnotfound = TRUE))) {
  message("01d : recodage des départements hors table de passage désactivé (RECODER_GEO_HORS_PASSAGE = FALSE).")
} else if (!identical(get0("GEO_ANALYSE", ifnotfound = ""), "departement")) {
  message("01d : sans objet (zonage d'analyse « ", get0("GEO_ANALYSE", ifnotfound = ""), "», la table de passage département -> région ne s'applique pas).")
} else {
  if (!exists("bts")) stop("Objet 'bts' introuvable : exécutez R/01 et R/01b (ou main.R).")
  if (!exists("lire_passage_geo")) stop("Exécutez d'abord R/00c_fonctions_geo.R (ou main.R).")
  library(dplyr)
  passage_01d <- lire_passage_geo(GEO_PASSAGES[["departement->region"]], "departement->region", nom_requis = TRUE)
  codes_01d <- as.character(bts$geo_code)
  hors_01d <- !is.na(codes_01d) & codes_01d != "inconnu" & !(codes_01d %in% passage_01d$code_source)
  geo_hors_passage <- bts[hors_01d, ] |>
    mutate(geo_code = as.character(geo_code)) |>
    count(geo_code, geo_nom, name = "n_salaries") |>
    arrange(geo_code)
  if (nrow(geo_hors_passage) == 0) {
    message("01d : tous les départements de la BTS ont une région dans la table de passage ; rien à recoder.")
  } else {
    bts <- bts |>
      mutate(geo_code = as.character(geo_code),
             .hors = !is.na(geo_code) & geo_code != "inconnu" & !(geo_code %in% passage_01d$code_source),
             geo_nom  = ifelse(.hors, "Territoire inconnu", as.character(geo_nom)),
             geo_code = ifelse(.hors, "inconnu", geo_code)) |>
      select(-.hors)
    n_stock_01d <- 0L
    if (exists("stock_tous_ages") && !is.null(stock_tous_ages)) {       # même recodage, même regroupement
      s <- stock_tous_ages |> mutate(geo_code = as.character(geo_code))
      hs <- !is.na(s$geo_code) & s$geo_code != "inconnu" & !(s$geo_code %in% passage_01d$code_source)
      n_stock_01d <- sum(hs)
      if (n_stock_01d > 0) {
        s$geo_nom[hs] <- "Territoire inconnu"; s$geo_code[hs] <- "inconnu"
        stock_tous_ages <- s |> group_by(geo_code, geo_nom, cs1) |>
          summarise(effectif_tous_ages = sum(effectif_tous_ages), .groups = "drop")
      }
      rm(s, hs)
    }
    dir.create(DIR_SORTIES, showWarnings = FALSE, recursive = TRUE)
    fichier_01d <- file.path(DIR_SORTIES, sprintf("geo_hors_passage_%s.csv", GEO_ANALYSE))
    write.csv2(geo_hors_passage, fichier_01d, row.names = FALSE)
    message("01d : ", sum(geo_hors_passage$n_salaries), " salarié(s) dans ", nrow(geo_hors_passage),
            " département(s) sans région dans la table de passage (", paste(geo_hors_passage$geo_code, collapse = ", "),
            ") -> rattachés au territoire « inconnu »", if (n_stock_01d > 0) paste0(" (stock tous âges : ", n_stock_01d, " cellule(s) regroupée(s))") else "",
            ". Détail : ", fichier_01d)
    rm(n_stock_01d, fichier_01d)
  }
  rm(passage_01d, codes_01d, hors_01d)
}
