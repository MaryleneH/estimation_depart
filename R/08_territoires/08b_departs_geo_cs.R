# ==============================================================================
# 08b_departs_geo_cs.R — Départs attendus d'ici 2030 par TERRITOIRE x GRANDE CS
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (scripts 01-04 : champ AGE_MIN_BTS+, cs1 du
#             01b, contrat geo_code/geo_nom/geo_type) ; fonctions 00c
#             (filtrer_geo_interet, zonage_geo, fichier_sortie_geo) et 00d
#             (indicateurs_secret, masquer_cellules) ; GEO_ANALYSE, GEO_INTERET,
#             SECRET_* (règle Insee), PCS_VERS_CS1 (00).
# PRODUIT   : objet `departs_geo_cs` + sorties/departs_par_<zonage>_cs.csv
#             (+ effectif_tous_ages et part_a_remplacer_pct si le 01c a fourni
#             les effectifs actuels tous âges)
#             (une ligne = un territoire x une grande CS ; avec GEO_ANALYSE =
#             "departement" : departs_par_departement_cs.csv).
# MÉTHODE   : AUCUNE nouvelle définition. Départ = p_central du script 04
#             (espérance : somme des probabilités individuelles, comme 05/07/08),
#             fourchette = p_bas / p_haut ; effectif = tout le champ ; taux =
#             100 x départs / effectif (même formule que le 05, part_departs_pct).
#             Territoire = geo_code/geo_nom déjà construits (01) ; grande CS =
#             cs1 (01b), dans l'ordre métier de PCS_VERS_CS1.
# PÉRIMÈTRE : exactement celui du 08 et du 09 (territoires inconnus regroupés,
#             GEO_INTERET appliqué par filtrer_geo_interet).
# SECRET    : règle Insee de la Base Tous salariés (00_config SECRET_*) :
#             cellule de moins de 5 salariés, de moins de 3 entreprises ou
#             dominée (> 85 %) par une entreprise -> masquée, PLUS la
#             suppression secondaire par territoire (masquer_cellules, 00d) pour
#             que la cellule masquée ne se retrouve pas par différence. Valeurs
#             -> NA, la ligne reste et porte masque = TRUE (l'absence se voit).
# ==============================================================================
if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
if (!all(c("geo_code", "geo_nom", "geo_type", "cs1") %in% names(bts_projete)))
  stop("Colonnes geo_code / geo_nom / geo_type / cs1 requises dans bts_projete : exécutez 01, 01b et 04 à jour.")
if (!exists("filtrer_geo_interet")) stop("Exécutez d'abord R/00c_fonctions_geo.R (ou main.R).")
if (!exists("masquer_cellules")) stop("Exécutez d'abord R/00d_fonctions_fiches.R (ou main.R).")

library(dplyr)

# --- Fonction : agrégation territoire x CS, secret appliqué -------------------
# base     : table au contrat de bts_projete, déjà au périmètre voulu
# regles   : règle Insee (regles_secret(), 00d) ; NULL = aucun secret
# ordre_cs : ordre métier des grandes CS (valeurs de PCS_VERS_CS1)
# Retourne une liste : brut (non masqué, avec les indicateurs de secret
# n_entreprises / part_dominante_pct, pour les contrôles — usage interne) et
# diffusable (sans ces indicateurs, qui révéleraient la structure de la cellule).
calculer_departs_geo_cs <- function(base, regles = regles_secret(),
                                    ordre_cs = unname(PCS_VERS_CS1), stock = NULL) {
  inconnues <- setdiff(unique(base$cs1), ordre_cs)
  if (length(inconnues) > 0)
    stop("Grande(s) CS hors nomenclature PCS_VERS_CS1 : ", paste(inconnues, collapse = ", "))
  brut <- base |>
    mutate(cs1 = factor(cs1, levels = ordre_cs)) |>
    group_by(geo_code, geo_nom, cs1) |>
    summarise(effectif_champ   = n(),
              departs_2030     = sum(p_central),
              departs_bas      = sum(p_bas),
              departs_haut     = sum(p_haut),
              .groups = "drop") |>
    mutate(part_departs_pct = 100 * departs_2030 / effectif_champ) |>
    arrange(geo_code, cs1) |>
    mutate(cs1 = as.character(cs1)) |>
    # indicateurs de secret de la cellule (entreprises, dominance) : règle Insee
    left_join(indicateurs_secret(base, c("geo_code", "cs1")), by = c("geo_code", "cs1"))
  # Effectifs actuels tous âges (01c) : part de la catégorie à remplacer
  if (!is.null(stock))
    brut <- brut |>
      left_join(stock |> select(geo_code, cs1, effectif_tous_ages), by = c("geo_code", "cs1")) |>
      mutate(part_a_remplacer_pct = 100 * departs_2030 / effectif_tous_ages)
  # Secret statistique : règle Insee (primaire) + secondaire par territoire
  # (bloc = les CS d'un territoire, dont le total est publié par 08 et 09)
  diffusable <- brut |>
    group_by(geo_code) |>
    mutate(masque = masquer_cellules(effectif_champ, n_entreprises, part_dominante_pct, regles)) |>
    ungroup() |>
    mutate(across(any_of(c("departs_2030", "departs_bas", "departs_haut", "part_departs_pct", "part_a_remplacer_pct")),
                  ~ ifelse(masque, NA_real_, round(.x, 1))),
           across(any_of(c("effectif_champ", "effectif_tous_ages")), ~ ifelse(masque, NA_integer_, .x))) |>
    select(-n_entreprises, -part_dominante_pct)
  list(brut = brut, diffusable = diffusable)
}

# --- Base : même périmètre que le 08 et le 09 ---------------------------------
ZON_08B <- zonage_geo(GEO_ANALYSE)
base_geo_cs <- bts_projete |>
  mutate(geo_code = ifelse(is.na(geo_code), "inconnu", as.character(geo_code)),
         geo_nom  = ifelse(geo_code == "inconnu", paste(ZON_08B$libelle, "inconnu(e)"),
                           as.character(geo_nom))) |>
  filtrer_geo_interet(GEO_INTERET, prefixe = "08b")

res_08b <- calculer_departs_geo_cs(base_geo_cs, regles_secret(),
                                   stock = if (exists("stock_tous_ages")) stock_tous_ages else NULL)
departs_geo_cs <- res_08b$diffusable

# --- Contrôles (arrêt si l'un échoue) ----------------------------------------
# Objets temporaires suffixés _08b : ce script ne doit ni écraser ni supprimer
# un objet d'un autre script (ex. `brut` du 02c) — export SANS effet de bord.
brut_08b <- res_08b$brut
# 1. une ligne par territoire x CS
if (anyDuplicated(brut_08b[, c("geo_code", "cs1")]) > 0)
  stop("08b : doublons territoire x CS dans le résultat.")
# 2. cohérence des totaux avec la base (linéarité de l'espérance, comme 05/07/08)
tot_cs_08b <- inner_join(
  base_geo_cs |> group_by(cs1) |> summarise(base = sum(p_central), .groups = "drop"),
  brut_08b |> group_by(cs1) |> summarise(agrege = sum(departs_2030), .groups = "drop"), by = "cs1")
if (!isTRUE(all.equal(sum(brut_08b$departs_2030), sum(base_geo_cs$p_central))) ||
    nrow(tot_cs_08b) != n_distinct(base_geo_cs$cs1) || !isTRUE(all.equal(tot_cs_08b$base, tot_cs_08b$agrege)))
  stop("08b : la somme des départs territoire x CS ne retombe pas sur les totaux de la base.")
if (sum(brut_08b$effectif_champ) != nrow(base_geo_cs))
  stop("08b : l'effectif agrégé ne retombe pas sur l'effectif du champ.")
# 3. tous les territoires du périmètre sont présents
if (!setequal(unique(brut_08b$geo_code), unique(base_geo_cs$geo_code)))
  stop("08b : territoires manquants dans le résultat.")
# 4. aucune modalité de CS inattendue (déjà bloqué dans la fonction) ; 5. secret :
#    aucune cellule visible ne viole la règle Insee (salariés, entreprises, dominance)
if (any(!departs_geo_cs$masque &
        secret_primaire(brut_08b$effectif_champ, brut_08b$n_entreprises, brut_08b$part_dominante_pct, regles_secret())))
  stop("08b : une cellule contraire à la règle de secret statistique n'est pas masquée.")
if (any(is.na(departs_geo_cs$effectif_champ) & !departs_geo_cs$masque))
  stop("08b : NA sans indicateur de masquage.")

# --- Export tableur FR (mêmes conventions que 05/08) -------------------------
fichier_08b <- fichier_sortie_geo("departs_par_%s_cs.csv", GEO_ANALYSE)
write.csv2(departs_geo_cs, fichier_08b, row.names = FALSE)
# 6. le fichier est bien écrit
if (!file.exists(fichier_08b)) stop("08b : fichier non créé : ", fichier_08b)

cat(sprintf("\n--- Départs attendus d'ici 2030 par %s x grande CS (scénario central) ---\n",
            tolower(ZON_08B$libelle)))
print(head(as.data.frame(departs_geo_cs), 12), row.names = FALSE)
n_masq_08b <- sum(departs_geo_cs$masque)
message("08b OK -> ", fichier_08b, " (", nrow(departs_geo_cs), " lignes, ",
        n_distinct(departs_geo_cs$geo_code), " ", ZON_08B$pluriel,
        if (n_masq_08b > 0) paste0(" ; ", n_masq_08b, " cellule(s) masquée(s), secret statistique") else "", ")")
rm(brut_08b, tot_cs_08b, res_08b, n_masq_08b)   # uniquement les temporaires de CE script
