# ==============================================================================
# 02_importer_nettoyer_drees.R — Import et nettoyage de la table DREES
# ------------------------------------------------------------------------------
# PRÉREQUIS : 00_config.R exécuté (FICHIER_DREES)
# ENTRÉE    : data/departretraite_parcsp.csv (jeu DREES « Âge de départ à la
#             retraite et conditions de fin de carrière selon la CSP » ;
#             source : Insee, enquête Emploi / traitements DREES ; 2013-2020)
# PRODUIT   : objets `fdc` (table complète) et `fdc_salaries` (CSP 3 à 6)
#
# FORMAT D'EXPORT (constaté sur données réelles, à consigner en méthodo §3b) :
# les exports du portail DATA.DREES combinent séparateur ';' et décimale
# POINT ('62.9'). Surtout pas read_csv2, qui suppose la décimale virgule et
# traiterait le point en séparateur de milliers (bug x10 : 62.9 -> 629,
# signé par un verif_identite constant à -450). Leçon : on DÉCLARE le format
# avec read_delim, on ne laisse pas un lecteur le deviner.
# ==============================================================================
if (!exists("FICHIER_DREES")) stop("Exécutez d'abord R/00_config.R (ou main.R).")

library(readr)
library(dplyr)

fdc <- read_delim(FICHIER_DREES, delim = ";",
                  locale = locale(encoding = "UTF-8", decimal_mark = "."),
                  show_col_types = FALSE) |>
  rename(csp        = `Catégorie socioprofessionnelle`,
         age_conj   = `Âge conjoncturel de départ à la retraite`,
         d_emploi   = `Durée moyenne en emploi (hors cumul)`,
         d_horsempl = `Durée moyenne sans emploi ni retraite`)
# On ne renomme QUE ce qu'on utilise ; les colonnes GALI (proportions de
# personnes limitées) restent telles quelles pour un usage ultérieur.

# Conversion DÉFENSIVE : avec la bonne locale, d_horsempl arrive normalement
# déjà en numérique et on n'y touche pas. Si un export futur contient des
# marqueurs texte ("ns", "nd", ...), la colonne arrivera en caractère : on la
# convertit alors, les marqueurs devenant NA (warning "parsing failures"
# attendu dans ce cas). parse_number exige du texte, d'où la condition.
fdc <- fdc |>
  mutate(d_horsempl = if (is.character(d_horsempl))
                        parse_number(d_horsempl) else d_horsempl)
print(fdc |> filter(is.na(d_horsempl)) |> count(csp))  # qui a perdu ses valeurs ?

# Garde-fou de plausibilité (né du bug x10 ; migrera dans tests/testthat/) :
# des âges conjoncturels hors de [55, 70] ou un sas hors de [0, 10] ans
# signalent un problème de lecture, pas une réalité sociale.
stopifnot(
  "age_conj hors [55,70] : problème de lecture du CSV ?" =
    all(fdc$age_conj > 55 & fdc$age_conj < 70, na.rm = TRUE),
  "d_horsempl hors [0,10] : problème de lecture du CSV ?" =
    all(fdc$d_horsempl >= 0 & fdc$d_horsempl < 10, na.rm = TRUE)
)

# Décision de CHAMP : CSP 1-2 = indépendants, absents de la BTS (salariés).
fdc_salaries <- fdc |> filter(grepl("^[3-6]", csp))
print(fdc_salaries |> count(csp))                      # 4 CSP attendues
message("02 OK -> objets fdc, fdc_salaries (", nrow(fdc_salaries), " lignes)")
