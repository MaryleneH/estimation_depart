# ==============================================================================
# 07c_tableau_contribution_55plus_sans_gt.R — Tableau de contribution sans {gt},
#                                             restreint aux 55 ans et +
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (script 04) ; DIR_SORTIES, AGE_SENIOR (00)
#
# PRODUIT   :
#   - objet `tab_contribution_55plus`
#   - objet `html_contribution_55plus`
#   - sorties/tableau_contribution_55plus.html
#
# Rôle      : EXACTEMENT le même tableau que 07b (décomposition en « pelures » :
#             législation seule
#             -> + sas
#             -> + invalidité/décès
#             -> total)
#             mais sur le seul champ des AGE_SENIOR ans et +.
#
# Dépendances :
#   - dplyr
#   - tibble
#
# Aucune dépendance à {gt}.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Vérifications
# ------------------------------------------------------------------------------

if (!exists("bts_projete")) {
  stop(
    "Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R)."
  )
}

if (!exists("DIR_SORTIES")) {
  stop(
    "Objet 'DIR_SORTIES' introuvable : exécutez R/00 (ou main.R)."
  )
}

if (!exists("AGE_SENIOR")) {
  stop(
    "Paramètre 'AGE_SENIOR' introuvable : exécutez R/00 (ou main.R)."
  )
}

library(dplyr)


# ------------------------------------------------------------------------------
# 1. Champ : les seniors uniquement
# ------------------------------------------------------------------------------

bts_seniors <- bts_projete |>
  filter(age_2024 >= AGE_SENIOR)


# ------------------------------------------------------------------------------
# 2. Données : les postes de la décomposition
# ------------------------------------------------------------------------------

tot_legis <- sum(
  bts_seniors$pA_central,
  na.rm = TRUE
)

tot_sas <- sum(
  bts_seniors$pB_cal_central,
  na.rm = TRUE
)

tot_B <- sum(
  bts_seniors$pB_central,
  na.rm = TRUE
)

effectif <- nrow(bts_seniors)


# ------------------------------------------------------------------------------
# 3. Construction du tableau de données
# ------------------------------------------------------------------------------

tab_contribution_55plus <- tibble::tibble(

  poste = c(
    "1. Législation seule (âge légal)",
    "2. + Apport du sas de fin de carrière",
    "3. + Apport invalidité et décès",
    "Total (scénario central)"
  ),

  departs = round(
    c(
      tot_legis,
      tot_sas - tot_legis,
      tot_B - tot_sas,
      tot_B
    )
  ),

  lecture = c(
    "Effet pur de l'âge légal de liquidation",
    "Cessations d'emploi avant liquidation",
    "Accidents de la vie (invalidité, décès)",
    "Ensemble des sorties définitives d'ici 2030"
  ),

  est_total = c(
    FALSE,
    FALSE,
    FALSE,
    TRUE
  )

) |>
  mutate(
    part = departs / tot_B
  )


# ------------------------------------------------------------------------------
# 4. Quelques fonctions de mise en forme
# ------------------------------------------------------------------------------

# formatC(format = "d") : affichage ENTIER garanti (0 chiffre après la
# virgule), même si la valeur passée n'a pas été arrondie en amont.
format_nombre <- function(x) {

  formatC(
    round(x),
    format = "d",
    big.mark = " "
  )

}


format_pourcentage <- function(x) {

  paste0(
    round(100 * x),
    " %"
  )

}


# ------------------------------------------------------------------------------
# 5. Valeurs formatées pour l'affichage
# ------------------------------------------------------------------------------

tab_affichage <- tab_contribution_55plus |>
  mutate(

    departs_affichage = format_nombre(departs),

    departs_affichage = ifelse(
      row_number() %in% c(2, 3),
      paste0("+", departs_affichage),
      departs_affichage
    ),

    part_affichage = format_pourcentage(part)

  )


# ------------------------------------------------------------------------------
# 6. Construction des lignes HTML
# ------------------------------------------------------------------------------

lignes_html <- vapply(

  seq_len(nrow(tab_affichage)),

  FUN.VALUE = character(1),

  FUN = function(i) {

    ligne <- tab_affichage[i, ]

    # Style spécifique de la ligne Total
    classe_ligne <- if (ligne$est_total) {
      "ligne-total"
    } else {
      ""
    }

    # Largeur de la barre correspondant à la part du total
    largeur_barre <- max(
      0,
      min(
        100,
        round(100 * ligne$part)
      )
    )

    paste0(

      '<tr class="', classe_ligne, '">',

      '<td class="col-contribution">',
      ligne$poste,
      '</td>',

      '<td class="col-departs">',
      ligne$departs_affichage,
      '</td>',

      '<td class="col-lecture">',
      ligne$lecture,
      '</td>',

      '<td class="col-part">',

      '<div class="part-cell">',

      '<span class="part-valeur">',
      ligne$part_affichage,
      '</span>',

      '<div class="part-meter" role="img" aria-label="',
      ligne$part_affichage,
      ' du total">',

      '<div class="part-bar" style="width:',
      largeur_barre,
      '%;" aria-hidden="true"></div>',

      '</div>',

      '</div>',

      '</td>',

      '</tr>'

    )

  }

)


# ------------------------------------------------------------------------------
# 7. Construction du document HTML complet
# ------------------------------------------------------------------------------

html_contribution_55plus <- paste0(

'<!DOCTYPE html>
<html lang="fr">

<head>

<meta charset="UTF-8">

<meta name="viewport"
      content="width=device-width, initial-scale=1.0">

<title>Départs des salariés de ',
AGE_SENIOR,
' ans et + d’ici 2030</title>

<style>

/* -------------------------------------------------------------------------- */
/* PAGE                                                                       */
/* -------------------------------------------------------------------------- */

body {

  margin: 0;
  padding: 32px;

  background: #ffffff;

  font-family:
    -apple-system,
    BlinkMacSystemFont,
    "Segoe UI",
    Roboto,
    Helvetica,
    Arial,
    sans-serif;

  color: #111827;

}


/* -------------------------------------------------------------------------- */
/* CONTENEUR                                                                  */
/* -------------------------------------------------------------------------- */

.tableau-container {

  max-width: 1100px;

  margin: 0 auto;

}


/* -------------------------------------------------------------------------- */
/* TITRE                                                                      */
/* -------------------------------------------------------------------------- */

.tableau-titre {

  margin: 0 0 6px 0;

  font-size: 22px;

  font-weight: 700;

  color: #111827;

}


.tableau-sous-titre {

  margin: 0 0 24px 0;

  font-size: 15px;

  color: #3d4b5c;

}


/* -------------------------------------------------------------------------- */
/* TABLEAU                                                                    */
/* -------------------------------------------------------------------------- */

table {

  width: 100%;

  border-collapse: collapse;

  table-layout: fixed;

  font-size: 15px;

}


thead th {

  padding: 12px 12px;

  background: #1e3a5f;

  border-bottom: none;

  color: #ffffff;

  font-weight: 700;

  text-align: left;

}


tbody td {

  padding: 14px 12px;

  border-bottom: 1px solid #b9c4d0;

  vertical-align: middle;

}


/* -------------------------------------------------------------------------- */
/* LARGEUR DES COLONNES                                                       */
/* -------------------------------------------------------------------------- */

.col-contribution {

  width: 30%;

}


.col-departs {

  width: 12%;

  text-align: right;

  font-variant-numeric: tabular-nums;

}


.col-lecture {

  width: 38%;

}


tbody td.col-lecture {

  color: #3d4b5c;

}


.col-part {

  width: 20%;

}


/* -------------------------------------------------------------------------- */
/* LIGNE TOTAL                                                                */
/* -------------------------------------------------------------------------- */

.ligne-total td {

  background: #dbe5f1;

  color: #0f2a44;

  font-weight: 700;

  border-top: 3px solid #1e3a5f;

  border-bottom: none;

}


/* -------------------------------------------------------------------------- */
/* BARRE DE PART                                                              */
/* -------------------------------------------------------------------------- */

/* Le pourcentage vit HORS de la barre : un texte posé sur une barre qui
   grandit ne peut pas garantir son contraste ; posee a cote, si.        */

.part-cell {

  display: flex;

  align-items: center;

  gap: 10px;

}


.part-valeur {

  min-width: 3.2em;

  text-align: right;

  font-weight: 700;

  font-variant-numeric: tabular-nums;

  color: #111827;

}


.part-meter {

  position: relative;

  flex: 1;

  height: 12px;

  background: #cfd8e3;

  border-radius: 6px;

  overflow: hidden;

}


.part-bar {

  position: absolute;

  top: 0;
  left: 0;

  height: 100%;

  background: #1e5c8a;

  border-radius: 6px;

}


/* Masquage visuel accessible (caption lue par les lecteurs vocaux) */

.sr-only {

  position: absolute;

  width: 1px;
  height: 1px;

  margin: -1px;
  padding: 0;

  overflow: hidden;

  clip: rect(0 0 0 0);

  white-space: nowrap;

  border: 0;

}


/* -------------------------------------------------------------------------- */
/* SOURCE                                                                     */
/* -------------------------------------------------------------------------- */

.source {

  margin-top: 14px;

  padding-top: 10px;

  border-top: 1px solid #b9c4d0;

  font-size: 12.5px;

  line-height: 1.5;

  color: #3d4b5c;

}


/* -------------------------------------------------------------------------- */
/* IMPRESSION                                                                 */
/* -------------------------------------------------------------------------- */

@media print {

  body {

    padding: 0;

  }

  .tableau-container {

    max-width: none;

  }

}

</style>

</head>


<body>

<div class="tableau-container">


<h1 class="tableau-titre">
Départs des salariés de ',
AGE_SENIOR,
' ans et + d’ici 2030 : décomposition par cause
</h1>


<p class="tableau-sous-titre">
Salariés de ',
AGE_SENIOR,
' ans et + en 2024 — scénario central —
',
format_nombre(effectif),
' salariés
</p>


<table>

<caption class="sr-only">Décomposition par cause des départs attendus d’ici 2030 des salariés seniors</caption>

<thead>

<tr>

<th scope="col" class="col-contribution">
Contribution
</th>

<th scope="col" class="col-departs">
Départs
</th>

<th scope="col" class="col-lecture">
Lecture
</th>

<th scope="col" class="col-part">
Part du total
</th>

</tr>

</thead>


<tbody>

',
paste(
  lignes_html,
  collapse = "\n"
),
'

</tbody>

</table>


<div class="source">

<strong>Sources :</strong>
DREES, jeu <em>departretraite_parcsp</em>
(Insee, enquête Emploi) ;
quotients de mortalité Insee —
calculs propres SSM Défense.
Données individuelles : Insee - BTS 2024.

</div>


</div>

</body>

</html>'
)


# ------------------------------------------------------------------------------
# 8. Création du dossier si nécessaire
# ------------------------------------------------------------------------------

if (!dir.exists(DIR_SORTIES)) {

  dir.create(
    DIR_SORTIES,
    recursive = TRUE
  )

}


# ------------------------------------------------------------------------------
# 9. Export HTML
# ------------------------------------------------------------------------------

fichier_html <- file.path(
  DIR_SORTIES,
  "tableau_contribution_55plus.html"
)

writeLines(
  html_contribution_55plus,
  con = fichier_html,
  useBytes = TRUE
)


# ------------------------------------------------------------------------------
# 10. Message de contrôle
# ------------------------------------------------------------------------------

message(
  "07c OK -> ",
  fichier_html
)


# ------------------------------------------------------------------------------
# 11. Contrôles dans la console
# ------------------------------------------------------------------------------

# Affichage arrondi (0 chiffre après la virgule), part en % entier ; l'objet
# `tab_contribution_55plus` garde ses valeurs exactes pour d'éventuels calculs.
print(
  tab_contribution_55plus |>
    mutate(
      departs = round(departs),
      part = paste0(round(100 * part), " %")
    )
)
