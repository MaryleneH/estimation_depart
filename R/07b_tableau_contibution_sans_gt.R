# ==============================================================================
# 07_tableau_contribution.R — Tableau de contribution sans {gt}
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (script 04) ; DIR_SORTIES (00)
#
# PRODUIT   :
#   - objet `tab_contribution`
#   - objet `html_contribution`
#   - sorties/tableau_contribution.html
#
# Rôle      : présenter la décomposition en « pelures » :
#             législation seule
#             -> + sas
#             -> + invalidité/décès
#             -> total
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

library(dplyr)


# ------------------------------------------------------------------------------
# 1. Données : les postes de la décomposition
# ------------------------------------------------------------------------------

tot_legis <- sum(
  bts_projete$pA_central,
  na.rm = TRUE
)

tot_sas <- sum(
  bts_projete$pB_cal_central,
  na.rm = TRUE
)

tot_B <- sum(
  bts_projete$pB_central,
  na.rm = TRUE
)

effectif <- nrow(bts_projete)


# ------------------------------------------------------------------------------
# 2. Construction du tableau de données
# ------------------------------------------------------------------------------

tab_contribution <- tibble::tibble(

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
# 3. Quelques fonctions de mise en forme
# ------------------------------------------------------------------------------

format_nombre <- function(x) {

  format(
    x,
    big.mark = "\u00a0",
    scientific = FALSE,
    trim = TRUE
  )

}


format_pourcentage <- function(x) {

  paste0(
    round(100 * x),
    "\u00a0%"
  )

}


# ------------------------------------------------------------------------------
# 4. Valeurs formatées pour l'affichage
# ------------------------------------------------------------------------------

tab_affichage <- tab_contribution |>
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
# 5. Construction des lignes HTML
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

      '<div class="part-wrapper">',

      '<div class="part-bar" style="width:',
      largeur_barre,
      '%;"></div>',

      '<span class="part-label">',
      ligne$part_affichage,
      '</span>',

      '</div>',

      '</td>',

      '</tr>'

    )

  }

)


# ------------------------------------------------------------------------------
# 6. Construction du document HTML complet
# ------------------------------------------------------------------------------

html_contribution <- paste0(

'<!DOCTYPE html>
<html lang="fr">

<head>

<meta charset="UTF-8">

<meta name="viewport"
      content="width=device-width, initial-scale=1.0">

<title>Départs de salariés d’ici 2030</title>

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

  color: #1f2937;

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

  color: #1f2937;

}


.tableau-sous-titre {

  margin: 0 0 24px 0;

  font-size: 14px;

  color: #64748b;

}


/* -------------------------------------------------------------------------- */
/* TABLEAU                                                                    */
/* -------------------------------------------------------------------------- */

table {

  width: 100%;

  border-collapse: collapse;

  table-layout: fixed;

  font-size: 14px;

}


thead th {

  padding: 12px 12px;

  background: #f4f6f8;

  border-bottom: 2px solid #cbd5e1;

  color: #334155;

  font-weight: 700;

  text-align: left;

}


tbody td {

  padding: 14px 12px;

  border-bottom: 1px solid #e5e7eb;

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

  color: #475569;

}


.col-part {

  width: 20%;

}


/* -------------------------------------------------------------------------- */
/* LIGNE TOTAL                                                                */
/* -------------------------------------------------------------------------- */

.ligne-total td {

  background: #eef2f7;

  font-weight: 700;

  border-top: 2px solid #2f6da4;

  border-bottom: none;

}


/* -------------------------------------------------------------------------- */
/* BARRE DE PART                                                              */
/* -------------------------------------------------------------------------- */

.part-wrapper {

  position: relative;

  height: 28px;

  background: #edf2f7;

  border-radius: 4px;

  overflow: hidden;

}


.part-bar {

  position: absolute;

  top: 0;
  left: 0;

  height: 100%;

  background: #b8cee2;

}


.part-label {

  position: relative;

  z-index: 2;

  display: block;

  width: 100%;

  line-height: 28px;

  padding-right: 8px;

  box-sizing: border-box;

  text-align: right;

  font-weight: 600;

  color: #243746;

}


/* -------------------------------------------------------------------------- */
/* SOURCE                                                                     */
/* -------------------------------------------------------------------------- */

.source {

  margin-top: 14px;

  padding-top: 10px;

  border-top: 1px solid #e5e7eb;

  font-size: 11px;

  line-height: 1.5;

  color: #64748b;

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
Départs de salariés d’ici 2030 : décomposition par cause
</h1>


<p class="tableau-sous-titre">
Salariés de 43 ans et + en 2024 — scénario central —
',
format_nombre(effectif),
' salariés
</p>


<table>

<thead>

<tr>

<th class="col-contribution">
Contribution
</th>

<th class="col-departs">
Départs
</th>

<th class="col-lecture">
Lecture
</th>

<th class="col-part">
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
# 7. Création du dossier si nécessaire
# ------------------------------------------------------------------------------

if (!dir.exists(DIR_SORTIES)) {

  dir.create(
    DIR_SORTIES,
    recursive = TRUE
  )

}


# ------------------------------------------------------------------------------
# 8. Export HTML
# ------------------------------------------------------------------------------

fichier_html <- file.path(
  DIR_SORTIES,
  "tableau_contribution.html"
)

writeLines(
  html_contribution,
  con = fichier_html,
  useBytes = TRUE
)


# ------------------------------------------------------------------------------
# 9. Message de contrôle
# ------------------------------------------------------------------------------

message(
  "07 OK -> ",
  fichier_html
)


# ------------------------------------------------------------------------------
# 10. Contrôles dans la console
# ------------------------------------------------------------------------------

print(tab_contribution)
