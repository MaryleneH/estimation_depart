# ==============================================================================
# 08j_metiers_liste_territoires.R — Les métiers de la liste, territoire par territoire
# ------------------------------------------------------------------------------
# PRÉREQUIS : `exposition_tension$metiers` (08i : la liste de métiers du classeur PCS -> FAP,
#             tension, levier) et `departs_pcs$diffusion` (08c : tables de DIFFUSION
#             département x PCS, région x PCS, France x PCS). Fonctions 00k et 00l.
# PRODUIT   : sorties/typologie_territoriale/diffusion/
#               metiers_liste_territoires.csv     une ligne par métier x territoire (régions,
#                                                  départements) : salariés, départs attendus,
#                                                  part susceptible de partir, repère France,
#                                                  écart en points, lecture ; secret du 08c
#               fiche_metiers_liste.html          page sans script (imprimable) : un bloc par métier
#                                                  (régions puis départements), puis la vue par territoire
#               tableau_metiers_liste.html        la lecture en UN ÉCRAN (SPEC-TYPO-110) : grille métiers x
#                                                  régions colorée par lecture, panneau de détail au clic
#               metiers_liste_territoires.xlsx    classeur : Lisez-moi, Métiers x territoires,
#                                                  Par territoire, Paramètres (si TENSION_CLASSEUR_EXCEL)
#             Objet `metiers_liste_territoires` (table, fiche, tableau, classeur).
# RÈGLE     : rien n'est recalculé ; aucune valeur masquée n'est écrite ; la liste absente
#             (08i non exécuté, classeur absent) ou les tables de diffusion absentes = message,
#             jamais un arrêt (SPEC-TYPO-109).
# ==============================================================================
if (!exists("exposition_tension") || is.null(exposition_tension$metiers)) {
  message("08j : liste de métiers absente (08i non exécuté ou classeur PCS -> FAP de tension absent) : fiche des métiers par territoire non produite.")
} else if (!exists("departs_pcs") || is.null(departs_pcs$diffusion) || is.null(departs_pcs$diffusion$departement) || is.null(departs_pcs$diffusion$france)) {
  message("08j : tables de diffusion du 08c absentes (DEPARTS_PCS_SECRET = FALSE ou niveau département non produit) : fiche des métiers par territoire non produite.")
} else {
  if (!exists("table_metiers_territoires")) stop("08j : exécutez d'abord R/00_config_fonctions/00l_fonctions_metiers_liste.R (ou main.R).")
  library(dplyr)
  par_08j <- parametres_tension()
  dir_08j <- file.path(DIR_SORTIES, "typologie_territoriale", "diffusion"); dir.create(dir_08j, showWarnings = FALSE, recursive = TRUE)
  source_08j <- if (identical(get0("SOURCE_BTS", ifnotfound = ""), "parquet")) "BTS 2024, DREES, EACR, Insee — calculs propres" else "table test — calculs propres"
  dif_reg_08j <- if (!is.null(departs_pcs$diffusion$region)) departs_pcs$diffusion$region else NULL
  tab_08j <- table_metiers_territoires(exposition_tension$metiers, departs_pcs$diffusion$departement, dif_reg_08j, departs_pcs$diffusion$france, par_08j)
  # contrôles bloquants : aucune valeur pour une cellule non diffusée ; chaque cellule visible existe telle quelle dans la diffusion du 08c
  if (any(!is.na(as.matrix(tab_08j[!tab_08j$diffuse, c("effectif_champ", "departs_central", "taux_pct", "ecart_pts")]))))
    stop("08j : valeur présente pour une cellule non diffusée.")
  chk_08j <- tab_08j |> filter(diffuse, niveau == "département") |> mutate(pcs_n = normaliser_code_pcs(pcs)) |>
    inner_join(departs_pcs$diffusion$departement |> mutate(pcs_n = normaliser_code_pcs(pcs), geo_code = as.character(geo_code)) |> select(pcs_n, geo_code, m = masque, t = taux_depart_central_pct), by = c("pcs_n", "geo_code"))
  if (any(chk_08j$m %in% TRUE) || !isTRUE(all.equal(chk_08j$t, chk_08j$taux_pct))) stop("08j : une cellule visible ne correspond pas à la diffusion du 08c.")
  rm(chk_08j)
  write.csv2(formater_restitution(tab_08j |> select(-ordre_liste, -effectif_france, -departs_france, -departs_france_bas, -departs_france_haut, -region_code, -region_nom),
                                  taux = c("taux_pct", "taux_france_pct", "ecart_pts")),
             file.path(dir_08j, "metiers_liste_territoires.csv"), row.names = FALSE)
  fiche_08j <- file.path(dir_08j, "fiche_metiers_liste.html")
  generer_fiche_metiers_liste(tab_08j, exposition_tension$metiers, fiche_08j, construire_libelle_champ(), par_08j, source_08j)
  # la lecture en un écran (SPEC-TYPO-110) : grille métiers x régions + panneau de détail ; mêmes cellules, aucune valeur masquée
  tableau_08j <- file.path(dir_08j, "tableau_metiers_liste.html")
  generer_tableau_metiers_liste(tab_08j, exposition_tension$metiers, tableau_08j, construire_libelle_champ(), par_08j, source_08j)
  xlsx_08j <- NA_character_
  if (isTRUE(par_08j$classeur_excel))
    xlsx_08j <- tryCatch(ecrire_classeur_tension(tables_classeur_metiers_liste(tab_08j, par_08j, construire_libelle_champ(), source_08j), file.path(dir_08j, "metiers_liste_territoires.xlsx")),
                         error = function(e) { message("08j : classeur Excel non produit (", conditionMessage(e), ") — le CSV reste la référence."); NA_character_ })
  metiers_liste_territoires <- list(table = tab_08j, fiche = fiche_08j, tableau = tableau_08j, classeur = xlsx_08j)
  cat(sprintf("\n08j OK -> %s : %d métiers x %d territoires (%d régions, %d départements) ; %d cellule(s) diffusée(s) sur %d ; %d « plus forte qu’en France »\n",
              texte_liste("métiers stratégiques de la liste, territoire par territoire"), length(unique(tab_08j$pcs)), nrow(distinct(tab_08j, niveau, geo_code)),
              sum(tab_08j$niveau[!duplicated(paste(tab_08j$niveau, tab_08j$geo_code))] == "région"), sum(tab_08j$niveau[!duplicated(paste(tab_08j$niveau, tab_08j$geo_code))] == "département"),
              sum(tab_08j$diffuse), nrow(tab_08j), sum(tab_08j$lecture %in% "plus forte qu’en France")))
  cat(sprintf("  fiche : %s ; tableau (un écran) : %s ; classeur : %s\n", fiche_08j, tableau_08j, if (is.na(xlsx_08j)) "non produit" else xlsx_08j))
  rm(par_08j, dir_08j, source_08j, dif_reg_08j, tab_08j, fiche_08j, tableau_08j, xlsx_08j)
}
