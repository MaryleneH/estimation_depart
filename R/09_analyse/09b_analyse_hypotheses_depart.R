# ==============================================================================
# 09b_analyse_hypotheses_depart.R — Analyse « hypothèses de départ » prête pour une note (specs/11)
# ------------------------------------------------------------------------------
# PRÉREQUIS : bts_projete (04), departs_cs1$diffusion$france (08d) ; fonctions 09a (et 00k pour la
#             tension). Facultatifs : departs_pcs (08c), exposition_tension (08i),
#             metiers_liste_territoires (08j), typologie_departements (08f), synthese_geo_diffusable (08).
# PRODUIT   : sorties/analyse_hypotheses_depart/<zonage>/
#               analyse_hypotheses_depart.xlsx   une feuille par tableau : titre (ligne 1), note (ligne 2),
#                                                tableau (ligne 4) ; Lisez-moi ; Paramètres
#               NN_<tableau>.csv                 le même tableau, référence si le classeur manque
#             Objet `analyse_hypotheses_depart` (tables, classeur, dossier).
# RÈGLES    : seules des tables de DIFFUSION ou des agrégats passés par la règle de secret ; gabarits
#             fixes ; libellé public de la liste ; un prérequis facultatif absent = feuille avec la raison ;
#             GENERER_ANALYSE faux ou prérequis obligatoire absent = message, jamais un arrêt (SPEC-ANA-001).
# ==============================================================================
if (!isTRUE(get0("GENERER_ANALYSE", ifnotfound = TRUE))) {
  message("09b : GENERER_ANALYSE = FALSE : analyse « hypothèses de départ » ignorée.")
} else if (!exists("bts_projete") || !exists("departs_cs1") || is.null(departs_cs1$diffusion) || is.null(departs_cs1$diffusion$france)) {
  message("09b : prérequis absents (bts_projete, diffusion France du 08d) : analyse « hypothèses de départ » non produite.")
} else {
  if (!exists("table_volume_total")) stop("09b : exécutez d'abord R/09_analyse/09a_fonctions_analyse_hypotheses.R (ou main.R).")
  library(dplyr)
  par_09 <- parametres_analyse(); regles_09 <- regles_secret(); champ_09 <- construire_libelle_champ()
  zon_09 <- as.character(get0("GEO_ANALYSE", ifnotfound = "zonage"))
  dir_09 <- file.path(DIR_SORTIES, "analyse_hypotheses_depart", zon_09); dir.create(dir_09, showWarnings = FALSE, recursive = TRUE)
  source_09 <- if (identical(get0("SOURCE_BTS", ifnotfound = ""), "parquet")) sprintf("BTS %d, DREES, EACR, Insee — calculs propres", par_09$annee_bts) else "table test — calculs propres"
  plage_09 <- sprintf("%d → %d", par_09$annee_bts, par_09$fin)
  deltas_09 <- if (exists("delta_central") && exists("delta_bas") && exists("delta_haut")) list(bas = delta_bas, central = delta_central, haut = delta_haut) else NULL

  # --- 1. Hypothèses --------------------------------------------------------------------------
  hyp_09 <- table_hypotheses_analyse(deltas_09, par_09, champ_09)
  # --- 2. Volume ------------------------------------------------------------------------------
  tot_09 <- table_volume_total(bts_projete, regles_09, par_09); total_09 <- attr(tot_09, "total_central")
  tranche_09 <- as.character(cut(bts_projete$age_2024, breaks = BREAKS_TRANCHES, labels = LABELS_TRANCHES))
  age_09 <- table_volume_groupe(bts_projete, tranche_09, "Tranche d’âge", LABELS_TRANCHES, regles_09, total_09)
  lib_sen_09 <- sprintf("%d ans et plus", par_09$age_senior); lib_jeu_09 <- sprintf("moins de %d ans", par_09$age_senior)
  sen_09 <- table_volume_groupe(bts_projete, ifelse(bts_projete$age_2024 >= par_09$age_senior, lib_sen_09, lib_jeu_09), "Salariés", c(lib_jeu_09, lib_sen_09), regles_09, total_09)
  cs_09 <- table_volume_cs1(departs_cs1$diffusion$france, total_09)
  # --- 3. Métiers -----------------------------------------------------------------------------
  pcs_fr_09 <- if (exists("departs_pcs") && !is.null(departs_pcs$diffusion) && !is.null(departs_pcs$diffusion$france)) departs_pcs$diffusion$france else NULL
  lib_pcs_09 <- if (!is.null(pcs_fr_09) && exists("charger_libelles_pcs")) tryCatch(suppressMessages(charger_libelles_pcs()), error = function(e) NULL) else NULL
  met_vol_09 <- if (!is.null(pcs_fr_09)) table_metiers_classement(pcs_fr_09, "volume", lib_pcs_09, par_09$top_n, par_09$effectif_min_part, total_09) else NULL
  met_part_09 <- if (!is.null(pcs_fr_09)) table_metiers_classement(pcs_fr_09, "part", lib_pcs_09, par_09$top_n, par_09$effectif_min_part, total_09) else NULL
  mat_09 <- if (!is.null(pcs_fr_09) && exists("exposition_tension") && !is.null(exposition_tension$metiers) && exists("matrice_metiers_tension"))
    matrice_metiers_tension(exposition_tension$metiers, departs_pcs$analytique$france, pcs_fr_09, parametres_tension())$metiers else NULL
  liste_09 <- if (!is.null(mat_09)) table_metiers_liste_analyse(mat_09) else NULL
  noyau_09 <- if (!is.null(liste_09)) table_noyau_dur_analyse(liste_09) else NULL
  # --- 4. Territoires -------------------------------------------------------------------------
  typo_09 <- if (exists("typologie_departements") && !is.null(typologie_departements$diffusion)) typologie_departements$diffusion else NULL
  expo_dep_09 <- if (exists("exposition_tension") && !is.null(exposition_tension$diffusion)) exposition_tension$diffusion else NULL
  expo_reg_09 <- if (exists("exposition_tension") && !is.null(exposition_tension$diffusion_region)) exposition_tension$diffusion_region else NULL
  reg_09 <- if (!is.null(departs_cs1$diffusion$region)) enrichir_territoires_analyse(table_territoires_cs1(departs_cs1$diffusion$region, "région"), NULL, expo_reg_09) else NULL
  dep_09 <- if (!is.null(departs_cs1$diffusion$departement)) enrichir_territoires_analyse(table_territoires_cs1(departs_cs1$diffusion$departement, "département"), typo_09, expo_dep_09) else NULL
  expo_r_09 <- if (!is.null(expo_reg_09)) table_exposition_territoires_analyse(expo_reg_09, "région") else NULL
  tab_08j_09 <- if (exists("metiers_liste_territoires") && !is.null(metiers_liste_territoires$table)) metiers_liste_territoires$table else NULL
  liste_reg_09 <- if (!is.null(tab_08j_09)) table_liste_regions_analyse(tab_08j_09, par_09$top_n) else NULL
  s55_09 <- if (exists("synthese_geo_diffusable")) table_synthese_seniors_analyse(synthese_geo_diffusable, par_09$age_senior) else NULL
  # --- 5. Priorités ---------------------------------------------------------------------------
  prior_09 <- if (!is.null(mat_09)) table_priorites_analyse(mat_09, tab_08j_09, par_09$n_regions) else NULL

  # --- Assemblage, libellé public, contrôles ----------------------------------------------------
  tables_09 <- list("1 Hypothèses" = hyp_09, "2a Volume" = tot_09, "2b Par âge" = age_09, "2c Seniors" = sen_09, "2d Par catégorie" = cs_09,
                    "3a Métiers volume" = met_vol_09, "3b Métiers part" = met_part_09, "3c Métiers de la liste" = liste_09, "3d Noyau dur" = noyau_09,
                    "4a Régions" = reg_09, "4b Départements" = dep_09, "4c Exposition régions" = expo_r_09, "4d Liste x régions" = liste_reg_09, "4e Seniors par territoire" = s55_09,
                    "5 Priorités" = prior_09)
  presents_09 <- !vapply(tables_09, is.null, TRUE)
  if (exists("tables_texte_liste")) tables_09[presents_09] <- tables_texte_liste(tables_09[presents_09])
  controler_tables_analyse(tables_09)
  r_pcs <- "diffusion France par métier (08c) absente"; r_liste <- "liste de métiers absente (08i non exécuté ou classeur PCS -> FAP absent)"
  raisons_09 <- list("3a Métiers volume" = r_pcs, "3b Métiers part" = r_pcs, "3c Métiers de la liste" = r_liste, "3d Noyau dur" = r_liste, "5 Priorités" = r_liste,
                     "4a Régions" = "diffusion par région (08d) absente : niveau non produit pour ce zonage", "4b Départements" = "diffusion par département (08d) absente : niveau non produit pour ce zonage",
                     "4c Exposition régions" = "exposition par région (08i) absente", "4d Liste x régions" = "table métier x territoire (08j) absente",
                     "4e Seniors par territoire" = "synthèse par territoire (08) absente")
  lib_liste_09 <- if (exists("lib_liste")) lib_liste() else "métiers de la liste"
  titres_09 <- list(
    "1 Hypothèses" = c(sprintf("Hypothèses et critères de la projection %s", plage_09), "Chaque hypothèse, sa valeur retenue, sa source et le sens dans lequel elle tire le résultat. Toutes les valeurs viennent de la configuration de la chaîne."),
    "2a Volume" = c(sprintf("Volume des départs attendus, %s", plage_09), "Scénario central encadré par les hypothèses basse (départs retardés) et haute (départs avancés). Décomposition par cause au scénario central."),
    "2b Par âge" = c(sprintf("Départs attendus par tranche d’âge, %s", plage_09), "Âge à la date de la photo BTS. Une tranche non diffusée n’a aucune valeur (secret statistique)."),
    "2c Seniors" = c(sprintf("Départs attendus : seniors et autres salariés, %s", plage_09), sprintf("Seniors = %s à la date de la photo BTS.", lib_sen_09)),
    "2d Par catégorie" = c(sprintf("Départs attendus par grande catégorie sociale, %s", plage_09), "Table de diffusion nationale par catégorie (08d). Part susceptible de partir = départs / salariés de la catégorie."),
    "3a Métiers volume" = c(sprintf("Métiers portant le plus de départs, %s", plage_09), sprintf("Les %d métiers (PCS) diffusés classés par départs attendus décroissants. Lecture : les chantiers de remplacement en masse.", par_09$top_n)),
    "3b Métiers part" = c(sprintf("Métiers qui s’érodent le plus vite, %s", plage_09), sprintf("Les %d métiers (PCS) d’au moins %d salariés classés par part susceptible de partir décroissante. Lecture : les compétences en risque d’extinction.", par_09$top_n, par_09$effectif_min_part)),
    "3c Métiers de la liste" = c(sprintf("Les %s face aux départs et à la tension, %s", lib_liste_09, plage_09), "Situation = croisement de la vitesse des départs (part susceptible de partir) et de la tension nationale de la famille professionnelle (France Travail). La tension ne mesure jamais celle d’un territoire."),
    "3d Noyau dur" = c(sprintf("Noyau dur : départs rapides et métier en tension, %s", plage_09), "Métiers de la liste en situation « départs rapides et métier en tension », par part susceptible de partir décroissante, avec le levier dominant."),
    "4a Régions" = c(sprintf("Départs attendus par région, %s", plage_09), "Total des catégories sociales diffusées ; une région dont une cellule est masquée n’est pas totalisée. Rang par volume et rang par part parmi les régions diffusées."),
    "4b Départements" = c(sprintf("Départs attendus par département, %s", plage_09), "Même règle que les régions ; profil de la typologie et exposition aux remplacements difficiles quand ils sont diffusés. Un département non diffusé n’est pas un département sans départ."),
    "4c Exposition régions" = c("Exposition des régions aux remplacements difficiles", "Part des départs des métiers suivis qui concernent des métiers en tension en France ; classe, robustesse de la classe, plancher de départs dans ces métiers."),
    "4d Liste x régions" = c(sprintf("Où les %s partent plus vite qu’en France", lib_liste_09), sprintf("Couples métier x région dont la part susceptible de partir dépasse celle de la France d’au moins %s points, par écart décroissant.", .f1_ana(get0("TENSION_ECART_POINTS", ifnotfound = NA_real_)))),
    "4e Seniors par territoire" = c(sprintf("Seniors par %s, %s", tryCatch(GEO_ZONAGES[[zon_09]]$libelle, error = function(e) NULL) %||% zon_09, plage_09), sprintf("Part des %s dans les salariés du champ et leurs départs attendus, par territoire du zonage d’analyse.", lib_sen_09)),
    "5 Priorités" = c("Priorités : métier, territoire, levier", sprintf("Pour chaque métier du noyau dur : le levier dominant et ses %d premières régions diffusées par part susceptible de partir.", par_09$n_regions)))
  feuilles_09 <- lapply(names(tables_09), function(nom) list(titre = titres_09[[nom]][1], note = titres_09[[nom]][2], table = tables_09[[nom]], raison = if (is.null(tables_09[[nom]])) paste0("Non produit : ", raisons_09[[nom]], ".") else NULL))
  names(feuilles_09) <- names(tables_09)
  # CSV (référence)
  fichiers_09 <- c("1 Hypothèses" = "01_hypotheses", "2a Volume" = "02a_volume", "2b Par âge" = "02b_par_age", "2c Seniors" = "02c_seniors", "2d Par catégorie" = "02d_par_categorie",
                   "3a Métiers volume" = "03a_metiers_volume", "3b Métiers part" = "03b_metiers_part", "3c Métiers de la liste" = "03c_metiers_liste", "3d Noyau dur" = "03d_noyau_dur",
                   "4a Régions" = "04a_regions", "4b Départements" = "04b_departements", "4c Exposition régions" = "04c_exposition_regions", "4d Liste x régions" = "04d_liste_regions",
                   "4e Seniors par territoire" = "04e_seniors_territoires", "5 Priorités" = "05_priorites")
  for (nom in names(tables_09)[presents_09]) write.csv2(tables_09[[nom]], file.path(dir_09, paste0(fichiers_09[[nom]], ".csv")), row.names = FALSE, na = "")
  # Lisez-moi et paramètres
  P_09 <- construire_libelles_public(champ_09); gras <- function(x) structure(x, gras = TRUE)
  lignes_09 <- list(gras(sprintf("Hypothèses de départ %s : analyse prête pour une note", plage_09)),
    sprintf(texte_perimetre("%s en %d · entreprises du périmètre Base · départs attendus entre %d et %d (horizon du modèle : %d ans)"), P_09$salaries_age, par_09$annee_bts, par_09$annee_bts, par_09$fin, par_09$horizon),
    sprintf("Classeur produit le %s. Sources : %s.", format(Sys.Date(), "%d/%m/%Y"), source_09), "",
    gras("Comment coller un tableau dans Word"),
    "Sur une feuille, sélectionner le tableau à partir de la ligne 4 (en-tête comprise) jusqu’à la dernière ligne, copier, puis coller dans Word avec « Conserver la mise en forme source ». La ligne 1 donne le titre du tableau, la ligne 2 la note de lecture à reprendre en légende.", "",
    gras("Vocabulaire"),
    "Départ = sortie définitive de l’emploi d’ici la fin de la plage : retraite ou fin de carrière, invalidité, décès ; les mobilités vers un autre employeur ne sont pas comptées (le total est un plancher).",
    "Part susceptible de partir = départs attendus / salariés du champ, en %. Hypothèse basse = départs retardés ; hypothèse haute = départs avancés ; scénario central entre les deux.",
    sprintf("Secret statistique : une cellule est publiée si elle compte au moins %d salariés et %d entreprises et qu’aucune entreprise n’y pèse plus de %d %% ; secret secondaire ensuite. Une cellule non diffusée est vide, jamais zéro.", SECRET_MIN_SALARIES, SECRET_MIN_ENTREPRISES, SECRET_DOMINANCE_PCT),
    sprintf("La tension d’un métier est celle de sa famille professionnelle en France (France Travail) ; elle ne mesure jamais la tension d’un territoire. Seuls les %s sont observés sur cette dimension.", lib_liste_09), "",
    gras("Les feuilles"),
    paste0(names(tables_09), " : ", vapply(names(tables_09), function(n) titres_09[[n]][1], ""), ifelse(presents_09, "", " (non produit, voir la feuille)")),
    "Paramètres : valeurs de configuration utilisées.")
  gras_09 <- unlist(lapply(lignes_09, function(x) rep(isTRUE(attr(x, "gras")), length(x)))); lignes_09 <- unlist(lapply(lignes_09, as.character))
  lisez_09 <- data.frame(texte = lignes_09, stringsAsFactors = FALSE); attr(lisez_09, "gras") <- gras_09
  param_09 <- data.frame(parametre = c("annee_bts", "horizon", "plage", "age_min", "age_senior", "scenario", "zonage", "top_n", "effectif_min_part", "n_regions_priorites", "secret_min_salaries", "secret_min_entreprises", "secret_dominance_pct", "libelle_liste", "ecart_points"),
                         valeur = as.character(c(par_09$annee_bts, par_09$horizon, plage_09, get0("AGE_MIN_BTS", ifnotfound = NA), par_09$age_senior, get0("SCENARIO", ifnotfound = NA), zon_09, par_09$top_n, par_09$effectif_min_part, par_09$n_regions,
                                                 SECRET_MIN_SALARIES, SECRET_MIN_ENTREPRISES, SECRET_DOMINANCE_PCT, lib_liste_09, get0("TENSION_ECART_POINTS", ifnotfound = NA))), stringsAsFactors = FALSE)
  write.csv2(param_09, file.path(dir_09, "parametres.csv"), row.names = FALSE)
  # Classeur : jamais d'arrêt
  xlsx_09 <- tryCatch(ecrire_classeur_analyse(feuilles_09, file.path(dir_09, "analyse_hypotheses_depart.xlsx"), lisez_09, param_09),
                      error = function(e) { message("09b : classeur Excel non produit (", conditionMessage(e), ") — les CSV restent la référence."); NA_character_ })
  analyse_hypotheses_depart <- list(tables = tables_09, classeur = xlsx_09, dossier = dir_09)
  cat(sprintf("\n09b OK -> analyse « hypothèses de départ » %s : %d tableau(x) produit(s) sur %d (%s)\n", plage_09, sum(presents_09), length(tables_09),
              if (all(presents_09)) "tous" else paste("non produits :", paste(names(tables_09)[!presents_09], collapse = ", "))))
  cat(sprintf("  dossier : %s ; classeur : %s\n", dir_09, if (is.na(xlsx_09)) "non produit" else basename(xlsx_09)))
  rm(par_09, regles_09, champ_09, zon_09, dir_09, source_09, plage_09, deltas_09, hyp_09, tot_09, total_09, tranche_09, age_09, lib_sen_09, lib_jeu_09, sen_09, cs_09, pcs_fr_09, lib_pcs_09,
     met_vol_09, met_part_09, mat_09, liste_09, noyau_09, typo_09, expo_dep_09, expo_reg_09, reg_09, dep_09, expo_r_09, tab_08j_09, liste_reg_09, s55_09, prior_09,
     tables_09, presents_09, r_pcs, r_liste, raisons_09, lib_liste_09, titres_09, feuilles_09, fichiers_09, P_09, gras, lignes_09, gras_09, lisez_09, param_09, xlsx_09)
}
