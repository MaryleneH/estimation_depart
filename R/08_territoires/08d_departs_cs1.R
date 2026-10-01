# ==============================================================================
# 08d_departs_cs1.R — Départs attendus d'ici 2030 par GRANDE CS (cs1) :
#                     France entière, région administrative, département
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (scripts 01-04, cs1 du 01b, contrat geo_*) ;
#             fonctions 00c (ajouter_region), 00d (ajouter_parts_causes,
#             secret), 00e (calculer_departs & co) ; paramètres
#             GENERER_DEPARTS_CS1, DEPARTS_CS1_NIVEAUX, DEPARTS_CS1_SECRET,
#             SECRET_* (règle Insee), GEO_PASSAGES["departement->region"] (00).
# PRODUIT   : objet `departs_cs1` (liste : analytique, diffusion, niveaux) et
#             sorties/departs_cs1/
#               interne/departs_cs1_<niveau>.csv    COMPLET, sans secret (contrôles, usage interne)
#               diffusion/departs_cs1_<niveau>.csv  secret statistique appliqué (si DEPARTS_CS1_SECRET)
# MÉTHODE   : strictement celle du 08c, dimension cs1 au lieu de pcs : MÊME
#             fonction calculer_departs(), mêmes mesures, mêmes noms de
#             colonnes, même base (preparer_base_departs), même secret
#             (appliquer_secret_pcs, dimension = "cs1"). Aucune formule nouvelle.
#             Ne dépend d'aucun fichier écrit par le 08c : travaille sur bts_projete.
# PÉRIMÈTRE : « France entière » = TOUT bts_projete ; GEO_INTERET ne s'applique
#             pas. Région et département exigent geo_type = "departement".
# CONTRÔLES (bloquants, sur les tables analytiques, avant arrondi et secret) :
#   - totaux de chaque niveau = base ; Σ régions = France et Σ départements =
#     France par cs1 ; Σ départements d'une région = région par cs1 ;
#   - FONDAMENTAL : table cs1 = table PCS fine (recalculée par la même fonction)
#     regroupée par cs1, niveau par niveau, 7 mesures ;
#   - cohérence avec le 08b (territoire x cs1) quand il a tourné en zonage
#     département : mêmes cellules -> mêmes effectif, central, bas, haut.
# COMPARABILITÉ avec le 08b (sorties/departs_par_departement_cs.csv) :
#   mêmes individus, mêmes probabilités, mêmes cellules département x cs1 ; MAIS
#   - 08b est restreint par GEO_INTERET, 08d jamais ;
#   - noms : departs_2030 (08b) = departs_central (08d) ; part_departs_pct (08b)
#     = taux_depart_central_pct (08d) ;
#   - 08b n'a ni région, ni retraite/invalidité/décès ; 08d n'a pas
#     part_a_remplacer_pct (stock tous âges) ;
#   - secret : même règle primaire (mêmes masques primaires) ; le secondaire du
#     08d protège aussi la marge région x cs1 (bloc en plus), donc il peut masquer
#     une cellule que le 08b laisse visible. Ces cas sont comptés en console.
# SECRET (règle) — audit des marges publiées ensemble, dimension cs1 :
#   primaire   : règle Insee (00_config SECRET_*), identique au 08c.
#   secondaire : France x cs1 : bloc = la table (marge : total national) ;
#                région x cs1 : bloc cs1 (France x cs1 publiée) et bloc région
#                (total régional déductible du 08 par département) ;
#                département x cs1 : bloc région x cs1 (publié ci-dessus) et bloc
#                département (total départemental publié par 08 et fiches).
#   Les tables interne/ restent complètes (avec n_entreprises, part_dominante_pct).
# ==============================================================================
if (!isTRUE(get0("GENERER_DEPARTS_CS1", ifnotfound = FALSE))) {
  message("08d : départs par grande CS désactivés (GENERER_DEPARTS_CS1 = FALSE).")
} else {
  if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
  if (!exists("calculer_departs") || !exists("preparer_base_departs"))
    stop("Exécutez d'abord 00e_fonctions_departs_pcs.R à jour (calculer_departs, preparer_base_departs ; ou main.R).")
  if (!exists("ajouter_region")) stop("Exécutez d'abord 00c_fonctions_geo.R à jour (ajouter_region).")
  if (!"cs1" %in% names(bts_projete)) stop("08d : colonne 'cs1' absente de bts_projete (01b).")
  library(dplyr)

  # --- Base commune avec le 08c -------------------------------------------------
  prep_08d <- preparer_base_departs(bts_projete, DEPARTS_CS1_NIVEAUX, prefixe = "08d")
  base_cs1 <- prep_08d$base; niveaux_08d <- prep_08d$niveaux; rm(prep_08d)

  # --- Tables ANALYTIQUES (complètes) : cs1, et PCS recalculée pour le contrôle ----
  analytique_cs1 <- lapply(niveaux_08d, function(n) calculer_departs(base_cs1, n, dimension = "cs1"))
  names(analytique_cs1) <- niveaux_08d
  pcs_08d <- lapply(niveaux_08d, function(n) calculer_departs(base_cs1, n, dimension = "pcs"))
  names(pcs_08d) <- niveaux_08d

  # --- Contrôles bloquants (avant arrondi, avant secret) --------------------------
  controler_departs(analytique_cs1, base_cs1, dimension = "cs1", prefixe = "08d")
  controler_cs1_vs_pcs(analytique_cs1, pcs_08d, prefixe = "08d")
  if (!setequal(analytique_cs1$france$cs1, unique(bts_projete$cs1)) ||
      !all(analytique_cs1$france$cs1 %in% unname(PCS_VERS_CS1)))
    stop("08d : les grandes CS de la table France ne sont pas celles de bts_projete / PCS_VERS_CS1.")
  for (n in niveaux_08d) {
    cles <- c(cles_niveau_pcs(n, base_cs1), "cs1")
    if (anyDuplicated(analytique_cs1[[n]][, cles]) > 0) stop("08d : doublons dans la table ", n, " x cs1.")
  }
  # Cohérence avec le 08b (même session, zonage département) : mêmes cellules.
  # Le 08b ne laisse en session que sa table diffusée ; son brut (non masqué,
  # non arrondi) est recalculé par SA fonction sur SA base (base_geo_cs).
  n_cellules_08b <- NA_integer_
  if (exists("calculer_departs_geo_cs") && exists("base_geo_cs") && "departement" %in% niveaux_08d &&
      identical(unique(as.character(bts_projete$geo_type)), "departement")) {
    brut_08b <- calculer_departs_geo_cs(base_geo_cs)$brut
    cmp_08b <- inner_join(
      brut_08b |> select(geo_code, cs1, effectif_08b = effectif_champ, central_08b = departs_2030,
                         bas_08b = departs_bas, haut_08b = departs_haut),
      analytique_cs1$departement |> select(geo_code, cs1, effectif_champ, departs_central, departs_bas, departs_haut),
      by = c("geo_code", "cs1"))
    if (nrow(cmp_08b) != nrow(brut_08b))
      stop("08d : des cellules département x cs1 du 08b sont absentes de la table cs1.")
    if (any(cmp_08b$effectif_08b != cmp_08b$effectif_champ) ||
        max(abs(cmp_08b$central_08b - cmp_08b$departs_central),
            abs(cmp_08b$bas_08b - cmp_08b$departs_bas),
            abs(cmp_08b$haut_08b - cmp_08b$departs_haut)) > 1e-9)
      stop("08d : contrôle ÉCHOUÉ — département x cs1 différent du 08b sur des cellules communes.")
    n_cellules_08b <- nrow(cmp_08b); rm(cmp_08b, brut_08b)
  }

  # --- Écriture : interne/ toujours ; diffusion/ si DEPARTS_CS1_SECRET --------------
  dir_cs1 <- file.path(DIR_SORTIES, "departs_cs1")
  dir_int <- file.path(dir_cs1, "interne"); dir.create(dir_int, showWarnings = FALSE, recursive = TRUE)
  for (n in niveaux_08d)
    write.csv2(arrondir_departs_pcs(analytique_cs1[[n]]), file.path(dir_int, sprintf("departs_cs1_%s.csv", n)), row.names = FALSE)

  diffusion_cs1 <- NULL; n_secondaire_hors_08b <- NA_integer_
  if (isTRUE(get0("DEPARTS_CS1_SECRET", ifnotfound = TRUE))) {
    dir_dif <- file.path(dir_cs1, "diffusion"); dir.create(dir_dif, showWarnings = FALSE, recursive = TRUE)
    regles_08d <- regles_secret()
    diffusion_cs1 <- lapply(niveaux_08d, function(n) appliquer_secret_pcs(analytique_cs1[[n]], n, regles_08d, dimension = "cs1"))
    names(diffusion_cs1) <- niveaux_08d
    for (n in niveaux_08d) {
      d <- diffusion_cs1[[n]]; a <- analytique_cs1[[n]]
      if (any(!d$masque & secret_primaire(a$effectif_champ, a$n_entreprises, a$part_dominante_pct, regles_08d)))
        stop("08d : une cellule contraire à la règle de secret statistique resterait visible (", n, ").")
      if (any(c("n_entreprises", "part_dominante_pct") %in% names(d)))
        stop("08d : indicateurs de secret présents dans la table de diffusion (", n, ").")
      write.csv2(arrondir_departs_pcs(d), file.path(dir_dif, sprintf("departs_cs1_%s.csv", n)), row.names = FALSE)
    }
    # cellules masquées ici (secondaire, marge région x cs1) mais visibles dans le 08b
    if (exists("departs_geo_cs") && !is.na(n_cellules_08b)) {
      j <- inner_join(diffusion_cs1$departement |> select(geo_code, cs1, masque_08d = masque),
                      departs_geo_cs |> select(geo_code, cs1, masque_08b = masque), by = c("geo_code", "cs1"))
      n_secondaire_hors_08b <- sum(j$masque_08d & !j$masque_08b); rm(j)
    }
  } else message("08d : DEPARTS_CS1_SECRET = FALSE -> tables internes uniquement, aucun fichier diffusion/ écrit.")

  departs_cs1 <- list(analytique = analytique_cs1, diffusion = diffusion_cs1, niveaux = niveaux_08d)

  # --- Console -------------------------------------------------------------------
  cat("\n08d OK -> départs attendus d'ici 2030 par grande CS :\n")
  for (n in niveaux_08d)
    cat(sprintf("  %s : %d %s%s\n", formatC(c(france = "France", region = "Région", departement = "Département")[[n]],
                                             width = -12, flag = " "),
                nrow(analytique_cs1[[n]]), if (n == "france") "CS" else "lignes",
                if (!is.null(diffusion_cs1)) sprintf(" (diffusion : %d cellule(s) masquée(s), dont %d secondaire(s))",
                                                     sum(diffusion_cs1[[n]]$masque), sum(diffusion_cs1[[n]]$motif_masque %in% "secondaire")) else ""))
  cat(sprintf("  %s\n", dir_int))
  if (!is.null(diffusion_cs1)) cat(sprintf("  %s\n", dir_dif))
  if (!is.na(n_cellules_08b))
    cat(sprintf("  cohérence 08b : %d cellule(s) département x cs1 identiques%s\n", n_cellules_08b,
                if (!is.na(n_secondaire_hors_08b)) sprintf(" ; masquées ici (secondaire région x cs1) mais visibles dans le 08b : %d",
                                                           n_secondaire_hors_08b) else ""))
  print(analytique_cs1$france |>
          transmute(cs1, effectif_champ, departs = arrondir_nombre_personnes(departs_central), taux = arrondir_taux(taux_depart_central_pct)) |>
          as.data.frame(), row.names = FALSE)
  rm(analytique_cs1, diffusion_cs1, pcs_08d, n_cellules_08b, n_secondaire_hors_08b)
}
