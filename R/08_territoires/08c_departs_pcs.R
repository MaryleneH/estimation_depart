# ==============================================================================
# 08c_departs_pcs.R — Départs attendus d'ici 2030 par PCS FINE :
#                     France entière, région administrative, département
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (scripts 01-04, pcs conservée, cs1, contrat
#             geo_*) ; `pcs_exclues` (01b) ; fonctions 00c (lire_passage_geo,
#             ajouter_region), 00d (ajouter_parts_causes, masquer_cellules),
#             00e (calculer_departs_pcs & co) ; paramètres GENERER_DEPARTS_PCS,
#             DEPARTS_PCS_NIVEAUX, DEPARTS_PCS_SECRET, SECRET_* (règle Insee),
#             GEO_PASSAGES["departement->region"] (00).
# PRODUIT   : objet `departs_pcs` (liste : analytique, diffusion, niveaux) et
#             sorties/departs_pcs/
#               interne/departs_pcs_<niveau>.csv    COMPLET, sans secret (contrôles, usage interne)
#               diffusion/departs_pcs_<niveau>.csv  secret statistique appliqué (si DEPARTS_PCS_SECRET)
#               pcs_hors_champ.csv                  PCS exclues par le 01b, agrégées
# MÉTHODE   : aucune nouvelle formule (voir 00e). pcs = dimension, cs1 = clé.
# PÉRIMÈTRE : « France entière » = TOUT bts_projete (périmètre BITD, champ
#             AGE_MIN_BTS+, PCS rattachables) ; GEO_INTERET, FICHES_SELECTION et
#             toute sélection de restitution NE S'APPLIQUENT PAS ici — contrôle
#             explicite ci-dessous. Région et département exigent
#             geo_type = "departement" (sinon ignorés, message).
# SECRET (règle) — audit des marges publiées ensemble :
#   primaire   : règle Insee de la Base Tous salariés (00_config SECRET_*) :
#                moins de 5 salariés, OU moins de 3 entreprises (SIREN), OU une
#                entreprise représentant plus de 85 % de l'effectif ou des
#                départs attendus de la cellule.
#   secondaire : uniquement dans un bloc dont la marge est publiée ET qui ne
#                contiendrait qu'une seule cellule masquée (sinon rien n'est
#                déductible) : la plus petite cellule restante est masquée aussi.
#     France x pcs      : bloc cs1 (les totaux par grande CS sont publiés, 07/07b).
#     Région x pcs      : bloc pcs (France x pcs publié dans le même dossier) ;
#                         bloc région x cs1 (déductible de 08b, département x cs1).
#     Département x pcs : bloc région x pcs (publié ci-dessus) ; bloc
#                         département x cs1 (publié par 08b).
#   Les tables interne/ restent complètes et portent les indicateurs de secret
#   (n_entreprises, part_dominante_pct) ; les contrôles se font sur elles. Ces
#   indicateurs sont retirés des tables diffusion/.
# ==============================================================================
if (!isTRUE(get0("GENERER_DEPARTS_PCS", ifnotfound = FALSE))) {
  message("08c : départs par PCS fine désactivés (GENERER_DEPARTS_PCS = FALSE).")
} else {
  if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")
  if (!exists("calculer_departs_pcs")) stop("Exécutez d'abord 00e_fonctions_departs_pcs.R (ou main.R).")
  if (!exists("ajouter_region")) stop("Exécutez d'abord 00c_fonctions_geo.R à jour (ajouter_region).")
  if (!"pcs" %in% names(bts_projete)) stop("08c : colonne 'pcs' absente de bts_projete.")
  library(dplyr)

  # --- Base : TOUT le périmètre national, jamais restreint ; région rattachée ----
  #     (preparer_base_departs, 00e : même base pour 08c et 08d)
  prep_08c <- preparer_base_departs(bts_projete, DEPARTS_PCS_NIVEAUX, prefixe = "08c")
  base_pcs <- prep_08c$base; niveaux <- prep_08c$niveaux; rm(prep_08c)

  # --- Tables ANALYTIQUES (complètes) --------------------------------------------
  analytique <- lapply(niveaux, function(n) calculer_departs_pcs(base_pcs, n))
  names(analytique) <- niveaux

  # --- Contrôles bloquants (avant arrondi, avant secret) --------------------------
  controler_departs_pcs(analytique, base_pcs)
  controler_pcs_vs_cs(analytique$france, base_pcs)
  for (n in niveaux) {
    cles <- c(cles_niveau_pcs(n, base_pcs), "pcs")
    if (anyDuplicated(analytique[[n]][, cles]) > 0) stop("08c : doublons dans la table ", n, " x pcs.")
  }

  # --- Écriture : interne/ toujours ; diffusion/ si DEPARTS_PCS_SECRET --------------
  dir_pcs <- file.path(DIR_SORTIES, "departs_pcs")
  dir_int <- file.path(dir_pcs, "interne"); dir.create(dir_int, showWarnings = FALSE, recursive = TRUE)
  for (n in niveaux)
    write.csv2(arrondir_departs_pcs(analytique[[n]]), file.path(dir_int, sprintf("departs_pcs_%s.csv", n)), row.names = FALSE)

  diffusion <- NULL
  if (isTRUE(get0("DEPARTS_PCS_SECRET", ifnotfound = TRUE))) {
    dir_dif <- file.path(dir_pcs, "diffusion"); dir.create(dir_dif, showWarnings = FALSE, recursive = TRUE)
    regles_08c <- regles_secret()
    diffusion <- lapply(niveaux, function(n) appliquer_secret_pcs(analytique[[n]], n, regles_08c))
    names(diffusion) <- niveaux
    for (n in niveaux) {
      d <- diffusion[[n]]; a <- analytique[[n]]
      if (any(!d$masque & secret_primaire(a$effectif_champ, a$n_entreprises, a$part_dominante_pct, regles_08c)))
        stop("08c : une cellule contraire à la règle de secret statistique resterait visible (", n, ").")
      if (any(c("n_entreprises", "part_dominante_pct") %in% names(d)))
        stop("08c : indicateurs de secret présents dans la table de diffusion (", n, ").")
      write.csv2(arrondir_departs_pcs(d), file.path(dir_dif, sprintf("departs_pcs_%s.csv", n)), row.names = FALSE)
    }
  } else message("08c : DEPARTS_PCS_SECRET = FALSE -> tables internes uniquement, aucun fichier diffusion/ écrit.")

  # --- PCS hors champ (exclues par le 01b), agrégées, avec total --------------------
  hors_champ <- if (exists("pcs_exclues") && nrow(pcs_exclues) > 0) pcs_exclues else
    tibble::tibble(pcs = character(0), premier_caractere = character(0), effectif_exclu = integer(0))
  hors_champ <- bind_rows(hors_champ, tibble::tibble(pcs = "TOTAL", premier_caractere = NA_character_,
                                                     effectif_exclu = sum(hors_champ$effectif_exclu)))
  write.csv2(hors_champ, file.path(dir_pcs, "pcs_hors_champ.csv"), row.names = FALSE)

  departs_pcs <- list(analytique = analytique, diffusion = diffusion, niveaux = niveaux, hors_champ = hors_champ)

  # --- Console -------------------------------------------------------------------
  fr <- analytique$france
  cat(sprintf("\n--- Départs attendus d'ici 2030 par PCS fine — France entière (%d PCS, %s salariés) ---\n",
              nrow(fr), format(sum(fr$effectif_champ), big.mark = " ")))
  print(fr |> arrange(desc(departs_central)) |> head(10) |>
          transmute(pcs, cs1, effectif_champ, departs = arrondir_nombre_personnes(departs_central), taux = arrondir_taux(taux_depart_central_pct)) |>
          as.data.frame(), row.names = FALSE)
  message("08c OK -> ", dir_pcs, " : niveaux ", paste(niveaux, collapse = ", "),
          if (!is.null(diffusion)) paste0(" ; cellules masquées en diffusion : ",
                                          paste(sprintf("%s %d/%d", niveaux, sapply(diffusion, function(d) sum(d$masque)),
                                                        sapply(diffusion, nrow)), collapse = ", ")) else "",
          " ; PCS hors champ : ", hors_champ$effectif_exclu[nrow(hors_champ)], " salarié(s).")
  rm(fr, hors_champ, analytique, diffusion)
}
