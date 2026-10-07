# ==============================================================================
# Typologie nationale des territoires BITD (00h + 08f) — specs/10 (SPEC-TYPO-*).
# Données exclusivement SYNTHÉTIQUES. Chaque bloc cite les identifiants de spec.
# ==============================================================================
ET <- new.env()
.old_wd <- setwd(RACINE)
source(chemin_script("00_config.R"), local = ET)
setwd(.old_wd)
sys.source(chemin_script("00g_format_restitution.R"), envir = ET)
sys.source(chemin_script("00d_fonctions_fiches.R"), envir = ET)
sys.source(chemin_script("00h_fonctions_typologie.R"), envir = ET)
CS <- c("Cadres", "Prof. intermediaires", "Employes", "Ouvriers")
# paramètres FIXES et explicites pour les tests unitaires (jamais de tercile ici)
PARAMS <- list(seuil_dominance_cs = 40, seuil_concentration = 50, effectif_min = 50, methode_classes = "fixes",
               poids_faible = 1, poids_fort = 5, intensite_faible = 5, intensite_elevee = 10, volume_faible = 100, volume_eleve = 800,
               marge_frontiere_pct = 5, nb_frontiere_expertise = 2, facteur_intensite_extreme = 2, indicateur_intensite = "part_a_remplacer_pct")
# fabrique une table département x cs1 au contrat du 08d : effectifs (champ) et départs par CS, stock tous âges = champ x k
cellule <- function(code, nom, eff, dep, region = "R1") tibble::tibble(
  region_code = region, region_nom = region, geo_code = code, geo_nom = nom, cs1 = CS,
  effectif_champ = as.integer(eff), departs_central = dep, departs_bas = dep * 0.95, departs_haut = dep * 1.05)
stock_de <- function(cs_dep, k = 1) cs_dep |> dplyr::transmute(geo_code, geo_nom, cs1, effectif_tous_ages = as.integer(round(effectif_champ * k)))

test_that("SPEC-TYPO-001/002/004/041 — niveau département seul, Σ grandes CS = total, fourchette conservée, poids sur l'emploi tous âges, contrôles bloquants", {
  cs <- dplyr::bind_rows(cellule("A1", "Alpha", c(5000, 5000, 5000, 5000), c(250, 250, 250, 250)),
                         cellule("B2", "Beta",  c(500, 500, 500, 500),     c(125, 125, 125, 125)))
  t <- ET$construire_typologie_departements(cs, stock_de(cs, 2), PARAMS)
  d <- t$departements
  expect_identical(d$geo_code, c("A1", "B2")); expect_type(d$geo_code, "character")
  expect_equal(d$departs_central, c(1000, 500)); expect_equal(d$departs_bas, c(950, 475)); expect_equal(d$departs_haut, c(1050, 525))
  expect_equal(d$effectif_champ_total, c(20000L, 2000L)); expect_equal(d$effectif_bitd_total, c(40000L, 4000L))
  expect_identical(t$base_poids, "effectif_tous_ages"); expect_equal(sum(d$part_emploi_bitd_national_pct), 100)
  expect_true(ET$controler_typologie(t, cs))
  expect_false(any(grepl("bassin|tension", names(d))))                 # rien n'est descendu au bassin, rien n'est remonté du bassin
  # SPEC-TYPO-041 : contrôles bloquants — total altéré, profil inconnu, colonne interdite
  t_faux <- t; t_faux$departements$departs_central[1] <- t_faux$departements$departs_central[1] + 1
  expect_error(ET$controler_typologie(t_faux, cs), "Σ grandes CS")
  t_faux <- t; t_faux$departements$profil_typologie[1] <- "Profil inventé"; expect_error(ET$controler_typologie(t_faux, cs), "profil inconnu")
  t_faux <- t; t_faux$departements$tension_departement <- TRUE; expect_error(ET$controler_typologie(t_faux, cs), "interdite")
  # sans stock : poids et intensité sur le champ, signalé
  expect_message(t0 <- ET$construire_typologie_departements(cs, NULL, PARAMS), "taux_depart_central_pct")
  expect_identical(t0$base_poids, "effectif_champ"); expect_identical(t0$indicateur, "taux_depart_central_pct")
  expect_equal(t0$departements$intensite_renouvellement_pct, c(5, 25))
})

test_that("SPEC-TYPO-005/006 — volume ≠ intensité : A (1 000 / 20 000) plus gros volume, B (500 / 2 000) plus forte intensité", {
  cs <- dplyr::bind_rows(cellule("A1", "Alpha", c(5000, 5000, 5000, 5000), c(250, 250, 250, 250)),
                         cellule("B2", "Beta",  c(500, 500, 500, 500),     c(125, 125, 125, 125)))
  d <- ET$construire_typologie_departements(cs, stock_de(cs, 1), PARAMS)$departements
  expect_identical(d$geo_code[which.max(d$departs_central)], "A1")
  expect_identical(d$geo_code[which.max(d$intensite_renouvellement_pct)], "B2")
  expect_equal(d$intensite_renouvellement_pct, c(5, 25))               # numérateur départs centraux, dénominateur emploi actuel
  expect_identical(d$classe_volume_departs, c("Élevé", "Modéré")); expect_identical(d$classe_intensite, c("Modérée", "Élevée"))
})

test_that("SPEC-TYPO-003 — structure : 31/30/25/14 avec seuil > 31 = Mixte ; dominance à la borne, juste au-dessus, juste en dessous", {
  d <- ET$identifier_dominance_cs(CS, c(31, 30, 25, 14), seuil = 40)
  expect_identical(d$structure, "Mixte"); expect_identical(d$cs_principale, "Cadres"); expect_equal(d$part_pct, 31)
  expect_identical(ET$identifier_dominance_cs(CS, c(40, 30, 20, 10), 40)$structure, "Dominante cadres")        # borne incluse
  expect_identical(ET$identifier_dominance_cs(CS, c(40.1, 30, 19.9, 10), 40)$structure, "Dominante cadres")
  expect_identical(ET$identifier_dominance_cs(CS, c(39.9, 30, 20.1, 10), 40)$structure, "Mixte")
  e <- ET$identifier_dominance_cs(CS, c(30, 30, 25, 15), 40)                                                   # égalité : ordre métier, signalée
  expect_identical(e$cs_principale, "Cadres"); expect_true(e$egalite)
  expect_true(is.na(ET$identifier_dominance_cs(CS, rep(NA_real_, 4), 40)$structure))
})

test_that("SPEC-TYPO-008/009 — concentration : 60 % sur une CS, seuil 50 = Concentré ; répartition équilibrée = Diffus ; borne incluse", {
  expect_identical(ET$calculer_concentration_departs(CS, c(60, 20, 10, 10), 50)$type, "Concentré")
  expect_identical(ET$calculer_concentration_departs(CS, c(30, 25, 25, 20), 50)$type, "Diffus")
  expect_identical(ET$calculer_concentration_departs(CS, c(50, 20, 20, 10), 50)$type, "Concentré")
  expect_identical(ET$calculer_concentration_departs(CS, c(49.9, 20.1, 20, 10), 50)$type, "Diffus")
  expect_true(is.na(ET$calculer_concentration_departs(CS, c(0, 0, 0, 0), 50)$type))
})

test_that("SPEC-TYPO-007 — la CS au plus gros volume de départs peut différer de la CS à l'intensité la plus forte", {
  cs <- cellule("C3", "Gamma", eff = c(100, 1000, 1000, 1000), dep = c(50, 200, 100, 100))   # cadres : 50 % d'intensité ; PI : plus gros volume
  d <- ET$construire_typologie_departements(cs, stock_de(cs, 1), PARAMS)$departements
  expect_identical(d$cs_volume_departs_max, "Prof. intermediaires"); expect_identical(d$cs_taux_renouvellement_max, "Cadres")
  l <- ET$construire_typologie_departements(cs, stock_de(cs, 1), PARAMS)$departement_cs
  expect_identical(l$cs1[l$rang_volume == 1], "Prof. intermediaires"); expect_identical(l$cs1[l$rang_intensite == 1], "Cadres")
  expect_equal(sum(l$part_pct), 100); expect_equal(sum(l$part_departs_pct), 100)
})

test_that("SPEC-TYPO-010/011/012 — classes et frontières : exactement sur le seuil, juste au-dessus, juste en dessous ; plancher d'effectif", {
  s <- c(bas = 1, haut = 5)
  expect_identical(ET$classer_par_seuils(c(0.99, 1, 1.01, 4.99, 5, 5.01), s, ET$CLASSES_TYPOLOGIE$poids),
                   c("Faible", "Moyen", "Moyen", "Moyen", "Fort", "Fort"))
  expect_identical(ET$classer_poids_bitd(c(6, 6), c(49, 50), s, effectif_min = 50), c("Faible", "Fort"))   # plancher : 49 < 50 -> Faible
  expect_true(is.na(ET$classer_par_seuils(NA_real_, s, ET$CLASSES_TYPOLOGIE$poids)))
  # terciles = règle provisoire : bornes observées, écrites dans les paramètres
  q <- ET$seuils_classes(c(1, 2, 3, 4, 5, 6), "terciles"); expect_true(q[["bas"]] < q[["haut"]])
  expect_error(ET$seuils_classes(1:6, "fixes", c(NA, NA)), "manquants")
})

test_that("SPEC-TYPO-013 à 019 — profils à titres parlants : règles ordonnées, faible stock + intensité élevée ≠ pôle majeur, situation à expertiser avec motif, pôle majeur modéré", {
  P <- ET$PROFILS_TYPOLOGIE
  expect_identical(names(P), c("enjeu", "emergent", "majeur_modere", "concentre", "stable", "diffus", "expertiser"))
  expect_setequal(ET$DEFINITIONS_PROFILS$cle, names(P)); expect_identical(ET$DEFINITIONS_PROFILS$titre, unname(P[ET$DEFINITIONS_PROFILS$cle]))
  expect_true(all(nzchar(ET$DEFINITIONS_PROFILS$regle) & nzchar(ET$DEFINITIONS_PROFILS$signification) & nzchar(ET$DEFINITIONS_PROFILS$attention)))
  expect_false(any(grepl(":", c(ET$DEFINITIONS_PROFILS$signification, ET$DEFINITIONS_PROFILS$attention))))   # insérés après « Profil « … » : »
  prof <- function(po, vo, it, co = "Diffus", nf = 0, ext = FALSE) ET$attribuer_profil_typologie(po, vo, it, co, nf, ext, 2)$profil
  motif <- function(po, vo, it, co = "Diffus", nf = 0, ext = FALSE, ax = NULL) ET$attribuer_profil_typologie(po, vo, it, co, nf, ext, 2, ax)$motif
  expect_identical(prof("Fort", "Élevé", "Élevée"), P[["enjeu"]])
  expect_identical(prof("Fort", "Modéré", "Élevée"), P[["emergent"]])
  expect_identical(prof("Moyen", "Faible", "Élevée"), P[["emergent"]])
  expect_identical(prof("Moyen", "Modéré", "Modérée", "Concentré"), P[["concentre"]])
  expect_identical(prof("Fort", "Faible", "Faible", "Diffus"), P[["stable"]])
  expect_identical(prof("Faible", "Élevé", "Élevée"), P[["diffus"]])                      # faible poids : jamais « pôle majeur »
  expect_identical(prof("Faible", "Faible", "Élevée", ext = TRUE), P[["expertiser"]])     # intensité extrême sur stock faible
  expect_identical(prof("Fort", "Élevé", "Faible"), P[["expertiser"]])                    # fort volume mais faible intensité : contradiction
  expect_identical(prof("Fort", "Élevé", "Modérée"), P[["majeur_modere"]])                # SPEC-TYPO-019 : volume élevé, rythme modéré = gros département ordinaire
  expect_identical(prof("Moyen", "Élevé", "Modérée"), P[["majeur_modere"]])
  expect_identical(prof("Moyen", "Élevé", "Modérée", "Concentré"), P[["majeur_modere"]])  # le volume élevé prime sur la concentration
  expect_identical(prof("Fort", "Modéré", "Modérée", nf = 2), P[["expertiser"]])          # deux axes à la frontière
  expect_identical(prof("Fort", "Modéré", "Modérée", nf = 1), P[["stable"]])
  expect_identical(prof(NA, "Modéré", "Modérée"), P[["expertiser"]])
  # motif : renseigné pour chaque situation à expertiser, NA sinon
  expect_match(motif("Fort", "Élevé", "Faible"), "volume de départs élevé mais renouvellement lent")
  expect_match(motif("Faible", "Faible", "Élevée", ext = TRUE), "extrême sur un très petit effectif")
  expect_match(motif("Fort", "Modéré", "Modérée", nf = 2, ax = "poids, volume"), "^2 indicateurs à la frontière .*\\(poids, volume\\)$")
  expect_match(motif(NA, "Modéré", "Modérée"), "manquant")
  for (m in list(motif("Fort", "Élevé", "Élevée"), motif("Fort", "Élevé", "Modérée"), motif("Faible", "Élevé", "Élevée"), motif("Fort", "Faible", "Faible")))
    expect_true(is.na(m))
  # chaîne complète sur un cas contradictoire : gros volume, intensité faible -> expertiser, justification par règles, motif écrit
  cs <- dplyr::bind_rows(cellule("D4", "Delta", c(10000, 10000, 10000, 10000), c(300, 300, 300, 300)),   # 1 200 départs, 40 000 x 10 = 3 % -> faible intensité
                         cellule("E5", "Eps",   c(300, 300, 300, 300),         c(10, 10, 10, 10)))
  d <- ET$construire_typologie_departements(cs, stock_de(cs, 10), PARAMS)$departements
  expect_identical(d$profil_typologie[d$geo_code == "D4"], P[["expertiser"]])
  expect_match(d$motif_expertise[d$geo_code == "D4"], "renouvellement lent"); expect_true(is.na(d$motif_expertise[d$geo_code == "E5"]))
  expect_match(d$justification_profil[d$geo_code == "D4"], "^Poids BITD fort .* volume de départs élevé .* intensité de renouvellement faible")
  expect_false(grepl("LLM|modèle", d$justification_profil[1]))
  # frontière : seuls poids, intensité et volume comptent ; dominance et concentration restent informatives
  cs2 <- dplyr::bind_rows(cellule("P1", "Pi",   c(4000, 3000, 2000, 1000), c(200, 100, 70, 30)),        # dominance 40 % (= seuil), concentration 50 % (= seuil)
                          cellule("Q2", "Qu",   c(1000, 1000, 1000, 1000), c(40, 40, 40, 40)))
  d2 <- ET$construire_typologie_departements(cs2, stock_de(cs2, 1), PARAMS)$departements
  p1 <- d2[d2$geo_code == "P1", ]
  expect_true(p1$frontiere_dominance); expect_true(p1$frontiere_concentration); expect_equal(p1$n_criteres_frontiere, 0)
  expect_false(p1$profil_typologie == P[["expertiser"]])
})

test_that("SPEC-TYPO-014 — faible poids et intensité élevée : « Implantation réduite », intensité conservée et visible ; extrême -> expertiser", {
  cs <- dplyr::bind_rows(cellule("A1", "Alpha", c(5000, 5000, 5000, 5000), c(250, 250, 250, 250)),
                         cellule("F6", "Fi",    c(10, 10, 10, 10),         c(3, 3, 3, 3)),          # 40 salariés < plancher, 12 départs : 30 %
                         cellule("G7", "Ga",    c(20, 20, 20, 20),         c(1, 1, 1, 1)))
  d <- ET$construire_typologie_departements(cs, stock_de(cs, 1), PARAMS)$departements
  f <- d[d$geo_code == "F6", ]
  expect_identical(f$classe_poids_bitd, "Faible"); expect_equal(f$intensite_renouvellement_pct, 30)
  expect_identical(f$profil_typologie, ET$PROFILS_TYPOLOGIE[["expertiser"]])    # 30 % >= 2 x 10 % : extrême sur faible stock
  expect_true(f$intensite_extreme_faible_stock); expect_match(f$motif_expertise, "petit effectif")
  expect_match(f$situation_texte, "un poids faible, sous le plancher de 50 salariés retenu pour l’analyse\\.")   # 40 salariés < 50
  expect_match(f$situation_texte, "À examiner : intensité de renouvellement extrême sur un très petit effectif\\.")
  g <- d[d$geo_code == "G7", ]
  expect_identical(g$profil_typologie, ET$PROFILS_TYPOLOGIE[["diffus"]]); expect_equal(g$intensite_renouvellement_pct, 5)
  expect_false(grepl("sous le plancher", g$situation_texte)); expect_match(g$situation_texte, "Profil « Implantation réduite »")   # 80 salariés >= 50, part faible
})

test_that("SPEC-TYPO-050 à 053 — rédaction par règles : situation du département, motif, points d'attention, synthèse par profil, définitions", {
  P <- ET$PROFILS_TYPOLOGIE
  cs <- dplyr::bind_rows(cellule("A1", "Alpha", c(10000, 8000, 6000, 6000), c(600, 500, 400, 400)),     # 30 000 ; 1 900 départs (fourchette ±5 %) : 6,3 % -> Modérée
                         cellule("B2", "Beta",  c(1200, 1000, 1000, 1000),  c(360, 60, 60, 60)),       # 4 200 ; 540 départs, cadres 66,7 % -> Concentré ; 12,9 % -> Élevée
                         cellule("C3", "Gamma", c(3000, 2000, 2000, 2000),  c(60, 70, 50, 50)),        # 9 000 ; 230 départs : 2,6 % -> faible ; stable
                         cellule("D4", "Delta", c(1000, 1000, 1000, 1000),  c(60, 50, 40, 20)))        # 4 000 ; 170 départs ; 4,25 % ; médiane des 4 = 5,3 %
  t <- ET$construire_typologie_departements(cs, stock_de(cs, 1), PARAMS); d <- t$departements
  a <- d[d$geo_code == "A1", ]; b <- d[d$geo_code == "B2", ]; c3 <- d[d$geo_code == "C3", ]
  # SPEC-TYPO-050 : quatre phrases, dans l'ordre, nombres arrondis (entiers, virgule décimale)
  expect_identical(a$profil_typologie, P[["majeur_modere"]])                                   # Fort, Élevé (1 900 >= 800), Modérée (6,3 %)
  expect_match(a$situation_texte, "^Alpha \\(A1\\) pèse 63,6 % de l’emploi BITD national \\(30 000 salariés\\), un poids fort\\.")
  expect_match(a$situation_texte, "Son emploi se répartit entre les grandes catégories sans dominante \\(première : cadres, 33,3 %\\)\\.")
  expect_match(a$situation_texte, "D’ici 2030, 1 900 départs sont attendus \\(entre 1 805 et 1 995 selon l’hypothèse de départ en retraite\\), soit 6,3 % de l’emploi actuel à remplacer, au-dessus de la médiane des départements \\(5,3 %\\) : volume de départs élevé, intensité de renouvellement modérée\\.")
  expect_match(a$situation_texte, "Les cadres portent 31,6 % de ces départs, sans concentration marquée\\. C’est chez les employés que la part à remplacer est la plus élevée\\.")
  expect_match(a$situation_texte, "Profil « Pôle majeur au renouvellement modéré » : un pôle important .*\\.$")
  expect_false(grepl("À examiner", a$situation_texte)); expect_true(is.na(a$motif_expertise)); expect_true(is.na(a$points_attention))
  expect_identical(ET$rediger_situation_departement(d, PARAMS, t$comparaisons), d$situation_texte)   # déterministe
  # SPEC-TYPO-052 : points d'attention (concentration) sur un profil non « à expertiser », et mention en fin de texte
  expect_identical(b$profil_typologie, P[["emergent"]])
  expect_match(b$points_attention, "une seule catégorie porte 66,7 % des départs \\(cadres\\)")
  expect_match(b$situation_texte, "Son emploi se répartit .* \\(première : cadres, 28,6 %\\)")
  expect_match(b$situation_texte, "au-dessus de la médiane"); expect_match(b$situation_texte, "le renouvellement est concentré sur cette catégorie\\. C’est aussi la catégorie où la part à remplacer est la plus élevée\\.")
  expect_match(b$situation_texte, "Point\\(s\\) d’attention : une seule catégorie porte 66,7 % des départs \\(cadres\\)\\.$")
  expect_identical(c3$profil_typologie, P[["stable"]]); expect_match(c3$situation_texte, "Son emploi est dominé par les cadres \\(33,3 %\\)|sans dominante")
  # fourchette large : point d'attention
  cs_f <- cellule("F1", "Phi", c(1000, 1000, 1000, 1000), c(50, 50, 50, 50)) |> dplyr::mutate(departs_bas = departs_central * 0.8, departs_haut = departs_central * 1.2)
  df <- ET$construire_typologie_departements(dplyr::bind_rows(cs, cs_f), stock_de(dplyr::bind_rows(cs, cs_f), 1), PARAMS)$departements
  expect_match(df$points_attention[df$geo_code == "F1"], "fourchette d’estimation large \\(de 160 à 240 départs\\)")
  # frontière sur un seul axe : point d'attention, pas d'expertise ; deux axes : expertise avec motif (SPEC-TYPO-051)
  cs_g <- dplyr::bind_rows(cellule("G1", "Gam", c(2500, 2500, 2500, 2500), c(200, 200, 200, 200)),   # 10 000 ; 800 départs (= seuil volume élevé) ; 8 % (= seuil intensité faible ? non : 8 >= 5, < 10 -> Modérée)
                           cellule("H1", "Eta", c(25000, 25000, 25000, 25000), c(600, 600, 600, 600)))
  dg <- ET$construire_typologie_departements(cs_g, stock_de(cs_g, 1), PARAMS)$departements
  g1 <- dg[dg$geo_code == "G1", ]
  expect_true(g1$frontiere_volume); expect_equal(g1$n_criteres_frontiere, 1)
  expect_identical(g1$profil_typologie, P[["majeur_modere"]]); expect_match(g1$points_attention, "proche d’un seuil de classe \\(volume\\)")
  cs_h <- dplyr::bind_rows(cellule("G1", "Gam", c(2500, 2500, 2500, 2500), c(250, 250, 250, 250)),   # 10 000 ; 1 000 départs ; 10 % = seuil intensité élevée
                           cellule("H1", "Eta", c(47500, 47500, 47500, 47500), c(600, 600, 600, 600)))  # G1 : 5 % du total = seuil poids fort
  dh <- ET$construire_typologie_departements(cs_h, stock_de(cs_h, 1), PARAMS)$departements
  h <- dh[dh$geo_code == "G1", ]
  expect_true(h$frontiere_poids & h$frontiere_intensite); expect_identical(h$profil_typologie, P[["expertiser"]])
  expect_match(h$motif_expertise, "^2 indicateurs à la frontière d’un seuil de classe \\(poids, intensité\\)$")
  expect_match(h$situation_texte, "À examiner : 2 indicateurs à la frontière d’un seuil de classe \\(poids, intensité\\)\\.")
  expect_false(grepl("proche d’un seuil", h$situation_texte))                                  # pas de doublon avec le motif
  # SPEC-TYPO-053 : synthèse rédigée par profil + définitions
  s <- ET$synthese_profils(t)
  expect_true(all(c("regle", "signification", "consigne_lecture", "departements", "n_a_expertiser_motifs", "texte") %in% names(s)))
  expect_identical(s$profil_typologie, unname(P[P %in% d$profil_typologie]))
  sm <- s[s$profil_typologie == P[["majeur_modere"]], ]
  expect_match(sm$texte, "^1 département relève du profil « Pôle majeur au renouvellement modéré » : un pôle important")
  expect_match(sm$texte, "Ensemble, ils représentent 63,6 % de l’emploi BITD et [0-9]+,[0-9] % des départs attendus d’ici 2030 ; leur intensité de renouvellement médiane est de 6,3 % \\(ensemble des départements : 5,3 %\\)")
  expect_match(sm$texte, "Consigne de lecture : lire le volume en valeur absolue.*\\.$"); expect_identical(sm$departements, "Alpha (A1)")
  se <- s[s$profil_typologie == P[["emergent"]], ]; expect_match(se$texte, "Dans 1 cas, une seule catégorie porte au moins la moitié des départs\\.")
  expect_equal(sum(s$n_a_expertiser_motifs), sum(!is.na(d$motif_expertise)))
  s0 <- ET$synthese_profils(t, rediger = FALSE); expect_false("texte" %in% names(s0)); expect_equal(s0$n_departements, s$n_departements)
  defs <- ET$profils_definitions()
  expect_identical(names(defs), c("cle", "titre", "regle", "signification", "consigne_lecture")); expect_equal(nrow(defs), 7L)
  expect_false(any(grepl("LLM|modèle de langage|généré", c(d$situation_texte, s$texte, defs$signification))))
})

test_that("SPEC-TYPO-054 — secret des textes : territoire masqué -> NA ; cellule CS masquée -> aucune catégorie citée ; profil non diffusé -> NA", {
  cs <- dplyr::bind_rows(cellule("A1", "Alpha", c(5000, 5000, 5000, 5000), c(250, 250, 250, 250)),
                         cellule("K1", "Kappa", c(4, 2000, 2000, 2000), c(2, 500, 100, 100)),       # cadres masqués ; 702 départs : Modéré, 11,7 % Élevée -> à surveiller (publié), PI 71,2 % concentré
                         cellule("L2", "Lambda", c(3, 3, 3, 3), c(1, 1, 1, 1)),
                         cellule("M3", "Mu", c(15, 15, 15, 15), c(1, 1, 1, 1)))
  base <- dplyr::bind_rows(lapply(seq_len(nrow(cs)), function(i) tibble::tibble(
    geo_code = cs$geo_code[i], cs1 = cs$cs1[i], siren = if (cs$geo_code[i] == "L2") "E1" else sprintf("E%d", seq_len(cs$effectif_champ[i]) %% 7 + 1),
    p_central = cs$departs_central[i] / cs$effectif_champ[i])))
  dif_cs <- cs |> dplyr::left_join(ET$indicateurs_secret(base, c("geo_code", "cs1")), by = c("geo_code", "cs1")) |>
    dplyr::group_by(geo_code) |> dplyr::mutate(masque = ET$masquer_cellules(effectif_champ, n_entreprises, part_dominante_pct, ET$regles_secret())) |> dplyr::ungroup()
  t <- ET$construire_typologie_departements(cs, stock_de(cs, 1), PARAMS)
  k_int <- t$departements[t$departements$geo_code == "K1", ]
  expect_identical(k_int$profil_typologie, ET$PROFILS_TYPOLOGIE[["emergent"]]); expect_match(k_int$points_attention, "professions intermédiaires")
  expect_match(k_int$situation_texte, "professions intermédiaires")
  d <- ET$appliquer_secret_typologie(t, dif_cs, base, ET$regles_secret())
  k1 <- d[d$geo_code == "K1", ]; a1 <- d[d$geo_code == "A1", ]
  expect_true(all(is.na(d$situation_texte[d$masque])) && all(is.na(d$points_attention[d$masque])) && all(is.na(d$motif_expertise[d$masque])))
  expect_identical(k1$profil_typologie, ET$PROFILS_TYPOLOGIE[["emergent"]])                  # profil publié (ne dépend que des totaux)
  expect_false(is.na(k1$situation_texte))
  for (mot in c("cadres", "professions intermédiaires", "employés", "ouvriers", "dominé", "sans dominante", "portent", "concentr"))
    expect_false(grepl(mot, k1$situation_texte), label = mot)
  expect_true(is.na(k1$points_attention))                                                    # le point « une seule catégorie porte … » a disparu
  expect_match(k1$situation_texte, "^Kappa \\(K1\\) pèse .* D’ici 2030, .* Profil « Renouvellement rapide à surveiller »")
  expect_identical(a1$situation_texte, t$departements$situation_texte[t$departements$geo_code == "A1"])   # département intact : texte intact
  expect_true(all(is.na(d$situation_texte[d$profil_typologie == "Non diffusé (secret statistique)"])))
})

test_that("SPEC-TYPO-020 à 023 — tensions : un bassin signalé ne qualifie jamais le département ; aucune correspondance FAP -> CS", {
  cs <- cellule("H8", "Eta", c(1000, 1000, 1000, 1000), c(100, 100, 100, 100))
  d <- ET$construire_typologie_departements(cs, stock_de(cs, 1), PARAMS)$departements
  tensions <- utils::read.csv2(file.path(RACINE, "data", "templates", "tensions_fap_territoires_template.csv"), colClasses = "character")
  tensions$territoire_code <- c("BX1", "BX2")
  passage <- tibble::tibble(code_source = c("BX1", "BX2"), code_cible = c("H8", "H8"))
  j <- ET$joindre_signal_tension_localise(d, tensions, passage)
  expect_equal(j$nb_bassins_signales, 1L); expect_true(j$presence_signal_tension_localise)   # seul le bassin « forte » compte
  expect_false(any(grepl("tension_departement|departement_en_tension", names(j))))
  expect_identical(j$profil_typologie, d$profil_typologie)                                   # la tension n'entre pas dans le profil
  expect_identical(j$departs_central, d$departs_central)                                     # aucun départ attribué au bassin
  expect_error(ET$joindre_signal_tension_localise(d, tensions |> dplyr::mutate(tension_departement = TRUE), passage), "interdite")
  expect_error(ET$joindre_signal_tension_localise(d, tensions |> dplyr::mutate(cs1 = "Cadres"), passage), "FAP")
  s <- ET$joindre_signal_tension_localise(d, NULL, NULL); expect_true(is.na(s$nb_bassins_signales))
})

test_that("SPEC-TYPO-030 à 034 — secret : territoire non diffusable masqué ; variable dérivée jamais révélatrice d'une cellule masquée ; interne complet", {
  cs <- dplyr::bind_rows(cellule("A1", "Alpha", c(5000, 5000, 5000, 5000), c(250, 250, 250, 250)),
                         cellule("K1", "Kappa", c(4, 2000, 2000, 2000), c(2, 100, 100, 100)),       # cadres : 4 salariés -> cellule masquée (primaire) + secondaire
                         cellule("L2", "Lambda", c(3, 3, 3, 3), c(1, 1, 1, 1)),                      # 12 salariés d'une seule entreprise -> département masqué (primaire)
                         cellule("M3", "Mu", c(15, 15, 15, 15), c(1, 1, 1, 1)))                      # 60 salariés, diffusable, mais plus petit restant -> masqué (secondaire : bloc national)
  base <- dplyr::bind_rows(lapply(seq_len(nrow(cs)), function(i) tibble::tibble(
    geo_code = cs$geo_code[i], cs1 = cs$cs1[i], siren = if (cs$geo_code[i] == "L2") "E1" else sprintf("E%d", seq_len(cs$effectif_champ[i]) %% 7 + 1),
    p_central = cs$departs_central[i] / cs$effectif_champ[i])))
  dif_cs <- cs |> dplyr::left_join(ET$indicateurs_secret(base, c("geo_code", "cs1")), by = c("geo_code", "cs1")) |>
    dplyr::group_by(geo_code) |> dplyr::mutate(masque = ET$masquer_cellules(effectif_champ, n_entreprises, part_dominante_pct, ET$regles_secret())) |> dplyr::ungroup()
  expect_true(dif_cs$masque[dif_cs$geo_code == "K1" & dif_cs$cs1 == "Cadres"])
  expect_equal(sum(dif_cs$masque[dif_cs$geo_code == "K1"]), 2)                                      # secondaire : plus petite cellule restante
  t <- ET$construire_typologie_departements(cs, stock_de(cs, 1), PARAMS)
  interne <- t$departements; expect_false(anyNA(interne$cs_principale)); expect_false(anyNA(interne$profil_typologie))   # interne COMPLET
  d <- ET$appliquer_secret_typologie(t, dif_cs, base, ET$regles_secret())
  l2 <- d[d$geo_code == "L2", ]; k1 <- d[d$geo_code == "K1", ]; a1 <- d[d$geo_code == "A1", ]
  expect_true(l2$masque); expect_true(is.na(l2$departs_central)); expect_true(is.na(l2$effectif_bitd_total)); expect_identical(l2$profil_typologie, "Non diffusé (secret statistique)")
  m3 <- d[d$geo_code == "M3", ]; expect_true(m3$masque); expect_identical(m3$motif_masque, "territoire")   # secondaire : le total national est publié (08d France)
  expect_equal(sum(d$masque), 2)
  expect_false(k1$masque); expect_true(is.na(k1$effectif_Cadres)); expect_true(is.na(k1$departs_Cadres)); expect_false(is.na(k1$effectif_Ouvriers))
  for (v in c("cs_principale", "structure_emploi", "cs_volume_departs_max", "cs_taux_renouvellement_max", "type_concentration", "part_cs_principale_pct", "part_departs_cs_max_pct"))
    expect_true(is.na(k1[[v]]), label = v)                                                           # variables dérivées masquées
  expect_true(k1$profil_typologie %in% c(ET$PROFILS_TYPOLOGIE[c("enjeu", "emergent", "majeur_modere", "diffus", "expertiser")], "Non diffusé (secret statistique)"))
  expect_false(k1$profil_typologie %in% ET$PROFILS_TYPOLOGIE[c("concentre", "stable")])
  expect_false(a1$masque); expect_false(is.na(a1$cs_principale)); expect_identical(a1$profil_typologie, interne$profil_typologie[interne$geo_code == "A1"])
  expect_false(any(c("n_entreprises", "part_dominante_pct", "n_criteres_frontiere") %in% names(d)))
  expect_true(all(is.na(d$justification_profil[d$masque])))
  # les règles existantes ne sont pas affaiblies : la cellule masquée du 08d reste NA dans toute colonne qui en dérive
  expect_true(all(is.na(unlist(k1[grepl("_Cadres", names(k1))]))))
})

test_that("SPEC-TYPO-040 — chaîne réelle en mode département : 08f en aval du 08d, sorties, synthèse, PNG, non-régression des objets 08/08b/08c/08d", {
  skip_if_not_installed("ggplot2")
  env <- new.env(); old <- setwd(RACINE); on.exit(setwd(old), add = TRUE)
  sys.source(chemin_script("00_config.R"), envir = env)
  assign("GEO_ANALYSE", "departement", envir = env); assign("GEO_SOURCE", "departement", envir = env)
  assign("DIR_SORTIES", file.path(tempdir(), "typo_chaine"), envir = env); dir.create(env$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(c("00c_fonctions_geo.R", "00g_format_restitution.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R", "00h_fonctions_typologie.R",
                    "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
                    "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R", "08_analyse_55plus_geo.R", "08b_departs_geo_cs.R",
                    "08c_departs_pcs.R", "08d_departs_cs1.R"), env)
  avant <- lapply(c("synthese_geo", "departs_geo_cs", "departs_pcs", "departs_cs1", "bts_projete"), function(o) get(o, envir = env))
  sys.source(chemin_script("08f_typologie_departements.R"), envir = env)
  apres <- lapply(c("synthese_geo", "departs_geo_cs", "departs_pcs", "departs_cs1", "bts_projete"), function(o) get(o, envir = env))
  expect_identical(avant, apres)                                                                     # 08f ne touche à rien en amont
  T <- env$typologie_departements; d <- T$departements; cs <- env$departs_cs1$analytique$departement
  expect_equal(nrow(d), dplyr::n_distinct(cs$geo_code)); expect_equal(sum(d$departs_central), sum(cs$departs_central), tolerance = 1e-9)
  expect_equal(sum(d$effectif_champ_total), nrow(env$bts_projete))
  expect_identical(T$parametres$valeur[T$parametres$parametre == "indicateur_intensite"], "part_a_remplacer_pct")
  expect_true(all(d$profil_typologie %in% env$PROFILS_TYPOLOGIE))
  expect_equal(sum(T$synthese$n_departements), nrow(d))
  dir_t <- file.path(env$DIR_SORTIES, "typologie_territoriale")
  for (f in c("interne/typologie_departements.csv", "interne/typologie_departement_cs.csv", "interne/synthese_profils.csv", "interne/profils_definitions.csv",
              "interne/parametres_typologie.csv", "interne/matrice_typologie.png", "diffusion/typologie_departements.csv")) expect_true(file.exists(file.path(dir_t, f)), label = f)
  csv <- utils::read.csv2(file.path(dir_t, "diffusion", "typologie_departements.csv"), colClasses = c(geo_code = "character"))
  expect_false(any(c("n_entreprises", "part_dominante_pct", "n_criteres_frontiere") %in% names(csv)))
  expect_true(all(csv$departs_central[!csv$masque] == round(csv$departs_central[!csv$masque])))     # personnes entières en restitution
  expect_true(all(c("situation_texte", "motif_expertise", "points_attention") %in% names(csv)))
  expect_true(all(nzchar(d$situation_texte)) && all(c("texte", "consigne_lecture") %in% names(T$synthese)))
  defs <- utils::read.csv2(file.path(dir_t, "interne", "profils_definitions.csv")); expect_equal(nrow(defs), 7L)
  # mode zone d'emploi : 08f ignoré avec message, aucun fichier
  env2 <- new.env(); sys.source(chemin_script("00_config.R"), envir = env2)
  assign("DIR_SORTIES", file.path(tempdir(), "typo_ze"), envir = env2); dir.create(env2$DIR_SORTIES, showWarnings = FALSE)
  sourcer_scripts(c("00c_fonctions_geo.R", "00g_format_restitution.R", "00d_fonctions_fiches.R", "00e_fonctions_departs_pcs.R", "00h_fonctions_typologie.R",
                    "01_fabriquer_donnees_test.R", "01b_agreger_pcs.R", "01c_stock_tous_ages.R", "02_importer_nettoyer_drees.R", "02b_importer_mortalite_insee.R",
                    "02c_importer_invalidite_eacr.R", "03_parametres_csp.R", "04_projection_2030.R", "08d_departs_cs1.R"), env2)
  expect_message(sys.source(chemin_script("08f_typologie_departements.R"), envir = env2), "non produite")
  expect_false(dir.exists(file.path(env2$DIR_SORTIES, "typologie_territoriale")))
})

test_that("SPEC-TYPO-042 — proposer_seuils_typologie : terciles arrondis au pas, comptes par classe, bloc prêt à coller, seuils métier seulement observés, rien d'appliqué", {
  set.seed(7)
  d <- tibble::tibble(geo_code = sprintf("%02d", 1:30), effectif_bitd_total = c(20L, 30L, round(runif(28, 200, 5000))),
                      part_emploi_bitd_national_pct = NA_real_, intensite_renouvellement_pct = round(runif(30, 6, 22), 2), departs_central = round(runif(30, 20, 900), 1),
                      part_cs_principale_pct = round(runif(30, 28, 55), 1), part_departs_cs_max_pct = round(runif(30, 30, 70), 1))
  d$part_emploi_bitd_national_pct <- 100 * d$effectif_bitd_total / sum(d$effectif_bitd_total)
  out <- ET$proposer_seuils_typologie(d, effectif_min = 50, pas = c(poids = 0.5, intensite = 1, volume = 10), afficher = FALSE)
  t <- out$tableau
  expect_identical(t$axe, c("poids", "intensite", "volume")); expect_true(all(t$n == 28))              # 2 départements sous le plancher écartés
  expect_true(all(t$proposition_bas < t$proposition_haut))
  expect_true(all(abs(t$proposition_bas - t$tercile_bas) <= t$pas / 2 + 1e-9)); expect_true(all(abs(t$proposition_haut - t$tercile_haut) <= t$pas / 2 + 1e-9))
  expect_true(all(t$proposition_bas %% t$pas < 1e-9 | abs(t$proposition_bas %% t$pas - t$pas) < 1e-9))  # multiples du pas
  expect_true(all(t$n_classe_1 + t$n_classe_2 + t$n_classe_3 == 28))
  expect_identical(t$parametre_bas, c("TYPO_SEUIL_POIDS_FAIBLE", "TYPO_SEUIL_INTENSITE_FAIBLE", "TYPO_SEUIL_VOLUME_FAIBLE"))
  expect_identical(t$parametre_haut, c("TYPO_SEUIL_POIDS_FORT", "TYPO_SEUIL_RENOUVELLEMENT_ELEVE", "TYPO_SEUIL_VOLUME_ELEVE"))
  expect_true(any(grepl('^TYPO_METHODE_CLASSES <- "fixes"$', out$bloc)))
  for (p in c(t$parametre_bas, t$parametre_haut)) expect_true(any(grepl(paste0("^", p, " +<- "), out$bloc)), label = p)
  expect_true(any(grepl("TYPO_SEUIL_DOMINANCE_CS", out$bloc))); expect_equal(out$metier$n_sous_plancher[3], 2L)
  expect_false(any(grepl("^TYPO_SEUIL_DOMINANCE_CS +<-", out$bloc)))                                  # seuil métier : jamais proposé
  expect_output(ET$proposer_seuils_typologie(d, 50, afficher = TRUE), "TYPO_SEUIL_VOLUME_ELEVE")
  expect_identical(d, d)                                                                               # rien n'est modifié
  # arrondi au pas : 2,37 -> 2,5 (pas 0,5) ; 119 -> 120 (pas 10) ; 12,16 -> 12 (pas 1)
  expect_equal(ET$arrondir_pas(c(2.37, 119, 12.16), c(0.5, 10, 1)), c(2.5, 120, 12))
  # bornes confondues après arrondi : écartées d'un pas ; moins de 3 départements : arrêt explicite
  d2 <- d; d2$departs_central <- 100 + (seq_len(30) %% 3); out2 <- ET$proposer_seuils_typologie(d2, 50, afficher = FALSE)
  expect_true(out2$tableau$proposition_haut[3] > out2$tableau$proposition_bas[3])
  expect_error(ET$proposer_seuils_typologie(d, effectif_min = 10000), "moins de 3")
})
