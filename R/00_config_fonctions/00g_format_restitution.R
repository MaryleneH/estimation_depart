# ==============================================================================
# 00g_format_restitution.R — Convention UNIQUE d'arrondi des restitutions
# ------------------------------------------------------------------------------
# RÈGLE : les calculs sont conservés à leur précision complète (sommes de
#         probabilités individuelles, contrôles, agrégations, secret). Les
#         NOMBRES ATTENDUS DE PERSONNES sont arrondis à l'entier UNIQUEMENT au
#         moment de la restitution (CSV, HTML, fiches, graphiques, cartes,
#         tableaux de bord) ; les TAUX gardent une décimale et sont calculés sur
#         les valeurs exactes, jamais sur les personnes arrondies ; les codes et
#         identifiants ne sont jamais touchés ; NA reste NA (secret), jamais 0.
# ORDRE : calcul exact -> contrôles exacts -> secret statistique -> arrondi de
#         restitution -> écriture. Jamais d'arrondi avant un group_by/summarise.
# CONVENTION .5 : arrondi « au plus proche, demi vers le haut » (0,5 -> 1 ;
#         1,5 -> 2 ; 2,5 -> 3 ; 27,5 -> 28), celui qu'attend un lecteur et celui
#         d'Excel et de Intl.NumberFormat (JavaScript des cartes). R round()
#         suit IEC 60559 (demi vers le pair : 0,5 -> 0, 2,5 -> 2), contre-intuitif
#         pour des personnes ; il n'est donc PAS utilisé pour les personnes.
#         Les quantités de personnes étant positives, la règle est floor(x + 0,5).
# COHÉRENCE total / composantes : quand un total et ses composantes (départs =
#         retraite + invalidité + décès ; ensemble = Σ catégories) sont AFFICHÉS
#         ENSEMBLE, les composantes sont arrondies par la méthode des plus forts
#         restes pour que leur somme affichée soit EXACTEMENT le total affiché.
#         Elle ne s'applique qu'à cette situation : les territoires sont
#         arrondis indépendamment (Σ départements affichés peut différer de la
#         région affichée ; jamais d'ajustement qui déformerait un territoire).
# ==============================================================================

# --- Colonnes « personnes » connues du projet (nombres attendus ou observés) ---
COLONNES_PERSONNES <- c(
  "departs_central", "departs_bas", "departs_haut", "departs_2030", "departs",
  "departs_55plus", "departs_55plus_bas", "departs_55plus_haut", "departs_seniors",
  "dep_retraite", "dep_invalidite", "dep_deces", "dep_55_retraite", "dep_55_invalidite", "dep_55_deces",
  "dont_retraite", "dont_invalidite", "dont_deces", "bas", "haut", "fourchette_bas", "fourchette_haut",
  "effectif", "effectif_champ", "effectif_champ_2024", "effectif_55plus", "effectif_tous_ages", "n", "n55")
# Décompositions total = Σ composantes affichées ensemble (par ligne)
DECOMPOSITIONS_PERSONNES <- list(
  list(total = "departs_central", composantes = c("dep_retraite", "dep_invalidite", "dep_deces")),
  list(total = "departs_55plus",  composantes = c("dep_55_retraite", "dep_55_invalidite", "dep_55_deces")),
  list(total = "departs",         composantes = c("dont_retraite", "dont_invalidite", "dont_deces")))
DECIMALES_TAUX <- 1

# --- Personnes : entier, demi vers le haut, NA conservé --------------------------
arrondir_nombre_personnes <- function(x) {
  x <- as.numeric(x)
  out <- ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) + 0.5))
  out[is.nan(x) | is.infinite(x)] <- NA_real_
  out
}

# --- Taux et parts : une décimale (R round suffit : pas de contrainte « demi ») --
arrondir_taux <- function(x, decimales = DECIMALES_TAUX) round(as.numeric(x), decimales)

# --- Composantes cohérentes avec leur total (plus forts restes) ----------------
# composantes : valeurs EXACTES (>= 0) ; total : valeur exacte du total (défaut
# : somme des composantes). Retourne des entiers dont la somme vaut
# arrondir_nombre_personnes(total). Déterministe : les unités restantes vont
# aux plus grandes parties fractionnaires, puis aux plus grandes composantes,
# puis au premier dans l'ordre. Si une composante ou le total est NA : chaque
# composante est arrondie seule (aucune contrainte possible), NA restant NA.
arrondir_composantes_avec_total <- function(composantes, total = sum(composantes)) {
  x <- as.numeric(composantes)
  if (length(x) == 0) return(x)
  if (anyNA(x) || is.na(total)) return(arrondir_nombre_personnes(x))
  if (any(x < 0)) stop("arrondir_composantes_avec_total : composantes négatives.")
  cible <- arrondir_nombre_personnes(total)
  base  <- floor(x)
  reste <- as.integer(cible - sum(base))
  if (reste > 0) {
    ordre <- order(-round(x - base, 9), -x, seq_along(x))   # parties fractionnaires arrondies : 10,6 - 10 vaut 0,6 (pas 0,5999…)
    base[ordre[seq_len(min(reste, length(x)))]] <- base[ordre[seq_len(min(reste, length(x)))]] + 1
    reste <- as.integer(cible - sum(base))
    while (reste > 0) {                                 # total exact > Σ composantes (cas atypique) : on complète la plus grande
      base[which.max(x)] <- base[which.max(x)] + 1; reste <- reste - 1L
    }
  } else if (reste < 0) {                               # total exact < Σ composantes (cas atypique) : on retire à la plus grande
    while (reste < 0) { i <- which.max(base); base[i] <- base[i] - 1; reste <- reste + 1L }
  }
  base
}

# --- Table de restitution : personnes -> entier, taux -> 1 décimale, reste intact --
# personnes     : colonnes à traiter en personnes (défaut : COLONNES_PERSONNES présentes)
# taux          : colonnes à une décimale (défaut : noms en _pct, taux_*, part_*)
# decompositions: liste de list(total, composantes) appliquée LIGNE PAR LIGNE
#                 quand toutes les colonnes existent (défaut : DECOMPOSITIONS_PERSONNES)
# NA (secret) conservés ; codes et libellés jamais modifiés.
formater_restitution <- function(df, personnes = COLONNES_PERSONNES, taux = NULL,
                                 decompositions = DECOMPOSITIONS_PERSONNES, decimales_taux = DECIMALES_TAUX) {
  if (is.null(taux)) taux <- grep("_pct$|^taux_|^part_", names(df), value = TRUE)
  personnes <- intersect(personnes, names(df)); taux <- setdiff(intersect(taux, names(df)), personnes)
  traitees <- character(0)
  for (d in decompositions) {
    if (!all(c(d$total, d$composantes) %in% names(df))) next
    comp <- as.matrix(df[, d$composantes, drop = FALSE]); tot <- as.numeric(df[[d$total]])
    for (i in seq_len(nrow(df))) comp[i, ] <- arrondir_composantes_avec_total(comp[i, ], tot[i])
    for (k in seq_along(d$composantes)) df[[d$composantes[k]]] <- as.numeric(comp[, k])
    df[[d$total]] <- arrondir_nombre_personnes(tot)
    traitees <- c(traitees, d$total, d$composantes)
  }
  for (v in setdiff(personnes, traitees))                      # entiers observés (effectifs) : déjà entiers, type conservé
    if (is.numeric(df[[v]]) && !is.integer(df[[v]])) df[[v]] <- arrondir_nombre_personnes(df[[v]])
  for (v in taux) if (is.numeric(df[[v]])) df[[v]] <- arrondir_taux(df[[v]], decimales_taux)
  df
}

# --- Formats texte (français) partagés par les restitutions HTML ----------------
fmt_personnes <- function(x, na = "n.d.") ifelse(is.finite(as.numeric(x)),
                                                 formatC(arrondir_nombre_personnes(x), format = "d", big.mark = " "), na)
