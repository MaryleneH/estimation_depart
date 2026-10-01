# ==============================================================================
# 00d_fonctions_fiches.R — Fiches « chiffres clés » territoriales (fonctions)
# ------------------------------------------------------------------------------
# Rôle : transformer les résultats du modèle en une fiche d'UNE page par
#        territoire, lisible en une minute par un décideur non statisticien.
#        Fonctions PURES (testables sans lancer la chaîne), génériques : elles
#        ne connaissent que le contrat geo_code / geo_nom / geo_type et les
#        libellés de GEO_ZONAGES — jamais le zonage ni le schéma source.
#
# Chaîne :  calculer_contexte_perimetre()   médianes + totaux, UNE fois
#           calculer_indicateurs_territoire()  -> objet `ind` (seule source des chiffres)
#           phrases_a_retenir()              règles déterministes (2-3 phrases)
#           generer_html_fiche()             page 1 en 5 zones + annexe optionnelle
#                                            (HTML/CSS autonome, barres en CSS)
#           selectionner_territoires() -> generer_fiches()   boucle + journal
#
# Dépendances : R de base, dplyr, tibble. Ni ggplot2, ni gt, ni navigateur.
# Secret statistique : règle Insee de la Base Tous salariés (00_config :
#   SECRET_MIN_SALARIES / SECRET_MIN_ENTREPRISES / SECRET_DOMINANCE_PCT), portée
#   par les fonctions regles_secret(), indicateurs_secret(), secret_primaire(),
#   secret_secondaire() et masquer_cellules() ci-dessous — utilisées aussi par
#   08, 08b et 08c/00e. Cellule masquée -> pas de barre, note discrète.
# ==============================================================================
library(dplyr)

# --- Formats FR (espace insécable, virgule) -----------------------------------
fmt_n   <- function(x) fmt_personnes(x, na = "n.d.")   # personnes -> entier (convention 00g : demi vers le haut, NA conservé)
fmt_pct <- function(x, dec = 0) ifelse(is.finite(x),
                                       paste0(formatC(round(x, dec), format = "f", digits = dec,
                                                      decimal.mark = ","), " %"), "n.d.")
fmt_pts <- function(x) { s <- if (x > 0) "+" else if (x < 0) "−" else "±"
                         paste0(s, formatC(abs(round(x, 1)), format = "f", digits = 1, decimal.mark = ","),
                                if (abs(round(x, 1)) >= 2) " points" else " point") }
echap_html <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE)
                            x <- gsub("<", "&lt;", x, fixed = TRUE)
                            gsub(">", "&gt;", x, fixed = TRUE) }

# Libellés « en toutes lettres » des CS pour les phrases automatiques
LIBELLES_CS <- c("Cadres" = "les cadres", "Prof. intermediaires" = "les professions intermédiaires",
                 "Employes" = "les employés", "Ouvriers" = "les ouvriers")
LIBELLES_CS_COURTS <- c("Cadres" = "Cadres", "Prof. intermediaires" = "Prof. intermédiaires",
                        "Employes" = "Employés", "Ouvriers" = "Ouvriers")
libelle_cs <- function(cs, court = FALSE) {
  tab <- if (court) LIBELLES_CS_COURTS else LIBELLES_CS
  ifelse(cs %in% names(tab), tab[cs], cs)
}

# --- Nom de fichier stable : <code>_<slug du nom>.html ------------------------
slug_fiche <- function(code, nom) {
  s <- iconv(nom, from = "UTF-8", to = "ASCII//TRANSLIT")
  s[is.na(s)] <- nom[is.na(s)]
  s <- gsub("[^a-z0-9]+", "-", tolower(s)); s <- gsub("^-+|-+$", "", s)
  c <- tolower(gsub("[^A-Za-z0-9]", "", code))
  paste0(c, "_", ifelse(nzchar(s), s, "territoire"))
}

# --- Secret statistique : règle Insee de la Base Tous salariés ---------------
# Source : fiche « Confidentialité » Insee de la Base Tous salariés et Guide du
# secret statistique Insee (sept. 2025). Une cellule est masquée (PRIMAIRE) si
#   - elle porte sur moins de SECRET_MIN_SALARIES salariés, ou
#   - sur moins de SECRET_MIN_ENTREPRISES entreprises (SIREN distincts), ou
#   - une entreprise représente plus de SECRET_DOMINANCE_PCT % d'une grandeur
#     étudiée (effectif de la cellule OU départs attendus).
# Puis secret SECONDAIRE (secret_secondaire). Les trois paramètres vivent dans
# 00_config.R ; `regles = NULL` = aucun secret (usage interne).
regles_secret <- function(min_salaries = SECRET_MIN_SALARIES,
                          min_entreprises = SECRET_MIN_ENTREPRISES,
                          dominance_pct = SECRET_DOMINANCE_PCT) {
  stopifnot(is.numeric(min_salaries), min_salaries >= 1,
            is.numeric(min_entreprises), min_entreprises >= 1,
            is.numeric(dominance_pct), dominance_pct > 0, dominance_pct <= 100)
  list(min_salaries = min_salaries, min_entreprises = min_entreprises, dominance_pct = dominance_pct)
}

# Phrase unique décrivant la règle (fiches, index, notes de lecture).
texte_regle_secret <- function(regles) {
  if (is.null(regles)) return("Secret statistique non appliqué.")
  sprintf(paste0("Secret statistique (règle Insee de la Base Tous salariés) : toute cellule de moins de %d salariés, ",
                 "de moins de %d entreprises ou dont une entreprise représente plus de %s %% n’est pas diffusée, ",
                 "avec suppression secondaire."),
          regles$min_salaries, regles$min_entreprises, format(regles$dominance_pct))
}

# Indicateurs de secret d'une cellule, calculés sur les LIGNES INDIVIDUELLES
# (contrat bts_projete : siren, p_central) : nombre d'entreprises et part de
# l'entreprise dominante (max entre la part en effectif et la part en départs
# attendus, « chaque grandeur étudiée »). `cles` = colonnes de la cellule
# (vide = une seule cellule, ex. France entière).
indicateurs_secret <- function(d, cles = character(0)) {
  requis <- c(cles, "siren", "p_central")
  manque <- setdiff(requis, names(d))
  if (length(manque) > 0) stop("indicateurs_secret : colonnes absentes : ", paste(manque, collapse = ", "))
  d |>
    group_by(across(all_of(c(cles, "siren")))) |>
    summarise(.n = n(), .dep = sum(p_central), .groups = "drop") |>
    group_by(across(all_of(cles))) |>
    summarise(n_entreprises = n(),
              part_dominante_pct = 100 * max(max(.n) / sum(.n),
                                             if (sum(.dep) > 0) max(.dep) / sum(.dep) else 0),
              .groups = "drop")
}

# Secret primaire : TRUE si l'une des trois conditions est vraie. Vectorisé.
secret_primaire <- function(n, n_entreprises, part_dominante_pct, regles = regles_secret()) {
  if (is.null(regles)) return(rep(FALSE, length(n)))
  if (length(n_entreprises) != length(n) || length(part_dominante_pct) != length(n))
    stop("secret_primaire : n, n_entreprises et part_dominante_pct doivent avoir la même longueur.")
  n < regles$min_salaries |
    n_entreprises < regles$min_entreprises |
    part_dominante_pct > regles$dominance_pct
}

# Secret secondaire : si UNE seule cellule d'un bloc est masquée alors que la
# marge du bloc est publiée, elle se retrouve par différence : on masque aussi
# la plus petite cellule restante (en salariés). Si deux cellules ou plus sont
# déjà masquées, rien n'est déductible : on n'en masque pas davantage.
secret_secondaire <- function(masque, n) {
  if (sum(masque) == 1 && sum(!masque) > 1) {
    cand <- which(!masque)
    masque[cand[which.min(n[cand])]] <- TRUE
  }
  masque
}

# Règle complète d'un bloc (primaire puis secondaire) : décision par cellule.
masquer_cellules <- function(n, n_entreprises, part_dominante_pct, regles = regles_secret()) {
  if (is.null(regles)) return(rep(FALSE, length(n)))
  secret_secondaire(secret_primaire(n, n_entreprises, part_dominante_pct, regles), n)
}

# --- Décomposition par cause (mêmes maths que 06/08 : risques concurrents) ----
ajouter_parts_causes <- function(d) {
  d |> mutate(.brut_ret = p_cal_central * (1 - p_inval) * (1 - p_deces),
              .brut_inv = p_inval * (1 - p_cal_central) * (1 - p_deces),
              .brut_dec = p_deces * (1 - p_cal_central) * (1 - p_inval),
              .som      = pmax(.brut_ret + .brut_inv + .brut_dec, 1e-12),
              part_ret  = p_central * .brut_ret / .som,
              part_inv  = p_central * .brut_inv / .som,
              part_dec  = p_central * .brut_dec / .som)
}

# --- Contexte du périmètre : médianes (mêmes formules que le 08) et totaux ----
calculer_contexte_perimetre <- function(base, age_senior = AGE_SENIOR) {
  base <- base |> mutate(senior = age_2024 >= age_senior)
  par_territoire <- base |>
    group_by(geo_code) |>
    summarise(effectif_champ = n(), effectif_55plus = sum(senior),
              part_55plus_pct = 100 * mean(senior),
              departs_55plus  = sum(p_central[senior]), .groups = "drop") |>
    mutate(taux_depart_55plus_pct = ifelse(effectif_55plus > 0,
                                           100 * departs_55plus / effectif_55plus, NA_real_))
  ens <- base |>
    summarise(effectif_champ = n(), effectif_55plus = sum(senior),
              part_55plus_pct = 100 * mean(senior),
              departs_central = sum(p_central), departs_bas = sum(p_bas),
              departs_haut = sum(p_haut), departs_55plus = sum(p_central[senior]))
  etendue <- function(v) { v <- v[is.finite(v)]; if (length(v) == 0) c(NA_real_, NA_real_) else range(v) }
  list(n_territoires = nrow(par_territoire),
       med_part = median(par_territoire$part_55plus_pct, na.rm = TRUE),
       med_taux = median(par_territoire$taux_depart_55plus_pct, na.rm = TRUE),
       # étendue (min, max) du périmètre : échelle du repère de position des fiches
       etendue = list(part = etendue(par_territoire$part_55plus_pct),
                      taux = etendue(par_territoire$taux_depart_55plus_pct)),
       ensemble = ens,
       age_senior = age_senior)
}

# --- Position par rapport à la médiane ---------------------------------------
# Règle : dichotomie « au-dessus / en dessous » (celle du quadrant). Si un
# seuil en points est fourni (FICHES_SEUIL_PROCHE), « proche » à l'intérieur.
position_mediane <- function(x, med, seuil_proche = NULL) {
  if (!is.finite(x) || !is.finite(med)) return(list(ecart = NA_real_, libelle = "n.d.", classe = "nd"))
  ecart <- x - med
  if (!is.null(seuil_proche) && abs(ecart) <= seuil_proche)
    return(list(ecart = ecart, libelle = "proche de la médiane", classe = "proche"))
  if (ecart >= 0) list(ecart = ecart, libelle = "au-dessus de la médiane", classe = "dessus")
  else            list(ecart = ecart, libelle = "en dessous de la médiane", classe = "dessous")
}

# --- Indicateurs d'un territoire : l'objet `ind`, seule source des chiffres ---
calculer_indicateurs_territoire <- function(base, code, contexte,
                                            regles = regles_secret(),
                                            age_senior = AGE_SENIOR,
                                            breaks = BREAKS_TRANCHES,
                                            labels = LABELS_TRANCHES,
                                            seuil_proche = NULL,
                                            age_min = AGE_MIN_BTS,
                                            stock = NULL,
                                            detail_entreprises = FALSE,
                                            ref_siren = NULL) {
  d <- base |> filter(geo_code == code)
  if (nrow(d) == 0) stop("Territoire '", code, "' absent des données.")
  d <- d |> mutate(senior = age_2024 >= age_senior,
                   tranche = cut(age_2024, breaks = breaks, labels = labels)) |>
    ajouter_parts_causes()
  n_champ <- nrow(d); n55 <- sum(d$senior)

  # Secret statistique : indicateurs (entreprises, dominance) du territoire
  # entier et de chaque cellule, puis règle Insee primaire + secondaire.
  sec_terr <- indicateurs_secret(d)
  n_ent <- sec_terr$n_entreprises
  joindre_secret <- function(t, cle) {      # cellule absente (tranche vide) : 0 entreprise
    s <- indicateurs_secret(d, cle) |> mutate(across(all_of(cle), as.character))
    t |> left_join(s, by = cle) |>
      mutate(n_entreprises = coalesce(n_entreprises, 0L),
             part_dominante_pct = coalesce(part_dominante_pct, 0))
  }
  # Âges (stock) — masquage + suppression secondaire. Une tranche est « senior »
  # si sa borne inférieure (borne cut exclue + 1) atteint age_senior.
  bornes_inf <- breaks[-length(breaks)] + 1
  ages <- d |> count(tranche, name = "n", .drop = FALSE) |>
    mutate(tranche = as.character(tranche)) |>
    joindre_secret("tranche") |>
    mutate(part = 100 * n / n_champ,
           senior = (bornes_inf >= age_senior)[match(tranche, labels)],
           masque = masquer_cellules(n, n_entreprises, part_dominante_pct, regles)) |>
    select(-n_entreprises, -part_dominante_pct)

  # Départs (flux)
  dep <- list(central = sum(d$p_central), bas = sum(d$p_bas), haut = sum(d$p_haut),
              seniors_central = sum(d$p_central[d$senior]),
              seniors_bas = sum(d$p_bas[d$senior]), seniors_haut = sum(d$p_haut[d$senior]))
  dep$taux_seniors <- if (n55 > 0) 100 * dep$seniors_central / n55 else NA_real_

  # Causes (sur l'ensemble des départs attendus du territoire)
  tot <- max(dep$central, 1e-12)
  causes <- tibble::tibble(
    cause = c("Retraite / fin de carrière", "Invalidité", "Décès"),
    pct   = 100 * c(sum(d$part_ret), sum(d$part_inv), sum(d$part_dec)) / tot)

  # Catégories sociales — même règle, cellule = territoire x CS (comme 08b)
  cs <- d |> group_by(cs1) |>
    summarise(n = n(), n55 = sum(senior), part55 = 100 * mean(senior),
              departs = sum(p_central), departs55 = sum(p_central[senior]), .groups = "drop") |>
    joindre_secret("cs1") |>
    mutate(masque = masquer_cellules(n, n_entreprises, part_dominante_pct, regles)) |>
    select(-n_entreprises, -part_dominante_pct) |>
    arrange(masque, desc(n55))
  cs_masquee <- any(cs$masque)
  # Effectifs actuels TOUS ÂGES (01c) : dénominateur « part de la catégorie à
  # remplacer ». Disponible seulement si le stock couvre toutes les CS du territoire.
  st <- if (!is.null(stock)) stock |> filter(geo_code == code) |> select(cs1, n_tous = effectif_tous_ages) else NULL
  cs <- cs |> left_join(if (is.null(st)) tibble::tibble(cs1 = character(0), n_tous = integer(0)) else st, by = "cs1")
  stock_disponible <- !is.null(st) && all(is.finite(cs$n_tous)) && all(cs$n_tous >= cs$n)
  n_tous_ages <- if (stock_disponible) sum(cs$n_tous) else NA_integer_

  # Position dans le périmètre
  part55 <- 100 * n55 / n_champ
  pos <- list(part = position_mediane(part55, contexte$med_part, seuil_proche),
              taux = position_mediane(dep$taux_seniors, contexte$med_taux, seuil_proche))

  secret <- !is.null(regles)
  # Le territoire entier est une cellule : même règle (un département dont les
  # salariés relèvent d'une ou deux entreprises n'a pas de fiche diffusable).
  diffusable <- !secret_primaire(n_champ, n_ent, sec_terr$part_dominante_pct, regles)
  # Bloc entreprises (usage INTERNE uniquement) : une ligne par SIREN, nom si
  # un référentiel  code;nom  est fourni, sinon le numéro.
  entreprises <- if (isTRUE(detail_entreprises)) {
    e <- d |> group_by(siren = as.character(siren)) |>
      summarise(n = n(), n55 = sum(senior), departs = sum(p_central), .groups = "drop") |>
      arrange(desc(departs))
    if (!is.null(ref_siren) && all(c("code", "nom") %in% names(ref_siren)))
      e <- e |> left_join(ref_siren |> transmute(siren = as.character(code), nom), by = "siren")
    else e$nom <- NA_character_
    e
  } else NULL
  list(code = code, nom = as.character(d$geo_nom[1]), geo_type = as.character(d$geo_type[1]),
       diffusable = diffusable, regles = regles, secret = secret,
       age_senior = age_senior, age_min = age_min,
       population = list(n_champ = n_champ, n55 = n55, part55 = part55,
                         n_tous_ages = n_tous_ages,
                         n_entreprises = if (!secret || n_ent >= regles$min_entreprises) n_ent else NA_integer_),
       stock_disponible = stock_disponible,
       ages = ages, departs = dep, causes = causes, cs = cs, cs_masquee = cs_masquee,
       position = pos, entreprises = entreprises)
}

# --- Textes automatiques : règles déterministes, descriptives ----------------
# UN seul bloc « À retenir » : deux ou trois phrases, factuelles, jamais
# prescriptives, et jamais le chiffre HERO (les départs attendus sont déjà en
# tête de fiche : chaque donnée n'apparaît qu'une fois).
phrases_a_retenir <- function(ind, contexte = NULL) {
  p <- ind$population; dep <- ind$departs; s <- ind$age_senior; a <- ind$age_min
  ph <- sprintf("%s des salariés de %d ans et + ont %d ans ou plus.", fmt_pct(p$part55), a, s)
  if (is.finite(dep$taux_seniors))
    ph <- c(ph, sprintf("%s des salariés de %d ans et + devraient avoir quitté l’emploi d’ici 2030.",
                        fmt_pct(dep$taux_seniors), s))
  else
    ph <- c(ph, sprintf("Le territoire ne compte aucun salarié de %d ans et + dans le champ.", s))
  # Troisième phrase : la part des effectifs ACTUELS à remplacer (stock tous
  # âges, 01c). La précision par catégorie n'est donnée que si aucune CS n'est
  # masquée (sinon le maximum n'est pas certain).
  if (isTRUE(ind$stock_disponible) && is.finite(p$n_tous_ages) && p$n_tous_ages > 0 && dep$central > 0) {
    part_tot <- 100 * dep$central / p$n_tous_ages
    if (!ind$cs_masquee && nrow(ind$cs) >= 2) {
      o <- ind$cs |> mutate(part = 100 * departs / n_tous) |> arrange(desc(part))
      ph <- c(ph, sprintf("D’ici 2030, %s des salariés actuels (tous âges) devraient être partis, jusqu’à %s chez %s.",
                          fmt_pct(part_tot), fmt_pct(o$part[1]), libelle_cs(o$cs1[1])))
    } else
      ph <- c(ph, sprintf("D’ici 2030, %s des salariés actuels (tous âges) devraient être partis.", fmt_pct(part_tot)))
  } else if (!ind$cs_masquee && nrow(ind$cs) >= 2 && dep$central > 0) {
    # Sans stock : part des départs attendus portée par la première catégorie
    # (rang + part explicites), seulement si l'écart avec la suivante est net.
    o <- ind$cs |> arrange(desc(departs))
    if (o$departs[2] > 0 && (o$departs[1] - o$departs[2]) / o$departs[2] > 0.10)
      ph <- c(ph, sprintf("%s représentent %s des départs attendus du territoire.",
                          cap1(libelle_cs(o$cs1[1])), fmt_pct(100 * o$departs[1] / dep$central)))
  }
  head(ph, 3)
}
cap1 <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))

# --- Briques HTML/CSS ---------------------------------------------------------
# Repère de position : piste = étendue du périmètre (min..max), trait = médiane,
# point = ce territoire. Rien n'est dessiné si une valeur manque.
html_echelle <- function(x, med, etendue) {
  if (!is.finite(x) || !is.finite(med) || length(etendue) != 2 ||
      !all(is.finite(etendue)) || diff(etendue) <= 0) return("")
  rel <- function(v) max(0, min(100, 100 * (v - etendue[1]) / diff(etendue)))
  sprintf(paste0('<div class="ech" aria-hidden="true"><span class="ech-med" style="left:%.1f%%"></span>',
                 '<span class="ech-pt" style="left:%.1f%%"></span></div>'), rel(med), rel(x))
}

# Une ligne de barre générique (annexe) : cellule masquée = pas de barre.
html_ligne_barre <- function(libelle, pct_largeur, valeur, masque = FALSE,
                             note = "non diffusé (secret statistique)") {
  if (masque)
    return(sprintf('<div class="an-row"><span class="an-lib">%s</span><span class="an-nd">%s</span><span class="an-val"></span></div>',
                   echap_html(libelle), note))
  sprintf('<div class="an-row"><span class="an-lib">%s</span><span class="an-piste"><span class="an-bar" style="width:%.1f%%"></span></span><span class="an-val">%s</span></div>',
          echap_html(libelle), max(0, min(100, pct_largeur)), valeur)
}

# --- Feuille de style : une seule, partagée par les fiches et l'index ---------
# Note de direction + dataviz éditoriale : fond blanc, une couleur institution-
# nelle, gris pour le secondaire, typographie système, pas de cartes ni d'ombres.
css_fiches <- function() paste(
  ':root{--encre:#1a1f2b;--texte:#4a5260;--gris:#8a919c;--filet:#e4e7ec;--bleu:#1e3a5f;--bleu-2:#a9bacd;--fond-2:#f3f5f8}',
  '*{box-sizing:border-box}html{-webkit-text-size-adjust:100%}',
  'body{margin:0;background:#fff;color:var(--encre);font-family:-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif;font-size:15px;line-height:1.45}',
  '.page{max-width:760px;margin:0 auto;padding:44px 36px 32px}',
  'a{color:var(--bleu)}',
  # zone 1
  '.kicker{font-size:12px;letter-spacing:.14em;text-transform:uppercase;color:var(--gris);margin:0 0 10px}',
  'h1{font-size:46px;line-height:1.02;letter-spacing:-.022em;font-weight:700;margin:0 0 12px;overflow-wrap:anywhere}',
  '.titre2{font-size:18px;color:var(--texte);margin:0}',
  '.perim{font-size:13px;color:var(--gris);margin:6px 0 0}',
  '.zone{padding:26px 0;border-top:1px solid var(--filet)}.zone:first-of-type{border-top:0;padding-top:0}',
  # zone 2
  '.hero{display:grid;grid-template-columns:1.25fr 1fr;gap:32px;align-items:start}',
  '.hero-n{font-size:74px;line-height:.95;font-weight:700;letter-spacing:-.03em;color:var(--bleu);font-variant-numeric:tabular-nums;margin:2px 0 8px}',
  '.hero-l{font-size:17px;font-weight:600;margin:0}',
  '.hero-d{font-size:13px;color:var(--gris);margin:6px 0 0}',
  '.hero-c{font-size:14px;color:var(--texte);margin:14px 0 0;max-width:34ch}',
  '.ctx{border-left:1px solid var(--filet);padding-left:28px;display:flex;flex-direction:column;gap:18px;padding-top:6px}',
  '.ctx-n{font-size:30px;font-weight:700;line-height:1;font-variant-numeric:tabular-nums;letter-spacing:-.02em}',
  '.ctx-l{font-size:14px;color:var(--texte);margin-top:5px}',
  '.ctx-l b{color:var(--encre);font-weight:600}',
  # titres de zone
  'h2{font-size:21px;line-height:1.2;font-weight:600;letter-spacing:-.012em;margin:0 0 4px}',
  '.sous{font-size:13.5px;color:var(--gris);margin:0 0 18px}',
  # zone 3 (tableau)
  '.note{font-size:12.5px;color:var(--gris);margin:10px 0 0}',
  '.sr-only{position:absolute;width:1px;height:1px;overflow:hidden;clip:rect(0 0 0 0);white-space:nowrap}',
  '.bandeau{background:#7a1f1f;color:#fff;font-size:13px;font-weight:600;letter-spacing:.04em;text-transform:uppercase;padding:9px 14px;margin:0 0 22px}',
  '.tab td.siren{font-family:ui-monospace,Menlo,Consolas,monospace;font-size:13px;text-align:left}',
  '.tab{width:100%;border-collapse:collapse;font-variant-numeric:tabular-nums}',
  '.tab th,.tab td{padding:10px 8px;text-align:right;border-bottom:1px solid var(--filet);font-size:15px;vertical-align:bottom}',
  '.tab thead th{font-weight:500;color:var(--texte);font-size:12.5px;line-height:1.25;padding-bottom:8px}',
  '.tab thead th small,.tab thead th{white-space:normal}.tab th small{display:block;color:var(--gris);font-weight:400;font-size:11.5px}',
  '.tab th[scope=row]{text-align:left;font-weight:400;color:var(--encre)}.tab thead th:first-child{text-align:left}',
  '.tab td.fort{font-weight:700;color:var(--bleu);font-size:17px}',
  '.tab tr.total th,.tab tr.total td{border-top:2px solid var(--encre);border-bottom:0;font-weight:600}.tab tr.total td.fort{font-weight:700}',
  # zone 4
  '.pos{margin:0 0 18px}.pos-tete{display:flex;justify-content:space-between;gap:16px;align-items:baseline}',
  '.pos-lib{font-size:15px}.pos-val{font-weight:600;font-variant-numeric:tabular-nums;font-size:16px}',
  '.pos-verdict{font-size:12px;letter-spacing:.1em;text-transform:uppercase;font-weight:600;color:var(--bleu);margin:2px 0 8px}',
  '.pos-verdict.dessous{color:var(--texte)}.pos-verdict.nd{color:var(--gris);text-transform:none;letter-spacing:0;font-weight:400}',
  '.ech{position:relative;height:14px;margin:0 0 4px}.ech::before{content:"";position:absolute;left:0;right:0;top:6px;height:2px;background:var(--filet)}',
  '.ech-med{position:absolute;top:0;width:2px;height:14px;background:var(--gris);transform:translateX(-1px)}',
  '.ech-pt{position:absolute;top:2px;width:10px;height:10px;border-radius:50%;background:var(--bleu);transform:translateX(-5px)}',
  '.pos-med{font-size:12px;color:var(--gris)}',
  '.legende{font-size:12px;color:var(--gris);margin:-8px 0 16px}.legende i{display:inline-block;width:9px;height:9px;border-radius:50%;background:var(--bleu);margin:0 5px 0 0;vertical-align:-1px}.legende b{display:inline-block;width:2px;height:11px;background:var(--gris);margin:0 5px 0 14px;vertical-align:-2px}',
  # zone 5
  '.retenir{list-style:none;margin:0;padding:0}.retenir li{position:relative;padding-left:22px;margin:0 0 10px;font-size:16px;line-height:1.4}',
  '.retenir li::before{content:"";position:absolute;left:0;top:.6em;width:10px;height:2px;background:var(--bleu)}',
  # sources
  'footer{margin-top:8px;padding-top:14px;border-top:1px solid var(--filet);font-size:11.5px;line-height:1.5;color:var(--gris)}',
  # annexe
  '.annexe{border-top:2px solid var(--filet);margin-top:28px;padding-top:24px}.annexe h2{font-size:17px}.annexe h3{font-size:13px;letter-spacing:.1em;text-transform:uppercase;color:var(--gris);margin:22px 0 8px}',
  '.an-row{display:grid;grid-template-columns:150px 1fr 70px;gap:12px;align-items:center;margin:0 0 7px;font-size:14px}',
  '.an-piste{height:12px;background:var(--fond-2)}.an-bar{display:block;height:100%;background:var(--bleu-2)}.an-val{text-align:right;font-variant-numeric:tabular-nums}.an-nd{font-size:12px;color:var(--gris);font-style:italic}',
  '.an-txt{font-size:14px;color:var(--texte);margin:0 0 6px}',
  # index
  '.recherche{width:100%;font:inherit;font-size:16px;padding:10px 12px;border:1px solid var(--filet);margin:0 0 18px;background:#fff;color:var(--encre)}',
  '.liste{list-style:none;margin:0;padding:0}.liste li{display:grid;grid-template-columns:52px 1fr auto;gap:14px;align-items:baseline;padding:9px 0;border-bottom:1px solid var(--filet)}',
  '.liste .code{color:var(--gris);font-variant-numeric:tabular-nums}.liste a{text-decoration:none;color:var(--encre);font-size:16px}.liste a:hover{text-decoration:underline}.liste .n{font-variant-numeric:tabular-nums;color:var(--texte);font-size:14px;white-space:nowrap}',
  '.vide{color:var(--gris);display:none}.methode{font-size:13px;color:var(--texte);line-height:1.5;margin-top:32px;padding-top:16px;border-top:1px solid var(--filet)}',
  # petits écrans
  '@media (max-width:640px){.page{padding:28px 18px 24px}h1{font-size:36px}.hero{grid-template-columns:1fr;gap:22px}',
  '.ctx{border-left:0;padding-left:0;border-top:1px solid var(--filet);padding-top:18px;flex-direction:row;gap:28px;flex-wrap:wrap}.hero-n{font-size:60px}',
  '.tab th,.tab td{padding:8px 4px;font-size:13px}.tab td.fort{font-size:15px}.tab thead th{font-size:11px}',
  '.an-row{grid-template-columns:1fr 60px}.an-lib{grid-column:1/-1}.liste li{grid-template-columns:44px 1fr auto}}',
  # impression A4
  '@page{size:A4;margin:15mm 16mm}',
  '@media print{body{font-size:12px;line-height:1.35}.page{padding:0;max-width:none}.zone{padding:15px 0}',
  'h1{font-size:34px;margin-bottom:8px}.titre2{font-size:15px}.perim{font-size:11.5px}.hero{gap:24px}.hero-n{font-size:56px;margin:0 0 6px}.hero-l{font-size:14px}.hero-d,.hero-c{font-size:11.5px}.hero-c{margin-top:8px}',
  '.ctx{gap:12px;padding-top:2px}.ctx-n{font-size:24px}.ctx-l{font-size:12px;margin-top:3px}h2{font-size:17px}.sous{font-size:11.5px;margin-bottom:10px}',
  '.tab th,.tab td{padding:5px 6px;font-size:11.5px}.tab td.fort{font-size:13px}.tab thead th{font-size:10.5px}.tab th small{font-size:9.5px}',
  '.legende{margin:-4px 0 8px;font-size:10.5px}.pos{margin:0 0 9px}.pos-lib{font-size:12.5px}.pos-val{font-size:13px}.pos-verdict{font-size:10px;margin:0 0 4px}.ech{height:12px;margin-bottom:2px}.pos-med{font-size:10.5px}',
  '.retenir li{font-size:13px;margin:0 0 5px;padding-left:18px}footer{font-size:9.5px;line-height:1.4;padding-top:9px}',
  '.note{font-size:10.5px;margin-top:6px}.zone,.pos,.tab tr{break-inside:avoid}.annexe{break-before:page;border-top:0;margin-top:0}.recherche{display:none}a{text-decoration:none;color:inherit}}',
  sep = "\n")

# --- La fiche HTML ------------------------------------------------------------
# Page 1 = cinq zones : identité, message principal (HERO), départs par CS,
# position dans le périmètre, à retenir. Annexe optionnelle (FICHES_ANNEXE).
generer_html_fiche <- function(ind, contexte, zonage, seuil_proche = NULL,
                               source_note = "données : table test", annexe = FALSE) {
  p <- ind$population; dep <- ind$departs; s <- ind$age_senior; a <- ind$age_min
  nom <- echap_html(ind$nom); code <- echap_html(ind$code)
  zl <- echap_html(zonage$libelle); zp <- echap_html(zonage$pluriel)

  # ---- Zone 2 : cause dominante (retraite / fin de carrière), si elle domine
  c_ret <- ind$causes$pct[1]
  ligne_cause <- if (dep$central > 0 && is.finite(c_ret) && c_ret >= 50)
    sprintf('<p class="hero-c">%s de ces départs relèvent de la retraite ou d’une fin de carrière.</p>', fmt_pct(c_ret)) else ""

  # ---- Zone 3 : tableau départs par CS, décroissants, rapportés aux effectifs
  #      actuels tous âges (01c) — à défaut, aux salariés du champ. Une CS masquée
  #      n'a pas de ligne ; la ligne « Ensemble » porte les totaux du territoire.
  cs_ok <- ind$cs |> filter(!masque) |> arrange(desc(departs))
  # Départs affichés par CS : si toutes les CS sont affichées, arrondis cohérents
  # avec le total « Ensemble » (plus forts restes, 00g) ; sinon chaque CS seule.
  dep_cs_aff <- if (nrow(cs_ok) > 0 && !ind$cs_masquee) arrondir_composantes_avec_total(cs_ok$departs, dep$central)
                else arrondir_nombre_personnes(cs_ok$departs)
  avec_stock <- isTRUE(ind$stock_disponible)
  eff_cs  <- if (avec_stock) cs_ok$n_tous else cs_ok$n
  eff_tot <- if (avec_stock) p$n_tous_ages else p$n_champ
  lib_eff <- if (avec_stock) "Salariés aujourd’hui<small>tous âges</small>" else
    sprintf("Salariés de %d ans et +<small>aujourd’hui</small>", a)
  lib_part <- if (avec_stock) "Part de la catégorie<small>à remplacer d’ici 2030</small>" else
    sprintf("Part des %d ans et +<small>de la catégorie, d’ici 2030</small>", a)
  sous_titre_cs <- if (avec_stock) "Départs attendus d’ici 2030 par catégorie sociale, rapportés aux effectifs actuels" else
    sprintf("Départs attendus d’ici 2030 par catégorie sociale, rapportés aux salariés de %d ans et +", a)
  lignes_cs <- if (nrow(cs_ok) == 0)
    '<p class="note">Aucune catégorie ne peut être affichée en application du secret statistique.</p>' else
    paste0('<table class="tab"><caption class="sr-only">Départs attendus par catégorie sociale</caption><thead><tr>',
           sprintf('<th scope="col">Catégorie</th><th scope="col">%s</th><th scope="col">Départs attendus<small>d’ici 2030</small></th><th scope="col">%s</th></tr></thead><tbody>',
                   lib_eff, lib_part),
           paste(sprintf('<tr><th scope="row">%s</th><td>%s</td><td class="fort">%s</td><td>%s</td></tr>',
                         echap_html(libelle_cs(cs_ok$cs1, TRUE)), fmt_n(eff_cs), fmt_n(dep_cs_aff),
                         fmt_pct(100 * cs_ok$departs / eff_cs)), collapse = "\n"),
           sprintf('<tr class="total"><th scope="row">Ensemble</th><td>%s</td><td class="fort">%s</td><td>%s</td></tr>',
                   fmt_n(eff_tot), fmt_n(dep$central), fmt_pct(100 * dep$central / eff_tot)),
           '</tbody></table>')
  n_masq <- sum(ind$cs$masque)
  note_masq <- if (n_masq > 0)
    sprintf('<p class="note">%s en application du secret statistique (règle Insee, voir sources).</p>',
            if (n_masq == 1) "Une catégorie n’est pas affichée" else sprintf("%d catégories ne sont pas affichées", n_masq)) else ""

  # ---- Zone 4 : position, une ligne par indicateur, verdict en toutes lettres
  ligne_pos <- function(libelle, x, pos, med, etendue, note_na) {
    verdict <- if (pos$classe == "nd") note_na else pos$libelle
    paste0(sprintf('<div class="pos"><div class="pos-tete"><span class="pos-lib">%s</span><span class="pos-val">%s</span></div>',
                   libelle, if (is.finite(x)) fmt_pct(x, 1) else ""),
           sprintf('<div class="pos-verdict %s">%s</div>', pos$classe, verdict),
           html_echelle(x, med, etendue),
           if (is.finite(med)) sprintf('<div class="pos-med">médiane des %d %s : %s</div>',
                                        contexte$n_territoires, zp, fmt_pct(med, 1)) else "",
           '</div>')
  }
  bloc_pos <- paste0(
    ligne_pos(sprintf("Part des salariés de %d ans et +", s), p$part55, ind$position$part,
              contexte$med_part, contexte$etendue$part, "non calculable"),
    ligne_pos(sprintf("Part des salariés de %d ans et + qui devraient avoir quitté l’emploi d’ici 2030", s),
              dep$taux_seniors, ind$position$taux, contexte$med_taux, contexte$etendue$taux,
              sprintf("non calculable : aucun salarié de %d ans et +", s)))
  regle_proche <- if (!is.null(seuil_proche))
    sprintf(" « Proche de la médiane » : écart inférieur ou égal à %s points.", format(seuil_proche)) else ""

  # ---- Zone 5
  retenir <- phrases_a_retenir(ind, contexte)

  # ---- Usage interne : bandeau + bloc entreprises (SIREN)
  interne <- !isTRUE(ind$secret)
  bandeau <- if (interne) '<div class="bandeau" role="note">Document de travail · usage interne · secret statistique non appliqué · ne pas diffuser</div>' else ""
  bloc_entreprises <- if (interne && !is.null(ind$entreprises) && nrow(ind$entreprises) > 0) {
    e <- ind$entreprises
    c('<section class="zone">',
      sprintf('<h2>Entreprises du territoire</h2><p class="sous">%d SIREN · salariés de %d ans et + rattachés à un établissement du territoire · tri par départs attendus</p>',
              nrow(e), a),
      '<table class="tab"><thead><tr><th scope="col">SIREN</th><th scope="col">Nom</th>',
      sprintf('<th scope="col">Salariés de %d ans et +</th><th scope="col">dont %d ans et +</th><th scope="col">Départs attendus<small>d’ici 2030</small></th></tr></thead><tbody>', a, s),
      paste(sprintf('<tr><td class="siren">%s</td><td style="text-align:left">%s</td><td>%s</td><td>%s</td><td class="fort">%s</td></tr>',
                    echap_html(e$siren), echap_html(ifelse(is.na(e$nom), "", e$nom)),
                    fmt_n(e$n), fmt_n(e$n55), fmt_n(e$departs)), collapse = "\n"),
      '</tbody></table></section>')
  } else ""


  # ---- Annexe (optionnelle) : détails retirés de la page 1
  bloc_annexe <- if (!isTRUE(annexe)) "" else {
    max_age <- max(ind$ages$part[!ind$ages$masque], 1)
    ages <- paste(mapply(function(tr, part, masq)
      html_ligne_barre(tr, 100 * part / max_age, fmt_pct(part), masque = masq),
      ind$ages$tranche, ind$ages$part, ind$ages$masque), collapse = "\n")
    ca <- ind$causes
    c('<section class="annexe">', '<h2>Annexe — éléments détaillés</h2>',
      sprintf('<h3>Structure par âge <small>(part des salariés de %d ans et +)</small></h3>', a), ages,
      '<h3>Origine des départs attendus</h3>',
      sprintf('<p class="an-txt">Retraite ou fin de carrière : %s · Invalidité : %s · Décès : %s.</p>',
              fmt_pct(ca$pct[1]), fmt_pct(ca$pct[2]), fmt_pct(ca$pct[3])),
      '<h3>Scénarios</h3>',
      sprintf('<p class="an-txt">Central : %s départs · hypothèse réglementaire basse : %s · haute : %s. Parmi les %d ans et + : %s (fourchette %s – %s).</p>',
              fmt_n(dep$central), fmt_n(dep$bas), fmt_n(dep$haut), s,
              fmt_n(dep$seniors_central), fmt_n(dep$seniors_bas), fmt_n(dep$seniors_haut)),
      '<h3>Comparaison détaillée</h3>',
      sprintf('<p class="an-txt">Part des %d ans et + : %s (médiane %s, écart %s). Part des %d ans et + ayant quitté l’emploi d’ici 2030 : %s (médiane %s, écart %s).</p>',
              s, fmt_pct(p$part55), fmt_pct(contexte$med_part),
              if (is.finite(ind$position$part$ecart)) fmt_pts(ind$position$part$ecart) else "n.d.",
              s, fmt_pct(dep$taux_seniors), fmt_pct(contexte$med_taux),
              if (is.finite(ind$position$taux$ecart)) fmt_pts(ind$position$taux$ecart) else "n.d."),
      '</section>')
  }

  c('<!DOCTYPE html>', '<html lang="fr"><head><meta charset="utf-8">',
    sprintf('<title>%s — départs attendus à l’horizon 2030</title>', nom),
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<style>', css_fiches(), '</style></head><body><div class="page">',
    bandeau,
    # ---- Zone 1 : identité
    '<header class="zone">',
    sprintf('<p class="kicker">%s · %s</p>', zl, code),
    sprintf('<h1>%s</h1>', nom),
    '<p class="titre2">Renouvellement des effectifs — départs attendus à l’horizon 2030</p>',
    sprintf('<p class="perim">Périmètre étudié · salariés de %d ans et plus en 2024 · entreprises de la BITD</p>', a),
    '</header>',
    # ---- Zone 2 : message principal
    '<section class="zone hero">',
    '<div>',
    sprintf('<div class="hero-n">%s</div>', fmt_n(dep$central)),
    '<p class="hero-l">départs attendus d’ici 2030</p>',
    sprintf('<p class="hero-d">Scénario central · fourchette %s – %s</p>', fmt_n(dep$bas), fmt_n(dep$haut)),
    ligne_cause,
    '</div>',
    '<div class="ctx">',
    sprintf('<div><div class="ctx-n">%s</div><div class="ctx-l">salariés de %d ans et +%s</div></div>',
            fmt_n(p$n_champ), a,
            if (is.finite(p$n_entreprises)) sprintf(" dans %s entreprises", fmt_n(p$n_entreprises)) else ""),
    sprintf('<div><div class="ctx-n">%s</div><div class="ctx-l">ont %d ans et +, soit <b>%s</b></div></div>',
            fmt_n(p$n55), s, fmt_pct(p$part55)),
    '</div></section>',
    # ---- Zone 3 : où se concentrent les départs
    '<section class="zone">',
    '<h2>Où se concentreraient les départs ?</h2>',
    sprintf('<p class="sous">%s</p>', sous_titre_cs),
    lignes_cs, note_masq,
    '</section>',
    # ---- Zone 4 : position
    '<section class="zone">',
    sprintf('<h2>Par rapport aux autres %s du périmètre</h2>', zp),
    sprintf('<p class="sous">Comparaison à la médiane des %d %s analysés</p>', contexte$n_territoires, zp),
    sprintf('<p class="legende"><i></i>%s <b></b>médiane</p>', nom),
    bloc_pos,
    '</section>',
    bloc_entreprises,
    # ---- Zone 5 : à retenir
    '<section class="zone">', '<h2>À retenir</h2>',
    '<ul class="retenir">', paste0('<li>', echap_html(retenir), '</li>', collapse = "\n"), '</ul>',
    '</section>',
    # ---- Sources
    '<footer>',
    sprintf(paste0('Champ : salariés de %d ans et + en 2024 des entreprises du périmètre BITD, établissements situés dans le territoire. ',
                   'Départs = sorties définitives de l’emploi d’ici 2030 (retraite ou fin de carrière, invalidité, décès) ; ',
                   'les mobilités vers d’autres employeurs ne sont pas comptées. Scénario central, fourchette = hypothèses réglementaires basse et haute. ',
                   '%s%s%s ',
                   'Sources : BTS 2024, DREES, EACR invalidité, mortalité Insee — calculs propres · %s.'),
            a, if (interne) "USAGE INTERNE : secret statistique non appliqué, document à ne pas diffuser."
               else texte_regle_secret(ind$regles), regle_proche,
            if (avec_stock) sprintf(" Effectifs actuels : tous âges, même périmètre ; part à remplacer = départs attendus des %d ans et + rapportés à l’effectif actuel de la catégorie (plancher).", a) else "",
            source_note),
    '</footer>',
    bloc_annexe,
    '</div></body></html>')
}

# --- Sélection des territoires : tous / sélection, avec contrôles ------------
selectionner_territoires <- function(base, mode = "tous", selection = NULL,
                                     regles = regles_secret()) {
  if (!mode %in% c("tous", "selection"))
    stop("FICHES_MODE doit valoir \"tous\" ou \"selection\" (reçu : ", mode, ").")
  # Le territoire entier est une cellule : règle Insee complète (salariés,
  # entreprises, dominance) pour décider s'il a une fiche diffusable.
  dispo <- base |> count(geo_code, geo_nom, name = "n") |>
    left_join(indicateurs_secret(base, c("geo_code", "geo_nom")), by = c("geo_code", "geo_nom")) |>
    mutate(secret = secret_primaire(n, n_entreprises, part_dominante_pct, regles),
           motif = if (is.null(regles)) NA_character_ else
             case_when(n < regles$min_salaries ~ sprintf("%d salarié(s) < %d", n, regles$min_salaries),
                       n_entreprises < regles$min_entreprises ~ sprintf("%d entreprise(s) < %d", n_entreprises, regles$min_entreprises),
                       part_dominante_pct > regles$dominance_pct ~ sprintf("une entreprise > %s %%", format(regles$dominance_pct)),
                       TRUE ~ NA_character_)) |>
    arrange(geo_code)
  # doublons code <-> nom : un code doit avoir un seul nom (l'inverse est toléré,
  # le nom de fichier étant préfixé du code)
  dbl <- dispo |> count(geo_code) |> filter(n > 1)
  if (nrow(dbl) > 0)
    stop("Codes portant plusieurs libellés : ", paste(dbl$geo_code, collapse = ", "),
         " — corrigez le référentiel ou la configuration géographique (00_config.R).")
  ecartes <- tibble::tibble(code = character(0), motif = character(0))
  if (mode == "selection") {
    if (is.null(selection) || length(selection) == 0)
      stop("FICHES_MODE = \"selection\" mais aucun code (FICHES_SELECTION ou GEO_INTERET vide).")
    selection <- trimws(as.character(selection))
    absents <- setdiff(selection, dispo$geo_code)
    if (length(absents) > 0) {
      warning("Fiches : ", length(absents), " territoire(s) demandé(s) absent(s) des données : ",
              paste(absents, collapse = ", "))
      ecartes <- bind_rows(ecartes, tibble::tibble(code = absents, motif = "absent des données"))
    }
    dispo <- dispo |> filter(geo_code %in% selection)
  }
  inconnu <- dispo |> filter(geo_code == "inconnu")
  if (nrow(inconnu) > 0)
    ecartes <- bind_rows(ecartes, tibble::tibble(code = "inconnu", motif = "territoire non identifié"))
  sous <- dispo |> filter(geo_code != "inconnu", secret)
  if (nrow(sous) > 0)
    ecartes <- bind_rows(ecartes, tibble::tibble(code = sous$geo_code,
                                                 motif = paste0("secret statistique : ", sous$motif)))
  retenus <- dispo |> filter(geo_code != "inconnu", !secret) |> select(code = geo_code, nom = geo_nom, n)
  if (nrow(retenus) == 0)
    stop("Fiches : aucun territoire diffusable (", nrow(ecartes), " écarté(s)).")
  list(retenus = retenus, ecartes = ecartes)
}

# --- Page d'index -------------------------------------------------------------
# Porte d'entrée : titre, recherche (JavaScript natif, facultatif), liste code ·
# nom · départs, note méthodologique complète (retirée des fiches).
generer_html_index <- function(journal, zonage, age_min = AGE_MIN_BTS, regles = regles_secret(),
                               n_territoires = NA, source_note = "données : table test") {
  interne <- is.null(regles)
  j <- journal |> filter(statut == "ok") |> arrange(code)
  zl <- echap_html(zonage$libelle); zp <- echap_html(zonage$pluriel)
  cle <- tolower(iconv(paste(j$code, j$nom), from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""))
  lignes <- sprintf('<li data-cle="%s"><span class="code">%s</span><a href="%s">%s</a><span class="n">%s départs attendus</span></li>',
                    echap_html(cle), echap_html(j$code), j$fichier, echap_html(j$nom), fmt_n(j$departs))
  ecartes <- journal |> filter(statut != "ok")
  note_ecartes <- if (nrow(ecartes) > 0)
    sprintf('<p class="note">%d territoire(s) sans fiche : %s.</p>', nrow(ecartes),
            echap_html(paste(sprintf("%s (%s)", ecartes$code, ecartes$motif), collapse = " ; "))) else ""
  c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">',
    sprintf('<title>Fiches territoriales — %s</title>', zp),
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<style>', css_fiches(), '</style></head><body><div class="page">',
    if (interne) '<div class="bandeau" role="note">Document de travail · usage interne · secret statistique non appliqué · ne pas diffuser</div>' else "",
    '<header class="zone"><p class="kicker">Fiches territoriales</p>',
    '<h1>Départs attendus à l’horizon 2030</h1>',
    sprintf('<p class="titre2">Une fiche par %s · %d fiches</p>', tolower(zl), nrow(j)),
    sprintf('<p class="perim">Périmètre étudié · salariés de %d ans et plus en 2024 · entreprises de la BITD</p>', age_min),
    '</header>',
    '<section class="zone">',
    sprintf('<input class="recherche" type="search" id="q" placeholder="Rechercher %s (nom ou code)" aria-label="Rechercher un territoire">', echap_html(zonage$un)),
    '<ul class="liste" id="liste">', lignes, '</ul>',
    '<p class="vide" id="vide">Aucun territoire ne correspond.</p>',
    note_ecartes,
    '</section>',
    '<section class="methode">',
    sprintf(paste0('<b>Méthode.</b> Champ : salariés de %d ans et + en 2024 des entreprises du périmètre BITD, rattachés à l’établissement employeur. ',
                   'Départ = sortie définitive de l’emploi d’ici 2030 : retraite ou fin de carrière (calendrier par catégorie sociale, DREES), invalidité (EACR) et décès (Insee) ; ',
                   'les mobilités vers d’autres employeurs ne sont pas comptées, les volumes sont un plancher. Les départs sont des espérances (somme de probabilités individuelles) ; ',
                   'le scénario central est encadré par deux hypothèses réglementaires. Comparaison territoriale : position par rapport à la médiane des %s %s du périmètre. ',
                   '%s ',
                   'Sources : BTS 2024, DREES, EACR invalidité, mortalité Insee — calculs propres · %s.'),
            age_min, if (is.finite(n_territoires)) n_territoires else nrow(j), zp,
            if (interne) "USAGE INTERNE : aucun secret statistique appliqué (tous les territoires, toutes les cellules, entreprises nommées par leur SIREN) ; document à ne pas diffuser."
            else paste(texte_regle_secret(regles), "Un territoire qui ne respecte pas cette règle n’a pas de fiche."),
            source_note),
    '</section>',
    '<script>',
    '(function(){var q=document.getElementById("q"),l=document.getElementById("liste"),v=document.getElementById("vide");if(!q)return;',
    'function n(s){return s.toLowerCase().normalize("NFD").replace(/[\\u0300-\\u036f]/g,"")}',
    'q.addEventListener("input",function(){var t=n(q.value.trim()),k=0;Array.prototype.forEach.call(l.children,function(li){',
    'var ok=!t||li.getAttribute("data-cle").indexOf(t)>=0;li.style.display=ok?"":"none";if(ok)k++});v.style.display=k?"none":"block"})})();',
    '</script>',
    '</div></body></html>')
}

# --- Boucle de génération -----------------------------------------------------
generer_fiches <- function(base, dir, mode = "tous", selection = NULL,
                           regles = regles_secret(), age_senior = AGE_SENIOR,
                           zonage = zonage_geo(GEO_ANALYSE), seuil_proche = NULL,
                           source_note = "données : table test", index = TRUE,
                           prefixe = "09", age_min = AGE_MIN_BTS,
                           annexe = isTRUE(get0("FICHES_ANNEXE", ifnotfound = FALSE)),
                           stock = NULL, detail_entreprises = FALSE, ref_siren = NULL) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  contexte <- calculer_contexte_perimetre(base, age_senior)
  sel <- selectionner_territoires(base, mode, selection, regles)
  journal <- lapply(seq_len(nrow(sel$retenus)), function(i) {
    code <- sel$retenus$code[i]
    ind  <- calculer_indicateurs_territoire(base, code, contexte, regles, age_senior,
                                            seuil_proche = seuil_proche, age_min = age_min, stock = stock,
                                            detail_entreprises = detail_entreprises, ref_siren = ref_siren)
    fichier <- paste0(slug_fiche(ind$code, ind$nom), ".html")
    writeLines(generer_html_fiche(ind, contexte, zonage, seuil_proche, source_note, annexe = annexe),
               file.path(dir, fichier), useBytes = TRUE)
    tibble::tibble(code = ind$code, nom = ind$nom, fichier = fichier, statut = "ok",
                   effectif = ind$population$n_champ, effectif55 = ind$population$n55,
                   departs = ind$departs$central, motif = NA_character_)
  }) |> bind_rows()
  if (nrow(sel$ecartes) > 0)
    journal <- bind_rows(journal, sel$ecartes |>
                           transmute(code, nom = NA_character_, fichier = NA_character_,
                                     statut = "écarté", effectif = NA_integer_,
                                     effectif55 = NA_integer_, departs = NA_real_, motif))
  if (index) writeLines(generer_html_index(journal, zonage, age_min, regles, contexte$n_territoires, source_note),
                        file.path(dir, "index.html"), useBytes = TRUE)
  message(prefixe, " : ", sum(journal$statut == "ok"), " fiche(s) écrite(s) dans ", dir,
          if (any(journal$statut != "ok")) paste0(" — ", sum(journal$statut != "ok"),
                                                  " territoire(s) écarté(s) : ",
                                                  paste(sprintf("%s (%s)", journal$code[journal$statut != "ok"],
                                                                journal$motif[journal$statut != "ok"]), collapse = " ; ")) else "")
  invisible(journal)
}
