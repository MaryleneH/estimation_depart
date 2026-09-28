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
#           phrases_a_retenir() / points_attention()   règles déterministes
#           generer_html_fiche()             HTML/CSS autonome (barres en CSS)
#           selectionner_territoires() -> generer_fiches()   boucle + journal
#
# Dépendances : R de base, dplyr, tibble. Ni ggplot2, ni gt, ni navigateur.
# Secret statistique : cellule < seuil -> « n.d. » (chiffre ET barre) ;
#   suppression secondaire quand une seule cellule d'un bloc est masquée.
# ==============================================================================
library(dplyr)

# --- Formats FR (espace insécable, virgule) -----------------------------------
fmt_n   <- function(x) ifelse(is.finite(x), formatC(round(x), format = "d", big.mark = " "), "n.d.")
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

# --- Suppression secondaire ---------------------------------------------------
# Si UNE seule cellule d'un bloc est sous le seuil alors que le total est
# affiché, elle serait déductible par différence : on masque aussi la plus
# petite cellule restante (règle standard de la statistique publique).
masquer_cellules <- function(n, seuil) {
  masque <- n < seuil
  if (sum(masque) == 1 && sum(!masque) > 1) {
    cand <- which(!masque)
    masque[cand[which.min(n[cand])]] <- TRUE
  }
  masque
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
  list(n_territoires = nrow(par_territoire),
       med_part = median(par_territoire$part_55plus_pct, na.rm = TRUE),
       med_taux = median(par_territoire$taux_depart_55plus_pct, na.rm = TRUE),
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
                                            seuil = SEUIL_DIFFUSION,
                                            age_senior = AGE_SENIOR,
                                            breaks = BREAKS_TRANCHES,
                                            labels = LABELS_TRANCHES,
                                            seuil_proche = NULL,
                                            age_min = AGE_MIN_BTS) {
  d <- base |> filter(geo_code == code)
  if (nrow(d) == 0) stop("Territoire '", code, "' absent des données.")
  d <- d |> mutate(senior = age_2024 >= age_senior,
                   tranche = cut(age_2024, breaks = breaks, labels = labels)) |>
    ajouter_parts_causes()
  n_champ <- nrow(d); n55 <- sum(d$senior)

  # Âges (stock) — masquage + suppression secondaire. Une tranche est « senior »
  # si sa borne inférieure (borne cut exclue + 1) atteint age_senior.
  bornes_inf <- breaks[-length(breaks)] + 1
  ages <- d |> count(tranche, name = "n", .drop = FALSE) |>
    mutate(tranche = as.character(tranche), part = 100 * n / n_champ,
           senior = (bornes_inf >= age_senior)[match(tranche, labels)],
           masque = masquer_cellules(n, seuil))

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

  # Catégories sociales — masquage sur l'effectif de la cellule (règle du 08)
  cs <- d |> group_by(cs1) |>
    summarise(n = n(), n55 = sum(senior), part55 = 100 * mean(senior),
              departs = sum(p_central), departs55 = sum(p_central[senior]), .groups = "drop") |>
    mutate(masque = masquer_cellules(n, seuil)) |>
    arrange(masque, desc(n55))
  cs_masquee <- any(cs$masque)

  # Position dans le périmètre
  part55 <- 100 * n55 / n_champ
  pos <- list(part = position_mediane(part55, contexte$med_part, seuil_proche),
              taux = position_mediane(dep$taux_seniors, contexte$med_taux, seuil_proche))

  n_ent <- n_distinct(d$siren)
  list(code = code, nom = as.character(d$geo_nom[1]), geo_type = as.character(d$geo_type[1]),
       diffusable = n_champ >= seuil, seuil = seuil, age_senior = age_senior, age_min = age_min,
       population = list(n_champ = n_champ, n55 = n55, part55 = part55,
                         n_entreprises = if (n_ent >= 3) n_ent else NA_integer_),
       ages = ages, departs = dep, causes = causes, cs = cs, cs_masquee = cs_masquee,
       position = pos)
}

# --- Textes automatiques : règles déterministes, descriptives ----------------
phrases_a_retenir <- function(ind) {
  p <- ind$population; dep <- ind$departs
  ph <- c(
    sprintf("Les %d ans et plus représentent %s des salariés de %d ans et plus.",
            ind$age_senior, fmt_pct(p$part55), ind$age_min),
    sprintf("Environ %s départs sont attendus d'ici 2030 dans le scénario central (fourchette %s – %s).",
            fmt_n(dep$central), fmt_n(dep$bas), fmt_n(dep$haut)))
  # CS la plus concernée : seulement si aucune CS masquée ET écart net (> 10 %)
  # avec la deuxième — sinon la primauté n'est pas certaine, on se tait.
  if (!ind$cs_masquee && nrow(ind$cs) >= 2) {
    o <- ind$cs |> arrange(desc(departs))
    if (o$departs[2] > 0 && (o$departs[1] - o$departs[2]) / o$departs[2] > 0.10)
      ph <- c(ph, sprintf("%s sont la catégorie la plus concernée en volume (%s départs attendus).",
                          cap1(libelle_cs(o$cs1[1])), fmt_n(o$departs[1])))
  }
  ph
}
cap1 <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))

points_attention <- function(ind, contexte) {
  pts <- character(0)
  if (ind$position$part$classe == "dessus")
    pts <- c(pts, sprintf("Part des %d ans et + supérieure à la médiane du périmètre (%s).",
                          ind$age_senior, fmt_pts(ind$position$part$ecart)))
  if (ind$position$taux$classe == "dessus")
    pts <- c(pts, sprintf("Taux de départ des %d ans et + supérieur à la médiane du périmètre (%s).",
                          ind$age_senior, fmt_pts(ind$position$taux$ecart)))
  if (!ind$cs_masquee && nrow(ind$cs) >= 3 && ind$departs$central > 0) {
    o <- ind$cs |> arrange(desc(departs))
    part2 <- 100 * sum(o$departs[1:2]) / ind$departs$central
    if (part2 >= 60)
      pts <- c(pts, sprintf("Deux catégories (%s, %s) concentrent %s des départs attendus.",
                            libelle_cs(o$cs1[1], TRUE), libelle_cs(o$cs1[2], TRUE), fmt_pct(part2)))
  }
  a61 <- ind$ages |> filter(!masque)
  derniere <- a61 |> filter(tranche == tail(tranches_de(ind), 1))
  if (nrow(derniere) == 1 && derniere$part >= 15)
    pts <- c(pts, sprintf("%s des salariés ont %s : départs très rapprochés dans le temps.",
                          fmt_pct(derniere$part), derniere$tranche))
  head(pts, 3)
}
# dernière tranche d'âge disponible dans `ind` (évite de dépendre d'une globale)
tranches_de <- function(ind) ind$ages$tranche

# --- Briques HTML/CSS ---------------------------------------------------------
# Une barre = un <div> à largeur proportionnelle ; une cellule masquée n'a PAS
# de barre (rien à mesurer) et affiche « n.d. ».
html_ligne_barre <- function(libelle, pct_largeur, valeur, classe = "b", masque = FALSE,
                             note = "effectif sous seuil de diffusion") {
  if (masque)
    return(sprintf('<div class="ligne"><span class="lib">%s</span><span class="piste nd">n.d. — %s</span><span class="val">n.d.</span></div>',
                   echap_html(libelle), note))
  sprintf('<div class="ligne"><span class="lib">%s</span><span class="piste"><span class="%s" style="width:%.1f%%"></span></span><span class="val">%s</span></div>',
          echap_html(libelle), classe, max(0, min(100, pct_largeur)), valeur)
}

# --- La fiche HTML ------------------------------------------------------------
generer_html_fiche <- function(ind, contexte, zonage, seuil_proche = NULL,
                               source_note = "données : table test") {
  p <- ind$population; dep <- ind$departs; s <- ind$age_senior; a <- ind$age_min
  nom <- echap_html(ind$nom); code <- echap_html(ind$code)

  # Bloc 2 — âges : largeur relative à la tranche la plus peuplée
  max_age <- max(ind$ages$part[!ind$ages$masque], 1)
  bloc_ages <- paste(mapply(function(tr, part, sen, masq)
    html_ligne_barre(tr, 100 * part / max_age, fmt_pct(part),
                     classe = if (sen) "b senior" else "b", masque = masq),
    ind$ages$tranche, ind$ages$part, ind$ages$senior, ind$ages$masque), collapse = "\n")

  # Bloc 3 — CS : barre = effectif 55+ (stock), chiffre à droite = départs (flux)
  max_cs <- max(ind$cs$n55[!ind$cs$masque], 1)
  bloc_cs <- paste(mapply(function(cs1, n55, departs, masq)
    html_ligne_barre(libelle_cs(cs1, TRUE), 100 * n55 / max_cs,
                     if (masq) "n.d." else sprintf('%s <small>salariés de %d+</small> · <b>%s</b> <small>départs</small>',
                                                   fmt_n(n55), s, fmt_n(departs)),
                     classe = "b senior", masque = masq),
    ind$cs$cs1, ind$cs$n55, ind$cs$departs, ind$cs$masque), collapse = "\n")

  # Bloc 4 — causes : une barre empilée 100 %
  c3 <- ind$causes$pct
  bloc_causes <- sprintf(paste0(
    '<div class="empile"><span class="c1" style="width:%.1f%%"></span>',
    '<span class="c2" style="width:%.1f%%"></span><span class="c3" style="width:%.1f%%"></span></div>',
    '<div class="legende"><span><i class="c1"></i>Retraite / fin de carrière <b>%s</b></span>',
    '<span><i class="c2"></i>Invalidité <b>%s</b></span><span><i class="c3"></i>Décès <b>%s</b></span></div>'),
    c3[1], c3[2], c3[3], fmt_pct(c3[1]), fmt_pct(c3[2]), fmt_pct(c3[3]))

  # Bloc 5 — position : deux paires de barres (territoire / médiane), même échelle
  paire <- function(titre, x, med, pos, max_ech) {
    paste0(
      sprintf('<div class="paire"><div class="paire-titre">%s <span class="pos %s">%s%s</span></div>',
              titre, pos$classe, pos$libelle,
              if (is.finite(pos$ecart)) paste0(" (", fmt_pts(pos$ecart), ")") else ""),
      html_ligne_barre(nom, 100 * x / max_ech, fmt_pct(x), "b senior", masque = !is.finite(x),
                       note = "aucun salarié de 55 ans et +"),
      html_ligne_barre("Médiane du périmètre", 100 * med / max_ech, fmt_pct(med), "b ref"),
      "</div>")
  }
  ech_part <- max(p$part55, contexte$med_part, 1, na.rm = TRUE)
  ech_taux <- max(dep$taux_seniors, contexte$med_taux, 1, na.rm = TRUE)
  bloc_pos <- paste0(
    paire(sprintf("Part des %d ans et + (aujourd'hui)", s), p$part55, contexte$med_part,
          ind$position$part, ech_part),
    paire(sprintf("Taux de départ des %d ans et + (d'ici 2030)", s), dep$taux_seniors,
          contexte$med_taux, ind$position$taux, ech_taux))

  retenir <- phrases_a_retenir(ind)
  attention <- points_attention(ind, contexte)
  li <- function(v) if (length(v) == 0) "<li class=\"muet\">Aucun point d'attention au regard des règles retenues.</li>" else
    paste0("<li>", echap_html(v), "</li>", collapse = "\n")

  regle_proche <- if (!is.null(seuil_proche))
    sprintf(" « Proche de la médiane » : écart inférieur ou égal à %s points.", format(seuil_proche)) else ""

  c('<!DOCTYPE html>', '<html lang="fr"><head><meta charset="utf-8">',
    sprintf('<title>%s (%s) — Chiffres clés</title>', nom, code),
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<style>',
    ':root { --encre:#111827; --gris:#4b5563; --gris-clair:#e5e7eb; --bleu:#1e3a5f; --bleu-clair:#b8c9dc; --ref:#9aa5b1; --ambre:#b45309; }',
    'body { margin:0; background:#fff; color:var(--encre); font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif; font-size:14px; line-height:1.35; }',
    '.page { max-width:860px; margin:0 auto; padding:28px 32px 20px; }',
    'header { border-bottom:3px solid var(--bleu); padding-bottom:10px; margin-bottom:18px; display:flex; justify-content:space-between; align-items:flex-end; gap:16px; }',
    'header h1 { margin:0; font-size:26px; letter-spacing:-.01em; } header h1 small { font-weight:400; color:var(--gris); font-size:15px; margin-left:8px; }',
    'header .sous { color:var(--gris); margin-top:4px; font-size:14px; } header .meta { text-align:right; color:var(--gris); font-size:12px; line-height:1.4; }',
    'h2 { font-size:12px; letter-spacing:.08em; text-transform:uppercase; color:var(--bleu); margin:22px 0 8px; padding-top:12px; border-top:1px solid var(--gris-clair); }',
    'h2 small { text-transform:none; letter-spacing:0; color:var(--gris); font-weight:400; margin-left:8px; }',
    '.kpis { display:grid; grid-template-columns:1fr 1fr 1.35fr; gap:14px; }',
    '.groupe { grid-column:span 1; } .groupe-titre { font-size:11px; letter-spacing:.08em; text-transform:uppercase; color:var(--gris); margin-bottom:6px; }',
    '.stock { grid-column:1 / span 2; display:grid; grid-template-columns:1fr 1fr; gap:14px; } .stock .groupe-titre { grid-column:1 / span 2; }',
    '.flux .groupe-titre { color:var(--bleu); }',
    '.kpi { border:1px solid var(--gris-clair); border-radius:8px; padding:12px 14px; min-height:86px; }',
    '.kpi .n { font-size:30px; font-weight:700; line-height:1.05; font-variant-numeric:tabular-nums; } .kpi .l { color:var(--gris); margin-top:3px; }',
    '.kpi .d { color:var(--gris); font-size:12px; margin-top:6px; } .kpi.flux { border-color:var(--bleu); background:#f5f8fb; } .kpi.flux .n { color:var(--bleu); }',
    '.ligne { display:grid; grid-template-columns:170px 1fr 220px; gap:10px; align-items:center; margin:5px 0; }',
    '.lib { color:var(--encre); } .val { text-align:right; font-variant-numeric:tabular-nums; } .val small, .kpi small { color:var(--gris); font-size:11px; }',
    '.piste { height:14px; background:#f1f3f6; border-radius:3px; overflow:hidden; } .piste.nd { background:none; color:var(--gris); font-size:12px; font-style:italic; height:auto; }',
    '.piste .b { display:block; height:100%; background:var(--bleu-clair); } .piste .senior { background:var(--bleu); } .piste .ref { background:var(--ref); }',
    '.empile { display:flex; height:22px; border-radius:4px; overflow:hidden; margin-top:6px; } .empile span { display:block; height:100%; }',
    '.c1 { background:var(--bleu); } .c2 { background:#7f9bb8; } .c3 { background:#c9d3de; }',
    '.legende { display:flex; gap:22px; margin-top:8px; color:var(--gris); font-size:13px; flex-wrap:wrap; } .legende i { display:inline-block; width:10px; height:10px; border-radius:2px; margin-right:6px; vertical-align:-1px; }',
    '.paire { margin:8px 0 12px; } .paire-titre { font-weight:600; margin-bottom:2px; }',
    '.pos { font-weight:400; color:var(--gris); margin-left:8px; font-size:13px; } .pos.dessus { color:var(--ambre); }',
    '.deux { display:grid; grid-template-columns:1fr 1fr; gap:24px; } .deux ul { margin:6px 0 0; padding-left:18px; } .deux li { margin:4px 0; } .muet { color:var(--gris); list-style:none; margin-left:-18px; }',
    'footer { margin-top:20px; padding-top:10px; border-top:1px solid var(--gris-clair); color:var(--gris); font-size:11px; line-height:1.45; }',
    '@page { size:A4; margin:12mm; } @media print { body { font-size:12.5px; } .page { padding:0; max-width:none; } h2, .kpis, .paire, .deux { break-inside:avoid; } .kpi .n { font-size:26px; } }',
    '</style></head><body><div class="page">',
    # ---- En-tête
    '<header><div>',
    sprintf('<h1>%s <small>%s %s</small></h1>', nom, echap_html(zonage$libelle), code),
    '<div class="sous">Chiffres clés — départs attendus à l’horizon 2030</div></div>',
    sprintf('<div class="meta">Salariés de %d ans et + en 2024<br>Périmètre BITD · scénario central</div></header>', a),
    # ---- Bloc 1 : combien ?
    '<div class="kpis">',
    '<div class="stock"><div class="groupe-titre">Aujourd’hui (2024)</div>',
    sprintf('<div class="kpi"><div class="n">%s</div><div class="l">salariés de %d ans et +</div>%s</div>',
            fmt_n(p$n_champ), a, if (is.finite(p$n_entreprises)) sprintf('<div class="d">dans %s entreprises du périmètre</div>', fmt_n(p$n_entreprises)) else ""),
    sprintf('<div class="kpi"><div class="n">%s</div><div class="l">salariés de %d ans et +</div><div class="d"><b>%s</b> des %d ans et +</div></div>',
            fmt_n(p$n55), s, fmt_pct(p$part55), a),
    '</div>',
    '<div class="groupe flux"><div class="groupe-titre">D’ici 2030</div>',
    sprintf('<div class="kpi flux"><div class="n">%s</div><div class="l">départs attendus (sorties définitives de l’emploi)</div><div class="d">fourchette %s – %s<br>dont <b>%s</b> parmi les %d ans et + (%s d’entre eux)</div></div>',
            fmt_n(dep$central), fmt_n(dep$bas), fmt_n(dep$haut), fmt_n(dep$seniors_central), s, fmt_pct(dep$taux_seniors)),
    '</div></div>',
    # ---- Bloc 2 : âges
    sprintf('<h2>Quel âge ont-ils ? <small>part des salariés de %d ans et + · en bleu foncé : %d ans et +</small></h2>', a, s),
    bloc_ages,
    # ---- Bloc 3 : CS
    sprintf('<h2>Quelles catégories sont les plus concernées ? <small>barre : salariés de %d ans et + (aujourd’hui) · chiffre : départs attendus d’ici 2030</small></h2>', s),
    bloc_cs,
    # ---- Bloc 4 : causes
    sprintf('<h2>D’où viendraient les départs ? <small>répartition des %s départs attendus</small></h2>', fmt_n(dep$central)),
    bloc_causes,
    # ---- Bloc 5 : position
    sprintf('<h2>Comment se situe %s dans le périmètre ? <small>%d %s comparés</small></h2>',
            nom, contexte$n_territoires, echap_html(zonage$pluriel)),
    bloc_pos,
    # ---- Bloc 6 : textes
    '<div class="deux"><div><h2>À retenir</h2><ul>', li(retenir), '</ul></div>',
    '<div><h2>Points d’attention</h2><ul>', li(attention), '</ul></div></div>',
    # ---- Pied
    '<footer>',
    sprintf(paste0('Champ : salariés de %d ans et + en 2024 des entreprises du périmètre BITD, établissements situés dans le territoire. ',
                   'Départs = sorties définitives de l’emploi d’ici 2030 (retraite ou fin de carrière, invalidité, décès) ; ',
                   'les mobilités vers d’autres employeurs ne sont pas comptées : les volumes sont un plancher. ',
                   'Scénario central ; fourchette = hypothèses réglementaires basse et haute. ',
                   'Secret statistique : cellules de moins de %d salariés non diffusées (n.d.), avec suppression secondaire. ',
                   'Position : comparaison à la médiane des %d %s du périmètre analysé.%s ',
                   'Points d’attention : règles fixes (au-dessus de la médiane ; deux catégories ≥ 60 %% des départs ; dernière tranche d’âge ≥ 15 %%). ',
                   'Sources : BTS 2024, DREES, EACR invalidité, mortalité Insee — calculs propres · %s.'),
            a, ind$seuil, contexte$n_territoires, echap_html(zonage$pluriel), regle_proche, source_note),
    '</footer></div></body></html>')
}

# --- Sélection des territoires : tous / sélection, avec contrôles ------------
selectionner_territoires <- function(base, mode = "tous", selection = NULL,
                                     seuil = SEUIL_DIFFUSION) {
  if (!mode %in% c("tous", "selection"))
    stop("FICHES_MODE doit valoir \"tous\" ou \"selection\" (reçu : ", mode, ").")
  dispo <- base |> count(geo_code, geo_nom, name = "n") |> arrange(geo_code)
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
  sous <- dispo |> filter(geo_code != "inconnu", n < seuil)
  if (nrow(sous) > 0)
    ecartes <- bind_rows(ecartes, tibble::tibble(code = sous$geo_code,
                                                 motif = sprintf("effectif %d < seuil de diffusion %d", sous$n, seuil)))
  retenus <- dispo |> filter(geo_code != "inconnu", n >= seuil) |> select(code = geo_code, nom = geo_nom, n)
  if (nrow(retenus) == 0)
    stop("Fiches : aucun territoire diffusable (", nrow(ecartes), " écarté(s)).")
  list(retenus = retenus, ecartes = ecartes)
}

# --- Page d'index -------------------------------------------------------------
generer_html_index <- function(journal, zonage, age_min = AGE_MIN_BTS) {
  j <- journal |> filter(statut == "ok") |> arrange(desc(departs))
  lignes <- sprintf('<tr><td><a href="%s">%s</a></td><td>%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td></tr>',
                    j$fichier, echap_html(j$nom), echap_html(j$code), fmt_n(j$effectif), fmt_n(j$effectif55), fmt_n(j$departs))
  c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">',
    sprintf('<title>Fiches chiffres clés — %s</title>', echap_html(zonage$pluriel)),
    '<style>body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;color:#111827;margin:2em;} table{border-collapse:collapse;font-size:14px;} th{text-align:left;padding:.4em .8em;border-bottom:2px solid #1e3a5f;} td{padding:.35em .8em;border-bottom:1px solid #e5e7eb;} .num,th.num{text-align:right;font-variant-numeric:tabular-nums;} a{color:#1e3a5f;}</style></head><body>',
    sprintf('<h1>Fiches chiffres clés — %s</h1><p>%d fiches · départs attendus à l’horizon 2030 · classement par départs décroissants</p>',
            echap_html(zonage$pluriel), nrow(j)),
    sprintf('<table><thead><tr><th>%s</th><th>Code</th><th class="num">Salariés %d+</th><th class="num">Salariés 55+</th><th class="num">Départs attendus</th></tr></thead><tbody>',
            echap_html(zonage$libelle), age_min),
    lignes, '</tbody></table></body></html>')
}

# --- Boucle de génération -----------------------------------------------------
generer_fiches <- function(base, dir, mode = "tous", selection = NULL,
                           seuil = SEUIL_DIFFUSION, age_senior = AGE_SENIOR,
                           zonage = zonage_geo(GEO_ANALYSE), seuil_proche = NULL,
                           source_note = "données : table test", index = TRUE,
                           prefixe = "09", age_min = AGE_MIN_BTS) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  contexte <- calculer_contexte_perimetre(base, age_senior)
  sel <- selectionner_territoires(base, mode, selection, seuil)
  journal <- lapply(seq_len(nrow(sel$retenus)), function(i) {
    code <- sel$retenus$code[i]
    ind  <- calculer_indicateurs_territoire(base, code, contexte, seuil, age_senior,
                                            seuil_proche = seuil_proche, age_min = age_min)
    fichier <- paste0(slug_fiche(ind$code, ind$nom), ".html")
    writeLines(generer_html_fiche(ind, contexte, zonage, seuil_proche, source_note),
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
  if (index) writeLines(generer_html_index(journal, zonage, age_min), file.path(dir, "index.html"), useBytes = TRUE)
  message(prefixe, " : ", sum(journal$statut == "ok"), " fiche(s) écrite(s) dans ", dir,
          if (any(journal$statut != "ok")) paste0(" — ", sum(journal$statut != "ok"),
                                                  " territoire(s) écarté(s) : ",
                                                  paste(sprintf("%s (%s)", journal$code[journal$statut != "ok"],
                                                                journal$motif[journal$statut != "ok"]), collapse = " ; ")) else "")
  invisible(journal)
}
