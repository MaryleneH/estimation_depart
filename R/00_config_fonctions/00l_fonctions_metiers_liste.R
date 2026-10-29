# ==============================================================================
# 00l_fonctions_metiers_liste.R — Les métiers de la liste, territoire par territoire
# ------------------------------------------------------------------------------
# QUESTION  : pour un métier de la liste (classeur PCS -> FAP, 08i), quelle part de ses
#             salariés est susceptible de partir d'ici 2030 dans chaque région et chaque
#             département, et cette part est-elle plus forte, proche ou plus faible qu'en
#             France ? Et, vu d'un territoire, quels métiers de la liste partent le plus ?
# ENTRÉES   : tension_metiers() (00k : liste, libellés, famille, tension, levier) et les
#             tables de DIFFUSION du 08c (département x PCS, région x PCS, France x PCS :
#             secret appliqué, colonne masque). Rien n'est recalculé : les départs et la
#             part susceptible de partir sont ceux du 08c, déjà arrondis par 00g à l'écriture.
# SORTIES   : table_metiers_territoires() (une ligne par métier x territoire),
#             generer_fiche_metiers_liste() (page HTML sans script : un bloc par métier,
#             puis une vue par territoire), tables_classeur_metiers_liste() (classeur
#             Excel : table longue + vue par territoire ; écrit par ecrire_classeur_tension()).
# LECTURE   : écart = part du territoire - part France, en points ; « plus forte qu'en
#             France » à partir de TENSION_ECART_POINTS, « plus faible » en deçà de son
#             opposé, « proche de la France » entre les deux (00_config, source unique).
# SECRET    : une cellule masquée (territoire x métier, ou France x métier) n'a aucune valeur
#             et porte la mention du secret ; une page ne contient jamais une valeur masquée.
# LIBELLÉ   : « métiers stratégiques » du code -> TENSION_LIBELLE_LISTE (texte_liste, 00k).
# ==============================================================================
library(dplyr)

# --- Lecture de l'écart à la France (SPEC-TYPO-109) -------------------------------------
lecture_ecart_liste <- function(ecart, seuil = parametres_tension()$ecart_pts) {
  if (!is.numeric(seuil) || length(seuil) != 1 || is.na(seuil) || seuil <= 0) stop("lecture_ecart_liste : seuil (points) > 0 requis.")
  ifelse(is.na(ecart), NA_character_, ifelse(ecart >= seuil, "plus forte qu’en France", ifelse(ecart <= -seuil, "plus faible qu’en France", "proche de la France")))
}

# --- Table métier x territoire (régions puis départements), secret du 08c conservé ------------
# metiers : tension_metiers() ; dif_dep / dif_reg / dif_fr : tables de DIFFUSION du 08c.
table_metiers_territoires <- function(metiers, dif_dep, dif_reg = NULL, dif_fr, params = parametres_tension()) {
  for (nom in c("dif_dep", "dif_fr")) if (!"masque" %in% names(get(nom))) stop("table_metiers_territoires : table '", nom, "' sans colonne masque — seule une table de DIFFUSION est admise.")
  if (!is.null(dif_reg) && !"masque" %in% names(dif_reg)) stop("table_metiers_territoires : table 'dif_reg' sans colonne masque — seule une table de DIFFUSION est admise.")
  requis <- c("pcs", "effectif_champ", "departs_central", "departs_bas", "departs_haut", "taux_depart_central_pct", "masque")
  for (nom in c("dif_dep", "dif_fr")) { manque <- setdiff(requis, names(get(nom))); if (length(manque)) stop("table_metiers_territoires : colonnes absentes de ", nom, " : ", paste(manque, collapse = ", ")) }
  m <- tibble::tibble(pcs_n = metiers$pcs, pcs = if ("pcs_source" %in% names(metiers)) as.character(metiers$pcs_source) else as.character(metiers$pcs),
                      pcs_libelle = as.character(metiers$pcs_libelle), fap_code = as.character(metiers$fap_code), fap_libelle = as.character(metiers$fap_libelle),
                      en_tension = metiers$en_tension, levier = as.character(metiers$levier), ordre_liste = seq_len(nrow(metiers)))
  fr <- dif_fr |> mutate(pcs_n = normaliser_code_pcs(pcs)) |>
    transmute(pcs_n, masque_fr = masque %in% TRUE, taux_france_pct = ifelse(masque %in% TRUE, NA_real_, taux_depart_central_pct),
              effectif_france = ifelse(masque %in% TRUE, NA_integer_, as.integer(effectif_champ)), departs_france = ifelse(masque %in% TRUE, NA_real_, departs_central),
              departs_france_bas = ifelse(masque %in% TRUE, NA_real_, departs_bas), departs_france_haut = ifelse(masque %in% TRUE, NA_real_, departs_haut))
  niveau_de <- function(d, niveau, code, nom) {
    if (is.null(d)) return(NULL)
    if (!all(c(code, nom) %in% names(d))) stop("table_metiers_territoires : colonnes ", code, " / ", nom, " absentes de la table ", niveau, ".")
    d |> mutate(pcs_n = normaliser_code_pcs(pcs), niveau = niveau, geo_code = as.character(.data[[code]]), geo_nom = as.character(.data[[nom]])) |>
      select(pcs_n, niveau, geo_code, geo_nom, masque, effectif_champ, departs_central, departs_bas, departs_haut, taux_pct = taux_depart_central_pct)
  }
  t <- bind_rows(niveau_de(dif_reg, "région", "region_code", "region_nom"), niveau_de(dif_dep, "département", "geo_code", "geo_nom")) |>
    filter(!geo_code %in% c("inconnu", ""))
  out <- m |> inner_join(t, by = "pcs_n", relationship = "one-to-many") |> left_join(fr, by = "pcs_n") |>
    mutate(diffuse = !(masque %in% TRUE), masque_fr = masque_fr %in% TRUE,
           ecart_pts = ifelse(diffuse & !masque_fr, taux_pct - taux_france_pct, NA_real_),
           lecture = ifelse(!diffuse, "non diffusé (secret statistique)", ifelse(masque_fr, "repère France non diffusé", lecture_ecart_liste(ecart_pts, params$ecart_pts))))
  for (v in c("effectif_champ", "departs_central", "departs_bas", "departs_haut", "taux_pct")) out[[v]][!out$diffuse] <- NA
  out$effectif_champ <- as.integer(out$effectif_champ)
  out |> arrange(ordre_liste, niveau == "département", desc(ifelse(is.na(taux_pct), -Inf, taux_pct)), geo_code) |>
    select(ordre_liste, pcs, pcs_libelle, fap_code, fap_libelle, en_tension, levier, niveau, geo_code, geo_nom, diffuse,
           effectif_champ, departs_central, departs_bas, departs_haut, taux_pct, taux_france_pct, ecart_pts, lecture,
           effectif_france, departs_france, departs_france_bas, departs_france_haut)
}

# --- Mise en forme commune ---------------------------------------------------------------
.e_liste <- function(x) { x <- gsub("&", "&amp;", as.character(x), fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
.f0_liste <- function(x) ifelse(is.na(x), "n.d.", formatC(arrondir_nombre_personnes(x), format = "d", big.mark = " "))
.f1_liste <- function(x) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = 1, decimal.mark = ","))
.fpts_liste <- function(x) ifelse(is.na(x), "n.d.", paste0(ifelse(x > 0, "+", ifelse(x < 0, "−", "±")), formatC(abs(x), format = "f", digits = 1, decimal.mark = ","), " pt", ifelse(abs(x) >= 2, "s", "")))
.lib_metier <- function(pcs, libelle) ifelse(is.na(libelle) | libelle == "", paste0("PCS ", pcs), libelle)
.oui_non_na <- function(x, inconnu = "inconnue") ifelse(is.na(x), inconnu, ifelse(x, "oui", "non"))
.nom_court <- function(nom, code) paste0(nom, " (", code, ")")

# Tableau des territoires d'un métier (régions puis départements) ; les non diffusés d'un niveau tiennent sur une ligne
html_tableau_territoires_metier <- function(tm, du_terr = "du territoire") {
  lig <- function(r) sprintf('<tr><td>%s</td><td class="num">%s</td><td class="num">%s<br><small>%s à %s</small></td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td>%s</td></tr>',
                             .e_liste(.nom_court(r$geo_nom, r$geo_code)), .f0_liste(r$effectif_champ), .f0_liste(r$departs_central), .f0_liste(r$departs_bas), .f0_liste(r$departs_haut),
                             .f1_liste(r$taux_pct), .f1_liste(r$taux_france_pct), .fpts_liste(r$ecart_pts), .e_liste(r$lecture))
  bloc <- function(niveau, titre) {
    x <- tm[tm$niveau == niveau, ]; if (nrow(x) == 0) return("")
    vis <- x[x$diffuse, ]; nd <- x[!x$diffuse, ]
    c(sprintf('<tr class="grp"><th colspan="7">%s</th></tr>', titre),
      if (nrow(vis)) vapply(seq_len(nrow(vis)), function(i) lig(vis[i, ]), "") else '<tr class="nd"><td colspan="7">Aucun territoire diffusable à ce niveau pour ce métier (secret statistique).</td></tr>',
      if (nrow(nd)) sprintf('<tr class="nd"><td colspan="7">Non diffusés (secret statistique), %d : %s</td></tr>', nrow(nd), .e_liste(paste(.nom_court(nd$geo_nom, nd$geo_code), collapse = ", "))) else "")
  }
  c('<div class="defile"><table class="tab-liste"><colgroup><col style="width:22%"><col style="width:11%"><col style="width:15%"><col style="width:12%"><col style="width:10%"><col style="width:10%"><col style="width:20%"></colgroup>',
    '<thead><tr><th>Territoire</th><th class="num">Salariés du métier</th><th class="num">Départs attendus d’ici 2030<br><small>estimation centrale ; basse à haute</small></th><th class="num">Part susceptible de partir (%)</th><th class="num">France (%)</th><th class="num">Écart</th><th>Lecture</th></tr></thead><tbody>',
    bloc("région", "Régions"), bloc("département", "Départements"), '</tbody></table></div>')
}

# Tableau des métiers d'un territoire (vue inverse)
html_tableau_metiers_territoire <- function(tt) {
  vis <- tt[tt$diffuse, ]; vis <- vis[order(-ifelse(is.na(vis$ecart_pts), -Inf, vis$ecart_pts), vis$ordre_liste), ]; nd <- tt[!tt$diffuse, ]
  lig <- function(r) sprintf('<tr><td>%s<br><small>PCS %s · %s</small></td><td>%s</td><td class="num">%s</td><td class="num">%s<br><small>%s à %s</small></td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td>%s</td></tr>',
                             .e_liste(.lib_metier(r$pcs, r$pcs_libelle)), .e_liste(r$pcs), .e_liste(ifelse(is.na(r$fap_libelle), "", r$fap_libelle)), .oui_non_na(r$en_tension),
                             .f0_liste(r$effectif_champ), .f0_liste(r$departs_central), .f0_liste(r$departs_bas), .f0_liste(r$departs_haut), .f1_liste(r$taux_pct), .f1_liste(r$taux_france_pct), .fpts_liste(r$ecart_pts), .e_liste(r$lecture))
  c('<div class="defile"><table class="tab-liste"><colgroup><col style="width:26%"><col style="width:8%"><col style="width:10%"><col style="width:14%"><col style="width:11%"><col style="width:9%"><col style="width:9%"><col style="width:13%"></colgroup>',
    '<thead><tr><th>Métier</th><th>En tension en France</th><th class="num">Salariés du métier</th><th class="num">Départs attendus d’ici 2030<br><small>estimation centrale ; basse à haute</small></th><th class="num">Part susceptible de partir (%)</th><th class="num">France (%)</th><th class="num">Écart</th><th>Lecture</th></tr></thead><tbody>',
    if (nrow(vis)) vapply(seq_len(nrow(vis)), function(i) lig(vis[i, ]), "") else '<tr class="nd"><td colspan="8">Aucun métier de la liste diffusable dans ce territoire (secret statistique).</td></tr>',
    if (nrow(nd)) sprintf('<tr class="nd"><td colspan="8">Métiers non diffusés (secret statistique), %d : %s</td></tr>', nrow(nd), .e_liste(paste(.lib_metier(nd$pcs, nd$pcs_libelle), collapse = " ; "))) else "",
    '</tbody></table></div>')
}

# --- Page HTML (SPEC-TYPO-109) : un bloc par métier, puis la vue par territoire ; sans script ----
generer_fiche_metiers_liste <- function(tab, metiers, fichier, champ = construire_libelle_champ(), params = parametres_tension(), source_note = "calculs propres") {
  e <- .e_liste; n <- params$n_suivis; seuil <- params$ecart_pts
  liste <- lib_liste(); liste_maj <- paste0(toupper(substr(liste, 1, 1)), substr(liste, 2, nchar(liste)))
  nmot <- if (is.na(n)) paste0("les ", liste) else sprintf("les %d %s", n, liste)
  m <- tibble::tibble(pcs_n = metiers$pcs, pcs = if ("pcs_source" %in% names(metiers)) as.character(metiers$pcs_source) else as.character(metiers$pcs),
                      pcs_libelle = as.character(metiers$pcs_libelle), fap_code = as.character(metiers$fap_code), fap_libelle = as.character(metiers$fap_libelle),
                      en_tension = metiers$en_tension, levier = as.character(metiers$levier), present = if ("present_base" %in% names(metiers)) metiers$present_base %in% TRUE else TRUE)
  anc <- function(pcs) paste0("m-", gsub("[^A-Za-z0-9]", "_", pcs))
  sommaire <- c('<nav class="sommaire"><p class="legend-title">Sommaire</p><ol>',
                sprintf('<li><a href="#%s">%s</a> <small>PCS %s</small></li>', anc(m$pcs), e(.lib_metier(m$pcs, m$pcs_libelle)), e(m$pcs)),
                '</ol><p><a href="#par-territoire">Vue par territoire</a></p></nav>')
  blocs <- unlist(lapply(seq_len(nrow(m)), function(i) { r <- m[i, ]; tm <- tab[tab$pcs == r$pcs, ]
    fr <- if (nrow(tm)) tm[1, ] else NULL
    ligne_fr <- if (!r$present || is.null(fr)) texte_perimetre("France : métier absent des départs de la Base (aucun salarié du champ).") else
      if (is.na(fr$taux_france_pct)) "France : non diffusé (secret statistique)." else
      sprintf("France : %s salariés du métier · %s départs attendus d’ici 2030 (%s à %s) · <b>%s %%</b> susceptibles de partir.", .f0_liste(fr$effectif_france), .f0_liste(fr$departs_france), .f0_liste(fr$departs_france_bas), .f0_liste(fr$departs_france_haut), .f1_liste(fr$taux_france_pct))
    c(sprintf('<section class="metier" id="%s"><h2>%s <small>PCS %s</small></h2>', anc(r$pcs), e(.lib_metier(r$pcs, r$pcs_libelle)), e(r$pcs)),
      sprintf('<p class="reperes">Famille professionnelle : %s%s · En tension en France : <b>%s</b>%s</p>', e(ifelse(is.na(r$fap_libelle), "n.d.", r$fap_libelle)), if (!is.na(r$fap_code)) sprintf(" (FAP %s)", e(r$fap_code)) else "",
              .oui_non_na(r$en_tension), if (!is.na(r$levier) && isTRUE(r$en_tension)) sprintf(" · Levier : %s", e(r$levier)) else ""),
      sprintf('<p class="france">%s</p>', ligne_fr),
      if (nrow(tm)) html_tableau_territoires_metier(tm) else "", '<p class="retour"><a href="#haut">Sommaire</a></p></section>') }))
  terr <- tab |> distinct(niveau, geo_code, geo_nom) |> arrange(niveau == "département", geo_code)
  par_terr <- unlist(lapply(seq_len(nrow(terr)), function(i) { r <- terr[i, ]; tt <- tab[tab$niveau == r$niveau & tab$geo_code == r$geo_code, ]
    c(sprintf('<details class="terr"><summary>%s <small>%s · %d métier%s diffusable%s sur %d</small></summary>', e(.nom_court(r$geo_nom, r$geo_code)), r$niveau, sum(tt$diffuse), if (sum(tt$diffuse) > 1) "s" else "", if (sum(tt$diffuse) > 1) "s" else "", nrow(tt)),
      html_tableau_metiers_territoire(tt), '</details>') }))
  css <- paste('body{margin:0;background:#fff;color:#1F2933;font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;font-size:15px;line-height:1.5}',
               '.page{max-width:1180px;margin:0 auto;padding:28px 32px 40px}h1{font-size:26px;margin:0 0 4px}h2{font-size:19px;margin:30px 0 4px;padding-top:10px;border-top:2px solid #DDE3EA}h2 small{font-size:13px;color:#52606D;font-weight:400;margin-left:8px}',
               '.sous{color:#52606D;margin:0 0 14px}.note{font-size:13.5px;color:#52606D}.reperes,.france{margin:2px 0;font-size:14px}.retour{font-size:12.5px;margin:4px 0 0}',
               '.cadre{border:1px solid #e0b96a;border-left-width:5px;background:#fff8e6;border-radius:8px;padding:10px 14px;margin:10px 0 16px}',
               '.sommaire{columns:2;column-gap:28px;font-size:13.5px;margin:0 0 10px}.sommaire ol{margin:0;padding-left:20px}.sommaire small{color:#52606D}.legend-title{font-size:11.5px;letter-spacing:.04em;text-transform:uppercase;color:#52606D;margin:0 0 4px;column-span:all}',
               '.tab-liste{width:100%;border-collapse:collapse;font-size:13.5px;margin:6px 0;table-layout:fixed}.tab-liste th,.tab-liste td{text-align:left;vertical-align:top;padding:5px 8px;border-top:1px solid #DDE3EA;line-height:1.4;word-wrap:break-word}',
               '.tab-liste thead th{font-size:11.5px;letter-spacing:.04em;text-transform:uppercase;color:#52606D;border-top:0;border-bottom:2px solid #c9ced6}.tab-liste small{display:block;font-size:11.5px;color:#52606D;text-transform:none;letter-spacing:0;font-weight:400}',
               '.tab-liste .num{text-align:right}.tab-liste td.num{white-space:nowrap}.tab-liste tr.grp th{background:#f1f4f8;font-size:12.5px;text-transform:none;letter-spacing:0;color:#1F2933;padding:6px 8px}.tab-liste tr.nd td{color:#52606D;font-style:italic}',
               '.defile{overflow-x:auto}details.terr{border:1px solid #DDE3EA;border-radius:8px;padding:6px 12px;margin:6px 0}details.terr summary{cursor:pointer;font-weight:600}details.terr summary small{font-weight:400;color:#52606D;margin-left:6px}',
               '@media(max-width:800px){.tab-liste{min-width:720px}.page{padding:18px 14px 30px}.sommaire{columns:1}}', sep = "\n")
  P <- construire_libelles_public(champ)
  html <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
            sprintf('<title>%s : où les départs sont-ils les plus forts ?</title><style>', e(liste_maj)), css, '</style></head><body><div class="page" id="haut">',
            sprintf('<h1>%s : où les départs sont-ils les plus forts ?</h1>', e(liste_maj)),
            sprintf(texte_perimetre('<p class="sous">%s · entreprises du périmètre Base · départs attendus d’ici 2030 · %s</p>'), e(P$salaries_age), e(nmot)),
            sprintf('<div class="cadre"><b>Comment lire.</b> Pour chaque métier de la liste : la <b>part susceptible de partir</b> est la part des salariés du métier qui devraient avoir quitté l’emploi d’ici 2030 (estimation centrale). Elle est comparée à la même part en France : <b>plus forte qu’en France</b> à partir de %s points d’écart, <b>plus faible</b> en deçà de %s points, <b>proche</b> entre les deux. Les régions ouvrent chaque tableau : au niveau du département, le secret statistique masque souvent la cellule. Une cellule non diffusée n’a aucune valeur. La tension est celle de la famille professionnelle du métier en France, pas celle du territoire.</div>',
                    .f1_liste(seuil), .f1_liste(-seuil)),
            sommaire, blocs,
            '<h2 id="par-territoire">Vue par territoire</h2><p class="note">Pour chaque région puis chaque département : les métiers de la liste diffusables, classés par écart à la France décroissant ; les métiers non diffusés sont nommés sans valeur.</p>',
            par_terr,
            sprintf('<p class="note">Départs : estimation centrale et fourchette basse – haute (hypothèses d’âge de départ). Secret statistique : règle Insee (salariés, entreprises, dominance) puis secret secondaire, appliqués par le 08c ; un territoire ou un métier non diffusé n’embarque aucune valeur. Pages associées : <a href="fiche_exposition_tension.html">fiche d’exposition</a>, <a href="../cartes/carte_typologie.html">carte des profils</a>.</p>'),
            sprintf('<p class="note">Sources : %s ; tension par famille professionnelle (FAP) en France, rattachée aux PCS de la liste.</p>', e(source_note)), '</div></body></html>')
  html <- texte_liste(html)
  if (any(grepl("<script", html, fixed = TRUE)) || any(grepl(" on[a-z]+=", html)) || any(grepl("javascript:", html, fixed = TRUE))) stop("generer_fiche_metiers_liste : contenu actif détecté.")
  dir.create(dirname(fichier), showWarnings = FALSE, recursive = TRUE)
  writeLines(enc2utf8(html), fichier, useBytes = TRUE)
  invisible(fichier)
}

# --- Classeur Excel (SPEC-TYPO-109) : Lisez-moi, table longue, vue par territoire, paramètres ----
tables_classeur_metiers_liste <- function(tab, params = parametres_tension(), champ = construire_libelle_champ(), source_note = "calculs propres", date = Sys.Date()) {
  liste <- lib_liste(); seuil <- params$ecart_pts
  long <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
    `Métier` = .lib_metier(tab$pcs, tab$pcs_libelle), `Code PCS` = tab$pcs, `Famille professionnelle` = tab$fap_libelle, `Code FAP` = tab$fap_code,
    `En tension en France` = .oui_non_na(tab$en_tension), Levier = tab$levier, Niveau = tab$niveau, Code = tab$geo_code, Territoire = tab$geo_nom,
    `Diffusé` = ifelse(tab$diffuse, "oui", "non diffusé (secret statistique)"),
    `Salariés du métier` = tab$effectif_champ, `Départs d’ici 2030` = arrondir_nombre_personnes(tab$departs_central),
    `Fourchette basse` = arrondir_nombre_personnes(tab$departs_bas), `Fourchette haute` = arrondir_nombre_personnes(tab$departs_haut),
    `Part susceptible de partir (%)` = arrondir_taux(tab$taux_pct), `Repère France (%)` = arrondir_taux(tab$taux_france_pct), `Écart à la France (points)` = arrondir_taux(tab$ecart_pts),
    Lecture = tab$lecture)
  if (any(!is.na(as.matrix(long[!tab$diffuse, c("Salariés du métier", "Départs d’ici 2030", "Fourchette basse", "Fourchette haute", "Part susceptible de partir (%)", "Écart à la France (points)"), drop = FALSE]))))
    stop("tables_classeur_metiers_liste : valeur présente pour une cellule non diffusée.")
  par_metier <- long[order(tab$ordre_liste, tab$niveau == "département", -ifelse(is.na(tab$taux_pct), -Inf, tab$taux_pct), tab$geo_code), , drop = FALSE]; rownames(par_metier) <- NULL
  par_terr <- long[order(tab$niveau == "département", tab$geo_code, -ifelse(is.na(tab$ecart_pts), -Inf, tab$ecart_pts), tab$ordre_liste), c(7:10, 1:6, 11:18), drop = FALSE]; rownames(par_terr) <- NULL
  titre <- function(x) structure(x, gras = TRUE)
  lignes <- list(titre(paste0(toupper(substr(liste, 1, 1)), substr(liste, 2, nchar(liste)), " : où les départs sont-ils les plus forts ?")),
    paste0(construire_libelles_public(champ)$salaries_age, texte_perimetre(" · entreprises du périmètre Base · départs attendus d’ici 2030")),
    paste0("Classeur produit le ", format(date, "%d/%m/%Y"), ". Sources : ", source_note, " ; tension par famille professionnelle (FAP) en France, rattachée aux PCS de la liste."), "",
    titre("Comment lire"),
    "Part susceptible de partir = part des salariés du métier qui devraient avoir quitté l’emploi d’ici 2030 (estimation centrale, 08c) ; Repère France = la même part en France ; Écart = territoire moins France, en points.",
    sprintf("Lecture : « plus forte qu’en France » à partir de %s points d’écart, « plus faible qu’en France » en deçà de %s points, « proche de la France » entre les deux (TENSION_ECART_POINTS).", .f1_liste(seuil), .f1_liste(-seuil)),
    "La tension est celle de la famille professionnelle du métier en France ; elle ne mesure pas la tension d’un territoire.",
    "Secret statistique : règle Insee (salariés, entreprises, dominance) puis secret secondaire, appliqués par le 08c ; une cellule non diffusée n’a aucune valeur. Au niveau du département, la cellule est souvent masquée : lire d’abord la région.", "",
    titre("Les feuilles"),
    "Métiers x territoires : une ligne par métier de la liste et par territoire (régions puis départements), triée par métier puis part susceptible de partir décroissante. Filtrer sur un métier donne ses territoires classés.",
    "Par territoire : les mêmes lignes, triées par territoire puis écart à la France décroissant. Filtrer sur un département donne ses métiers classés.",
    "Paramètres : libellé de la liste, nombre de métiers suivis, seuil d'écart, seuil de tension.")
  gras <- vapply(lignes, function(x) isTRUE(attr(x, "gras")), TRUE); gras <- rep(gras, lengths(lignes)); lignes <- unlist(lapply(lignes, as.character))
  lisez <- data.frame(texte = lignes, stringsAsFactors = FALSE); attr(lisez, "gras") <- gras
  parametres <- data.frame(parametre = c("libelle_liste", "n_metiers_suivis", "ecart_points", "seuil_rang", "rang_max", "n_metiers_table", "n_territoires"),
                           valeur = as.character(c(liste, params$n_suivis, seuil, params$seuil_rang, params$rang_max, length(unique(tab$pcs)), nrow(distinct(tab, niveau, geo_code)))), stringsAsFactors = FALSE)
  tables_texte_liste(list("Lisez-moi" = lisez, "Métiers x territoires" = par_metier, "Par territoire" = par_terr, "Paramètres" = parametres))
}
