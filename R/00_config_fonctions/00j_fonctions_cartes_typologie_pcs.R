# ==============================================================================
# 00j_fonctions_cartes_typologie_pcs.R — Cartes « profils et PCS » : de la région
#                                        au département (fonctions pures, 08h)
# ------------------------------------------------------------------------------
# Rôle : à partir des tables de DIFFUSION (typologie 08f, départs par PCS fine
#        08c aux trois niveaux, total France cs1 08d), produire :
#          - carte_typologie_pcs.html : régions colorées par le profil le plus
#            fréquent de leurs départements ; clic = niveau département (région
#            mise en évidence, top des PCS régional) ; clic département = profil,
#            chiffres, top des PCS en barres, spécificité face à la France ;
#            clic sur une PCS = coloration de la carte par ses départs ;
#          - carte_typologie_pcs_courriel.html : AUCUN contenu actif, deux cartes
#            à infobulles natives, blocs dépliables natifs par territoire.
# SECRET : seules les cellules diffusables sont classées et embarquées ; au plus
#        TYPO_PCS_TOP_N lignes, jamais le complément ; un territoire comptant
#        exactement UNE cellule masquée n'a pas de top (différenciation avec le
#        total publié) ; les masquées ne sont qu'un COMPTE. Une région n'a jamais
#        de profil propre (specs/10 §11, SPEC-TYPO-080 à 085).
# RÉUTILISE 00f (lecture diffusion, fond, projection, CSS, libellés PCS, classes
#        rondes), 00h (profils), 00i (préparation typologie, détail, pied).
# ==============================================================================
library(dplyr)

LIBELLES_TYPO_PCS <- list(
  kicker = texte_perimetre("Typologie des territoires Base · métiers concernés par les départs"),
  titre = "Profils et métiers : de la région au département",
  sous_titre = "Chaque région est colorée par le profil le plus fréquent de ses départements ; cliquez-la pour voir ses départements, puis un département pour lire son profil et les métiers (PCS) les plus concernés par les départs d’ici 2030.",
  egalite = "Profils à égalité", non_diffuse = "Non diffusé (secret statistique)",
  top_titre = "Métiers les plus concernés par les départs", top_retenu = "Top non diffusé : une seule cellule PCS protégée (secret statistique)",
  top_vide = "Aucune PCS diffusable", pcs_non_diffusees = "PCS non diffusées (secret statistique)",
  specificite = "Part dans les départs du territoire, face à la France", colorer = "Colorer la carte par cette PCS", retour_profils = "Revenir aux profils",
  pyramide_titre = "Pyramide des âges (emploi actuel tous âges)",
  retour_regions = "Toutes les régions", niveau_regions = "Régions", niveau_departements = "Départements",
  courriel_note = "Version pour envoi par courriel, sans script : survolez un territoire pour lire ses chiffres ; le détail de chaque territoire est dans les blocs dépliables sous les cartes. La coloration de la carte par PCS est réservée à la page interactive.")
COULEUR_EGALITE <- "#C9D2DC"

# --- Top des PCS d'un territoire (SPEC-TYPO-080 / 081) ------------------------------------
# cellules : lignes de la table de diffusion PCS d'UN territoire (masque, departs_*, taux, pcs, cs1) ;
# total : total publié du territoire (NA = parts non calculables) ; libelles : charger_libelles_pcs().
preparer_top_pcs <- function(cellules, total = NA_real_, libelles = NULL, top_n = 5) {
  m <- sum(cellules$masque %in% TRUE); dif <- cellules[cellules$masque %in% FALSE & !is.na(cellules$departs_central), ]
  if (m == 1) return(list(top = NULL, n_masquees = m, n_pcs = nrow(cellules), statut = "retenu", total = total))   # différenciation : total publié - diffusées = la masquée
  if (nrow(dif) == 0) return(list(top = NULL, n_masquees = m, n_pcs = nrow(cellules), statut = "vide", total = total))
  dif <- dif[order(-dif$departs_central, dif$pcs), ][seq_len(min(top_n, nrow(dif))), ]
  lib <- if (is.null(libelles)) rep(NA_character_, nrow(dif)) else unname(libelles[normaliser_code_pcs(dif$pcs)])
  top <- tibble(pcs = as.character(dif$pcs), libelle = ifelse(is.na(lib), NA_character_, lib), cs1 = as.character(dif$cs1),
                departs_central = dif$departs_central, departs_bas = dif$departs_bas, departs_haut = dif$departs_haut,
                taux_depart_central_pct = dif$taux_depart_central_pct, effectif_champ = dif$effectif_champ,
                part_pct = if (is.na(total) || total <= 0) NA_real_ else 100 * dif$departs_central / total, part_france_pct = NA_real_)   # part France : remplie pour les départements seulement
  list(top = top, n_masquees = m, n_pcs = nrow(cellules), statut = "ok", total = total)
}
libelle_pcs_court <- function(pcs, libelle, cs1) ifelse(is.na(libelle), paste0("PCS ", pcs, " · ", ifelse(is.na(cs1), "", libelle_cs_typo(cs1))), libelle)

# --- Régions : profil le plus fréquent des départements (SPEC-TYPO-082) -------------------
preparer_regions_typologie <- function(terr, typo_csv, fond_regions, profils = PROFILS_TYPOLOGIE, passage = NULL) {
  # terr : prep$territoires de 00i (code, nom, statut, cle) ; typo_csv : table de diffusion (geo_code, region_code, region_nom)
  reg_de <- setNames(as.character(typo_csv$region_code), as.character(typo_csv$geo_code))
  # départements absents de la table (aucun salarié observé = masqués, SPEC-PUB-007) : région lue dans la table de passage
  # departement->region si elle est fournie, pour qu'ils soient comptés parmi les non diffusés de leur région
  if (!is.null(passage) && all(c("code_source", "code_cible") %in% names(passage))) {
    p <- setNames(as.character(passage$code_cible), as.character(passage$code_source)); manq <- setdiff(terr$code, names(reg_de))
    reg_de <- c(reg_de, p[intersect(manq, names(p))])
  }
  d <- terr; d$region <- unname(reg_de[d$code])
  regions <- lapply(names(fond_regions), function(rc) {
    dd <- d[!is.na(d$region) & d$region == rc, ]
    comptes <- vapply(names(profils), function(k) sum(dd$cle %in% k), 0L); n_nd <- sum(is.na(dd$cle))
    cle <- NA_character_; egalite <- FALSE
    if (sum(comptes) > 0) { mx <- max(comptes); g <- names(comptes)[comptes == mx]; if (length(g) == 1) cle <- g else egalite <- TRUE }
    list(code = rc, nom = fond_regions[[rc]]$nom, cle = cle, egalite = egalite, comptes = as.list(comptes), n_non_diffuses = n_nd,
         n_departements = nrow(dd), departements = dd$code[order(dd$code)],
         total = if (nrow(dd) > 0 && !anyNA(dd$departs_central)) sum(dd$departs_central) else NA_real_)
  })
  names(regions) <- names(fond_regions)
  list(regions = regions, n_sans_region = sum(is.na(d$region)))
}
couleur_region_typo <- function(r) if (!is.na(r$cle)) COULEURS_PROFILS[[r$cle]] else if (isTRUE(r$egalite)) COULEUR_EGALITE else COULEURS_CARTE$secret   # aucun département diffusable = non diffusé (SPEC-PUB-007)
titre_region_typo <- function(r, profils = PROFILS_TYPOLOGIE) if (!is.na(r$cle)) profils[[r$cle]] else if (isTRUE(r$egalite)) LIBELLES_TYPO_PCS$egalite else LIBELLES_TYPO_PCS$non_diffuse

# --- Préparation complète -----------------------------------------------------------------------
# typo_prep : preparer_carte_typologie() (00i) ; typo_csv : lire_typologie_diffusion() ;
# pcs_dep / pcs_reg / pcs_fr : lire_csv_diffusion() des trois tables 08c ; cs1_fr : table cs1 France (ou NULL).
preparer_cartes_typologie_pcs <- function(typo_prep, typo_csv, pcs_dep, pcs_reg, pcs_fr, fond_dep, fond_reg, libelles = NULL, cs1_fr = NULL, top_n = 5, passage_regions = NULL) {
  for (nom in c("pcs_dep", "pcs_reg", "pcs_fr")) { x <- get(nom); if (!"masque" %in% names(x)) stop("preparer_cartes_typologie_pcs : table '", nom, "' sans colonne masque — seule une table de DIFFUSION est admise.") }
  if (!is.numeric(top_n) || top_n < 1) stop("preparer_cartes_typologie_pcs : top_n doit être >= 1.")
  pcs_dep$geo_code <- as.character(pcs_dep$geo_code); pcs_reg$region_code <- as.character(pcs_reg$region_code)
  absents <- setdiff(unique(pcs_dep$geo_code[!pcs_dep$geo_code %in% c("inconnu", "")]), names(fond_dep))
  if (length(absents)) stop("Cartes typologie / PCS : ", length(absents), " code(s) département sans géométrie : ", paste(sort(absents), collapse = ", "))
  terr <- typo_prep$territoires
  # France : total publié (cs1 France si disponible et entièrement diffusé, sinon PCS France si entièrement diffusé)
  total_fr <- NA_real_
  if (!is.null(cs1_fr) && "masque" %in% names(cs1_fr) && !any(cs1_fr$masque %in% TRUE)) total_fr <- sum(cs1_fr$departs_central, na.rm = TRUE)
  else if (!any(pcs_fr$masque %in% TRUE)) total_fr <- sum(pcs_fr$departs_central, na.rm = TRUE)
  france <- preparer_top_pcs(pcs_fr, total_fr, libelles, top_n = nrow(pcs_fr))     # toutes les PCS diffusables, pour la spécificité
  part_fr <- if (!is.null(france$top)) setNames(france$top$part_pct, france$top$pcs) else numeric(0)
  # Départements : top + spécificité
  deps <- lapply(seq_len(nrow(terr)), function(i) {
    r <- terr[i, ]; cel <- pcs_dep[pcs_dep$geo_code == r$code, ]
    if (r$statut == "masque" || nrow(cel) == 0) { t <- list(top = NULL, n_masquees = sum(cel$masque %in% TRUE), n_pcs = nrow(cel), statut = if (r$statut == "masque") "territoire_masque" else "vide", total = NA_real_) }
    else { t <- preparer_top_pcs(cel, r$departs_central, libelles, top_n); if (!is.null(t$top)) t$top$part_france_pct <- unname(part_fr[t$top$pcs]) }
    t })
  names(deps) <- terr$code
  # Régions : profil le plus fréquent + top régional
  reg <- preparer_regions_typologie(terr, typo_csv, fond_reg, passage = passage_regions)
  for (rc in names(reg$regions)) { cel <- pcs_reg[pcs_reg$region_code == rc, ]
    # total régional : somme des départements s'ils sont tous diffusés ; sinon somme des cellules région x PCS de la table de
    # DIFFUSION régionale si aucune n'est masquée (chaque cellule est publiée) ; sinon n.d. (un département absent = masqué, SPEC-PUB-007)
    if (is.na(reg$regions[[rc]]$total) && nrow(cel) > 0 && !any(cel$masque %in% TRUE) && !anyNA(cel$departs_central)) reg$regions[[rc]]$total <- sum(cel$departs_central)
    reg$regions[[rc]]$top_pcs <- if (nrow(cel)) preparer_top_pcs(cel, reg$regions[[rc]]$total, libelles, top_n) else list(top = NULL, n_masquees = 0L, n_pcs = 0L, statut = "vide", total = NA_real_) }
  # PCS à colorer : union des tops départementaux ; valeur par département (diffusée), "m" (masquée) ou absente
  pcs_top <- unique(unlist(lapply(deps, function(t) if (!is.null(t$top)) t$top$pcs else character(0))))
  par_pcs <- lapply(pcs_top, function(p) { cel <- pcs_dep[pcs_dep$pcs == p & pcs_dep$geo_code %in% names(fond_dep), ]
    v <- lapply(seq_len(nrow(cel)), function(j) if (cel$masque[j] %in% TRUE) "m" else cel$departs_central[j]); names(v) <- cel$geo_code
    lib <- if (is.null(libelles)) NA_character_ else unname(libelles[normaliser_code_pcs(p)])
    list(pcs = p, libelle = if (is.na(lib)) NA_character_ else lib, cs1 = as.character(cel$cs1[1]), valeurs = v) })
  names(par_pcs) <- pcs_top
  list(territoires = terr, departements = deps, regions = reg$regions, n_sans_region = reg$n_sans_region, france = france, total_france = total_fr,
       par_pcs = par_pcs, top_n = top_n, profils = typo_prep$profils, comptes = typo_prep$comptes, indicateur = typo_prep$indicateur,
       n_retenus = sum(vapply(deps, function(t) identical(t$statut, "retenu"), TRUE)) + sum(vapply(reg$regions, function(r) identical(r$top_pcs$statut, "retenu"), TRUE)))
}
controler_cartes_typologie_pcs <- function(prep, prefixe = "08h") {
  # aucune valeur masquée embarquée : les cellules "m" ne portent pas de nombre ; les tops ne contiennent que des cellules diffusées
  for (p in prep$par_pcs) for (v in p$valeurs) if (!identical(v, "m") && !is.numeric(v)) stop(prefixe, " : valeur de PCS inattendue.")
  cat(sprintf("  typologie x PCS : %d départements | %d régions | %d PCS colorables | tops retenus (différenciation) %d | total France %s\n",
              nrow(prep$territoires), length(prep$regions), length(prep$par_pcs), prep$n_retenus, if (is.na(prep$total_france)) "n.d." else fmt_fr_carte(prep$total_france)))
  if (isTRUE(prep$n_sans_region > 0)) cat(sprintf("    %d département(s) sans région connue (absents de la table et sans table de passage) : non comptés dans les régions\n", prep$n_sans_region))
  invisible(TRUE)
}

# --- Formats et briques communes --------------------------------------------------------------
f0_ <- function(x) fmt_fr_carte(x, 0); f1_ <- function(x) fmt_fr_carte(x, 1)
lignes_top_pcs <- function(t) {   # phrases du top (infobulles natives, tableaux)
  if (identical(t$statut, "territoire_masque")) return(character(0))              # département masqué : rien de plus que le libellé du secret
  if (identical(t$statut, "retenu")) return(LIBELLES_TYPO_PCS$top_retenu)
  if (is.null(t$top) || nrow(t$top) == 0) return(LIBELLES_TYPO_PCS$top_vide)
  l <- sprintf("%d. %s : %s départs (%s à %s)%s", seq_len(nrow(t$top)), libelle_pcs_court(t$top$pcs, t$top$libelle, t$top$cs1), f0_(t$top$departs_central), f0_(t$top$departs_bas), f0_(t$top$departs_haut),
               ifelse(is.na(t$top$part_pct), "", paste0(" · ", f1_(t$top$part_pct), " % des départs")))
  if (t$n_masquees > 0) l <- c(l, sprintf("%d %s", t$n_masquees, LIBELLES_TYPO_PCS$pcs_non_diffusees))
  l
}
html_table_top <- function(t, avec_france = FALSE) {
  if (identical(t$statut, "retenu")) return(sprintf('<p class="note secret-note">%s</p>', echap_html_carte(LIBELLES_TYPO_PCS$top_retenu)))
  if (is.null(t$top) || nrow(t$top) == 0) return(sprintf('<p class="note">%s</p>', echap_html_carte(LIBELLES_TYPO_PCS$top_vide)))
  lignes <- vapply(seq_len(nrow(t$top)), function(i) { r <- t$top[i, ]
    sprintf('<tr><td>%d</td><td>%s<br><small>PCS %s · %s</small></td><td class="num">%s<br><small>%s à %s</small></td><td class="num">%s</td><td class="num">%s</td>%s</tr>', i,
            echap_html_carte(libelle_pcs_court(r$pcs, r$libelle, r$cs1)), echap_html_carte(r$pcs), echap_html_carte(libelle_cs_typo(r$cs1)), f0_(r$departs_central), f0_(r$departs_bas), f0_(r$departs_haut),
            if (is.na(r$taux_depart_central_pct)) "n.d." else paste0(f1_(r$taux_depart_central_pct), " %"), if (is.na(r$part_pct)) "n.d." else paste0(f1_(r$part_pct), " %"),
            if (avec_france) sprintf('<td class="num">%s</td>', if (is.na(r$part_france_pct)) "n.d." else paste0(f1_(r$part_france_pct), " %")) else "") }, "")
  c(sprintf('<table class="tab-typo tab-top"><thead><tr><th></th><th>PCS</th><th>Départs d’ici 2030</th><th>Part des salariés susceptibles de partir</th><th>Part des départs du territoire</th>%s</tr></thead><tbody>',
            if (avec_france) "<th>Part des départs en France</th>" else ""), lignes, "</tbody></table>",
    if (t$n_masquees > 0) sprintf('<p class="note">%d %s.</p>', t$n_masquees, echap_html_carte(LIBELLES_TYPO_PCS$pcs_non_diffusees)) else "")
}
html_barres_pyramide <- function(r) {   # pyramide en barres statiques (courriel) ; rien si absente ou non diffusée
  py <- parts_pyramide_typo(r); if (is.null(py)) return("")
  mx <- max(py$parts); if (!is.finite(mx) || mx <= 0) mx <- 1
  c(sprintf('<h3>%s</h3>', LIBELLES_TYPO_PCS$pyramide_titre), '<ul class="top">',
    sprintf('<li><div class="lib"><span>%s</span><small>%s %%</small></div><div class="b"><span style="width:%s%%"></span></div></li>', echap_html_carte(py$libelles),
            vapply(py$parts, function(x) f1_(x), ""), formatC(100 * py$parts / mx, format = "f", digits = 2, decimal.mark = ".")), '</ul>')
}
html_comptes_region <- function(r, profils = PROFILS_TYPOLOGIE) {
  parts <- c(unlist(r$comptes), non_diffuses = r$n_non_diffuses); tot <- sum(parts)
  if (tot == 0) return(sprintf('<p class="note">%s</p>', LIBELLES_TYPO_PCS$non_diffuse))
  barre <- paste(vapply(names(parts)[parts > 0], function(k) sprintf('<span style="width:%s%%;background:%s" title="%s : %d"></span>', formatC(100 * parts[[k]] / tot, format = "f", digits = 2, decimal.mark = "."),
                                                                   if (k == "non_diffuses") COULEURS_CARTE$secret else COULEURS_PROFILS[[k]], if (k == "non_diffuses") LIBELLES_TYPO_PCS$non_diffuse else profils[[k]], parts[[k]]), ""), collapse = "")
  lis <- paste(vapply(names(parts)[parts > 0], function(k) sprintf('<li><span class="sw" style="background:%s"></span>%s <b>%d</b></li>', if (k == "non_diffuses") COULEURS_CARTE$secret else COULEURS_PROFILS[[k]],
                                                                  echap_html_carte(if (k == "non_diffuses") LIBELLES_TYPO_PCS$non_diffuse else profils[[k]]), parts[[k]]), ""), collapse = "")
  c(sprintf('<div class="barre" aria-hidden="true">%s</div><ul class="comptes">%s</ul>', barre, lis))
}

# --- CSS / JS ------------------------------------------------------------------------------------
css_cartes_typologie_pcs <- function(S = STYLE_CONTOURS_CARTE) paste(   # complète css_carte_typologie() (00i) : règles propres à la page profils x PCS
  '.niveaux{display:flex;gap:8px;margin:0 0 12px}.niveaux button{flex:1;font:inherit;font-size:15px;font-weight:600;padding:9px 8px;border-radius:8px;border:1px solid #AEB7C2;background:#fff;color:var(--encre);cursor:pointer;min-height:42px}.niveaux button[aria-pressed=true]{background:var(--bleu);border-color:var(--bleu);color:#fff}',
  '.detail{height:62vh}@media(max-width:900px){.detail{height:50vh}}',   # hauteur FIXE : le panneau ne change jamais la hauteur de la page quand son contenu change
  '.detail h3{font-size:14px;margin:14px 0 6px;color:var(--bleu)}.detail .lignes{margin:0 0 6px;padding:0;list-style:none;font-size:14px}.detail .lignes li{margin:2px 0}',
  '.detail .lien{background:none;border:0;padding:0;color:var(--bleu);font:inherit;font-size:14px;text-decoration:underline;cursor:pointer}',
  '.top{list-style:none;margin:0;padding:0}.top li{margin:6px 0;font-size:14px}.top .lib{display:flex;justify-content:space-between;gap:8px}.top .lib small{color:var(--texte);white-space:nowrap}.top .b{height:10px;border-radius:3px;background:#e6ebf1;overflow:hidden;margin-top:3px}.top .b span{display:block;height:100%;background:var(--bleu)}',
  '.chips{display:flex;flex-wrap:wrap;gap:6px;margin:6px 0 0}.chips button{font:inherit;font-size:13px;padding:5px 9px;border-radius:999px;border:1px solid #AEB7C2;background:#fff;color:var(--encre);cursor:pointer;text-align:left}.chips button[aria-pressed=true]{background:var(--bleu);border-color:var(--bleu);color:#fff}',
  '.spec{list-style:none;margin:0;padding:0}.spec li{font-size:13.5px;margin:7px 0}.spec .axe{position:relative;height:14px;margin:3px 6px 0;background:#e6ebf1;border-radius:3px}.spec .pt{position:absolute;top:2px;width:10px;height:10px;border-radius:50%;margin-left:-5px}.spec .pt.d{background:var(--bleu)}.spec .pt.f{background:#9aa5b1}.spec .seg{position:absolute;top:6px;height:2px;background:#9aa5b1}.spec .val{display:flex;justify-content:space-between;font-size:12.5px;color:var(--texte)}',
  '.comptes{list-style:none;margin:6px 0 0;padding:0;font-size:14px}.comptes li{display:flex;align-items:center;gap:8px;margin:3px 0}.comptes .sw{width:14px;height:14px;border-radius:3px;flex:none;border:1px solid #5B6470}.barre{display:flex;height:16px;border-radius:5px;overflow:hidden;background:#eef1f5}.barre span{height:100%}',
  'path.t.hors{opacity:.18}path.r{cursor:pointer;outline:none}path.t{cursor:pointer}',
  '.secret-note{color:#5b4300;background:#fff8e6;border-left:4px solid #e0b96a;padding:8px 10px;border-radius:0 6px 6px 0}.tab-top small{color:var(--texte)}',
  'details.terr{margin:8px 0;border:1px solid var(--filet);border-radius:8px;padding:0 12px}details.terr summary{cursor:pointer;font-size:15px;font-weight:600;padding:9px 0;list-style:none}details.terr summary::-webkit-details-marker{display:none}details.terr summary .sw{display:inline-block;width:12px;height:12px;border-radius:2px;vertical-align:-1px;margin-right:6px;border:1px solid #5B6470}details.terr h3{font-size:14px;margin:10px 0 6px;color:var(--bleu)}details.terr .lignes{margin:0 0 6px;padding-left:18px;font-size:14px}',
  sprintf('.courriel-typo path{stroke:%s;stroke-width:%s;vector-effect:non-scaling-stroke}.courriel-typo path:hover{stroke:%s;stroke-width:%s}.deux-cartes{display:grid;grid-template-columns:1fr 1fr;gap:16px}@media(max-width:900px){.deux-cartes{grid-template-columns:1fr}}.deux-cartes h2{font-size:16px;margin:0 0 6px}', S$couleur, S$largeur, S$survol_couleur, S$survol_largeur),
  sep = "\n")

js_cartes_typologie_pcs <- function() paste(
  '(function(){',
  'var sj=document.getElementById("sans-js");if(sj)sj.style.display="none";',
  'var D=JSON.parse(document.getElementById("donnees").textContent),L=D.libelles,$=function(id){return document.getElementById(id)};',
  'var B=$("bulle"),C=$("carte"),U=$("survol"),DP=$("detail"),LEG=$("legende"),LEGT=$("legende-titre"),etat={niveau:"region",region:null,dep:null,pcs:null},codeBulle=null,tScroll=0;',
  'var nf=function(d){return new Intl.NumberFormat("fr-FR",{minimumFractionDigits:d,maximumFractionDigits:d})};var fmt=function(x,d){return (x==null||isNaN(x))?"n.d.":nf(d).format(d===0?Math.floor(x+0.5):x)};',
  'function el(tag,cls,txt){var e=document.createElement(tag);if(cls)e.className=cls;if(txt!=null)e.textContent=txt;return e}',
  'function libPcs(p){var o=D.pcs[p]||{};return o.lib?o.lib:("PCS "+p+(o.cs1?" · "+o.cs1:""))}',
  'function pasRond(x){if(x<=0)return 1;var p=Math.pow(10,Math.floor(Math.log(x)/Math.LN10)),r=x/p;var c=r<=1?1:(r<=2?2:(r<=2.5?2.5:(r<=5?5:10)));return c*p}',
  'function classesRondes(vals){var dist=vals.filter(function(v,i,a){return a.indexOf(v)===i});if(dist.length===0)return null;var mn=Math.min.apply(null,vals),mx=Math.max.apply(null,vals);if(mn===mx||dist.length<3)return{unique:true,vals:dist.sort(function(a,b){return a-b})};',
  ' var pas=pasRond((mx-mn)/5),lo,hi,n,tours=0;do{lo=Math.floor(mn/pas)*pas;hi=Math.ceil(mx/pas)*pas;if(hi<=mx)hi+=pas;n=Math.round((hi-lo)/pas);if(n>5)pas=pasRond(pas*1.01);tours++}while(n>5&&tours<8);var b=[];for(var k=1;k<n;k++)b.push(+(lo+k*pas).toFixed(6));return{unique:false,bornes:b,n:b.length+1}}',
  'function classeDe(v,cl){if(cl.unique)return cl.vals.indexOf(v);var k=0;while(k<cl.bornes.length&&v>=cl.bornes[k])k++;return k}',
  'function couleur(k,n){var pal=D.couleurs.classes;if(n<=1)return pal[2];return pal[Math.round(k*(pal.length-1)/(n-1))]}',
  'function li(col,txt,sans,n){var e=el("li");var it=el("span","item");var sw=el("span","sw"+(sans?" sans":""));sw.style.background=col;it.appendChild(sw);var t=el("span",null,txt);if(n!=null){var s=el("span","n",String(n));t.appendChild(s)}it.appendChild(t);e.appendChild(it);LEG.appendChild(e)}',
  # --- rendu de la carte selon le niveau et le mode (profils / PCS)
  'function rendre(){document.body.className="niveau-"+etat.niveau;LEG.innerHTML="";document.querySelectorAll(".niveaux button").forEach(function(x){x.setAttribute("aria-pressed",x.getAttribute("data-niveau")===etat.niveau?"true":"false")});',
  ' if(etat.niveau==="region"){LEGT.textContent="Profil le plus fréquent parmi les départements de la région";Object.keys(D.profils).forEach(function(k){li(D.profils[k].couleur,D.profils[k].titre,false,D.profils[k].n_regions)});li(D.couleurs.egalite,L.egalite,false,D.comptes.regions_egalite);li(D.couleurs.secret,L.non_diffuse,false,D.comptes.regions_non_diffusees);return}',
  ' var paths=document.querySelectorAll("path.t");',
  ' if(etat.pcs){var p=D.pcs[etat.pcs],vals=[];Object.keys(p.v).forEach(function(c){if(p.v[c]!=="m")vals.push(p.v[c])});var cl=classesRondes(vals);',
  '  paths.forEach(function(q){var c=q.getAttribute("data-code"),v=p.v[c];if(v==null||v==="m")q.setAttribute("fill",D.couleurs.secret);else q.setAttribute("fill",cl?couleur(classeDe(v,cl),cl.unique?cl.vals.length:cl.n):D.couleurs.classes[2])});',
  '  LEGT.textContent="Départs estimés d’ici 2030 · "+libPcs(etat.pcs);if(!cl)li(D.couleurs.classes[2],"Aucun résultat affichable");else if(cl.unique)cl.vals.forEach(function(v,i){li(couleur(i,cl.vals.length),fmt(v,0))});else for(var k=0;k<cl.n;k++)li(couleur(k,cl.n),k===0?"Moins de "+fmt(cl.bornes[0],0):(k===cl.n-1?fmt(cl.bornes[k-1],0)+" et plus":fmt(cl.bornes[k-1],0)+" à "+fmt(cl.bornes[k],0)));',
  '  li(D.couleurs.secret,"Résultat non diffusé — secret statistique")}',
  ' else{paths.forEach(function(q){var c=q.getAttribute("data-code"),t=D.departements[c];q.setAttribute("fill",t.couleur)});',
  '  LEGT.textContent="Profil du département";Object.keys(D.profils).forEach(function(k){li(D.profils[k].couleur,D.profils[k].titre,false,D.profils[k].n)});li(D.couleurs.secret,"Profil ou département non diffusé (secret statistique)",false,D.comptes.non_diffuses)}',
  ' paths.forEach(function(q){var c=q.getAttribute("data-code");q.classList.toggle("hors",!!etat.region&&D.departements[c].region!==etat.region)})}',
  # --- panneau
  'function titre(t,pf,col,clair){var h=el("p","detail-titre",t);if(pf){var s=el("span","pf",pf);s.style.background=col;s.style.color=clair?"#1F2933":"#fff";h.appendChild(s)}return h}',
  'function barresTop(t,cont,avecChips){if(t.statut==="retenu"){cont.appendChild(el("p","note secret-note",L.top_retenu));return}if(!t.top||!t.top.length){cont.appendChild(el("p","note",L.top_vide));return}',
  ' var mx=Math.max.apply(null,t.top.map(function(r){return r.c})),ul=el("ul","top");t.top.forEach(function(r,i){var e=el("li");var lib=el("div","lib");lib.appendChild(el("span",null,(i+1)+". "+libPcs(r.pcs)));var sm=el("small",null,fmt(r.c,0)+" départs"+(r.part!=null?" · "+fmt(r.part,1)+" %":""));lib.appendChild(sm);e.appendChild(lib);',
  '  e.title=fmt(r.b,0)+" à "+fmt(r.h,0)+" départs selon l’hypothèse · "+(r.t!=null?fmt(r.t,1)+" % des salariés de cette PCS susceptibles de partir":"");var b=el("div","b"),s=el("span");s.style.width=(100*r.c/mx)+"%";b.appendChild(s);e.appendChild(b);ul.appendChild(e)});cont.appendChild(ul);',
  ' if(t.n_masquees>0)cont.appendChild(el("p","note",t.n_masquees+" "+L.pcs_non_diffusees+"."));',
  ' if(avecChips){var ch=el("div","chips");t.top.forEach(function(r){var b=el("button",null,libPcs(r.pcs));b.type="button";b.setAttribute("data-pcs",r.pcs);b.setAttribute("aria-pressed",etat.pcs===r.pcs?"true":"false");b.title=L.colorer;b.addEventListener("click",function(){etat.pcs=(etat.pcs===r.pcs)?null:r.pcs;rendre();panneau()});ch.appendChild(b)});',
  '  if(etat.pcs){var rb=el("button",null,L.retour_profils);rb.type="button";rb.addEventListener("click",function(){etat.pcs=null;rendre();panneau()});ch.appendChild(rb)}cont.appendChild(el("h3",null,L.colorer));cont.appendChild(ch)}}',
  'function barresPyramide(py,cont){if(!py)return;cont.appendChild(el("h3",null,L.pyramide_titre));var mx=Math.max.apply(null,py.parts)||1,ul=el("ul","top");',
  ' py.libelles.forEach(function(l,i){var e=el("li");var lib=el("div","lib");lib.appendChild(el("span",null,l));lib.appendChild(el("small",null,fmt(py.parts[i],1)+" %"));e.appendChild(lib);var b=el("div","b"),s=el("span");s.style.width=(100*py.parts[i]/mx)+"%";b.appendChild(s);e.appendChild(b);ul.appendChild(e)});cont.appendChild(ul)}',
  'function specificite(t,cont){if(!t.top||!t.top.length||t.statut!=="ok")return;var ok=t.top.filter(function(r){return r.part!=null&&r.pf!=null});if(!ok.length)return;cont.appendChild(el("h3",null,L.specificite));',
  ' var mx=Math.max.apply(null,ok.map(function(r){return Math.max(r.part,r.pf)}))||1,ul=el("ul","spec");ok.forEach(function(r){var e=el("li");e.appendChild(el("div",null,libPcs(r.pcs)));var ax=el("div","axe");var a=100*r.part/mx,f=100*r.pf/mx;var seg=el("span","seg");seg.style.left=Math.min(a,f)+"%";seg.style.width=Math.abs(a-f)+"%";ax.appendChild(seg);',
  '  var pd=el("span","pt d");pd.style.left=a+"%";pd.title="Territoire : "+fmt(r.part,1)+" %";ax.appendChild(pd);var pfr=el("span","pt f");pfr.style.left=f+"%";pfr.title="France : "+fmt(r.pf,1)+" %";ax.appendChild(pfr);e.appendChild(ax);',
  '  var v=el("div","val");v.appendChild(el("span",null,"territoire "+fmt(r.part,1)+" %"));v.appendChild(el("span",null,"France "+fmt(r.pf,1)+" %"));e.appendChild(v);ul.appendChild(e)});cont.appendChild(ul);cont.appendChild(el("p","detail-aide","Point bleu : part de la PCS dans les départs du département ; point gris : sa part dans les départs nationaux. Un écart décrit une spécificité, pas une tension."))}',
  # rubriques titrées + puces du département (mêmes que la carte des profils, SPEC-TYPO-083) ; repli sur les lignes
  'function rendreBlocs(cont,t){if(t.blocs&&t.blocs.length){t.blocs.forEach(function(b){cont.appendChild(el("h3","rub",b.t));var ul=el("ul","puces");b.p.forEach(function(x){ul.appendChild(el("li",null,x))});cont.appendChild(ul)})}else{var ul=el("ul","lignes");(t.lignes||[]).forEach(function(l){ul.appendChild(el("li",null,l))});cont.appendChild(ul)}}',
  'function panneau(){DP.innerHTML="";DP.scrollTop=0;',
  ' if(etat.dep){var t=D.departements[etat.dep],p=t.cle?D.profils[t.cle]:null;DP.appendChild(titre(t.nom+" · "+etat.dep,p?p.titre:L.non_diffuse,p?p.couleur:D.couleurs.secret,p?p.clair:false));',
  '  if(t.region&&D.regions[t.region]){var rl=el("button","lien","← "+D.regions[t.region].nom);rl.type="button";rl.addEventListener("click",function(){etat.dep=null;etat.region=t.region;rendre();panneau()});DP.appendChild(rl)}',
  '  rendreBlocs(DP,t);barresPyramide(t.pyramide,DP);DP.appendChild(el("h3",null,L.top_titre));barresTop(t.top,DP,true);specificite(t.top,DP);return}',
  ' if(etat.region){var r=D.regions[etat.region];DP.appendChild(titre(r.nom,r.titre,r.couleur,r.clair));var rb=el("button","lien","← "+L.retour_regions);rb.type="button";rb.addEventListener("click",function(){etat.region=null;etat.dep=null;etat.niveau="region";etat.pcs=null;rendre();panneau()});DP.appendChild(rb);',
  '  DP.appendChild(el("h3",null,"Départements par profil"));var w=el("div");w.innerHTML=r.html_comptes;DP.appendChild(w);var dl=el("ul","lignes");r.departements.forEach(function(c){var li2=el("li");var b=el("button","lien",D.departements[c].nom+" · "+c);b.type="button";b.addEventListener("click",function(){etat.dep=c;etat.niveau="departement";rendre();panneau()});li2.appendChild(b);var pp=D.departements[c].cle?D.profils[D.departements[c].cle].titre:L.non_diffuse;li2.appendChild(el("span",null," — "+pp));dl.appendChild(li2)});DP.appendChild(dl);',
  '  DP.appendChild(el("h3",null,L.top_titre+" (région)"));barresTop(r.top,DP,false);return}',
  ' DP.appendChild(el("p","detail-titre",etat.niveau==="region"?L.aide_regions:L.aide_departements))}',
  # --- infobulle (fixe, contenu reconstruit seulement au changement)
  'function bulle(code,niveau){var e=el("div");if(niveau==="region"){var r=D.regions[code];e.appendChild(el("div","tooltip-title",r.nom));var c=el("div","tooltip-profil",r.titre);c.style.borderLeftColor=r.couleur;e.appendChild(c);var b=el("div","tooltip-rows");b.appendChild(el("div","tooltip-small",r.n_departements>0?r.n_departements+" département(s) : "+r.resume:L.non_diffuse));if(r.n_departements>0){r.lignes_top.forEach(function(l){b.appendChild(el("div","tooltip-small",l))});b.appendChild(el("div","tooltip-small","Cliquez pour voir les départements"))}e.appendChild(b)}',
  ' else{var t=D.departements[code],p=t.cle?D.profils[t.cle]:null;e.appendChild(el("div","tooltip-title",t.nom+" · "+code));var c2=el("div","tooltip-profil",etat.pcs?libPcs(etat.pcs):(p?p.titre:L.non_diffuse));c2.style.borderLeftColor=etat.pcs?"#fff":(p?p.couleur:D.couleurs.secret);e.appendChild(c2);var b2=el("div","tooltip-rows");',
  '  if(etat.pcs){var v=D.pcs[etat.pcs].v[code];b2.appendChild(el("div","tooltip-value",(v==null||v==="m")?"Résultat non diffusé — secret statistique":fmt(v,0)+" départs estimés d’ici 2030"))}else{t.lignes.slice(0,3).forEach(function(l){b2.appendChild(el("div","tooltip-value",l))});t.lignes_top.slice(0,3).forEach(function(l){b2.appendChild(el("div","tooltip-small",l))})}e.appendChild(b2)}return e.innerHTML}',
  'function placer(x,y){var vw=window.innerWidth,vh=window.innerHeight,w=B.offsetWidth,h=B.offsetHeight,px=x+16,py=y+16;if(px+w>vw-8)px=x-w-16;if(py+h>vh-8)py=y-h-16;B.style.left=Math.max(4,px)+"px";B.style.top=Math.max(4,py)+"px"}',
  'function surligner(p){if(!U)return;if(p){U.setAttribute("d",p.getAttribute("d"));U.classList.add("on")}else{U.classList.remove("on");U.setAttribute("d","")}}',
  'function brancher(sel,niveau){document.querySelectorAll(sel).forEach(function(p){var code=p.getAttribute("data-code");',
  ' p.addEventListener("mousemove",function(ev){var cle=niveau+code+(etat.pcs||"");if(cle!==codeBulle){B.innerHTML=bulle(code,niveau);codeBulle=cle}surligner(p);if(Date.now()-tScroll<150)return;B.style.display="block";placer(ev.clientX,ev.clientY)});',
  ' p.addEventListener("mouseleave",function(){B.style.display="none";surligner(null)});',
  ' p.addEventListener("click",function(){if(niveau==="region"){etat.region=code;etat.dep=null;etat.niveau="departement"}else{etat.dep=code;etat.region=D.departements[code].region||etat.region}rendre();panneau();if(niveau!=="region"&&!window.matchMedia("(min-width:901px)").matches)DP.scrollIntoView({behavior:"smooth",block:"start"})});',
  ' p.addEventListener("keydown",function(ev){if(ev.key==="Enter"||ev.key===" "){ev.preventDefault();p.dispatchEvent(new MouseEvent("click"))}})})}',
  'brancher("path.r","region");brancher("path.t","departement");',
  'document.querySelectorAll(".niveaux button").forEach(function(b){b.addEventListener("click",function(){etat.niveau=b.getAttribute("data-niveau");if(etat.niveau==="region"){etat.region=null;etat.dep=null;etat.pcs=null}document.querySelectorAll(".niveaux button").forEach(function(x){x.setAttribute("aria-pressed",x===b?"true":"false")});rendre();panneau()})});',
  'window.addEventListener("scroll",function(){tScroll=Date.now();if(B.style.display==="block")B.style.display="none"},{passive:true});',
  'rendre();panneau();',
  '})();', sep = "\n")

# --- JSON (aucune valeur masquée) ---------------------------------------------------------------
donnees_json_typo_pcs <- function(prep, champ = construire_libelle_champ()) {
  defs <- DEFINITIONS_PROFILS; P <- prep$profils; clair <- c(stable = TRUE, diffus = TRUE, expertiser = TRUE)
  top_json <- function(t) list(statut = t$statut, n_masquees = t$n_masquees, top = if (is.null(t$top)) list() else lapply(seq_len(nrow(t$top)), function(i) { r <- t$top[i, ]
    x <- list(pcs = r$pcs, c = r$departs_central, b = r$departs_bas, h = r$departs_haut); if (!is.na(r$taux_depart_central_pct)) x$t <- r$taux_depart_central_pct
    if (!is.na(r$part_pct)) x$part <- round(r$part_pct, 2); if (!is.na(r$part_france_pct)) x$pf <- round(r$part_france_pct, 2); x }))
  reg_de <- list(); for (rc in names(prep$regions)) for (c in prep$regions[[rc]]$departements) reg_de[[c]] <- rc
  deps <- setNames(lapply(seq_len(nrow(prep$territoires)), function(i) { r <- prep$territoires[i, ]; t <- prep$departements[[r$code]]
    x <- list(nom = r$nom, st = r$statut, couleur = couleur_territoire_typo(r), lignes = as.list(lignes_detail_typologie(r, prep$indicateur)), blocs = blocs_detail_typologie(r, prep$indicateur),
              lignes_top = as.list(lignes_top_pcs(t)), top = top_json(t))
    if (!is.na(r$cle)) x$cle <- r$cle; if (!is.null(reg_de[[r$code]])) x$region <- reg_de[[r$code]]
    py <- if (r$statut == "masque") NULL else parts_pyramide_typo(r); if (!is.null(py)) x$pyramide <- list(libelles = py$libelles, parts = round(py$parts, 2)); x }), prep$territoires$code)
  regs <- lapply(prep$regions, function(r) { parts <- c(unlist(r$comptes), non_diffuses = r$n_non_diffuses)
    list(nom = r$nom, cle = if (is.na(r$cle)) NULL else r$cle, titre = titre_region_typo(r, P), couleur = couleur_region_typo(r), clair = is.na(r$cle) || isTRUE(clair[r$cle]),
         n_departements = r$n_departements, departements = as.list(r$departements), comptes = r$comptes, n_non_diffuses = r$n_non_diffuses,
         resume = paste(vapply(names(parts)[parts > 0], function(k) sprintf("%d %s", parts[[k]], if (k == "non_diffuses") "non diffusé(s)" else tolower(P[[k]])), ""), collapse = ", "),
         html_comptes = paste(html_comptes_region(r, P), collapse = ""), lignes_top = as.list(lignes_top_pcs(r$top_pcs)), top = top_json(r$top_pcs)) })
  n_reg <- vapply(names(P), function(k) sum(vapply(prep$regions, function(r) identical(r$cle, k), TRUE)), 0L)
  profils <- setNames(lapply(names(P), function(k) list(titre = P[[k]], couleur = COULEURS_PROFILS[[k]], clair = isTRUE(clair[k]), n = prep$comptes[[k]], n_regions = n_reg[[k]],
                                                      signification = defs$signification[defs$cle == k])), names(P))
  pcs <- lapply(prep$par_pcs, function(p) list(lib = if (is.na(p$libelle)) NULL else p$libelle, cs1 = libelle_cs_typo(p$cs1), v = p$valeurs))
  j <- jsonlite::toJSON(list(departements = deps, regions = regs, profils = profils, pcs = pcs,
                             comptes = list(non_diffuses = prep$comptes[["profil_masque"]] + prep$comptes[["masque"]],
                                            regions_egalite = sum(vapply(prep$regions, function(r) isTRUE(r$egalite), TRUE)), regions_non_diffusees = sum(vapply(prep$regions, function(r) is.na(r$cle) && !isTRUE(r$egalite), TRUE))),
                             couleurs = c(COULEURS_CARTE[c("classes", "secret")], list(egalite = COULEUR_EGALITE)),
                             libelles = c(LIBELLES_TYPO_PCS, list(aide_regions = "Cliquez une région pour voir ses départements et les métiers les plus concernés.",
                                                                  aide_departements = "Cliquez un département pour lire son profil, ses métiers les plus concernés et sa spécificité face à la France."))),
                       auto_unbox = TRUE, digits = NA, na = "null", null = "null")
  gsub("</", "<\\/", j, fixed = TRUE)
}

# --- Pages ---------------------------------------------------------------------------------------
svg_regions_typo <- function(prep, fond_reg, lay_reg, interactive = TRUE, S = STYLE_CONTOURS_CARTE) {
  vapply(names(fond_reg), function(rc) { r <- prep$regions[[rc]]; col <- couleur_region_typo(r)
    titre <- paste(c(r$nom, titre_region_typo(r, prep$profils), sprintf("%d département(s)", r$n_departements), lignes_top_pcs(r$top_pcs)), collapse = "\n")
    sprintf('<path class="r"%s data-code="%s" fill="%s" stroke="%s" stroke-width="%s" d="%s"%s>%s</path>', if (interactive) sprintf(' id="r-%s"', rc) else "", rc, col,
            S$couleur, S$largeur, lay_reg$chemins[[rc]],
            if (interactive) sprintf(' tabindex="0" aria-label="%s"', echap_html_carte(paste(r$nom, titre_region_typo(r, prep$profils), sep = " — "))) else "",
            if (interactive) "" else paste0("<title>", echap_html_carte(titre), "</title>")) }, "")
}
svg_departements_typo <- function(prep, fond_dep, lay_dep, interactive = TRUE, S = STYLE_CONTOURS_CARTE) {
  t <- prep$territoires
  vapply(names(fond_dep), function(code) { r <- t[t$code == code, ]; tp <- prep$departements[[code]]
    titre <- paste(c(paste0(r$nom, " · ", code), titre_profil_typo(r, prep), lignes_detail_typologie(r, prep$indicateur)[1:min(3, length(lignes_detail_typologie(r, prep$indicateur)))], lignes_top_pcs(tp)), collapse = "\n")
    sprintf('<path class="t"%s data-code="%s" data-st="%s"%s fill="%s" stroke="%s" stroke-width="%s" d="%s"%s>%s</path>', if (interactive) sprintf(' id="t-%s"', code) else "", code, r$statut,
            if (!is.na(r$cle)) sprintf(' data-cle="%s"', r$cle) else "", couleur_territoire_typo(r), S$couleur, S$largeur,
            lay_dep$chemins[[code]], if (interactive) sprintf(' tabindex="0" aria-label="%s"', echap_html_carte(paste0(r$nom, " — ", titre_profil_typo(r, prep)))) else "",
            if (interactive) "" else paste0("<title>", echap_html_carte(titre), "</title>")) }, "")
}
html_entete_typo_pcs <- function(champ, bandeau, sous_titre, compact = FALSE) {
  P <- construire_libelles_public(champ); LT <- LIBELLES_TYPO_PCS
  c('<header class="page-header">', sprintf('<p class="kicker">%s</p>', echap_html_carte(LT$kicker)),
    sprintf('<h1 class="page-title" id="titre">%s<small>Départs attendus d’ici 2030 · régions et départements</small></h1>', echap_html_carte(LT$titre)),
    if (bandeau) html_bandeau_sans_js() else "", if (compact) sprintf('<details class="apropos"><summary>%s</summary>', LIBELLES_CARTE_TYPO$apropos) else "",
    sprintf('<p class="page-subtitle"><span class="cat-qui">%s</span></p>', echap_html_carte(sous_titre)),
    '<div class="study-scope" role="note" aria-label="Qui est concerné">', sprintf(texte_perimetre('<span class="scope-text"><b>%s</b> · entreprises du périmètre Base</span>'), echap_html_carte(P$salaries_age)),
    '<span class="scope-text">Une région n’a pas de profil propre : elle est colorée par le profil le plus fréquent de ses départements.</span></div>',
    '<p class="context">Les métiers sont les PCS fines (nomenclature PCS-ESE 2017). Seules les PCS diffusables sont classées ; les PCS protégées par le secret statistique ne sont comptées qu’en nombre, jamais en valeur.</p>',
    if (compact) '</details>' else "", '</header>')
}
html_legende_statique_typo_pcs <- function(prep, niveau) {
  P <- prep$profils; S <- STYLE_CONTOURS_CARTE
  item <- function(col, lib, n) sprintf('<li><span class="item"><span class="sw" style="background:%s"></span><span>%s<span class="n">%d</span></span></span></li>', col, echap_html_carte(lib), n)
  if (niveau == "region") { n_reg <- vapply(names(P), function(k) sum(vapply(prep$regions, function(r) identical(r$cle, k), TRUE)), 0L)
    c(vapply(names(P), function(k) item(COULEURS_PROFILS[[k]], P[[k]], n_reg[[k]]), ""), item(COULEUR_EGALITE, LIBELLES_TYPO_PCS$egalite, sum(vapply(prep$regions, function(r) isTRUE(r$egalite), TRUE))),
      item(COULEURS_CARTE$secret, LIBELLES_TYPO_PCS$non_diffuse, sum(vapply(prep$regions, function(r) is.na(r$cle) && !isTRUE(r$egalite), TRUE)))) }
  else c(vapply(names(P), function(k) item(COULEURS_PROFILS[[k]], P[[k]], prep$comptes[[k]]), ""), item(COULEURS_CARTE$secret, "Profil ou département non diffusé (secret statistique)", prep$comptes[["profil_masque"]] + prep$comptes[["masque"]]))   # aucune entrée « absent » (SPEC-PUB-007)
}
html_blocs_territoires <- function(prep) {   # version courriel : un bloc dépliable par région (comptes + top) et par département (profil + top + spécificité)
  P <- prep$profils; t <- prep$territoires
  regs <- unlist(lapply(names(prep$regions)[order(vapply(prep$regions, `[[`, "", "nom"))], function(rc) { r <- prep$regions[[rc]]
    c(sprintf('<details class="terr"><summary><span class="sw" style="background:%s"></span>%s — %s · %d département(s)</summary>', couleur_region_typo(r), echap_html_carte(r$nom), echap_html_carte(titre_region_typo(r, P)), r$n_departements),
      html_comptes_region(r, P), sprintf('<p class="note">%s</p>', echap_html_carte(paste(vapply(r$departements, function(c) paste0(t$nom[t$code == c], " (", c, ")"), ""), collapse = ", "))),
      sprintf('<h3>%s (région)</h3>', LIBELLES_TYPO_PCS$top_titre), html_table_top(r$top_pcs), '</details>') }))
  tt <- t[order(t$nom), ]   # tous les départements du fond (SPEC-PUB-007)
  deps <- unlist(lapply(seq_len(nrow(tt)), function(i) { r <- tt[i, ]; tp <- prep$departements[[r$code]]
    c(sprintf('<details class="terr"><summary><span class="sw" style="background:%s"></span>%s · %s — %s</summary>', couleur_territoire_typo(r), echap_html_carte(r$nom), r$code, echap_html_carte(titre_profil_typo(r, prep))),
      sprintf('<ul class="lignes">%s</ul>', paste(sprintf("<li>%s</li>", echap_html_carte(lignes_detail_typologie(r, prep$indicateur))), collapse = "")),
      if (r$statut == "masque") "" else c(html_barres_pyramide(r), sprintf('<h3>%s</h3>', LIBELLES_TYPO_PCS$top_titre), html_table_top(tp, avec_france = TRUE)), '</details>') }))
  c('<details class="bloc-typo" open><summary>Le détail par région</summary>', regs, '</details>',
    '<details class="bloc-typo" open><summary>Le détail par département</summary>', deps, '</details>')
}
titre_page_typo_pcs <- function() paste0(LIBELLES_TYPO_PCS$titre, " — départs attendus d’ici 2030")

generer_cartes_typologie_pcs <- function(prep, fond_dep, fond_reg, fichier_html, champ = construire_libelle_champ(), source_note = "calculs propres") {
  lay_d <- projeter_fond(fond_dep); lay_r <- projeter_fond(fond_reg); LT <- LIBELLES_TYPO_PCS
  html <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">', sprintf('<title>%s</title>', echap_html_carte(titre_page_typo_pcs())),
            '<meta name="viewport" content="width=device-width, initial-scale=1">', '<style>', css_cartes(), css_carte_typologie(), css_cartes_typologie_pcs(),
            'body.niveau-region #g-deps{display:none}body.niveau-departement #g-regs{display:none}body.niveau-departement #g-limites{display:inline}#g-limites{display:none}', '</style></head><body class="niveau-region"><div class="page page-ecran">',
            '<div class="ecran">', html_entete_typo_pcs(champ, TRUE, LT$sous_titre, compact = TRUE),
            '<div class="grille scene"><aside class="panneau">',
            sprintf('<div class="niveaux" role="group" aria-label="Niveau"><button type="button" data-niveau="region" aria-pressed="true">%s</button><button type="button" data-niveau="departement" aria-pressed="false">%s</button></div>', LT$niveau_regions, LT$niveau_departements),
            '<div class="detail" id="detail" role="status"><p class="detail-titre">Cliquez une région pour voir ses départements et les métiers les plus concernés.</p></div>',
            sprintf('<a class="retour-carte" href="#carte">%s</a>', LIBELLES_CARTE_TYPO$retour_carte),
            '<div class="legende-bloc"><div class="legend-title" id="legende-titre">Profil le plus fréquent parmi les départements de la région</div><ul class="typo-legende" id="legende">', html_legende_statique_typo_pcs(prep, "region"), '</ul></div>',
            if (length(lay_d$encarts) > 0) '<p class="note">Outre-mer en encarts, chacun à sa propre échelle.</p>' else "",
            '</aside><div class="carte" id="carte">',
            sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">', lay_d$largeur, lay_d$hauteur, echap_html_carte(titre_page_typo_pcs())),
            html_encarts_typo(lay_d), '<g id="g-regs">', svg_regions_typo(prep, fond_reg, lay_r, TRUE), '</g>', '<g id="g-deps">', svg_departements_typo(prep, fond_dep, lay_d, TRUE), '</g>',
            '<g id="g-limites">', html_limites_regions_typo(fond_reg), '</g>', '<path id="survol" fill="none" d="" aria-hidden="true"></path>', '</svg><div class="bulle" id="bulle" role="status"></div></div></div></div>',
            html_definitions_profils(prep), html_pied_typologie(champ, source_note),
            '<script type="application/json" id="donnees">', donnees_json_typo_pcs(prep, champ), '</script>', '<script>', js_cartes_typologie_pcs(), '</script>', '</div></body></html>')
  dir.create(dirname(fichier_html), showWarnings = FALSE, recursive = TRUE); writeLines(enc2utf8(html), fichier_html, useBytes = TRUE); invisible(fichier_html)
}
generer_cartes_typologie_pcs_courriel <- function(prep, fond_dep, fond_reg, fichier_html, champ = construire_libelle_champ(), source_note = "calculs propres") {
  lay_d <- projeter_fond(fond_dep); lay_r <- projeter_fond(fond_reg); LT <- LIBELLES_TYPO_PCS
  svg <- function(contenu, lab) c(sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">', lay_d$largeur, lay_d$hauteur, echap_html_carte(lab)), html_encarts_typo(lay_d), contenu, '</svg>')
  html <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">', sprintf('<title>%s</title>', echap_html_carte(titre_page_typo_pcs())),
            '<meta name="viewport" content="width=device-width, initial-scale=1">', '<style>', css_cartes(), css_carte_typologie(), css_cartes_typologie_pcs(), '</style></head><body><div class="page courriel-typo">',
            html_entete_typo_pcs(champ, FALSE, LT$courriel_note),
            '<div class="deux-cartes"><div><h2>Régions : profil le plus fréquent de leurs départements</h2>', svg(svg_regions_typo(prep, fond_reg, lay_r, FALSE), "Régions"),
            '<ul class="typo-legende">', html_legende_statique_typo_pcs(prep, "region"), '</ul></div>',
            '<div><h2>Départements : profil de la typologie</h2>', svg(c(svg_departements_typo(prep, fond_dep, lay_d, FALSE), html_limites_regions_typo(fond_reg)), "Départements"),
            '<ul class="typo-legende">', html_legende_statique_typo_pcs(prep, "departement"), '</ul></div></div>',
            html_blocs_territoires(prep), html_definitions_profils(prep), html_pied_typologie(champ, source_note), '</div></body></html>')
  if (any(grepl("<script", html, fixed = TRUE)) || any(grepl(" on[a-z]+=", html)) || any(grepl("javascript:", html, fixed = TRUE))) stop("generer_cartes_typologie_pcs_courriel : contenu actif détecté dans la version courriel.")
  dir.create(dirname(fichier_html), showWarnings = FALSE, recursive = TRUE); writeLines(enc2utf8(html), fichier_html, useBytes = TRUE); invisible(fichier_html)
}
