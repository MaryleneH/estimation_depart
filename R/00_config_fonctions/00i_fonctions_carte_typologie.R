# ==============================================================================
# 00i_fonctions_carte_typologie.R — Carte des profils de la typologie (fonctions pures)
# ------------------------------------------------------------------------------
# Rôle : transformer la table de DIFFUSION de la typologie (08f,
#        sorties/typologie_territoriale/diffusion/typologie_departements.csv) en
#        deux pages HTML autonomes (fond local, aucune ressource réseau) :
#          - carte_typologie.html : carte interactive (JavaScript natif inline) :
#            survol = infobulle + panneau « situation » rédigé, clic = épingler,
#            légende cliquable pour isoler un profil ;
#          - carte_typologie_courriel.html : AUCUN contenu actif (SISMEL) : mêmes
#            couleurs écrites en attributs, infobulles natives <title> du SVG,
#            tableau de toutes les situations sous la carte ;
#        + un PNG ggplot2 de la carte (facultatif, jamais bloquant).
# SOURCE ABSOLUE : la table de diffusion (secret appliqué par 08f). Une table
#        sans colonne `masque` (interne/) est refusée ; les mesures et textes
#        d'un département masqué sont effacés AVANT toute sérialisation.
# RÉUTILISE 00f (fond, projection Lambert-93, encarts DROM, CSS, contours,
#        libellés publics, champ) et 00h (titres, définitions des profils).
# Spécification : specs/10 §9 (SPEC-TYPO-060 à 064).
# ==============================================================================
library(dplyr)

# --- Couleurs des profils (catégorielles, distinctes du gris du secret et du blanc) --
COULEURS_PROFILS <- c(enjeu = "#b2182b", emergent = "#ef8a62", majeur_modere = "#2166ac", concentre = "#8c6bb1",
                      stable = "#74c476", diffus = "#dfe6ee", expertiser = "#f2c744")
LIBELLE_NON_DIFFUSE_TYPO <- "Non diffusé (secret statistique)"
LIBELLES_CARTE_TYPO <- list(
  kicker = "Typologie des territoires BITD · résultats diffusables",
  titre = "Profils de renouvellement par département",
  sous_titre = "Chaque département est classé par des règles simples (poids dans l’emploi BITD, volume et intensité des départs attendus d’ici 2030, concentration sur une catégorie).",
  profil_non_diffuse = "Profil non diffusé (secret statistique)",
  absent = "Département absent de la typologie",
  detail_initial = "Survolez un département pour lire sa situation ; cliquez pour la garder affichée.",
  detail_non_diffuse = "Le détail de ce département n’est pas diffusé (secret statistique).",
  tableau_titre = "Le détail par département",
  definitions_titre = "Que signifient les profils ?")
COLONNES_NUM_TYPO <- c("effectif_bitd_total", "effectif_champ_total", "effectif_tous_ages_total", "departs_central", "departs_bas", "departs_haut",
                       "part_emploi_bitd_national_pct", "intensite_renouvellement_pct", "taux_depart_central_pct", "part_a_remplacer_pct",
                       "part_cs_principale_pct", "part_departs_cs_max_pct")
COLONNES_TEXTE_TYPO <- c("profil_typologie", "classe_poids_bitd", "classe_intensite", "classe_volume_departs", "structure_emploi", "cs_principale",
                         "cs_volume_departs_max", "cs_taux_renouvellement_max", "type_concentration", "motif_expertise", "points_attention",
                         "situation_texte", "justification_profil", "indicateur_intensite", "base_poids")

# --- Lecture de la table de diffusion (write.csv2, avec ou sans BOM) --------------
lire_typologie_diffusion <- function(fichier) {
  if (!file.exists(fichier)) stop("Table de diffusion de la typologie introuvable : ", fichier)
  d <- read.csv2(fichier, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character", na.strings = c("NA", ""))
  names(d)[1] <- sub("^﻿", "", names(d)[1])
  for (v in intersect(COLONNES_NUM_TYPO, names(d))) d[[v]] <- as.numeric(sub(",", ".", d[[v]], fixed = TRUE))
  if ("masque" %in% names(d)) d$masque <- toupper(trimws(d$masque)) == "TRUE"
  d
}

# --- Préparation : table de diffusion -> structure de carte (sans aucune fuite) -----
# Statuts : diffuse (profil + chiffres), profil_masque (chiffres diffusés mais profil
# « Non diffusé » : cellule CS masquée), masque (département entier), sans (absent).
preparer_carte_typologie <- function(d, fond, profils = PROFILS_TYPOLOGIE) {
  if (!"masque" %in% names(d)) stop("preparer_carte_typologie : colonne 'masque' absente — seule la table de DIFFUSION est admise (jamais interne/).")
  requis <- c("geo_code", "geo_nom", "profil_typologie", "effectif_bitd_total", "departs_central", "departs_bas", "departs_haut",
              "part_emploi_bitd_national_pct", "intensite_renouvellement_pct")
  manque <- setdiff(requis, names(d)); if (length(manque)) stop("preparer_carte_typologie : colonnes absentes : ", paste(manque, collapse = ", "))
  if (!is.character(d$geo_code)) stop("preparer_carte_typologie : geo_code doit être un texte.")
  d$masque <- as.logical(d$masque); d$masque[is.na(d$masque)] <- TRUE          # doute = masqué
  for (v in setdiff(c(COLONNES_NUM_TYPO, COLONNES_TEXTE_TYPO), names(d))) d[[v]] <- NA
  # SÉCURITÉ : toute mesure et tout texte d'une ligne masquée sont retirés ici
  for (v in c(COLONNES_NUM_TYPO, setdiff(COLONNES_TEXTE_TYPO, "profil_typologie"))) d[[v]][d$masque] <- NA
  d$profil_typologie[d$masque] <- LIBELLE_NON_DIFFUSE_TYPO
  non_loc <- is.na(d$geo_code) | d$geo_code %in% c("inconnu", "")
  n_non_loc <- length(unique(d$geo_code[non_loc])); d <- d[!non_loc, ]
  if (anyDuplicated(d$geo_code) > 0) stop("preparer_carte_typologie : doublons de département.")
  absents <- setdiff(d$geo_code, names(fond))
  if (length(absents) > 0) stop("Carte de la typologie : ", length(absents), " code(s) sans géométrie dans le fond : ", paste(sort(absents), collapse = ", "), " — aucune carte partielle.")
  inconnus <- setdiff(unique(d$profil_typologie), c(unname(profils), LIBELLE_NON_DIFFUSE_TYPO))
  if (length(inconnus) > 0) stop("Carte de la typologie : profil(s) inconnu(s) dans la table : ", paste(inconnus, collapse = " ; "),
                                 " — la table et le code (PROFILS_TYPOLOGIE, 00h) ne sont pas de la même version.")
  d$cle <- names(profils)[match(d$profil_typologie, profils)]
  d$statut <- ifelse(d$masque, "masque", ifelse(is.na(d$cle), "profil_masque", "diffuse"))
  terr <- tibble(code = names(fond), nom = unname(vapply(fond, `[[`, "", "nom"))) |>
    left_join(d |> select(code = geo_code, nom_csv = geo_nom, statut, cle, all_of(c(COLONNES_NUM_TYPO, COLONNES_TEXTE_TYPO))), by = "code") |>
    mutate(nom = coalesce(nom_csv, nom), statut = coalesce(statut, "sans")) |> select(-nom_csv)
  comptes <- c(vapply(names(profils), function(k) sum(terr$cle %in% k), 0L),
               profil_masque = sum(terr$statut == "profil_masque"), masque = sum(terr$statut == "masque"), sans = sum(terr$statut == "sans"))
  premier <- function(v) { x <- terr[[v]][!is.na(terr[[v]])]; if (length(x)) x[1] else NA_character_ }
  list(territoires = terr, comptes = comptes, n_non_localises = n_non_loc, n_csv = nrow(d),
       indicateur = premier("indicateur_intensite"), base_poids = premier("base_poids"), profils = profils)
}
controler_carte_typologie <- function(prep, prefixe = "08g") {
  t <- prep$territoires
  if (sum(t$statut != "sans") != prep$n_csv) stop(prefixe, " : départements du CSV non joints au fond.")
  m <- t$statut == "masque"
  if (any(m) && any(!is.na(as.matrix(t[m, c(COLONNES_NUM_TYPO, setdiff(COLONNES_TEXTE_TYPO, "profil_typologie"))]))))
    stop(prefixe, " : une valeur masquée subsiste dans la structure de carte.")
  cat(sprintf("  typologie x departement : départements CSV %3d | joints %3d | profil non diffusé %3d | masqués %3d | sans donnée %3d%s\n",
              prep$n_csv, sum(t$statut != "sans"), prep$comptes[["profil_masque"]], prep$comptes[["masque"]], prep$comptes[["sans"]],
              if (prep$n_non_localises > 0) sprintf(" | non localisables %d", prep$n_non_localises) else ""))
  invisible(prep$comptes)
}

# --- Détail d'un département : des phrases courtes (infobulle, <title>, tableau) -----
libelle_intensite_typo <- function(indicateur) if (identical(indicateur, "taux_depart_central_pct")) "des salariés concernés susceptibles de partir d’ici 2030" else "de l’emploi actuel à remplacer d’ici 2030"
lignes_detail_typologie <- function(r, indicateur = NULL) {
  if (r$statut == "sans") return(LIBELLES_CARTE_TYPO$absent)
  if (r$statut == "masque") return(LIBELLES_UI$secret)
  f0 <- function(x) fmt_fr_carte(x, 0); f1 <- function(x) fmt_fr_carte(x, 1)
  ind <- if (!is.null(indicateur)) indicateur else r$indicateur_intensite
  l <- c(if (r$statut == "profil_masque") LIBELLES_CARTE_TYPO$profil_non_diffuse else NULL,
         if (!is.na(r$part_emploi_bitd_national_pct)) sprintf("%s %% de l’emploi BITD national · %s salariés", f1(r$part_emploi_bitd_national_pct), f0(r$effectif_bitd_total)) else NULL,
         if (!is.na(r$departs_central)) sprintf("%s départs estimés d’ici 2030 (entre %s et %s)", f0(r$departs_central), f0(r$departs_bas), f0(r$departs_haut)) else NULL,
         if (!is.na(r$intensite_renouvellement_pct)) sprintf("%s %% %s", f1(r$intensite_renouvellement_pct), libelle_intensite_typo(ind)) else NULL,
         if (!is.na(r$classe_poids_bitd)) sprintf("Poids %s · volume %s · intensité %s", tolower(r$classe_poids_bitd), tolower(r$classe_volume_departs), tolower(r$classe_intensite)) else NULL,
         if (!is.na(r$structure_emploi)) { if (r$structure_emploi == "Mixte") sprintf("Emploi mixte (première catégorie : %s, %s %%)", libelle_cs_typo(r$cs_principale), f1(r$part_cs_principale_pct))
                                           else sprintf("Emploi dominé par les %s (%s %%)", libelle_cs_typo(r$cs_principale), f1(r$part_cs_principale_pct)) } else NULL,
         if (!is.na(r$cs_volume_departs_max)) sprintf("Départs portés d’abord par les %s (%s %%%s)", libelle_cs_typo(r$cs_volume_departs_max), f1(r$part_departs_cs_max_pct),
                                                     if (identical(r$type_concentration, "Concentré")) ", concentrés sur cette catégorie" else "") else NULL,
         if (!is.na(r$motif_expertise)) paste0("À examiner : ", r$motif_expertise) else NULL,
         if (!is.na(r$points_attention)) paste0("Point(s) d’attention : ", r$points_attention) else NULL)
  if (length(l) == 0) LIBELLES_CARTE_TYPO$profil_non_diffuse else l
}
titre_profil_typo <- function(r, prep) if (!is.na(r$cle)) prep$profils[[r$cle]] else if (r$statut == "sans") LIBELLES_CARTE_TYPO$absent else LIBELLE_NON_DIFFUSE_TYPO
couleur_territoire_typo <- function(r) if (!is.na(r$cle)) COULEURS_PROFILS[[r$cle]] else if (r$statut == "sans") COULEURS_CARTE$sans_donnee else COULEURS_CARTE$secret

# --- Sérialisation JSON (page interactive) : aucun chiffre d'un département masqué ---
donnees_json_typologie <- function(prep) {
  t <- prep$territoires; defs <- DEFINITIONS_PROFILS
  terr <- setNames(lapply(seq_len(nrow(t)), function(i) { r <- t[i, ]
    x <- list(nom = r$nom, st = r$statut, lignes = as.list(lignes_detail_typologie(r, prep$indicateur)))
    if (!is.na(r$cle)) x$cle <- r$cle
    if (!is.na(r$situation_texte)) x$texte <- r$situation_texte
    x }), t$code)
  profils <- setNames(lapply(names(prep$profils), function(k) list(titre = prep$profils[[k]], couleur = COULEURS_PROFILS[[k]], n = prep$comptes[[k]],
                                                                   signification = defs$signification[defs$cle == k], consigne = defs$attention[defs$cle == k])), names(prep$profils))
  j <- jsonlite::toJSON(list(territoires = terr, profils = profils, comptes = as.list(prep$comptes), libelles = LIBELLES_CARTE_TYPO,
                             couleurs = COULEURS_CARTE[c("secret", "sans_donnee")]), auto_unbox = TRUE, digits = NA, na = "null", null = "null")
  gsub("</", "<\\/", j, fixed = TRUE)
}

# --- CSS et JavaScript propres à la carte de la typologie ---------------------------
css_carte_typologie <- function(S = STYLE_CONTOURS_CARTE) paste(
  '.typo-legende{list-style:none;margin:0;padding:0}.typo-legende li{margin:4px 0}',
  '.typo-legende button,.typo-legende .item{display:flex;align-items:flex-start;gap:10px;width:100%;text-align:left;font:inherit;font-size:15px;color:var(--encre);background:#fff;border:1px solid transparent;border-radius:8px;padding:6px 8px;cursor:pointer}',
  '.typo-legende .item{cursor:default}.typo-legende button:hover,.typo-legende button:focus-visible{border-color:#AEB7C2;outline:none}.typo-legende button[aria-pressed=true]{background:var(--fond);border-color:var(--bleu)}',
  sprintf('.typo-legende .sw{width:22px;height:16px;border-radius:3px;border:1px solid %s;flex:none;margin-top:3px}.typo-legende .sw.sans{border:1.5px dashed %s}', S$couleur, S$sans_couleur),
  '.typo-legende .lib{font-weight:600;line-height:1.3}.typo-legende .n{color:var(--texte);font-weight:500;margin-left:6px}.typo-legende .sig{display:block;font-size:13px;color:var(--texte);font-weight:400;line-height:1.35;margin-top:1px}',
  'path.t.dim{opacity:.22}',
  '.detail{margin:0 0 16px;border:1px solid var(--filet);border-radius:12px;padding:14px 16px;background:var(--fond);min-height:120px}',
  '.detail-titre{font-size:16px;font-weight:700;margin:0 0 6px}.detail-titre .pf{display:inline-block;margin-left:6px;padding:1px 8px;border-radius:999px;font-size:13px;font-weight:600;color:#fff;background:#6b7280}',
  '.detail-texte{font-size:15px;line-height:1.5;margin:0}.detail-aide{font-size:13.5px;color:var(--texte);margin:8px 0 0}',
  '.tooltip-profil{font-weight:700;margin:0 0 8px;padding-left:9px;border-left:4px solid #6b7280;color:#fff}',
  'details.bloc-typo{margin-top:20px;border:1px solid var(--filet);border-radius:12px;padding:0 18px 4px}details.bloc-typo summary{cursor:pointer;font-size:17px;font-weight:700;padding:14px 0;list-style:none}details.bloc-typo summary::-webkit-details-marker{display:none}details.bloc-typo summary::before{content:"▸ ";color:var(--bleu)}details.bloc-typo[open] summary::before{content:"▾ "}',
  '.tab-typo{width:100%;border-collapse:collapse;font-size:14px;margin:0 0 14px}.tab-typo th,.tab-typo td{text-align:left;vertical-align:top;padding:8px 8px;border-top:1px solid var(--filet);line-height:1.45}.tab-typo th{font-size:13px;color:var(--texte);letter-spacing:.03em;text-transform:uppercase;border-top:0}.tab-typo td.num{white-space:nowrap;text-align:right}.tab-typo .sw{display:inline-block;width:12px;height:12px;border-radius:2px;vertical-align:-1px;margin-right:6px;border:1px solid #5B6470}.tab-typo td.sit{min-width:280px}',
  '.tab-defs td:first-child{white-space:nowrap;font-weight:700}@media(max-width:700px){.tab-typo td.sit{min-width:0}.tab-typo th,.tab-typo td{padding:6px 5px}}',
  sprintf('.courriel-typo use{stroke:%s;stroke-width:%s;vector-effect:non-scaling-stroke}.courriel-typo use:hover{stroke:%s;stroke-width:%s}', S$couleur, S$largeur, S$survol_couleur, S$survol_largeur),
  sep = "\n")
js_carte_typologie <- function() paste(
  '(function(){',
  'var sj=document.getElementById("sans-js");if(sj)sj.style.display="none";',
  'var D=JSON.parse(document.getElementById("donnees").textContent),L=D.libelles;var $=function(id){return document.getElementById(id)};',
  'var B=$("bulle"),C=$("carte"),U=$("survol"),DET=$("detail-texte"),DT=$("detail-titre"),DA=$("detail-aide"),pin=null,filtre=null;',
  'function ligne(el,txt,cls){var d=document.createElement("div");if(cls)d.className=cls;d.textContent=txt;el.appendChild(d);return d}',
  'function surligner(code){if(!U)return;var p=code&&$("t-"+code);if(p){U.setAttribute("d",p.getAttribute("d"));U.classList.add("on")}else{U.classList.remove("on");U.setAttribute("d","")}}',
  'function bulle(code){var t=D.territoires[code],el=document.createElement("div");ligne(el,t.nom+" · "+code,"tooltip-title");',
  ' var p=t.cle?D.profils[t.cle]:null;var c=ligne(el,p?p.titre:(t.st==="sans"?L.absent:"Non diffusé (secret statistique)"),"tooltip-profil");c.style.borderLeftColor=p?p.couleur:(t.st==="sans"?D.couleurs.sans_donnee:D.couleurs.secret);',
  ' var b=document.createElement("div");b.className="tooltip-rows";t.lignes.forEach(function(l,i){ligne(b,l,i<3?"tooltip-value":"tooltip-small")});el.appendChild(b);return el.innerHTML}',
  'function detail(code){if(!DET)return;if(!code){DT.textContent="";DET.textContent=L.detail_initial;DA.textContent="";return}var t=D.territoires[code],p=t.cle?D.profils[t.cle]:null;',
  ' DT.innerHTML="";DT.appendChild(document.createTextNode(t.nom+" · "+code));if(p){var s=document.createElement("span");s.className="pf";s.textContent=p.titre;s.style.background=p.couleur;s.style.color=(t.cle==="diffus"||t.cle==="expertiser"||t.cle==="stable")?"#1F2933":"#fff";DT.appendChild(s)}',
  ' DET.textContent=t.texte?t.texte:(t.st==="sans"?L.absent:(t.st==="masque"?L.detail_non_diffuse:t.lignes.join(" · ")));DA.textContent=pin===code?"Épinglé : cliquez de nouveau pour libérer.":(p?"Consigne de lecture : "+p.consigne+".":"")}',
  'function appliquerFiltre(){document.querySelectorAll("path.t").forEach(function(p){p.classList.toggle("dim",!!filtre&&p.getAttribute("data-cle")!==filtre)});document.querySelectorAll(".typo-legende button").forEach(function(b){b.setAttribute("aria-pressed",b.getAttribute("data-cle")===filtre?"true":"false")})}',
  'if(C){document.querySelectorAll("path.t").forEach(function(p){var code=p.getAttribute("data-code");',
  ' p.addEventListener("mousemove",function(ev){B.innerHTML=bulle(code);B.style.display="block";surligner(code);if(!pin)detail(code);var r=C.getBoundingClientRect();var x=ev.clientX-r.left+16,y=ev.clientY-r.top+16;if(x+330>r.width)x-=346;if(y+220>r.height)y-=230;B.style.left=Math.max(0,x)+"px";B.style.top=Math.max(0,y)+"px"});',
  ' p.addEventListener("mouseleave",function(){B.style.display="none";surligner(pin);if(pin)detail(pin);else detail(null)});',
  ' p.addEventListener("click",function(){pin=(pin===code)?null:code;surligner(pin);detail(pin||code)});',
  ' p.addEventListener("keydown",function(ev){if(ev.key==="Enter"||ev.key===" "){ev.preventDefault();pin=(pin===code)?null:code;detail(pin||code)}});',
  ' p.addEventListener("focus",function(){B.innerHTML=bulle(code);B.style.display="block";surligner(code);if(!pin)detail(code);B.style.left="12px";B.style.top="12px"});p.addEventListener("blur",function(){B.style.display="none";surligner(pin)})})}',
  'document.querySelectorAll(".typo-legende button").forEach(function(b){b.addEventListener("click",function(){var k=b.getAttribute("data-cle");filtre=(filtre===k)?null:k;appliquerFiltre()})});',
  'detail(null);',
  '})();', sep = "\n")

# --- Briques HTML --------------------------------------------------------------------
html_entete_typologie <- function(prep, champ, bandeau = TRUE, sous_qui = NULL) {
  P <- construire_libelles_public(champ); LT <- LIBELLES_CARTE_TYPO
  base <- if (identical(prep$base_poids, "effectif_champ")) paste0("aux ", P$salaries_age_min, " des entreprises du périmètre BITD") else "à l’emploi actuel, tous âges, des entreprises du périmètre BITD"
  c('<header class="page-header">', sprintf('<p class="kicker">%s</p>', echap_html_carte(LT$kicker)),
    sprintf('<h1 class="page-title" id="titre">%s<small>Départs attendus d’ici 2030 · par département</small></h1>', echap_html_carte(LT$titre)),
    sprintf('<p class="page-subtitle"><span class="cat-qui">%s</span></p>', echap_html_carte(if (is.null(sous_qui)) LT$sous_titre else sous_qui)),
    if (bandeau) html_bandeau_sans_js() else "",
    '<div class="study-scope" role="note" aria-label="Qui est concerné">',
    sprintf('<span class="scope-text"><b>%s</b> · entreprises du périmètre BITD</span>', echap_html_carte(P$salaries_age)),
    sprintf('<span class="scope-text">Poids et intensité rapportés %s.</span>', echap_html_carte(base)),
    '</div>',
    '<p class="context">Les couleurs donnent le profil ; les zones grisées correspondent à un profil ou un département non diffusé (secret statistique) ; les zones blanches sont absentes de la typologie.</p>',
    '</header>')
}
html_legende_typologie <- function(prep, interactive = TRUE) {
  defs <- DEFINITIONS_PROFILS; S <- STYLE_CONTOURS_CARTE
  item <- function(col, lib, n, sig = NULL, cle = NULL, sans = FALSE) {
    inner <- sprintf('<span class="sw%s" style="background:%s"></span><span><span class="lib">%s<span class="n">%d</span></span>%s</span>',
                     if (sans) " sans" else "", col, echap_html_carte(lib), n, if (!is.null(sig)) sprintf('<span class="sig">%s</span>', echap_html_carte(sig)) else "")
    if (interactive && !is.null(cle)) sprintf('<li><button type="button" data-cle="%s" aria-pressed="false">%s</button></li>', cle, inner) else sprintf('<li><span class="item">%s</span></li>', inner)
  }
  c('<div class="legend-title">Profils (nombre de départements)</div><ul class="typo-legende" id="legende">',
    vapply(names(prep$profils), function(k) item(COULEURS_PROFILS[[k]], prep$profils[[k]], prep$comptes[[k]], defs$signification[defs$cle == k], k), ""),
    item(COULEURS_CARTE$secret, LIBELLES_CARTE_TYPO$profil_non_diffuse, prep$comptes[["profil_masque"]] + prep$comptes[["masque"]]),
    item(COULEURS_CARTE$sans_donnee, LIBELLES_CARTE_TYPO$absent, prep$comptes[["sans"]], sans = TRUE), '</ul>',
    if (interactive) '<p class="note">Cliquez un profil pour l’isoler sur la carte ; cliquez de nouveau pour tout afficher.</p>' else "")
}
html_definitions_profils <- function(prep, ouvert = FALSE) {
  defs <- DEFINITIONS_PROFILS
  lignes <- vapply(names(prep$profils), function(k) { r <- defs[defs$cle == k, ]
    sprintf('<tr><td><span class="sw" style="background:%s"></span>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>', COULEURS_PROFILS[[k]],
            echap_html_carte(prep$profils[[k]]), echap_html_carte(r$regle), echap_html_carte(r$signification), echap_html_carte(r$attention)) }, "")
  c(sprintf('<details class="bloc-typo"%s><summary>%s</summary>', if (ouvert) " open" else "", echap_html_carte(LIBELLES_CARTE_TYPO$definitions_titre)),
    '<table class="tab-typo tab-defs"><thead><tr><th>Profil</th><th>Règle</th><th>Ce que cela signifie</th><th>Consigne de lecture</th></tr></thead><tbody>', lignes, '</tbody></table></details>')
}
html_tableau_typologie <- function(prep, ouvert = FALSE) {
  t <- prep$territoires; t <- t[t$statut != "sans", ]
  ordre <- match(t$cle, names(prep$profils)); ordre[is.na(ordre)] <- length(prep$profils) + 1
  t <- t[order(ordre, -ifelse(is.na(t$departs_central), -1, t$departs_central), t$code), ]
  f0 <- function(x) fmt_fr_carte(x, 0); f1 <- function(x) fmt_fr_carte(x, 1)
  lignes <- vapply(seq_len(nrow(t)), function(i) { r <- t[i, ]
    if (r$statut == "masque") return(sprintf('<tr><td>%s · %s</td><td colspan="4"><span class="sw" style="background:%s"></span>%s</td></tr>', echap_html_carte(r$nom), r$code, COULEURS_CARTE$secret, LIBELLES_UI$secret))
    sit <- if (!is.na(r$situation_texte)) r$situation_texte else paste(lignes_detail_typologie(r, prep$indicateur), collapse = " ")
    sprintf('<tr><td>%s · %s</td><td><span class="sw" style="background:%s"></span>%s</td><td class="num">%s</td><td class="num">%s<br><small>%s à %s</small></td><td class="num">%s %%</td><td class="sit">%s</td></tr>',
            echap_html_carte(r$nom), r$code, couleur_territoire_typo(r), echap_html_carte(titre_profil_typo(r, prep)),
            f0(r$effectif_bitd_total), f0(r$departs_central), f0(r$departs_bas), f0(r$departs_haut), f1(r$intensite_renouvellement_pct), echap_html_carte(sit)) }, "")
  c(sprintf('<details class="bloc-typo"%s><summary>%s</summary>', if (ouvert) " open" else "", echap_html_carte(LIBELLES_CARTE_TYPO$tableau_titre)),
    sprintf('<table class="tab-typo"><thead><tr><th>Département</th><th>Profil</th><th>Salariés</th><th>Départs d’ici 2030</th><th>Intensité</th><th>Situation</th></tr></thead><tbody>'),
    lignes, '</tbody></table>',
    sprintf('<p class="note">Salariés : base du poids. Intensité : part %s. Départs : estimation centrale et fourchette basse – haute.</p>', echap_html_carte(libelle_intensite_typo(prep$indicateur))), '</details>')
}
html_pied_typologie <- function(champ, source_note) {
  sprintf(paste0('<footer><b>Qui est concerné ?</b> %s ',
                 '<b>Profils.</b> Attribués par des règles ordonnées et paramétrées (specs/10) ; une « situation à expertiser » n’est pas un défaut de données mais un cas que les règles simples ne tranchent pas. ',
                 '<b>Secret statistique.</b> Un département trop petit, ou dont une catégorie est protégée, n’a pas de profil diffusé ; ses chiffres ne figurent ni dans cette page ni dans ses données. ',
                 '<b>Source.</b> %s. Fond de carte : IGN Admin Express (COG 2018) via france-geojson, licence ouverte.</footer>'),
          echap_html_carte(paste(champ$detaille, collapse = " ")), echap_html_carte(source_note))
}
html_encarts_typo <- function(lay) vapply(lay$encarts, function(e) sprintf('<rect class="encart" fill="none" stroke="#c9ced6" x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="6"></rect><text class="encart-lib" fill="#3E4C59" x="%.1f" y="%.1f">%s</text>',
                                                                       e$cadre[1] + 1, e$cadre[2] + 1, e$cadre[3] - e$cadre[1] - 2, e$cadre[4] - e$cadre[2] - 2, e$x_lib, e$y_lib, echap_html_carte(e$nom)), "")
html_limites_regions_typo <- function(fond_regions, S = STYLE_CONTOURS_CARTE) {
  if (is.null(fond_regions)) return("")
  lr <- projeter_fond(fond_regions); metro <- setdiff(names(fond_regions), names(lr$encarts))
  c('<g class="limites-reg" aria-hidden="true">', sprintf('<path fill="none" stroke="%s" stroke-width="%s" d="%s"></path>', S$region_couleur, S$region_largeur, lr$chemins[metro]), '</g>')
}

# --- Page interactive -----------------------------------------------------------------
generer_carte_typologie <- function(prep, fond, fichier_html, champ = construire_libelle_champ(), fond_regions = NULL,
                                    source_note = "calculs propres", fichier_png = NULL) {
  lay <- projeter_fond(fond); t <- prep$territoires; S <- STYLE_CONTOURS_CARTE
  titre_page <- paste0(LIBELLES_CARTE_TYPO$titre, " — départs attendus d’ici 2030")
  paths <- vapply(names(fond), function(code) { r <- t[t$code == code, ]; sans <- r$statut == "sans"
    sprintf('<path class="t%s" id="t-%s" data-code="%s" data-st="%s"%s fill="%s" stroke="%s" stroke-width="%s"%s d="%s" tabindex="0" aria-label="%s"></path>',
            if (sans) " sans" else "", code, code, r$statut, if (!is.na(r$cle)) sprintf(' data-cle="%s"', r$cle) else "", couleur_territoire_typo(r),
            if (sans) S$sans_couleur else S$couleur, if (sans) S$sans_largeur else S$largeur, if (sans) ' stroke-dasharray="3 2"' else "",
            lay$chemins[[code]], echap_html_carte(paste0(r$nom, " — ", titre_profil_typo(r, prep)))) }, "")
  html <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">', sprintf('<title>%s</title>', echap_html_carte(titre_page)),
            '<meta name="viewport" content="width=device-width, initial-scale=1">', '<style>', css_cartes(), css_carte_typologie(), '</style></head><body><div class="page">',
            html_entete_typologie(prep, champ, bandeau = TRUE),
            '<div class="grille"><aside class="panneau">',
            sprintf('<div class="detail" id="detail" role="status"><p class="detail-titre" id="detail-titre"></p><p class="detail-texte" id="detail-texte">%s</p><p class="detail-aide" id="detail-aide"></p></div>', echap_html_carte(LIBELLES_CARTE_TYPO$detail_initial)),
            html_legende_typologie(prep, interactive = TRUE),
            if (length(lay$encarts) > 0) '<p class="note">Outre-mer en encarts, chacun à sa propre échelle.</p>' else "",
            '</aside><div class="carte" id="carte">',
            sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">', lay$largeur, lay$hauteur, echap_html_carte(titre_page)),
            html_encarts_typo(lay), paths, html_limites_regions_typo(fond_regions), '<path id="survol" fill="none" d="" aria-hidden="true"></path>',
            '</svg><div class="bulle" id="bulle" role="status"></div></div></div>',
            html_definitions_profils(prep), html_tableau_typologie(prep), html_pied_typologie(champ, source_note),
            '<script type="application/json" id="donnees">', donnees_json_typologie(prep), '</script>', '<script>', js_carte_typologie(), '</script>', '</div></body></html>')
  dir.create(dirname(fichier_html), showWarnings = FALSE, recursive = TRUE)
  writeLines(enc2utf8(html), fichier_html, useBytes = TRUE)
  if (!is.null(fichier_png))
    tryCatch(png_carte_typologie(prep, lay, fichier_png, champ, lay_regions = if (!is.null(fond_regions)) projeter_fond(fond_regions) else NULL),
             error = function(e) message("08g : PNG non produit (", conditionMessage(e), ") — le HTML reste la restitution de référence."))
  invisible(fichier_html)
}

# --- Version COURRIEL : aucun script, aucun contenu actif ----------------------------
# Couleurs en attributs, infobulles = <title> natifs du SVG (une par département :
# nom, profil, chiffres, motif, points d'attention), tableau des situations ouvert
# sous la carte, définitions des profils. Même structure que la page interactive.
generer_carte_typologie_courriel <- function(prep, fond, fichier_html, champ = construire_libelle_champ(), fond_regions = NULL, source_note = "calculs propres") {
  lay <- projeter_fond(fond); t <- prep$territoires; S <- STYLE_CONTOURS_CARTE
  titre_page <- paste0(LIBELLES_CARTE_TYPO$titre, " — départs attendus d’ici 2030")
  uses <- vapply(names(fond), function(code) { r <- t[t$code == code, ]; sans <- r$statut == "sans"
    sprintf('<path fill="%s" stroke="%s" stroke-width="%s"%s d="%s"><title>%s</title></path>', couleur_territoire_typo(r),
            if (sans) S$sans_couleur else S$couleur, if (sans) S$sans_largeur else S$largeur, if (sans) ' stroke-dasharray="3 2"' else "", lay$chemins[[code]],
            echap_html_carte(paste(c(paste0(r$nom, " · ", code), titre_profil_typo(r, prep), lignes_detail_typologie(r, prep$indicateur)), collapse = "\n"))) }, "")
  html <- c('<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8">', sprintf('<title>%s</title>', echap_html_carte(titre_page)),
            '<meta name="viewport" content="width=device-width, initial-scale=1">', '<style>', css_cartes(), css_carte_typologie(), '</style></head><body><div class="page courriel-typo">',
            html_entete_typologie(prep, champ, bandeau = FALSE, sous_qui = "Version pour envoi par courriel, sans script : survolez un département pour lire ses chiffres ; le détail de chaque situation est sous la carte."),
            '<div class="grille"><aside class="panneau">', html_legende_typologie(prep, interactive = FALSE),
            if (length(lay$encarts) > 0) '<p class="note">Outre-mer en encarts, chacun à sa propre échelle.</p>' else "",
            '</aside><div class="carte">',
            sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">', lay$largeur, lay$hauteur, echap_html_carte(titre_page)),
            html_encarts_typo(lay), uses, html_limites_regions_typo(fond_regions), '</svg></div></div>',
            html_definitions_profils(prep, ouvert = TRUE), html_tableau_typologie(prep, ouvert = TRUE), html_pied_typologie(champ, source_note), '</div></body></html>')
  if (any(grepl("<script", html, fixed = TRUE)) || any(grepl(" on[a-z]+=", html)) || any(grepl("javascript:", html, fixed = TRUE)))
    stop("generer_carte_typologie_courriel : contenu actif détecté dans la version courriel.")
  dir.create(dirname(fichier_html), showWarnings = FALSE, recursive = TRUE)
  writeLines(enc2utf8(html), fichier_html, useBytes = TRUE)
  invisible(fichier_html)
}

# --- PNG (ggplot2) : même palette, mêmes contours que les pages HTML ----------------------
png_carte_typologie <- function(prep, lay, fichier_png, champ = construire_libelle_champ(), lay_regions = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 absent")
  S <- STYLE_CONTOURS_CARTE; t <- prep$territoires
  niveaux <- c(unname(prep$profils), LIBELLES_CARTE_TYPO$profil_non_diffuse, LIBELLES_CARTE_TYPO$absent)
  cols <- setNames(c(unname(COULEURS_PROFILS[names(prep$profils)]), COULEURS_CARTE$secret, COULEURS_CARTE$sans_donnee), niveaux)
  lib <- vapply(seq_len(nrow(t)), function(i) { r <- t[i, ]; if (!is.na(r$cle)) prep$profils[[r$cle]] else if (r$statut == "sans") LIBELLES_CARTE_TYPO$absent else LIBELLES_CARTE_TYPO$profil_non_diffuse }, "")
  long <- lay$long; long$classe <- factor(lib[match(long$code, t$code)], levels = niveaux)
  enc <- if (length(lay$encarts)) bind_rows(lapply(lay$encarts, function(e) tibble(x1 = e$cadre[1], x2 = e$cadre[3], y1 = -e$cadre[2], y2 = -e$cadre[4], lib = e$nom, xl = e$x_lib, yl = -e$y_lib))) else NULL
  g <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = long, ggplot2::aes(x = x, y = y, group = groupe, fill = classe), colour = S$png_couleur, linewidth = S$png_largeur) +
    ggplot2::scale_fill_manual(values = cols, drop = FALSE, name = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = S$png_couleur), ncol = 2)) +
    ggplot2::coord_equal(expand = FALSE) + ggplot2::theme_void(base_size = 12) +
    ggplot2::labs(title = LIBELLES_CARTE_TYPO$titre, subtitle = paste0("Départs attendus d’ici 2030 · ", toupper(substr(champ$court, 1, 1)), substr(champ$court, 2, nchar(champ$court))),
                  caption = "Gris : profil ou département non diffusé (secret statistique) · blanc : absent de la typologie.\nFond IGN Admin Express via france-geojson.") +
    ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 10),
                   plot.title = ggplot2::element_text(face = "bold", size = 15, colour = COULEURS_CARTE$encre),
                   plot.subtitle = ggplot2::element_text(colour = COULEURS_CARTE$texte, size = 11),
                   plot.caption = ggplot2::element_text(colour = COULEURS_CARTE$texte, size = 9), plot.margin = ggplot2::margin(10, 10, 10, 10))
  if (!is.null(lay_regions)) {
    reg <- lay_regions$long[!(lay_regions$long$code %in% names(lay_regions$encarts)), ]
    g <- g + ggplot2::geom_polygon(data = reg, ggplot2::aes(x = x, y = y, group = groupe), fill = NA, colour = S$region_couleur, linewidth = S$png_region_largeur)
  }
  if (!is.null(enc)) g <- g + ggplot2::geom_rect(data = enc, ggplot2::aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1), fill = NA, colour = "#c9ced6") +
    ggplot2::geom_text(data = enc, ggplot2::aes(x = xl, y = yl, label = lib), hjust = 0, size = 3.6, colour = COULEURS_CARTE$texte)
  ggplot2::ggsave(fichier_png, g, width = 8, height = 11, dpi = 130, bg = "white")
  invisible(fichier_png)
}
