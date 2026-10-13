// Schémas « façon dessin à la main » de la note « Comprendre les nomenclatures PCS, CS, FAP, ROME » (SVG -> PNG via Chromium).
// Usage : node gen_schemas.js <dossier_sortie>   puis   node ../note_pedagogique/render_png.js <dossier>/*.svg
// Codes et libellés : référentiels officiels (Insee PCS 2020, PCS-ESE 2017 ; Dares FAP ; France Travail ROME), voir la note.
const fs = require("fs"), path = require("path");
const OUT = process.argv[2] || __dirname;
const NAVY = "#1E3A5F", ENCRE = "#2B2B2B", GRIS = "#7A8795", CLAIR = "#E9EEF3", FOND = "#F6F8FA", ORANGE = "#D97706", ORANGE_CLAIR = "#FDF1E1", BLEU_CLAIR = "#E3ECF6", VERT = "#2E7D5B", VERT_CLAIR = "#E6F2EC";
const HAND = 'font-family="Happy Monkey, Comic Neue, Humor Sans, sans-serif"';
const K = 3;
let seed = 11; const rnd = () => { seed = (seed * 9301 + 49297) % 233280; return seed / 233280 - 0.5; };
const j = (v, a = 1.6) => (v + rnd() * a).toFixed(1);
function box(x, y, w, h, o = {}) {
  const pass = (d) => `M${j(x + d)} ${j(y)} Q${j(x + w / 2)} ${j(y - 1 + d)} ${j(x + w)} ${j(y + d)} Q${j(x + w + 1)} ${j(y + h / 2)} ${j(x + w - d)} ${j(y + h)} Q${j(x + w / 2)} ${j(y + h + 1 - d)} ${j(x)} ${j(y + h - d)} Q${j(x - 1)} ${j(y + h / 2)} ${j(x + d)} ${j(y)} Z`;
  const fill = o.fill || "#fff", stroke = o.stroke || ENCRE, sw = o.sw || 1.6;
  return `<path d="${pass(0)}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}" stroke-linejoin="round"${o.dash ? ' stroke-dasharray="6 4"' : ""}/><path d="${pass(0.9)}" fill="none" stroke="${stroke}" stroke-width="${sw * 0.6}" opacity=".55"${o.dash ? ' stroke-dasharray="6 4"' : ""}/>`;
}
function ellipse(cx, cy, rx, ry, o = {}) {
  const p = []; for (let a = 0; a <= 360; a += 20) { const t = a * Math.PI / 180; p.push(`${(cx + rx * Math.cos(t) + rnd() * 1.4).toFixed(1)} ${(cy + ry * Math.sin(t) + rnd() * 1.4).toFixed(1)}`); }
  return `<path d="M${p.join(" L")} Z" fill="${o.fill || "#fff"}" stroke="${o.stroke || ENCRE}" stroke-width="${o.sw || 1.6}" stroke-linejoin="round"/>`;
}
function arrow(x1, y1, x2, y2, o = {}) {
  const mx = (x1 + x2) / 2 + rnd() * 6, my = (y1 + y2) / 2 + rnd() * 6, col = o.stroke || ENCRE, sw = o.sw || 1.7;
  const ang = Math.atan2(y2 - my, x2 - mx), L = 9;
  const head = (x, y, a) => `<path d="M${(x - L * Math.cos(a - 0.45)).toFixed(1)} ${(y - L * Math.sin(a - 0.45)).toFixed(1)} L${x} ${y} L${(x - L * Math.cos(a + 0.45)).toFixed(1)} ${(y - L * Math.sin(a + 0.45)).toFixed(1)}" fill="none" stroke="${col}" stroke-width="${sw}" stroke-linecap="round" stroke-linejoin="round"/>`;
  let s = `<path d="M${x1} ${y1} Q${mx.toFixed(1)} ${my.toFixed(1)} ${x2} ${y2}" fill="none" stroke="${col}" stroke-width="${sw}" stroke-linecap="round"${o.dash ? ' stroke-dasharray="5 4"' : ""}/>` + head(x2, y2, ang);
  if (o.double) s += head(x1, y1, Math.atan2(y1 - my, x1 - mx));
  return s;
}
const T = (x, y, t, s = 13, o = {}) => `<text x="${x}" y="${y}" font-size="${s}" fill="${o.fill || ENCRE}" ${o.anchor ? `text-anchor="${o.anchor}"` : ""} ${o.bold ? 'font-weight="bold"' : ""}${o.rot ? ` transform="rotate(${o.rot} ${x} ${y})"` : ""}>${t}</text>`;
const lines = (x, y, arr, s = 12, lh = 15, o = {}) => arr.map((t, i) => T(x, y + i * lh, t, s, o)).join("");
const svg = (w, h, body) => `<svg xmlns="http://www.w3.org/2000/svg" width="${w * K}" height="${h * K}" viewBox="0 0 ${w} ${h}" ${HAND}><rect width="${w}" height="${h}" fill="#fff"/>${body}</svg>`;
const out = {};

// ---------------------------------------------------------------- 1. Un emploi, quatre regards
{ const w = 760, h = 345; let b = "";
  b += ellipse(380, 165, 132, 46, { fill: ORANGE_CLAIR, stroke: ORANGE, sw: 2 }) + T(380, 160, "Un même emploi", 15, { anchor: "middle", bold: true, fill: ORANGE }) + T(380, 180, "technicienne de maintenance dans une usine", 10.5, { anchor: "middle" });
  const regard = (x, y, q, n, r, c) => { b += box(x, y, 230, 92, { fill: c }) + T(x + 115, y + 22, q, 11.5, { anchor: "middle", fill: GRIS }) + T(x + 115, y + 46, n, 16, { anchor: "middle", bold: true, fill: NAVY }) + lines(x + 115, y + 66, r, 11, 14, { anchor: "middle" }); };
  regard(20, 20, "Quelle profession, quelle position ?", "PCS", ["classement socioprofessionnel", "Insee"], BLEU_CLAIR);
  regard(510, 20, "À quel niveau de regroupement ?", "niveaux de la PCS", ["du groupe (1 chiffre)", "à la profession (4 caractères)"], BLEU_CLAIR);
  regard(20, 218, "Quelle famille de métiers ?", "FAP", ["compétences et activités proches", "Dares"], VERT_CLAIR);
  regard(510, 218, "Quel poste recherché ou proposé ?", "ROME", ["fiches métiers, offres et demandes", "France Travail"], VERT_CLAIR);
  b += arrow(300, 130, 250, 100) + arrow(460, 130, 510, 100) + arrow(300, 200, 250, 230) + arrow(460, 200, 510, 230);
  b += T(380, 334, "quatre questions différentes : quatre façons de classer, pas quatre niveaux d’une même liste", 11.5, { anchor: "middle", fill: ORANGE });
  out["n1_quatre_regards"] = svg(w, h, b); }

// ---------------------------------------------------------------- 2. Zoom dans la PCS 2020 (exemple vérifié)
{ const w = 760, h = 330; let b = "";
  const niv = (i, x, y, wd, et, code, lib, n, c) => { b += box(x, y, wd, 56, { fill: c }) + T(x + 12, y + 18, et, 10.5, { fill: GRIS }) + T(x + 12, y + 40, code + " · " + lib, 13.5, { bold: true, fill: NAVY });
    b += T(735, y + 34, n, 12.5, { anchor: "end", fill: GRIS }); if (i < 4) { b += arrow(x + 40, y + 60, x + 40 + 22, y + 74) + T(x + 72, y + 72, "zoom", 11, { fill: ORANGE }); } };
  niv(1, 20, 14, 470, "NIVEAU 1 · groupe socioprofessionnel · 1 chiffre", "4", "Professions intermédiaires", "6 groupes", BLEU_CLAIR);
  niv(2, 42, 88, 448, "NIVEAU 2 · catégorie socioprofessionnelle · 2 chiffres", "47", "Techniciens / Techniciennes", "29 catégories", "#fff");
  niv(3, 64, 162, 426, "NIVEAU 3 · profession regroupée · 3 caractères", "47D", "Techniciens des fonctions connexes de la production", "121 professions regroupées", "#fff");
  niv(4, 86, 236, 404, "NIVEAU 4 · profession · 4 caractères", "47D3", "Techniciens d’installation et de maintenance (hors informatique)", "311 professions", "#fff");
  b += T(735, 20, "PCS 2020 (Insee)", 12, { fill: GRIS, anchor: "end" });
  b += `<path d="M${j(530)} 14 L${j(530)} 290" stroke="${GRIS}" stroke-width="1.2" stroke-dasharray="4 4"/>`;
  b += T(380, 318, "un seul code à 4 caractères contient les trois niveaux au-dessus de lui : une hiérarchie emboîtée", 11.5, { anchor: "middle", fill: ORANGE });
  out["n2_zoom_pcs2020"] = svg(w, h, b); }

// ---------------------------------------------------------------- 3. Même intitulé, deux codes possibles (statut)
{ const w = 760, h = 210; let b = "";
  b += box(20, 62, 220, 86, { fill: ORANGE_CLAIR, stroke: ORANGE }) + T(130, 90, "« technicien de maintenance", 12.5, { anchor: "middle", bold: true }) + T(130, 108, "d’équipements industriels »", 12.5, { anchor: "middle", bold: true }) + T(130, 132, "l’intitulé seul ne suffit pas", 11, { anchor: "middle", fill: ORANGE });
  b += arrow(246, 92, 296, 52) + arrow(246, 118, 296, 158);
  const cas = (y, cond, code, lib, grp) => b += box(300, y, 440, 76, { fill: BLEU_CLAIR }) + T(314, y + 19, cond, 10.5, { fill: ORANGE }) + T(314, y + 41, code, 13, { bold: true, fill: NAVY }) + T(314, y + 57, lib, 11, { fill: NAVY }) + T(314, y + 70, grp, 10.5, { fill: GRIS });
  cas(14, "si l’emploi est classé « technicien » dans la convention collective :", "47D3 · Techniciens d’installation et de maintenance", "(hors informatique)", "groupe 4 · professions intermédiaires");
  cas(120, "si l’emploi est classé « ouvrier qualifié » dans la convention collective :", "62E5 · Ouvriers qualifiés de maintenance et d’entretien", "des équipements industriels", "groupe 6 · ouvriers");
  out["n3_statut_deux_codes"] = svg(w, h, b); }

// ---------------------------------------------------------------- 4. Hiérarchie emboîtée (A) vs table de passage (B)
{ const w = 760, h = 300; let b = "";
  b += T(190, 22, "A · À l’intérieur de la PCS : emboîtement", 13.5, { anchor: "middle", bold: true, fill: NAVY });
  const nd = (x, y, t, wd = 70, c = "#fff") => b += box(x - wd / 2, y - 14, wd, 28, { fill: c }) + T(x, y + 5, t, 12, { anchor: "middle", bold: true });
  nd(190, 60, "4", 50, BLEU_CLAIR); nd(120, 120, "41", 50); nd(190, 120, "47", 50, BLEU_CLAIR); nd(260, 120, "48", 50);
  nd(150, 180, "47C", 56); nd(230, 180, "47D", 56, BLEU_CLAIR); nd(170, 240, "47D1", 56); nd(230, 240, "47D3", 56, BLEU_CLAIR); nd(290, 240, "47D2", 56);
  for (const [x1, y1, x2, y2] of [[190, 74, 120, 106], [190, 74, 190, 106], [190, 74, 260, 106], [190, 134, 150, 166], [190, 134, 230, 166], [230, 194, 170, 226], [230, 194, 230, 226], [230, 194, 290, 226]]) b += arrow(x1, y1, x2, y2, { stroke: GRIS, sw: 1.3 });
  b += T(190, 284, "chaque code n’a qu’un seul parent : on remonte sans ambiguïté", 11, { anchor: "middle", fill: VERT });
  b += `<path d="M${j(385)} 14 L${j(385)} 290" stroke="${GRIS}" stroke-width="1.2" stroke-dasharray="4 4"/>`;
  b += T(575, 22, "B · Entre PCS et FAP : table de passage", 13.5, { anchor: "middle", bold: true, fill: NAVY });
  const L = (y, t, c) => b += box(410, y - 14, 120, 28, { fill: c || "#fff" }) + T(470, y + 5, t, 11.5, { anchor: "middle", bold: true });
  const R = (y, t, c) => b += box(620, y - 14, 120, 28, { fill: c || "#fff" }) + T(680, y + 5, t, 11.5, { anchor: "middle", bold: true });
  T; b += T(470, 48, "codes PCS", 11, { anchor: "middle", fill: GRIS }) + T(680, 48, "familles FAP", 11, { anchor: "middle", fill: GRIS });
  L(80, "PCS a"); L(130, "PCS b", BLEU_CLAIR); L(180, "PCS c"); L(230, "PCS d");
  R(80, "FAP 1"); R(130, "FAP 2", VERT_CLAIR); R(180, "FAP 3", VERT_CLAIR); R(230, "FAP 4");
  b += arrow(532, 80, 618, 80, { stroke: GRIS, sw: 1.3 });                                   // un -> un
  b += arrow(532, 130, 618, 130, { stroke: VERT, sw: 1.6 }) + arrow(532, 134, 618, 176, { stroke: VERT, sw: 1.6, dash: true });   // un -> plusieurs
  b += arrow(532, 180, 618, 184, { stroke: GRIS, sw: 1.3 }) + arrow(532, 230, 618, 230, { stroke: GRIS, sw: 1.3 });   // plusieurs -> un ...
  b += T(560, 112, "?", 16, { anchor: "middle", bold: true, fill: ORANGE }) + T(545, 208, "selon le libellé de profession", 10, { anchor: "middle", fill: ORANGE });
  b += T(575, 270, "un code peut se répartir entre plusieurs familles,", 11, { anchor: "middle", fill: ORANGE }) + T(575, 284, "plusieurs codes peuvent nourrir une même famille", 11, { anchor: "middle", fill: ORANGE });
  out["n4_emboitee_vs_passage"] = svg(w, h, b); }

// ---------------------------------------------------------------- 5. PCS <-> FAP <-> ROME
{ const w = 760, h = 230; let b = "";
  b += box(20, 60, 200, 96, { fill: BLEU_CLAIR }) + T(120, 86, "PCS", 18, { anchor: "middle", bold: true, fill: NAVY }) + lines(120, 108, ["Insee", "professions exercées,", "position socioprofessionnelle"], 11, 14, { anchor: "middle" });
  b += ellipse(380, 108, 108, 50, { fill: VERT_CLAIR, stroke: VERT }) + T(380, 102, "FAP", 18, { anchor: "middle", bold: true, fill: VERT }) + lines(380, 122, ["Dares", "familles de métiers"], 11, 14, { anchor: "middle" });
  b += box(540, 60, 200, 96, { fill: BLEU_CLAIR }) + T(640, 86, "ROME", 18, { anchor: "middle", bold: true, fill: NAVY }) + lines(640, 108, ["France Travail", "fiches métiers,", "offres et demandes d’emploi"], 11, 14, { anchor: "middle" });
  b += arrow(226, 108, 268, 108, { double: true }) + arrow(492, 108, 534, 108, { double: true });
  b += T(247, 90, "table de", 10, { anchor: "middle", fill: GRIS }) + T(247, 134, "passage", 10, { anchor: "middle", fill: GRIS }) + T(513, 90, "table de", 10, { anchor: "middle", fill: GRIS }) + T(513, 134, "passage", 10, { anchor: "middle", fill: GRIS });
  b += T(380, 30, "un pont entre deux univers, pas une traduction mot à mot", 12.5, { anchor: "middle", fill: ORANGE });
  b += T(380, 190, "les flèches ne sont ni univoques ni toujours réversibles : la table dit, cas par cas, ce qui va où", 11.5, { anchor: "middle", fill: GRIS });
  b += T(380, 212, "et chaque table ne vaut que pour un couple de versions (PCS 2020 ↔ FAP 2021 ; ROME v3 ↔ FAP 2009…)", 11.5, { anchor: "middle", fill: GRIS });
  out["n5_pont_pcs_fap_rome"] = svg(w, h, b); }

// ---------------------------------------------------------------- 6. Les versions
{ const w = 760, h = 250; let b = "";
  const ligne = (y, nom, etapes, c) => { b += T(20, y + 5, nom, 13, { bold: true, fill: NAVY }); etapes.forEach(([x, t, s, strong], i) => { b += box(x, y - 16, 150, 34, { fill: strong ? c : "#fff", stroke: strong ? NAVY : ENCRE }) + T(x + 75, y - 1, t, 12, { anchor: "middle", bold: !!strong }) + T(x + 75, y + 12, s, 9.5, { anchor: "middle", fill: GRIS }); if (i) b += arrow(x - 24, y, x - 4, y, { stroke: GRIS, sw: 1.2 }); }); };
  ligne(40, "PCS (ménages)", [[180, "PCS 2003", "486 professions"], [354, "PCS 2020", "311 professions", true]], BLEU_CLAIR);
  ligne(92, "PCS-ESE (employeurs)", [[180, "PCS-ESE 2003", ""], [354, "PCS-ESE 2017", "429 rubriques", true]], BLEU_CLAIR);
  ligne(144, "FAP (Dares)", [[180, "FAP 2003", ""], [354, "FAP 2009", "225 familles détaillées"], [528, "FAP 2021", "suit la PCS 2020", true]], VERT_CLAIR);
  ligne(196, "ROME (France Travail)", [[354, "ROME v3", "531 fiches"], [528, "ROME 4.0", "depuis 2023", true]], VERT_CLAIR);
  b += `<path d="M${j(700)} 20 L${j(700)} 215" stroke="${ORANGE}" stroke-width="1.5" stroke-dasharray="5 4"/>` + T(705, 118, "aujourd’hui", 10.5, { fill: ORANGE, rot: 90 });
  b += T(380, 240, "une table de passage relie deux versions précises ; mélanger les versions, c’est additionner des listes différentes", 11.5, { anchor: "middle", fill: ORANGE });
  out["n6_versions"] = svg(w, h, b); }

// ---------------------------------------------------------------- 7. Mémo : trois questions
{ const w = 760, h = 230; let b = "";
  const q = (y, question, rep, det, c) => { b += box(20, y, 330, 54, { fill: c }) + T(36, y + 23, "Je veux…", 10.5, { fill: GRIS }) + T(36, y + 43, question, 13, { bold: true });
    b += arrow(356, y + 27, 404, y + 27) + box(410, y, 330, 54, { fill: "#fff", stroke: NAVY }) + T(426, y + 23, rep, 14, { bold: true, fill: NAVY }) + T(426, y + 43, det, 11, { fill: GRIS }); };
  q(16, "connaître la structure socioprofessionnelle", "PCS, niveaux 1 et 2", "groupes et catégories (CS)", BLEU_CLAIR);
  q(86, "identifier précisément une profession", "PCS au niveau détaillé", "en précisant la version : PCS 2020 ou PCS-ESE 2017", BLEU_CLAIR);
  q(156, "analyser des métiers sur le marché du travail", "FAP", "en lien avec le ROME si besoin (offres, demandes)", VERT_CLAIR);
  out["n7_memo"] = svg(w, h, b); }

fs.mkdirSync(OUT, { recursive: true });
for (const [nom, s] of Object.entries(out)) fs.writeFileSync(path.join(OUT, nom + ".svg"), s);
console.log(Object.keys(out).join("\n"));
