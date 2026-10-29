// Schémas de la note méthodologique « Exposition au recrutement difficile » (SVG -> PNG via Chromium).
// Usage : node gen_schemas.js <dossier_sortie>   puis   node ../note_pedagogique/render_png.js <dossier>/*.svg
// Tous les chiffres sont FICTIFS (exemple suivi dans la note). Aucune donnée du projet.
const fs = require("fs"), path = require("path");
const OUT = process.argv[2] || __dirname;
const NAVY = "#1E3A5F", ENCRE = "#2B2B2B", GRIS = "#7A8795", CLAIR = "#E9EEF3", FOND = "#F6F8FA", ORANGE = "#D97706", ORANGE_CLAIR = "#FDF1E1", BLEU_CLAIR = "#E3ECF6", VERT = "#2E7D5B", VERT_CLAIR = "#E6F2EC", ROUGE = "#B2182B", ROUGE_CLAIR = "#FBE4E1";
const HAND = 'font-family="Happy Monkey, Comic Neue, Humor Sans, sans-serif"';
const K = 3;
let seed = 7; const rnd = () => { seed = (seed * 9301 + 49297) % 233280; return seed / 233280 - 0.5; };
const j = (v, a = 1.6) => (v + rnd() * a).toFixed(1);
function box(x, y, w, h, o = {}) {
  const pass = (d) => `M${j(x + d)} ${j(y)} Q${j(x + w / 2)} ${j(y - 1 + d)} ${j(x + w)} ${j(y + d)} Q${j(x + w + 1)} ${j(y + h / 2)} ${j(x + w - d)} ${j(y + h)} Q${j(x + w / 2)} ${j(y + h + 1 - d)} ${j(x)} ${j(y + h - d)} Q${j(x - 1)} ${j(y + h / 2)} ${j(x + d)} ${j(y)} Z`;
  const fill = o.fill || "#fff", stroke = o.stroke || ENCRE, sw = o.sw || 1.6;
  return `<path d="${pass(0)}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}" stroke-linejoin="round"${o.dash ? ' stroke-dasharray="6 4"' : ""}/><path d="${pass(0.9)}" fill="none" stroke="${stroke}" stroke-width="${sw * 0.6}" opacity=".55"${o.dash ? ' stroke-dasharray="6 4"' : ""}/>`;
}
function arrow(x1, y1, x2, y2, o = {}) {
  const mx = (x1 + x2) / 2 + rnd() * 6, my = (y1 + y2) / 2 + rnd() * 6, col = o.stroke || ENCRE, sw = o.sw || 1.7;
  const ang = Math.atan2(y2 - my, x2 - mx), L = 9;
  const head = (x, y, a) => `<path d="M${(x - L * Math.cos(a - 0.45)).toFixed(1)} ${(y - L * Math.sin(a - 0.45)).toFixed(1)} L${x} ${y} L${(x - L * Math.cos(a + 0.45)).toFixed(1)} ${(y - L * Math.sin(a + 0.45)).toFixed(1)}" fill="none" stroke="${col}" stroke-width="${sw}" stroke-linecap="round" stroke-linejoin="round"/>`;
  return `<path d="M${x1} ${y1} Q${mx.toFixed(1)} ${my.toFixed(1)} ${x2} ${y2}" fill="none" stroke="${col}" stroke-width="${sw}" stroke-linecap="round"${o.dash ? ' stroke-dasharray="5 4"' : ""}/>` + head(x2, y2, ang);
}
const T = (x, y, t, s = 13, o = {}) => `<text x="${x}" y="${y}" font-size="${s}" fill="${o.fill || ENCRE}" ${o.anchor ? `text-anchor="${o.anchor}"` : ""} ${o.bold ? 'font-weight="bold"' : ""}>${t}</text>`;
const lines = (x, y, arr, s = 12, lh = 15, o = {}) => arr.map((t, i) => T(x, y + i * lh, t, s, o)).join("");
const svg = (w, h, body) => `<svg xmlns="http://www.w3.org/2000/svg" width="${w * K}" height="${h * K}" viewBox="0 0 ${w} ${h}" ${HAND}><rect width="${w}" height="${h}" fill="#fff"/>${body}</svg>`;
// réglette de rangs 1..5 (cases), remplie jusqu'à `n`
const reglette = (x, y, n, o = {}) => { let s = ""; for (let i = 1; i <= 5; i++) s += `<rect x="${x + (i - 1) * 15}" y="${y}" width="12" height="16" rx="2" fill="${i <= n ? (o.fill || NAVY) : "#fff"}" stroke="${i <= n ? (o.fill || NAVY) : GRIS}" stroke-width="1.2"/>`; return s; };
const out = {};

// ---------------------------------------------------------------- 1. De la PCS à la tension : le pont par la FAP
{ const w = 760, h = 330; let b = "";
  b += T(20, 24, "1. La liste des PCS (première sous-liste)", 12, { fill: GRIS, bold: true });
  b += T(290, 24, "2. Le classeur : une FAP par PCS", 12, { fill: GRIS, bold: true });
  b += T(540, 24, "3. Rangs de tension de la FAP", 12, { fill: GRIS, bold: true });
  const rows = [["Technicienne de maintenance", "Maintenance industrielle", 5.2, 5], ["Soudeur", "Travail des métaux", 4, 4], ["Comptable", "Comptabilité", 2, 2], ["Agent logistique", "Logistique", 3, 3]];
  rows.forEach((r, i) => { const y = 44 + i * 58;
    b += box(20, y, 230, 42, { fill: BLEU_CLAIR }) + T(32, y + 18, r[0], 12.5, { bold: true, fill: NAVY }) + T(32, y + 34, "code PCS fictif", 10.5, { fill: GRIS });
    b += arrow(254, y + 21, 286, y + 21, { stroke: GRIS });
    b += box(290, y, 220, 42, { fill: VERT_CLAIR, stroke: VERT }) + T(302, y + 18, "FAP · " + r[1], 12.5, { bold: true, fill: VERT }) + T(302, y + 34, "famille professionnelle (Dares)", 10.5, { fill: GRIS });
    b += arrow(514, y + 21, 536, y + 21, { stroke: GRIS });
    const tension = r[2] >= 4;
    b += box(540, y, 200, 42, { fill: tension ? ROUGE_CLAIR : FOND, stroke: tension ? ROUGE : GRIS }) + reglette(552, y + 8, r[3], { fill: tension ? ROUGE : GRIS }) + T(636, y + 21, "rang " + String(r[2]).replace(".", ","), 12.5, { bold: true, fill: tension ? ROUGE : ENCRE });
    b += T(636, y + 36, tension ? "en tension (≥ 4)" : "hors tension", 10.5, { fill: tension ? ROUGE : GRIS }); });
  b += T(380, 292, "la tension est celle de la FAP en France entière : elle qualifie le métier, jamais un département", 11.5, { anchor: "middle", fill: ORANGE });
  b += T(380, 312, "le pont est la PCS ; aucune correspondance FAP → grande catégorie n’est construite", 11.5, { anchor: "middle", fill: ORANGE });
  out["s1_pcs_fap_rangs"] = svg(w, h, b); }

// ---------------------------------------------------------------- 2. Les trois parts et l'indice d'exposition (exemple fictif)
{ const w = 760, h = 330; let b = "";
  b += T(20, 24, "Départs attendus d’ici 2030 dans un département fictif : 200 départs", 13, { bold: true, fill: NAVY });
  // barre des 200 départs
  const x0 = 20, W = 720, y0 = 44, H = 44, tot = 200, parts = [["A", 60, ROUGE_CLAIR, ROUGE, "de la liste, en tension"], ["B", 70, BLEU_CLAIR, NAVY, "de la liste, hors tension"], ["C", 70, FOND, GRIS, "autres métiers (et tension inconnue)"]];
  let x = x0; parts.forEach(p => { const wd = W * p[1] / tot; b += box(x, y0, wd, H, { fill: p[2], stroke: p[3] }) + T(x + wd / 2, y0 + 19, p[0] + " = " + p[1], 14, { anchor: "middle", bold: true, fill: p[3] }) + T(x + wd / 2, y0 + 36, p[4], 10.5, { anchor: "middle", fill: p[3] }); x += wd; });
  // formules
  b += box(20, 118, 350, 82, { fill: ROUGE_CLAIR, stroke: ROUGE }) + T(36, 142, "Exposition", 13, { bold: true, fill: ROUGE }) + T(36, 164, "= A / (A + B) = 60 / 130 = 46 %", 13.5, { bold: true });
  b += T(36, 186, "sur 100 départs des métiers de la première sous-liste, 46 concernent", 10.5) ;
  b += box(390, 118, 350, 82, { fill: BLEU_CLAIR, stroke: NAVY }) + T(406, 142, "Couverture", 13, { bold: true, fill: NAVY }) + T(406, 164, "= (A + B) / total = 130 / 200 = 65 %", 13.5, { bold: true });
  b += T(406, 186, "les métiers de la liste portent 65 % des départs", 10.5);
  b += T(36, 198, "des métiers en tension en France", 10.5);
  b += T(406, 198, "au-dessus du minimum de 30 % : exposition interprétée", 10.5);
  // repère France et classe
  b += box(20, 222, 720, 60, { fill: "#fff", dash: true, stroke: GRIS }) + T(36, 246, "Repère : la même part calculée sur la France entière, par exemple 38 %.", 12) + T(36, 268, "Classe : Faible / Modérée / Forte selon des bornes écrites dans parametres_exposition_tension.csv (ici 46 % → Forte, borne haute 45 %).", 10.5);
  b += T(380, 312, "la part C n’entre pas dans l’exposition : elle sert à mesurer la couverture, c’est-à-dire ce que la liste ne voit pas", 11.5, { anchor: "middle", fill: ORANGE });
  out["s2_parts_abc"] = svg(w, h, b); }

// ---------------------------------------------------------------- 3. Du rang des composantes au levier
{ const w = 760, h = 360; let b = "";
  b += T(20, 24, "Pour un métier : les sept composantes de la tension de sa FAP (rangs fictifs)", 13, { bold: true, fill: NAVY });
  const comps = [["manque de main-d’œuvre disponible", 5, "attirer et élargir les viviers"], ["formation spécifique au métier", 4, "former tôt"], ["conditions de travail contraignantes", 3, "fidéliser"], ["faible attractivité salariale", 2, "rémunération"], ["inadéquation géographique", 2, "mobilité, implantation"], ["emplois peu durables", 1, "stabiliser les emplois"], ["forte intensité d’embauche", 3, "anticiper les recrutements"]];
  comps.forEach((c, i) => { const y = 42 + i * 26; const gagnant = i === 0;
    b += T(30, y + 13, c[0], 11.5, { fill: gagnant ? ROUGE : ENCRE, bold: gagnant }) + reglette(300, y, c[1], { fill: gagnant ? ROUGE : (c[1] >= 4 ? NAVY : GRIS) }) + T(385, y + 13, "rang " + c[1], 11, { fill: gagnant ? ROUGE : GRIS }); });
  b += `<path d="M${j(440)} 40 L${j(440)} 228" stroke="${GRIS}" stroke-width="1.2" stroke-dasharray="4 4"/>`;
  b += box(460, 60, 280, 64, { fill: ROUGE_CLAIR, stroke: ROUGE }) + T(474, 82, "composante dominante", 11, { fill: GRIS }) + T(474, 102, "manque de main-d’œuvre (rang 5)", 13, { bold: true, fill: ROUGE }) + T(474, 117, "retenue car son rang atteint le seuil (4)", 10.5, { fill: ROUGE });
  b += arrow(600, 128, 600, 150);
  b += box(460, 154, 280, 56, { fill: VERT_CLAIR, stroke: VERT }) + T(474, 176, "levier associé", 11, { fill: GRIS }) + T(474, 196, "attirer et élargir les viviers", 13, { bold: true, fill: VERT });
  b += T(30, 250, "Règles : rang le plus élevé ; ex aequo départagés par la valeur fine (5,2 avant 5,1) puis par l’ordre fixé dans la configuration ;", 11);
  b += T(30, 266, "si aucun rang n’atteint le seuil, « aucune composante au-dessus du seuil » : pas de levier.", 11);
  b += box(20, 284, 720, 70, { fill: BLEU_CLAIR }) + T(36, 304, "Pour un département : le levier retenu est la composante dominante la plus fréquente", 11.5, { bold: true, fill: NAVY }) + T(36, 322, "parmi les départs des métiers en tension (part A), chaque métier pesant par ses départs attendus.", 11.5, { bold: true, fill: NAVY }) + T(36, 342, "Exemple : main-d’œuvre 30 départs, formation 20, conditions de travail 10 → « attirer et élargir les viviers ».", 11, { fill: NAVY });
  out["s3_levier"] = svg(w, h + 10, b); }

// ---------------------------------------------------------------- 4. Les cinq variantes de robustesse
{ const w = 760, h = 300; let b = "";
  b += T(20, 24, "Calcul central : exposition 46 % → classe Forte (bornes : 35 % et 45 %)", 13, { bold: true, fill: NAVY });
  const vars = [["hypothèse de départ basse", "45 %", "Forte", true], ["hypothèse de départ haute", "47 %", "Forte", true], ["seuil de tension + 1 (rang 5)", "46 %", "Forte", true], ["seuil de tension − 1 (rang 3)", "61 %", "Forte", true], ["sans les rattachements de faible confiance", "42 %", "Modérée", false]];
  vars.forEach((v, i) => { const y = 44 + i * 40;
    b += box(20, y, 330, 30, { fill: FOND }) + T(32, y + 20, v[0], 12);
    b += arrow(354, y + 15, 384, y + 15, { stroke: GRIS });
    b += box(390, y, 120, 30, { fill: "#fff" }) + T(450, y + 20, v[1], 12.5, { anchor: "middle", bold: true });
    b += box(530, y, 130, 30, { fill: v[3] ? VERT_CLAIR : ORANGE_CLAIR, stroke: v[3] ? VERT : ORANGE }) + T(595, y + 20, v[2], 12.5, { anchor: "middle", bold: true, fill: v[3] ? VERT : ORANGE });
    b += T(680, y + 20, v[3] ? "inchangée" : "change", 11, { fill: v[3] ? VERT : ORANGE }); });
  b += box(20, 248, 720, 52, { fill: ORANGE_CLAIR, stroke: ORANGE }) + T(36, 270, "Résultat : « à confirmer (sensible : rattachement PCS → FAP) »", 12, { bold: true, fill: ORANGE }) + T(36, 290, "une seule variante qui change la classe suffit ; la cause est écrite à côté de la classe.", 11, { fill: ORANGE });
  out["s4_robustesse"] = svg(w, h + 12, b); }

fs.mkdirSync(OUT, { recursive: true });
for (const [n, s] of Object.entries(out)) { fs.writeFileSync(path.join(OUT, n + ".svg"), s); console.log("écrit :", n + ".svg"); }
