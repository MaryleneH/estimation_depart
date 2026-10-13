// Schémas pédagogiques « façon dessin à la main » de la note PCS / FAP / cartes (SVG -> PNG via Chromium).
// Usage : node gen_schemas.js <dossier_sortie>      (puis rendu PNG : voir README)
// Aucune donnée réelle : tous les chiffres sont fictifs et signalés comme tels dans la note.
const fs = require("fs"), path = require("path");
const OUT = process.argv[2] || __dirname;
const NAVY = "#1E3A5F", ENCRE = "#2B2B2B", GRIS = "#7A8795", CLAIR = "#E9EEF3", FOND = "#F6F8FA", ORANGE = "#D97706", ORANGE_CLAIR = "#FDF1E1", BLEU_CLAIR = "#E3ECF6", VERT = "#2E7D5B";
const HAND = 'font-family="Happy Monkey, Comic Neue, Humor Sans, sans-serif"';   // police manuscrite locale (accents corrects)
const K = 3;
let seed = 7; const rnd = () => { seed = (seed * 9301 + 49297) % 233280; return seed / 233280 - 0.5; };   // pseudo-aléa reproductible
const j = (v, a = 1.6) => (v + rnd() * a).toFixed(1);
// rectangle « tremblé » : deux passes légèrement décalées, comme un trait de feutre
function box(x, y, w, h, o = {}) {
  const pass = (d) => `M${j(x + d)} ${j(y)} Q${j(x + w / 2)} ${j(y - 1 + d)} ${j(x + w)} ${j(y + d)} Q${j(x + w + 1)} ${j(y + h / 2)} ${j(x + w - d)} ${j(y + h)} Q${j(x + w / 2)} ${j(y + h + 1 - d)} ${j(x)} ${j(y + h - d)} Q${j(x - 1)} ${j(y + h / 2)} ${j(x + d)} ${j(y)} Z`;
  const fill = o.fill || "#fff", stroke = o.stroke || ENCRE, sw = o.sw || 1.6;
  return `<path d="${pass(0)}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}" stroke-linejoin="round"/><path d="${pass(0.9)}" fill="none" stroke="${stroke}" stroke-width="${sw * 0.6}" opacity=".55"/>`;
}
function ellipse(cx, cy, rx, ry, o = {}) {
  const p = []; for (let a = 0; a <= 360; a += 20) { const t = a * Math.PI / 180; p.push(`${(cx + rx * Math.cos(t) + rnd() * 1.4).toFixed(1)} ${(cy + ry * Math.sin(t) + rnd() * 1.4).toFixed(1)}`); }
  return `<path d="M${p.join(" L")} Z" fill="${o.fill || "#fff"}" stroke="${o.stroke || ENCRE}" stroke-width="${o.sw || 1.6}" stroke-linejoin="round"/>`;
}
// flèche à main levée (légère courbure) avec pointe ouverte
function arrow(x1, y1, x2, y2, o = {}) {
  const mx = (x1 + x2) / 2 + rnd() * 6, my = (y1 + y2) / 2 + rnd() * 6, col = o.stroke || ENCRE, sw = o.sw || 1.7;
  const ang = Math.atan2(y2 - my, x2 - mx), L = 9;
  const h1 = `${(x2 - L * Math.cos(ang - 0.45)).toFixed(1)} ${(y2 - L * Math.sin(ang - 0.45)).toFixed(1)}`, h2 = `${(x2 - L * Math.cos(ang + 0.45)).toFixed(1)} ${(y2 - L * Math.sin(ang + 0.45)).toFixed(1)}`;
  return `<path d="M${x1} ${y1} Q${mx.toFixed(1)} ${my.toFixed(1)} ${x2} ${y2}" fill="none" stroke="${col}" stroke-width="${sw}" stroke-linecap="round"${o.dash ? ' stroke-dasharray="5 4"' : ""}/><path d="M${h1} L${x2} ${y2} L${h2}" fill="none" stroke="${col}" stroke-width="${sw}" stroke-linecap="round" stroke-linejoin="round"/>`;
}
const T = (x, y, t, s = 13, o = {}) => `<text x="${x}" y="${y}" font-size="${s}" fill="${o.fill || ENCRE}" ${o.anchor ? `text-anchor="${o.anchor}"` : ""} ${o.bold ? 'font-weight="bold"' : ""}${o.rot ? ` transform="rotate(${o.rot} ${x} ${y})"` : ""}>${t}</text>`;
const lines = (x, y, arr, s = 12, lh = 15, o = {}) => arr.map((t, i) => T(x, y + i * lh, t, s, o)).join("");
const svg = (w, h, body) => `<svg xmlns="http://www.w3.org/2000/svg" width="${w * K}" height="${h * K}" viewBox="0 0 ${w} ${h}" ${HAND}><rect width="${w}" height="${h}" fill="#fff"/>${body}</svg>`;
const note = (x, y, t, s = 11.5) => T(x, y, t, s, { fill: ORANGE });
const out = {};

// ---------------------------------------------------------------- 1. Trois lectures (page 1)
{ const w = 760, h = 250; let b = "";
  const col = (x, titre, m, r, c) => b += box(x, 22, 220, 60, { fill: c }) + T(x + 110, 48, titre, 13, { anchor: "middle", bold: true, fill: NAVY }) + T(x + 110, 68, m, 12, { anchor: "middle", fill: GRIS })
    + arrow(x + 110, 86, x + 110, 118) + box(x, 122, 220, 44, {}) + T(x + 110, 150, r.n, 14, { anchor: "middle", bold: true })
    + arrow(x + 110, 170, x + 110, 198) + box(x, 202, 220, 40, { fill: FOND, stroke: GRIS }) + T(x + 110, 227, r.f, 12.5, { anchor: "middle" });
  col(20, "EMPLOI OBSERVÉ", "qui occupe quel poste aujourd’hui", { n: "PCS", f: "structure de l’emploi" }, BLEU_CLAIR);
  col(270, "MÉTIERS / MARCHÉ DU TRAVAIL", "ce que cherchent les employeurs", { n: "FAP", f: "tensions · besoins · recrutements" }, BLEU_CLAIR);
  col(520, "PROJECTION", "ce qui pourrait se passer d’ici 2030", { n: "départs estimés", f: "renouvellement potentiel" }, ORANGE_CLAIR);
  b += T(380, 14, "trois lectures qui se complètent… mais ne se remplacent pas", 12.5, { anchor: "middle", fill: ORANGE });
  out["s1_trois_lectures"] = svg(w, h, b); }

// ---------------------------------------------------------------- 2. Zoom dans la hiérarchie PCS
{ const w = 760, h = 310; let b = "";
  // à gauche : niveaux emboîtés ; à droite : effectifs qui fondent
  b += box(20, 30, 330, 70, { fill: BLEU_CLAIR }) + T(34, 52, "GROUPE (niveau 1) : 1 chiffre", 11.5, { fill: GRIS }) + T(34, 76, "4 · Professions intermédiaires", 15, { bold: true, fill: NAVY });
  b += arrow(185, 104, 185, 128) + T(200, 122, "zoom", 12, { fill: ORANGE });
  b += box(50, 132, 300, 60, { fill: "#fff" }) + T(64, 152, "CATÉGORIE (niveau 2) : 2 chiffres", 11.5, { fill: GRIS }) + T(64, 176, "47 · Techniciens", 15, { bold: true, fill: NAVY });
  b += arrow(200, 196, 200, 220) + T(215, 214, "zoom", 12, { fill: ORANGE });
  b += box(80, 224, 270, 74, { fill: "#fff" }) + T(94, 244, "PROFESSION DÉTAILLÉE : 4 caractères", 11.5, { fill: GRIS }) + T(94, 268, "47xx · technicien de maintenance", 14, { bold: true, fill: NAVY });
  b += T(94, 288, "(exemple illustratif, code fictif)", 10.5, { fill: GRIS });
  // à droite : silhouettes décroissantes
  const grp = (x, y, n, s, c) => { let g = ""; for (let i = 0; i < n; i++) g += `<circle cx="${x + (i % 10) * s * 1.3 + s / 2}" cy="${y + Math.floor(i / 10) * s * 1.6}" r="${s / 2}" fill="${c}"/>`; return g; };
  b += T(400, 48, "effectif dans une case", 12.5, { fill: GRIS }) + grp(400, 62, 40, 8, NAVY) + T(400, 122, "beaucoup de salariés : résultat solide", 12);
  b += grp(430, 156, 12, 8, NAVY) + T(430, 196, "moins de salariés : résultat plus fragile", 12);
  b += grp(460, 244, 3, 8, ORANGE) + T(460, 268, "trop peu : secret statistique,", 12, { fill: ORANGE }) + T(460, 284, "la case n’est pas diffusée", 12, { fill: ORANGE });
  b += arrow(735, 60, 735, 290, { stroke: GRIS, dash: true }) + T(745, 170, "plus fin", 11.5, { fill: GRIS, rot: 90 });
  out["s2_zoom_pcs"] = svg(w, h, b); }

// ---------------------------------------------------------------- 3. Curseur précision / robustesse
{ const w = 760, h = 150; let b = "";
  b += `<path d="M60 80 Q380 ${j(78, 3)} 700 80" fill="none" stroke="${ENCRE}" stroke-width="2.2" stroke-linecap="round"/>`;
  b += arrow(120, 80, 62, 80) + arrow(640, 80, 698, 80);
  b += T(60, 110, "PRÉCISION", 15, { bold: true, fill: NAVY }) + T(60, 127, "on décrit finement chaque métier", 11.5, { fill: GRIS });
  b += T(700, 110, "ROBUSTESSE", 15, { bold: true, fill: NAVY, anchor: "end" }) + T(700, 127, "on compare des effectifs solides", 11.5, { fill: GRIS, anchor: "end" });
  const pin = (x, lab, c) => b += ellipse(x, 80, 9, 9, { fill: c, stroke: c }) + box(x - 95, 20, 190, 36, { fill: "#fff", stroke: c }) + T(x, 43, lab, 13, { anchor: "middle", bold: true, fill: c }) + arrow(x, 58, x, 69, { stroke: c });
  pin(190, "PCS fine (4 caractères)", NAVY); pin(560, "grande catégorie (CS)", VERT);
  b += T(380, 14, "le projet estime les départs par grande catégorie ; la PCS fine sert au détail et au contrôle", 11.5, { anchor: "middle", fill: ORANGE });
  out["s3_curseur"] = svg(w, h, b); }

// ---------------------------------------------------------------- 4. Pont PCS -> FAP <- ROME
{ const w = 760, h = 220; let b = "";
  b += box(30, 30, 230, 70, { fill: BLEU_CLAIR }) + T(145, 56, "PCS", 17, { anchor: "middle", bold: true, fill: NAVY }) + T(145, 78, "Insee · position dans l’emploi", 11.5, { anchor: "middle", fill: GRIS }) + T(145, 92, "(recensement, déclarations des employeurs)", 10.5, { anchor: "middle", fill: GRIS });
  b += box(30, 130, 230, 70, { fill: BLEU_CLAIR }) + T(145, 156, "ROME", 17, { anchor: "middle", bold: true, fill: NAVY }) + T(145, 178, "France Travail · fiches métiers", 11.5, { anchor: "middle", fill: GRIS }) + T(145, 192, "(offres et demandes d’emploi)", 10.5, { anchor: "middle", fill: GRIS });
  b += ellipse(470, 115, 110, 48, { fill: ORANGE_CLAIR, stroke: ORANGE }) + T(470, 110, "FAP", 19, { anchor: "middle", bold: true, fill: ORANGE }) + T(470, 132, "familles professionnelles (Dares)", 11.5, { anchor: "middle" });
  b += arrow(265, 65, 365, 100) + arrow(265, 165, 365, 130);
  b += T(300, 48, "table de passage", 10.5, { fill: GRIS }) + T(300, 192, "table de passage", 10.5, { fill: GRIS });
  b += arrow(585, 115, 640, 115) + lines(648, 100, ["tensions,", "besoins en", "main-d’œuvre"], 12, 15, { fill: NAVY });
  b += T(470, 28, "un pont, pas une équivalence case à case", 11.5, { anchor: "middle", fill: ORANGE });
  out["s4_pont"] = svg(w, h, b); }

// ---------------------------------------------------------------- 5. Département et bassins
{ const w = 760, h = 300; let b = "";
  // contour « département » irrégulier
  const pts = [[60, 60], [260, 34], [430, 50], [470, 140], [420, 250], [250, 270], [90, 240], [40, 150]].map(([x, y]) => `${j(x, 4)} ${j(y, 4)}`);
  b += `<path d="M${pts.join(" L")} Z" fill="${FOND}" stroke="${NAVY}" stroke-width="2.6" stroke-linejoin="round"/>`;
  // 4 bassins (lignes intérieures)
  b += `<path d="M${j(250)} ${j(36)} L${j(240)} ${j(150)} L${j(430)} ${j(150)}" fill="none" stroke="${GRIS}" stroke-width="1.6" stroke-dasharray="6 4"/>`;
  b += `<path d="M${j(240)} ${j(150)} L${j(250)} ${j(268)}" fill="none" stroke="${GRIS}" stroke-width="1.6" stroke-dasharray="6 4"/>`;
  b += `<path d="M${j(42)} ${j(150)} L${j(240)} ${j(150)}" fill="none" stroke="${GRIS}" stroke-width="1.6" stroke-dasharray="6 4"/>`;
  b += T(140, 100, "bassin 1", 12.5, { anchor: "middle", fill: GRIS }) + T(140, 205, "bassin 2", 12.5, { anchor: "middle", fill: GRIS }) + T(340, 210, "bassin 4", 12.5, { anchor: "middle", fill: GRIS });
  // bassin 3 en tension
  b += `<path d="M${j(252)} ${j(38)} L${j(428)} ${j(52)} L${j(462)} ${j(140)} L${j(242)} ${j(148)} Z" fill="${ORANGE_CLAIR}" stroke="${ORANGE}" stroke-width="2"/>`;
  b += T(350, 90, "bassin 3", 12.5, { anchor: "middle", fill: ORANGE, bold: true }) + T(350, 108, "tension observée (FAP)", 11.5, { anchor: "middle", fill: ORANGE });
  b += T(150, 290, "DÉPARTEMENT = maille des départs estimés", 13, { bold: true, fill: NAVY });
  // légende à droite
  b += box(510, 40, 230, 100, { fill: "#fff", stroke: ORANGE }) + lines(524, 62, ["Un signal dans un bassin", "ne décrit pas tout", "le département…"], 12.5, 17, { fill: ENCRE });
  b += arrow(505, 90, 465, 100, { stroke: ORANGE });
  b += box(510, 160, 230, 100, { fill: "#fff", stroke: NAVY }) + lines(524, 182, ["… et une estimation", "départementale ne se", "« redescend » pas à un bassin."], 12.5, 17, { fill: ENCRE });
  b += arrow(505, 210, 430, 235, { stroke: NAVY });
  out["s5_departement_bassins"] = svg(w, h, b); }

// ---------------------------------------------------------------- 6. Volume ≠ taux (chiffres fictifs)
{ const w = 760, h = 260; let b = "";
  const terr = (x, nom, sal, dep, pct, c) => {
    b += T(x, 30, nom, 16, { bold: true, fill: NAVY });
    // stock : grand rectangle proportionnel (1 unité = 1 000 salariés -> largeur)
    const ws = sal / 100, hs = 70;
    b += box(x, 44, ws, hs, { fill: CLAIR, stroke: GRIS }) + T(x + 6, 60, `${sal.toLocaleString("fr-FR")} salariés`, 12, { fill: GRIS });
    const wd = dep / 100;
    b += box(x, 44 + hs - 30, wd, 30, { fill: c, stroke: c }) + T(x + 6, 44 + hs + 20, `${dep.toLocaleString("fr-FR")} départs estimés`, 12.5, { bold: true, fill: c });
    b += T(x, 170, "taux = départs ÷ salariés", 11.5, { fill: GRIS }) + T(x, 198, `= ${pct} %`, 22, { bold: true, fill: c });
  };
  terr(30, "Territoire A", 20000, 1000, "5", NAVY); terr(420, "Territoire B", 2000, 500, "25", ORANGE);
  b += T(30, 236, "A porte le plus gros VOLUME (combien de personnes ?)", 12.5, { fill: NAVY, bold: true });
  b += T(420, 236, "B connaît la plus forte INTENSITÉ (quelle part ?)", 12.5, { fill: ORANGE, bold: true });
  b += T(380, 254, "chiffres fictifs", 10.5, { anchor: "middle", fill: GRIS });
  out["s6_volume_taux"] = svg(w, h, b); }

// ---------------------------------------------------------------- 7. Facteurs d'une tension
{ const w = 760, h = 270; let b = "";
  const f = ["Départs à venir", "Attractivité du métier", "Formation disponible", "Mobilité, localisation", "Compétences recherchées", "Concurrence entre employeurs", "Conditions de travail"];
  f.forEach((t, i) => { const x = 20 + (i % 4) * 185, y = 24 + Math.floor(i / 4) * 64; const bx = i >= 4 ? x + 92 : x;   // 4 boîtes puis 3 centrées
    b += box(bx, y, 178, 42, { fill: i === 0 ? ORANGE_CLAIR : "#fff", stroke: i === 0 ? ORANGE : ENCRE }) + T(bx + 89, y + 26, t, 11, { anchor: "middle", bold: i === 0, fill: i === 0 ? ORANGE : ENCRE });
    b += arrow(bx + 85, y + 46, 380 + (bx + 85 - 380) * 0.18, 196, { stroke: GRIS, sw: 1.2 }); });
  b += ellipse(380, 226, 150, 30, { fill: BLEU_CLAIR, stroke: NAVY }) + T(380, 232, "TENSION POSSIBLE", 16, { anchor: "middle", bold: true, fill: NAVY });
  b += T(740, 262, "les départs : un facteur parmi d’autres", 11.5, { anchor: "end", fill: ORANGE });
  out["s7_facteurs_tension"] = svg(w, h, b); }

// ---------------------------------------------------------------- 8. Fourchette basse / centrale / haute (fictif)
{ const w = 760, h = 150; let b = "";
  b += `<path d="M80 90 Q380 ${j(88, 3)} 680 90" fill="none" stroke="${GRIS}" stroke-width="1.6"/>`;
  const tick = (x, lab, v, c, big) => b += `<path d="M${x} 78 L${x} 102" stroke="${c}" stroke-width="${big ? 3 : 2}"/>` + T(x, 70, lab, 12.5, { anchor: "middle", fill: c, bold: big }) + T(x, 120, v, big ? 16 : 13, { anchor: "middle", fill: c, bold: big });
  tick(230, "hypothèse basse", "≈ 850", GRIS); tick(380, "estimation centrale", "≈ 900", NAVY, true); tick(530, "hypothèse haute", "≈ 950", GRIS);
  b += `<path d="M230 90 L530 90" stroke="${NAVY}" stroke-width="5" opacity=".25"/>`;
  b += T(380, 28, "« départs estimés d’ici 2030 » = un ordre de grandeur, avec sa fourchette", 13, { anchor: "middle", fill: NAVY, bold: true });
  b += T(380, 46, "pas un nombre certain, pas des départs programmés — chiffres fictifs", 11.5, { anchor: "middle", fill: ORANGE });
  out["s8_fourchette"] = svg(w, h, b); }

// ---------------------------------------------------------------- 9. Structure d'emploi : seuil de dominance (fictif)
{ const w = 760, h = 170; let b = "";
  const parts = [["Ouvriers", 31, NAVY], ["Prof. intermédiaires", 30, "#4A6A8F"], ["Cadres", 25, "#8FA6BF"], ["Employés", 14, "#C3D0DE"]];
  let x = 30; b += T(30, 30, "Sur 100 salariés en poste aujourd’hui (chiffres fictifs)", 12.5, { fill: GRIS });
  for (const [n, v, c] of parts) { const ww = v * 6.6; b += box(x, 44, ww, 44, { fill: c, stroke: c }) + T(x + ww / 2, 72, `${v} %`, 14, { anchor: "middle", bold: true, fill: v >= 25 ? "#fff" : ENCRE }); b += T(x + ww / 2, 106, n, 11, { anchor: "middle", fill: GRIS }); x += ww; }
  const xs = 30 + 40 * 6.6;   // seuil de dominance du projet (TYPO_SEUIL_DOMINANCE_CS, 40 %)
  b += `<path d="M${xs} 36 L${xs} 96" stroke="${ORANGE}" stroke-width="2" stroke-dasharray="5 4"/>` + T(xs + 6, 40, "seuil de dominance : 40 %", 11.5, { fill: ORANGE });
  b += T(30, 140, "aucune catégorie n’atteint le seuil : structure « MIXTE », pas « dominante ouvrière »", 13, { bold: true, fill: NAVY });
  b += T(30, 158, "une catégorie peut arriver en tête sans dominer", 11.5, { fill: GRIS });
  out["s9_structure_mixte"] = svg(w, h, b); }

fs.mkdirSync(OUT, { recursive: true });
for (const [nom, s] of Object.entries(out)) fs.writeFileSync(path.join(OUT, nom + ".svg"), s);
console.log(Object.keys(out).join("\n"));
