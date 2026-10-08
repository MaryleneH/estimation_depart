// Génère les mini-diagrammes (SVG) de la fiche pédagogique, puis ils sont rendus en PNG par Chromium.
const fs = require("fs"), path = require("path");
const NAVY = "#1E3A5F", ENCRE = "#1F2933", TEXTE = "#3E4C59", FILET = "#D9DEE5", CLAIR = "#E6EBF1", FOND = "#F5F7FA";
const ACCENT = "#B2182B", VIOLET = "#8C6BB1", OR = "#C9A227";
const font = 'font-family="Liberation Sans, DejaVu Sans, Arial, sans-serif"';
const SYM = `<defs><symbol id="p" viewBox="0 0 10 14"><circle cx="5" cy="2.6" r="2.4"/><path d="M1.6 14 V8.2 a3.4 3.4 0 0 1 6.8 0 V14 Z"/></symbol></defs>`;
const person = (x, y, s, fill) => `<use href="#p" x="${x}" y="${y}" width="${s}" height="${s * 1.4}" fill="${fill}"/>`;
const K = 3;  // facteur de rendu (PNG net)
const svg = (w, h, body) => `<svg xmlns="http://www.w3.org/2000/svg" width="${w * K}" height="${h * K}" viewBox="0 0 ${w} ${h}" data-w="${w}" data-h="${h}" ${font}>${SYM}<rect width="${w}" height="${h}" fill="#fff"/>${body}</svg>`;
const txt = (x, y, t, size = 12, fill = ENCRE, extra = "") => `<text x="${x}" y="${y}" font-size="${size}" fill="${fill}" ${extra}>${t}</text>`;
const out = {};

// 1. Poids : 100 silhouettes, 8 mises en évidence (« sur 100 salariés de la BITD en France, 8 travaillent ici »)
{ const w = 300, h = 130, s = 11, gx = 14, gy = 24; let b = "";
  for (let i = 0; i < 100; i++) { const r = Math.floor(i / 20), c = i % 20; b += person(10 + c * gx, 8 + r * gy, s, i < 8 ? NAVY : CLAIR); }
  out["poids"] = svg(w, h, b); }

// 2. Volume : blocs proportionnels, un bloc = 100 départs (9 blocs = 900 départs)
{ const w = 300, h = 130; let b = "";
  for (let i = 0; i < 9; i++) { const x = 10 + (i % 5) * 56, y = 14 + Math.floor(i / 5) * 52;
    b += `<rect x="${x}" y="${y}" width="46" height="40" rx="5" fill="${NAVY}"/>` + txt(x + 23, y + 25, "100", 12, "#fff", 'text-anchor="middle" font-weight="bold"'); }
  b += `<rect x="10" y="${h - 14}" width="12" height="10" rx="2" fill="${NAVY}"/>` + txt(27, h - 5, "= 100 départs estimés d’ici 2030", 11, TEXTE);
  out["volume"] = svg(w, h, b); }

// 3. Intensité : bande de 100 unités, 12 mises en évidence
{ const w = 300, h = 130, cw = 11.6, ch = 22; let b = "";
  for (let i = 0; i < 100; i++) { const r = Math.floor(i / 25), c = i % 25; b += `<rect x="${8 + c * cw}" y="${10 + r * (ch + 4)}" width="${cw - 2}" height="${ch}" rx="2" fill="${i < 12 ? ACCENT : CLAIR}"/>`; }
  b += txt(8, h - 6, "12 unités sur 100 = 12 % de l’emploi actuel à remplacer", 11, TEXTE);
  out["intensite"] = svg(w, h, b); }

// 4. Structure de l'emploi : barre empilée (ouvriers 55 %)
{ const w = 300, h = 130; const parts = [["Ouvriers", 55, NAVY], ["Prof. interm.", 20, "#4A6A8F"], ["Employés", 10, "#8FA6BF"], ["Cadres", 15, "#C3D0DE"]];
  let b = txt(10, 22, "Sur 100 salariés en poste aujourd’hui", 12, TEXTE), x = 10;
  for (const [n, v, col] of parts) { const ww = v * 2.8; b += `<rect x="${x}" y="34" width="${ww}" height="40" fill="${col}"/>`;
    b += txt(x + ww / 2, 59, v, 13, v >= 20 ? "#fff" : ENCRE, 'text-anchor="middle" font-weight="bold"'); x += ww; }
  let lx = 10; for (const [n, v, col] of parts) { b += `<rect x="${lx}" y="84" width="9" height="9" fill="${col}"/>` + txt(lx + 13, 92, n, 9.5, TEXTE); lx += 13 + n.length * 5.6 + 12; }
  b += txt(10, 118, "Dominante : les ouvriers (55 % de l’emploi)", 11.5, NAVY, 'font-weight="bold"');
  out["structure"] = svg(w, h, b); }

// 5. Concentration des départs : 70 départs sur 100 concernent les ouvriers
{ const w = 300, h = 130; const parts = [["Ouvriers", 70, VIOLET], ["Prof. interm.", 15, "#B9A9D6"], ["Employés", 8, "#D6CCE8"], ["Cadres", 7, "#EBE5F4"]];
  let b = txt(10, 22, "Sur 100 départs estimés d’ici 2030", 12, TEXTE), x = 10;
  for (const [n, v, col] of parts) { const ww = v * 2.8; b += `<rect x="${x}" y="34" width="${ww}" height="40" fill="${col}"/>`;
    if (v >= 8) b += txt(x + ww / 2, 59, v, 13, v >= 20 ? "#fff" : ENCRE, 'text-anchor="middle" font-weight="bold"'); x += ww; }
  let lx = 10; for (const [n, v, col] of parts) { b += `<rect x="${lx}" y="84" width="9" height="9" fill="${col}"/>` + txt(lx + 13, 92, n, 9.5, TEXTE); lx += 13 + n.length * 5.6 + 12; }
  b += txt(10, 118, "Concentrés : les ouvriers portent 70 % des départs", 11.5, VIOLET, 'font-weight="bold"');
  out["concentration"] = svg(w, h, b); }

// 6. Comparaison A / B : volume (blocs) et intensité (bande de 100)
{ const w = 640, h = 264; let b = "";
  const ligne = (y, nom, sal, dep, pct, col) => {
    let s = `<rect x="0" y="${y - 22}" width="${w}" height="112" rx="10" fill="${FOND}"/>`;
    s += txt(16, y + 4, nom, 20, col, 'font-weight="bold"') + txt(16, y + 24, `${sal} salariés BITD`, 11.5, TEXTE);
    s += txt(16, y + 40, `${dep} départs estimés`, 11.5, TEXTE) + txt(16, y + 56, `${pct} % de l’emploi actuel`, 11.5, TEXTE);
    // volume : blocs de 100 départs
    s += txt(190, y - 4, "VOLUME", 9.5, TEXTE, 'letter-spacing="1.5" font-weight="bold"');
    const nb = Math.round(parseInt(dep.replace(/\s/g, "")) / 100);
    for (let i = 0; i < nb; i++) s += `<rect x="${190 + i * 18}" y="${y + 4}" width="15" height="26" rx="3" fill="${col}"/>`;
    s += txt(190, y + 50, `${nb} blocs de 100 départs`, 10.5, TEXTE);
    // intensité : bande de 100 unités
    s += txt(400, y - 4, "INTENSITÉ", 9.5, TEXTE, 'letter-spacing="1.5" font-weight="bold"');
    for (let i = 0; i < 100; i++) { const r = Math.floor(i / 50), c = i % 50; s += `<rect x="${400 + c * 4.5}" y="${y + 4 + r * 14}" width="3.6" height="11" rx="1" fill="${i < pct ? col : CLAIR}"/>`; }
    s += txt(400, y + 50, `${pct} unités sur 100 mises en évidence`, 10.5, TEXTE);
    return s; };
  b += ligne(30, "Département A", "20 000", "1 000", 5, NAVY);
  b += ligne(158, "Département B", "2 000", "500", 25, ACCENT);
  out["ab"] = svg(w, h, b); }

for (const k in out) fs.writeFileSync(path.join(__dirname, "images", `${k}.svg`), out[k]);
console.log(Object.keys(out).join(" "));
