// Fiche pédagogique « typologie territoriale BITD » : quatre pages A4 paysage (docx-js).
// Usage : node gen_fiche.js [dossier_images] [fichier_sortie]
// Les images (poids.png, volume.png, intensite.png, structure.png, concentration.png, ab.png)
// sont produites par gen_svg.js + Chromium (voir README).
const fs = require("fs"), path = require("path");
const { Document, Packer, Paragraph, TextRun, ImageRun, Table, TableRow, TableCell, WidthType, AlignmentType, BorderStyle,
        ShadingType, PageOrientation, TableLayoutType, VerticalAlign, Footer, PageNumber } = require("docx");
const DIR_IMG = process.argv[2] || path.join(__dirname, "images"), SORTIE = process.argv[3] || path.join(__dirname, "fiche_pedagogique_typologie_BITD.docx");

// --- Charte --------------------------------------------------------------------
const NAVY = "1E3A5F", ENCRE = "1F2933", TEXTE = "3E4C59", FILET = "D9DEE5", FOND = "F5F7FA", BLANC = "FFFFFF", ACCENT = "B2182B";
const PROFILS = [
  { cle: "enjeu", titre: "Pôle majeur à renouveler rapidement", col: "B2182B", clair: false },
  { cle: "emergent", titre: "Renouvellement rapide à surveiller", col: "EF8A62", clair: false },
  { cle: "majeur_modere", titre: "Pôle majeur au renouvellement modéré", col: "2166AC", clair: false },
  { cle: "concentre", titre: "Renouvellement porté par une catégorie", col: "8C6BB1", clair: false },
  { cle: "stable", titre: "Implantation stable", col: "74C476", clair: true },
  { cle: "diffus", titre: "Implantation réduite", col: "DFE6EE", clair: true },
  { cle: "expertiser", titre: "Situation à expertiser", col: "F2C744", clair: true }];
const P = Object.fromEntries(PROFILS.map(p => [p.cle, p]));
const FONT = "Calibri";
const NONE = { style: BorderStyle.NONE, size: 0, color: "FFFFFF" };
const SANS = { top: NONE, bottom: NONE, left: NONE, right: NONE };

// --- Briques --------------------------------------------------------------------
const run = (t, o = {}) => new TextRun({ text: t, font: FONT, size: o.size || 19, color: o.color || ENCRE, bold: o.bold, italics: o.italics, characterSpacing: o.spacing });
const par = (runs, o = {}) => new Paragraph({ spacing: { before: o.before || 0, after: o.after === undefined ? 60 : o.after, line: o.line || 250 },
  alignment: o.align, children: Array.isArray(runs) ? runs : [runs] });
const etiquette = (t, col = NAVY) => par(run(t.toUpperCase(), { size: 17, bold: true, color: col, spacing: 16 }), { after: 50, before: 60 });
const texte = (t, o = {}) => par(run(t, { size: o.size || 18, color: o.color || ENCRE }), { after: o.after === undefined ? 40 : o.after, line: 240 });
const vide = (h = 60) => new Paragraph({ spacing: { before: 0, after: 0, line: h, lineRule: "exact" }, children: [run("", { size: 4 })] });
const image = (nom, wpx, hpx) => new Paragraph({ spacing: { before: 40, after: 40 }, children: [new ImageRun({ type: "png", data: fs.readFileSync(path.join(DIR_IMG, nom)), transformation: { width: wpx, height: hpx } })] });
const cell = (children, w, o = {}) => new TableCell({ width: { size: w, type: WidthType.DXA }, borders: o.borders || SANS, verticalAlign: o.valign || VerticalAlign.TOP,
  shading: o.fill ? { type: ShadingType.CLEAR, fill: o.fill, color: "auto" } : undefined,
  margins: { top: o.pad === undefined ? 110 : o.pad, bottom: o.pad === undefined ? 110 : o.pad, left: o.padx === undefined ? 130 : o.padx, right: o.padx === undefined ? 130 : o.padx }, children });
const gap = (w) => cell([vide(20)], w, { pad: 0, padx: 0 });
const table = (widths, rows, o = {}) => new Table({ layout: TableLayoutType.FIXED, width: { size: widths.reduce((a, b) => a + b, 0), type: WidthType.DXA }, columnWidths: widths,
  rows: rows.map(r => new TableRow({ cantSplit: true, height: o.hauteur ? { value: o.hauteur, rule: "atLeast" } : undefined, children: r })) });
const ligneGrille = (cards, wCard, wGap) => { const r = []; cards.forEach((c, i) => { if (i) r.push(gap(wGap)); r.push(c); }); return r; };
const filet = () => new Paragraph({ spacing: { before: 20, after: 60 }, border: { bottom: { style: BorderStyle.SINGLE, size: 6, color: FILET, space: 1 } }, children: [run("", { size: 2 })] });

// --- Dimensions : A4 paysage, marges 1 cm -> 15 700 DXA utiles ----------------------
const LARGEUR = 15478;   // 16 838 - 2 x 680
const PAGE = { size: { width: 11906, height: 16838, orientation: PageOrientation.LANDSCAPE }, margin: { top: 480, bottom: 400, left: 680, right: 680 } };
const pied = (t) => new Footer({ children: [new Paragraph({ alignment: AlignmentType.RIGHT, children: [run(t + "  ·  page ", { size: 16, color: TEXTE }), new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 16, color: TEXTE }), run(" / ", { size: 16, color: TEXTE }), new TextRun({ children: [PageNumber.TOTAL_PAGES], font: FONT, size: 16, color: TEXTE })] })] });

// --- En-tête de page ----------------------------------------------------------------
const entete = (kicker, titre, accroche) => [
  par(run(kicker, { size: 17, bold: true, color: TEXTE, spacing: 25 }), { after: 60, line: 240 }),
  par(run(titre, { size: 46, bold: true, color: NAVY }), { before: 0, after: 40, line: 300 }),
  par(run(accroche, { size: 24, color: ENCRE }), { after: 60, line: 255 }), filet()];

// ===================================================================================
// PAGE 1 — Comment lit-on un territoire ? (les cinq indicateurs)
// ===================================================================================
const W3 = 5026, G3 = 200;   // 3 cartes + 2 intervalles = 15 478
const W2 = 7639;             // 2 cartes + 1 intervalle = 15 478
const carteIndicateur = (num, titre, question, explication, img, w, iw, ih) => cell([
  par([run(num + "   ", { size: 26, bold: true, color: ACCENT }), run(titre.toUpperCase(), { size: 21, bold: true, color: NAVY, spacing: 14 })], { after: 40 }),
  par(run(question, { size: 23, bold: true, color: ENCRE }), { after: 60, line: 250 }),
  image(img, iw, ih),
  texte(explication, { size: 23, color: ENCRE, after: 0, line: 268 })], w, { fill: FOND, padx: 170, pad: 150 });

const rang1 = table([W3, G3, W3, G3, W3], [ligneGrille([
  carteIndicateur("1", "Le poids", "Quelle place occupe ce département dans l’emploi BITD national ?",
    "Sur 100 salariés de la BITD en France, combien travaillent ici ? Ici, 8 sur 100. Classé faible, moyen ou fort. Un très petit effectif est toujours classé « faible ».", "poids.png", W3, 300, 130),
  carteIndicateur("2", "Le volume", "Combien de salariés pourraient partir d’ici 2030 ?",
    "Un nombre de départs estimés, ici 900. Il dépend surtout de la taille du département. Classé faible, modéré ou élevé.", "volume.png", W3, 300, 130),
  carteIndicateur("3", "L’intensité", "Sur 100 salariés en poste, combien correspondent aux départs estimés ?",
    "Ici, 12 sur 100 : la part de l’emploi actuel à remplacer. Ce n’est pas un taux de recrutement observé. Classée faible, modérée ou élevée.", "intensite.png", W3, 300, 130)], W3, G3)]);
const rang2 = table([W2, G3, W2], [ligneGrille([
  carteIndicateur("4", "La structure de l’emploi", "Quels salariés sont les plus représentés aujourd’hui ?",
    "Une catégorie est « dominante » si elle atteint une part suffisante de l’emploi, sinon la structure est « mixte ». Ici, les ouvriers : 55 % de l’emploi. La structure décrit le département ; elle n’entre pas dans le choix du profil.", "structure.png", W2, 330, 143),
  carteIndicateur("5", "La concentration des départs", "Les départs concernent-ils surtout une catégorie de salariés ?",
    "Ici, 70 départs sur 100 concernent les ouvriers. À ne pas confondre avec la structure : une catégorie peut dominer l’emploi sans dominer les départs. La concentration peut décider du profil.", "concentration.png", W2, 330, 143)], W2, G3)]);

const page1 = [
  ...entete("Typologie des territoires BITD  ·  fiche de lecture  ·  1 / 4", "Comment lit-on un territoire ?",
    "La typologie ne cherche pas seulement où les départs seront nombreux. Elle distingue des situations de renouvellement différentes, à partir de cinq indicateurs lisibles et de règles explicites. Tous les chiffres ci-dessous sont fictifs."),
  etiquette("Les cinq ingrédients du classement"),
  rang1, vide(160), rang2];

// ===================================================================================
// PAGE 2 — Volume contre intensité ; des indicateurs au profil
// ===================================================================================
const WG = 7800, GG = 200, WD = 7478;   // 7 800 + 200 + 7 478 = 15 478
const blocAB = cell([
  etiquette("Volume et intensité : deux départements, deux lectures"),
  par(run("Le département A compte deux fois plus de départs. Pourtant, le département B est proportionnellement cinq fois plus concerné.", { size: 23, bold: true, color: ENCRE }), { after: 80, line: 260 }),
  image("ab.png", 540, 223),
  texte("Volume : combien de départs ? Intensité : quelle part de l’emploi actuel représentent-ils ?", { size: 21, color: ENCRE, after: 60, line: 260 }),
  texte("Exemples entièrement fictifs. Ces taux illustrent une ampleur de renouvellement, pas une pénurie démontrée.", { size: 19, color: TEXTE, after: 0, line: 250 })], WG, { fill: FOND, pad: 150, padx: 170 });

const pastille = (col) => run("■ ", { size: 22, color: col });
const LR = WD - 2 * 170; const CR = [380, Math.round((LR - 380) * 0.5), 0]; CR[2] = LR - CR[0] - CR[1];
const regle = (n, si, prof, colProf) => new TableRow({ cantSplit: true, children: [
  cell([par(run(n, { size: 19, bold: true, color: TEXTE }), { after: 0 })], CR[0], { pad: 12, padx: 40 }),
  cell([par(run(si, { size: 19, color: ENCRE }), { after: 0, line: 230 })], CR[1], { pad: 12, padx: 60 }),
  cell([par([pastille(colProf), run(prof, { size: 18.5, bold: true, color: ENCRE })], { after: 0, line: 230 })], CR[2], { pad: 12, padx: 60 })] });
const regles = new Table({ layout: TableLayoutType.FIXED, width: { size: LR, type: WidthType.DXA }, columnWidths: CR, rows: [
  new TableRow({ children: [cell([par(run("", { size: 12 }), { after: 0 })], CR[0], { pad: 10, padx: 40 }),
    cell([par(run("SI…", { size: 16, bold: true, color: TEXTE, spacing: 14 }), { after: 0 })], CR[1], { pad: 10, padx: 60 }),
    cell([par(run("ALORS LE PROFIL EST…", { size: 16, bold: true, color: TEXTE, spacing: 14 }), { after: 0 })], CR[2], { pad: 10, padx: 60 })] }),
  regle("1", "un indicateur de classement manque", P.expertiser.titre, P.expertiser.col),
  regle("2", "poids faible et intensité extrême", P.expertiser.titre, P.expertiser.col),
  regle("3", "poids faible", P.diffus.titre, "B8C4D2"),
  regle("4", "deux indicateurs tout près d’un seuil", P.expertiser.titre, P.expertiser.col),
  regle("5", "volume élevé mais intensité faible", P.expertiser.titre, P.expertiser.col),
  regle("6", "poids fort, volume élevé, intensité élevée", P.enjeu.titre, P.enjeu.col),
  regle("7", "intensité élevée", P.emergent.titre, P.emergent.col),
  regle("8", "volume élevé (intensité modérée)", P.majeur_modere.titre, P.majeur_modere.col),
  regle("9", "une catégorie porte la moitié des départs", P.concentre.titre, P.concentre.col),
  regle("10", "aucune des règles précédentes", P.stable.titre, P.stable.col)] });

const CA = [640, 1250, 80, 1350, 80, 1520, 360, 0]; CA[7] = LR - CA.slice(0, 7).reduce((a, b) => a + b, 0);
const chip = (t, w, surligne) => cell([par(run(t, { size: 19, bold: true, color: surligne ? BLANC : ENCRE }), { after: 0, align: AlignmentType.CENTER, line: 230 })], w,
  { fill: surligne ? ACCENT : "FFFFFF", pad: 30, padx: 40, valign: VerticalAlign.CENTER, borders: surligne ? SANS : { top: { style: BorderStyle.SINGLE, size: 4, color: FILET }, bottom: { style: BorderStyle.SINGLE, size: 4, color: FILET }, left: { style: BorderStyle.SINGLE, size: 4, color: FILET }, right: { style: BorderStyle.SINGLE, size: 4, color: FILET } } });
const fleche = (w) => cell([par(run("→", { size: 30, bold: true, color: TEXTE }), { after: 0, align: AlignmentType.CENTER })], w, { pad: 0, padx: 0, valign: VerticalAlign.CENTER });
const resultat = (t, col, w) => cell([par([pastille(col), run(t, { size: 20, bold: true, color: ENCRE })], { after: 0, line: 235 })], w, { pad: 40, padx: 60, valign: VerticalAlign.CENTER });
const etiq = (t) => cell([par(run(t, { size: 16, bold: true, color: TEXTE, spacing: 14 }), { after: 0 })], CA[0], { pad: 30, padx: 20, valign: VerticalAlign.CENTER });
const avantApres = new Table({ layout: TableLayoutType.FIXED, width: { size: LR, type: WidthType.DXA }, columnWidths: CA, rows: [
  new TableRow({ cantSplit: true, children: [etiq("AVANT"), chip("Poids fort", CA[1], false), gap(CA[2]), chip("Volume élevé", CA[3], false), gap(CA[4]), chip("Intensité élevée", CA[5], false), fleche(CA[6]), resultat(P.enjeu.titre, P.enjeu.col, CA[7])] }),
  new TableRow({ children: CA.map(w => gap(w)), height: { value: 50, rule: "exact" } }),
  new TableRow({ cantSplit: true, children: [etiq("APRÈS"), chip("Poids fort", CA[1], false), gap(CA[2]), chip("Volume élevé", CA[3], false), gap(CA[4]), chip("Intensité modérée", CA[5], true), fleche(CA[6]), resultat(P.majeur_modere.titre, P.majeur_modere.col, CA[7])] })] });

const blocDroite = cell([
  etiquette("Des indicateurs au profil : la première règle qui s’applique l’emporte"),
  texte("Les règles sont examinées dans l’ordre : situations à expertiser et petites implantations d’abord, puis le premier profil dont la condition est remplie. Aucun score, aucune addition d’indicateurs.", { size: 20, color: ENCRE, after: 60, line: 250 }),
  regles,
  etiquette("Un seul indicateur change la lecture"),
  texte("Mêmes poids et volume : une intensité modérée au lieu d’élevée change le profil.", { size: 20, color: ENCRE, after: 60, line: 255 }),
  avantApres], WD, { fill: FOND, pad: 150, padx: 170 });

const page2 = [
  ...entete("Typologie des territoires BITD  ·  fiche de lecture  ·  2 / 4", "Pourquoi un profil plutôt qu’un autre ?",
    "Deux départements peuvent compter des volumes de départs très différents et rencontrer des enjeux de renouvellement inverses. Le profil est ensuite attribué par des règles ordonnées, jamais par un score."),
  table([WG, GG, WD], [[blocAB, gap(GG), blocDroite]])];

// ===================================================================================
// PAGES 3 et 4 — Les sept situations territoriales
// ===================================================================================
const W4 = 3734, G4 = 180;   // 4 cartes + 3 intervalles = 15 476
const rubrique = (lab, t, o = {}) => [
  new Paragraph({ spacing: { before: 130, after: 30, line: 250 }, indent: { left: 170, right: 170 }, children: [run(lab.toUpperCase(), { size: 17, bold: true, color: o.col || NAVY, spacing: 14 })] }),
  new Paragraph({ spacing: { before: 0, after: 50, line: 270 }, indent: { left: 170, right: 170 }, children: [run(t, { size: 24, color: ENCRE })] })];
const carteProfil = (p, sens, pourquoi, exemple, retenir) => {
  const titre = new Table({ layout: TableLayoutType.FIXED, width: { size: W4, type: WidthType.DXA }, columnWidths: [W4], rows: [new TableRow({ children: [
    cell([par(run(p.titre, { size: 28, bold: true, color: p.clair ? ENCRE : BLANC }), { after: 0, line: 250 })], W4, { fill: p.col, pad: 120, padx: 170, valign: VerticalAlign.CENTER })] })] });
  return cell([titre,
    ...rubrique("Ce que cela veut dire", sens),
    ...rubrique("Pourquoi ce classement ?", pourquoi),
    ...rubrique("Exemple fictif", exemple),
    ...rubrique("Ce qu’il faut retenir", retenir, { col: ACCENT }),
    vide(80)], W4, { fill: FOND, pad: 0, padx: 0 }); };

const cartes = [
  [P.enjeu, "Un grand pôle de la BITD où beaucoup de salariés pourraient partir, et où ces départs représentent une part importante des effectifs actuels.",
    "Les trois voyants sont au plus haut : poids fort, volume de départs élevé, intensité élevée (règle 6).",
    "8 % de l’emploi BITD national · 900 départs estimés · 14 % de l’emploi actuel à remplacer.",
    "Priorité : anticiper les recrutements et la transmission des savoir-faire sur plusieurs années."],
  [P.emergent, "Une part importante des salariés pourrait partir d’ici 2030, même si le département n’est pas un très grand pôle ou si le nombre de départs reste limité.",
    "L’intensité est élevée, mais le poids ou le volume ne sont pas au plus haut (règle 7).",
    "1,5 % de l’emploi national · 180 départs · 16 % de l’emploi actuel.",
    "Un signal de vigilance, pas une pénurie annoncée : vérifier la capacité locale à recruter avant d’en faire une priorité."],
  [P.majeur_modere, "Un grand pôle avec beaucoup de départs en nombre, mais à un rythme ordinaire par rapport à ses effectifs.",
    "Volume élevé et intensité modérée, ni élevée ni faible (règle 8).",
    "9 % de l’emploi national · 1 000 départs · 9 % de l’emploi actuel.",
    "Lire le nombre : les postes à pourvoir sont nombreux, même si le rythme n’a rien d’exceptionnel."],
  [P.concentre, "Les départs attendus concernent surtout une catégorie de salariés.",
    "Une catégorie porte au moins la moitié des départs, alors que le volume et l’intensité ne sont pas élevés (règle 9, après les profils prioritaires).",
    "150 départs, dont 65 chez les ouvriers · 8 % de l’emploi actuel.",
    "Regarder cette catégorie et sa part à remplacer, plutôt que le total du département."],
  [P.stable, "Une implantation installée, dont les départs se répartissent entre les catégories à un rythme ordinaire.",
    "Aucune règle précédente ne s’applique : ni volume élevé, ni intensité élevée, ni départs concentrés (règle 10).",
    "1 % de l’emploi national · 120 départs · 7 % de l’emploi actuel · première catégorie : 38 % des départs.",
    "« Stable » ne veut dire ni zéro départ, ni absence de difficultés de recrutement : suivi ordinaire."],
  [P.diffus, "La BITD pèse peu dans ce département, ou y repose sur très peu de salariés.",
    "Le poids est faible : part nationale faible, ou effectif sous le plancher retenu pour l’analyse (règle 3).",
    "0,3 % de l’emploi national · 60 salariés · 9 départs, soit 15 % de l’emploi actuel.",
    "Un taux élevé sur un petit effectif ne se lit pas comme celui d’un grand pôle : il ne crée pas de priorité."],
  [P.expertiser, "Les règles simples ne permettent pas de conclure : le département est signalé, avec son motif, pour une analyse complémentaire.",
    "Quatre cas : un indicateur manque · intensité extrême sur une implantation réduite · deux indicateurs tout près d’un seuil · volume élevé mais intensité faible (règles 1, 2, 4, 5).",
    "30 salariés et 12 départs (40 %) ; ou 1 100 départs mais seulement 3 % de l’emploi actuel.",
    "Nous préférons signaler une situation à analyser plutôt que lui attribuer artificiellement un profil."]];
const cardCells = cartes.map(c => carteProfil(...c));

const garder = cell([
  etiquette("À garder en tête", NAVY),
  ...["Les estimations sont établies par département, pas par bassin d’emploi.",
      "Les tensions repérées par la DGA ou France Travail (métiers, bassins) sont une information distincte : elles ne déterminent pas ces sept profils.",
      "Des départs estimés ne sont ni des postes ouverts, ni une pénurie de main-d’œuvre.",
      "Faible, moyen, fort dépendent des seuils utilisés. Aujourd’hui, les bornes sont des terciles observés : une règle de classement statistique provisoire, pas des seuils de politique publique.",
      "Le secret statistique est respecté : tous les chiffres de cette fiche sont fictifs."].map(t => par([run("–  ", { size: 23, color: TEXTE }), run(t, { size: 23, color: ENCRE })], { after: 110, line: 268 })),
  vide(20)], W4, { fill: "FFFFFF", pad: 150, padx: 170, borders: { top: { style: BorderStyle.SINGLE, size: 6, color: FILET }, bottom: { style: BorderStyle.SINGLE, size: 6, color: FILET }, left: { style: BorderStyle.SINGLE, size: 6, color: FILET }, right: { style: BorderStyle.SINGLE, size: 6, color: FILET } } });

const grille = (cells) => table(Array(7).fill(0).map((_, i) => i % 2 ? G4 : W4), [ligneGrille(cells, W4, G4)]);
const page3 = [
  ...entete("Typologie des territoires BITD  ·  fiche de lecture  ·  3 / 4", "Les sept situations territoriales (1 à 4)",
    "Sept profils, sans échelle unique de gravité : chacun décrit une situation de renouvellement différente. Exemples fictifs ; les classes (faible, moyen, fort…) dépendent des seuils en vigueur."),
  grille(cardCells.slice(0, 4))];
const page4 = [
  ...entete("Typologie des territoires BITD  ·  fiche de lecture  ·  4 / 4", "Les sept situations territoriales (5 à 7)",
    "Les trois derniers profils, dont la situation à expertiser, et les précautions de lecture."),
  grille([...cardCells.slice(4, 7), garder])];

const doc = new Document({
  creator: "Estimation des départs BITD", title: "Typologie des territoires BITD — fiche de lecture",
  styles: { default: { document: { run: { font: FONT, size: 19, color: ENCRE } } } },
  sections: [
    ...[page1, page2, page3, page4].map(ch => ({ properties: { page: PAGE }, footers: { default: pied("Estimation des départs à l’horizon 2030 · chiffres fictifs · secret statistique respecté") }, children: ch }))] });
Packer.toBuffer(doc).then(b => { fs.writeFileSync(SORTIE, b); console.log("écrit :", SORTIE); });
