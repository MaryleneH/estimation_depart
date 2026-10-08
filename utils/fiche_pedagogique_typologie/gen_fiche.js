// Fiche pédagogique « typologie territoriale BITD » : deux pages A4 paysage (docx-js).
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
const etiquette = (t, col = NAVY) => par(run(t.toUpperCase(), { size: 14, bold: true, color: col, spacing: 15 }), { after: 20, before: 50 });
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
const LARGEUR = 15700;
const PAGE = { size: { width: 11906, height: 16838, orientation: PageOrientation.LANDSCAPE }, margin: { top: 400, bottom: 340, left: 569, right: 569 } };
const pied = (t) => new Footer({ children: [new Paragraph({ alignment: AlignmentType.RIGHT, children: [run(t + "  ·  page ", { size: 14, color: TEXTE }), new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 14, color: TEXTE }), run(" / ", { size: 14, color: TEXTE }), new TextRun({ children: [PageNumber.TOTAL_PAGES], font: FONT, size: 14, color: TEXTE })] })] });

// --- En-tête de page ----------------------------------------------------------------
const entete = (kicker, titre, accroche) => [
  par(run(kicker, { size: 15, bold: true, color: TEXTE, spacing: 25 }), { after: 0, line: 240 }),
  par(run(titre, { size: 38, bold: true, color: NAVY }), { before: 60, after: 20, line: 276 }),
  par(run(accroche, { size: 19, color: ENCRE }), { after: 40, line: 235 }), filet()];

// ===================================================================================
// PAGE 1 — Comment lit-on un territoire ?
// ===================================================================================
const W5 = 3020, G5 = 150;   // 5 cartes + 4 intervalles = 15 700
const carteIndicateur = (num, titre, question, explication, img) => cell([
  par([run(num + "  ", { size: 20, bold: true, color: ACCENT }), run(titre.toUpperCase(), { size: 17, bold: true, color: NAVY, spacing: 12 })], { after: 30 }),
  par(run(question, { size: 18, bold: true, color: ENCRE }), { after: 50, line: 230 }),
  image(img, 182, 79),
  texte(explication, { size: 15.5, color: TEXTE, after: 0 })], W5, { fill: FOND, padx: 100 });

const indicateurs = table(Array(9).fill(0).map((_, i) => i % 2 ? G5 : W5), [ligneGrille([
  carteIndicateur("1", "Le poids", "Quelle place occupe ce département dans l’emploi BITD national ?",
    "Sur 100 salariés de la BITD en France, combien travaillent ici ? Ici 8 sur 100. Classé faible, moyen ou fort ; un très petit effectif est toujours « faible ».", "poids.png"),
  carteIndicateur("2", "Le volume", "Combien de salariés pourraient partir d’ici 2030 ?",
    "Un nombre de départs estimés, ici 900. Il dépend surtout de la taille du département. Classé faible, modéré ou élevé.", "volume.png"),
  carteIndicateur("3", "L’intensité", "Sur 100 salariés en poste, combien correspondent aux départs estimés ?",
    "Ici 12 sur 100 : une part de l’emploi actuel à remplacer, pas un taux de recrutement observé. Classée faible, modérée ou élevée.", "intensite.png"),
  carteIndicateur("4", "La structure", "Quels salariés sont les plus représentés aujourd’hui ?",
    "« Dominante » si une catégorie atteint une part suffisante de l’emploi, sinon « mixte ». Décrit le département ; n’entre pas dans le choix du profil.", "structure.png"),
  carteIndicateur("5", "La concentration", "Les départs concernent-ils surtout une catégorie ?",
    "Ici 70 départs sur 100 concernent les ouvriers. Distinct de la structure : une catégorie peut dominer l’emploi sans dominer les départs. Peut décider du profil.", "concentration.png")], W5, G5)]);

// Comparaison A / B
const WG = 8300, GG = 200, WD = 7200;   // 8 300 + 200 + 7 200 = 15 700
const blocAB = cell([
  etiquette("Volume et intensité : deux départements, deux lectures"),
  par(run("A compte deux fois plus de départs. Pourtant, B est proportionnellement cinq fois plus concerné.", { size: 18, bold: true, color: ENCRE }), { after: 40, line: 235 }),
  image("ab.png", 530, 219),
  texte("Volume : combien de départs ? Intensité : quelle part de l’emploi actuel représentent-ils ? Exemples entièrement fictifs : ces taux illustrent une ampleur de renouvellement, pas une pénurie démontrée.", { size: 15.5, color: TEXTE, after: 0 })], WG, { fill: FOND });

// Des indicateurs au profil : règles ordonnées
const pastille = (col) => run("■ ", { size: 18, color: col });
const regle = (n, si, prof, colProf) => new TableRow({ cantSplit: true, children: [
  cell([par(run(n, { size: 15, bold: true, color: TEXTE }), { after: 0 })], 330, { pad: 8, padx: 40 }),
  cell([par(run(si, { size: 15, color: ENCRE }), { after: 0, line: 215 })], 3510, { pad: 8, padx: 60 }),
  cell([par([pastille(colProf), run(prof, { size: 14.5, bold: true, color: ENCRE })], { after: 0, line: 215 })], 3100, { pad: 8, padx: 60 })] });
const regles = new Table({ layout: TableLayoutType.FIXED, width: { size: 6940, type: WidthType.DXA }, columnWidths: [330, 3510, 3100], rows: [
  new TableRow({ children: [cell([par(run("", { size: 12 }), { after: 0 })], 330, { pad: 10, padx: 40 }),
    cell([par(run("SI…", { size: 13, bold: true, color: TEXTE, spacing: 12 }), { after: 0 })], 3510, { pad: 6, padx: 60 }),
    cell([par(run("ALORS LE PROFIL EST…", { size: 13, bold: true, color: TEXTE, spacing: 12 }), { after: 0 })], 3100, { pad: 6, padx: 60 })] }),
  regle("1", "un indicateur de classement manque", P.expertiser.titre, P.expertiser.col),
  regle("2", "poids faible et intensité extrême (petit effectif)", P.expertiser.titre, P.expertiser.col),
  regle("3", "poids faible", P.diffus.titre, "B8C4D2"),
  regle("4", "au moins deux indicateurs tout près d’un seuil", P.expertiser.titre, P.expertiser.col),
  regle("5", "volume élevé mais intensité faible (contradiction)", P.expertiser.titre, P.expertiser.col),
  regle("6", "poids fort, volume élevé et intensité élevée", P.enjeu.titre, P.enjeu.col),
  regle("7", "intensité élevée", P.emergent.titre, P.emergent.col),
  regle("8", "volume élevé (intensité modérée)", P.majeur_modere.titre, P.majeur_modere.col),
  regle("9", "une catégorie porte au moins la moitié des départs", P.concentre.titre, P.concentre.col),
  regle("10", "aucune des règles précédentes", P.stable.titre, P.stable.col)] });

// Avant / après
const chip = (t, w, surligne) => cell([par(run(t, { size: 15, bold: true, color: surligne ? BLANC : ENCRE }), { after: 0, align: AlignmentType.CENTER, line: 215 })], w,
  { fill: surligne ? ACCENT : "FFFFFF", pad: 24, padx: 40, valign: VerticalAlign.CENTER, borders: surligne ? SANS : { top: { style: BorderStyle.SINGLE, size: 4, color: FILET }, bottom: { style: BorderStyle.SINGLE, size: 4, color: FILET }, left: { style: BorderStyle.SINGLE, size: 4, color: FILET }, right: { style: BorderStyle.SINGLE, size: 4, color: FILET } } });
const fleche = (w) => cell([par(run("→", { size: 24, bold: true, color: TEXTE }), { after: 0, align: AlignmentType.CENTER })], w, { pad: 0, padx: 0, valign: VerticalAlign.CENTER });
const resultat = (t, col, w) => cell([par([pastille(col), run(t, { size: 16, bold: true, color: ENCRE })], { after: 0, line: 215 })], w, { pad: 40, padx: 60, valign: VerticalAlign.CENTER });
const avantApres = new Table({ layout: TableLayoutType.FIXED, width: { size: 6940, type: WidthType.DXA }, columnWidths: [560, 1200, 80, 1200, 80, 1300, 360, 2160], rows: [
  new TableRow({ cantSplit: true, children: [cell([par(run("AVANT", { size: 13, bold: true, color: TEXTE, spacing: 12 }), { after: 0 })], 560, { pad: 30, padx: 20, valign: VerticalAlign.CENTER }),
    chip("Poids fort", 1200, false), gap(80), chip("Volume élevé", 1200, false), gap(80), chip("Intensité élevée", 1300, false), fleche(360), resultat(P.enjeu.titre, P.enjeu.col, 2160)] }),
  new TableRow({ children: [gap(560), gap(1200), gap(80), gap(1200), gap(80), gap(1300), gap(360), gap(2160)], height: { value: 40, rule: "exact" } }),
  new TableRow({ cantSplit: true, children: [cell([par(run("APRÈS", { size: 13, bold: true, color: TEXTE, spacing: 12 }), { after: 0 })], 560, { pad: 30, padx: 20, valign: VerticalAlign.CENTER }),
    chip("Poids fort", 1200, false), gap(80), chip("Volume élevé", 1200, false), gap(80), chip("Intensité modérée", 1300, true), fleche(360), resultat(P.majeur_modere.titre, P.majeur_modere.col, 2160)] })] });

const blocDroite = cell([
  etiquette("Des indicateurs au profil : la première règle qui s’applique l’emporte"),
  texte("Règles examinées dans l’ordre : situations à expertiser et petites implantations d’abord, puis le premier profil dont la condition est remplie. Aucun score, aucune addition d’indicateurs.", { size: 15.5, color: TEXTE, after: 40 }),
  regles,
  etiquette("Un seul indicateur change la lecture"),
  avantApres, vide(30)], WD, { fill: FOND });

const page1 = [
  ...entete("Typologie des territoires BITD  ·  fiche de lecture  ·  page 1", "Comment lit-on un territoire ?",
    "La typologie ne cherche pas seulement où les départs seront nombreux : elle distingue des situations de renouvellement différentes, à partir de cinq indicateurs lisibles et de règles explicites."),
  etiquette("Les cinq ingrédients du classement (exemples fictifs)"),
  indicateurs, vide(100),
  table([WG, GG, WD], [[blocAB, gap(GG), blocDroite]])];

// ===================================================================================
// PAGE 2 — Les 7 situations territoriales
// ===================================================================================
const W4 = 3790, G4 = 180;   // 4 cartes + 3 intervalles = 15 700
const rubrique = (lab, t, o = {}) => [
  new Paragraph({ spacing: { before: 50, after: 15, line: 240 }, indent: { left: 130, right: 130 }, children: [run(lab.toUpperCase(), { size: 14, bold: true, color: o.col || NAVY, spacing: 15 })] }),
  new Paragraph({ spacing: { before: 0, after: 25, line: 235 }, indent: { left: 130, right: 130 }, children: [run(t, { size: 17, color: ENCRE })] })];
const carteProfil = (p, sens, pourquoi, exemple, retenir) => {
  const titre = new Table({ layout: TableLayoutType.FIXED, width: { size: W4, type: WidthType.DXA }, columnWidths: [W4], rows: [new TableRow({ children: [
    cell([par(run(p.titre, { size: 20, bold: true, color: p.clair ? ENCRE : BLANC }), { after: 0, line: 230 })], W4, { fill: p.col, pad: 70, padx: 130, valign: VerticalAlign.CENTER })] })] });
  return cell([titre,
    ...rubrique("Ce que cela veut dire", sens),
    ...rubrique("Pourquoi ce classement ?", pourquoi),
    ...rubrique("Exemple fictif", exemple),
    ...rubrique("Ce qu’il faut retenir", retenir, { col: ACCENT }),
    vide(40)], W4, { fill: FOND, pad: 0, padx: 0 }); };

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
    "Lire le nombre : les postes à pourvoir sont nombreux même si le rythme n’a rien d’exceptionnel."],
  [P.concentre, "Les départs attendus concernent surtout une catégorie de salariés.",
    "Une catégorie porte au moins la moitié des départs, alors que le volume et l’intensité ne sont pas élevés (règle 9, après les profils prioritaires).",
    "150 départs, dont 65 chez les ouvriers · 8 % de l’emploi actuel.",
    "Regarder cette catégorie et sa part à remplacer plutôt que le total du département."],
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
      "Le secret statistique est respecté : tous les chiffres de cette fiche sont fictifs."].map(t => par([run("–  ", { size: 16, color: TEXTE }), run(t, { size: 16, color: ENCRE })], { after: 50, line: 225 })),
  vide(20)], W4, { fill: "FFFFFF", borders: { top: { style: BorderStyle.SINGLE, size: 6, color: FILET }, bottom: { style: BorderStyle.SINGLE, size: 6, color: FILET }, left: { style: BorderStyle.SINGLE, size: 6, color: FILET }, right: { style: BorderStyle.SINGLE, size: 6, color: FILET } } });

const grille2 = (cells) => table(Array(7).fill(0).map((_, i) => i % 2 ? G4 : W4), [ligneGrille(cells, W4, G4)]);
const page2 = [
  ...entete("Typologie des territoires BITD  ·  fiche de lecture  ·  page 2", "Les sept situations territoriales",
    "Sept profils, sans échelle unique de gravité : chacun décrit une situation de renouvellement différente. Exemples fictifs ; les classes (faible, moyen, fort…) dépendent des seuils en vigueur."),
  grille2(cardCells.slice(0, 4)), vide(110),
  grille2([...cardCells.slice(4, 7), garder])];

const doc = new Document({
  creator: "Estimation des départs BITD", title: "Typologie des territoires BITD — fiche de lecture",
  styles: { default: { document: { run: { font: FONT, size: 19, color: ENCRE } } } },
  sections: [
    { properties: { page: PAGE }, footers: { default: pied("Estimation des départs à l’horizon 2030 · chiffres fictifs · secret statistique respecté") }, children: page1 },
    { properties: { page: PAGE }, footers: { default: pied("Estimation des départs à l’horizon 2030 · chiffres fictifs · secret statistique respecté") }, children: page2 }] });
Packer.toBuffer(doc).then(b => { fs.writeFileSync(SORTIE, b); console.log("écrit :", SORTIE); });
