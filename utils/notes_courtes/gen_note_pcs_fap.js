// Note courte « PCS, FAP : les repères pour lire les résultats » pour la décideuse (docx-js, A4 portrait, style sobre : deux à trois pages).
// Usage : node gen_note_pcs_fap.js [fichier_sortie.docx]   (défaut : docs/note_courte_pcs_fap.docx). Chiffres FICTIFS, aucune donnée du projet.
const fs = require("fs"), path = require("path");
const { Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell, WidthType, AlignmentType, BorderStyle, ShadingType,
        TableLayoutType, VerticalAlign, Footer, Header, PageNumber, LevelFormat, HeadingLevel, TabStopType } = require("docx");
const SORTIE = process.argv[2] || path.join(__dirname, "..", "..", "docs", "note_courte_pcs_fap.docx");
const NAVY = "1F2933", ENCRE = "1F2933", TEXTE = "3E4C59", GRIS = "7A8795", FILET = "C9CED6", BLEU_CLAIR = "FFFFFF", BLANC = "FFFFFF";
const ORANGE = "1F2933", ORANGE_CLAIR = "FFFFFF", VERT = "1F2933", VERT_CLAIR = "FFFFFF";
const FONT = "Calibri";
const NONE = { style: BorderStyle.NONE, size: 0, color: BLANC }, SANS = { top: NONE, bottom: NONE, left: NONE, right: NONE };
const LARGEUR = 8788;
const PAGE = { size: { width: 11906, height: 16838 }, margin: { top: 1560, bottom: 1400, left: 1559, right: 1559, header: 700, footer: 700 } };
const PERIMETRE = process.env.PERIMETRE || "Base";   // nom du périmètre dans le texte (en interne : PERIMETRE=BITD node …)
const run = (t, o = {}) => new TextRun({ text: t.replace(/\bBase\b(?! Tous)/g, PERIMETRE), font: FONT, size: o.size || 21, color: o.color || ENCRE, bold: o.bold, italics: o.italics, characterSpacing: o.spacing });
const rich = (t, o = {}) => t.split(/(\*\*[^*]+\*\*)/).filter(Boolean).map(s => s.startsWith("**") ? run(s.slice(2, -2), { ...o, bold: true }) : run(s, o));
const par = (runs, o = {}) => new Paragraph({ spacing: { before: o.before || 0, after: o.after === undefined ? 160 : o.after, line: o.line || 300 }, alignment: o.align, keepNext: o.keepNext, keepLines: o.keepLines, children: Array.isArray(runs) ? runs : [runs] });
const texte = (t, o = {}) => par(rich(t, o), o);
const h1 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 420, after: 160, line: 276 }, keepNext: true, children: [run(t, { size: 24, bold: true, color: ENCRE })] });
const puces = (items, o = {}) => items.map(t => new Paragraph({ numbering: { reference: "puces", level: 0 }, spacing: { before: 0, after: 110, line: 300 }, children: rich(t, o) }));
const vide = (h = 80) => new Paragraph({ spacing: { before: 0, after: 0, line: h, lineRule: "exact" }, children: [run("", { size: 4 })] });
const cell = (children, w, o = {}) => new TableCell({ width: { size: w, type: WidthType.DXA }, borders: o.borders || SANS, verticalAlign: VerticalAlign.TOP,
  margins: { top: o.pad === undefined ? 100 : o.pad, bottom: o.pad === undefined ? 100 : o.pad, left: 60, right: 200 }, children });
const table = (widths, rows) => new Table({ layout: TableLayoutType.FIXED, width: { size: widths.reduce((a, b) => a + b, 0), type: WidthType.DXA }, columnWidths: widths, rows: rows.map(r => new TableRow({ cantSplit: true, children: r })) });
const encadre = (etiquette, contenu, couleur, fond) => [h1(etiquette), ...contenu.map((t, i) => texte(t, { after: 160 }))];
const grille = (widths, lignes) => { const bord = { style: BorderStyle.SINGLE, size: 4, color: FILET }, B = { top: bord, bottom: bord, left: NONE, right: NONE };
  return table(widths, lignes.map((l, k) => l.map((t, i) => cell([par(rich(t, { size: 20, bold: k === 0, color: i === 0 || k === 0 ? ENCRE : TEXTE }), { after: 0, line: 280 })], widths[i], { borders: B, pad: 90 })))); };
const entete = new Header({ children: [new Paragraph({ children: [run("", { size: 4 })] })] });
const pied = new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER, children: [new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 16, color: GRIS })] })] });

const corps = [
  par(run("Note de lecture", { size: 20, color: GRIS }), { after: 200 }),
  par(run("PCS, FAP : les repères pour lire les résultats", { size: 32, bold: true, color: ENCRE }), { after: 120, line: 340 }),
  par(run("Deux façons de classer un emploi, et ce qu’elles permettent de dire.", { size: 22, color: TEXTE }), { after: 360, line: 300 }),

  h1("Deux questions, deux grilles"),
  texte("Un même salarié peut être décrit de deux façons. **Quelle place occupe-t-il dans l’emploi ?** C’est la question de la PCS, la nomenclature de l’Insee : cadre, profession intermédiaire, employé, ouvrier, puis, en descendant dans le détail, une profession précise. **Quel métier exerce-t-il, au sens du marché du travail ?** C’est la question des familles professionnelles (FAP), la nomenclature du ministère du Travail, qui regroupe les professions faisant appel aux mêmes compétences, quel que soit le statut : la maintenance industrielle réunit ainsi des ouvriers qualifiés, des techniciens et des agents de maîtrise."),
  texte("Ces deux grilles ne sont pas deux niveaux de précision d’une même liste. Elles ont été construites par deux institutions pour deux usages : décrire l’emploi pour la PCS, analyser les métiers et les recrutements pour la FAP. C’est dans la FAP que sont mesurées les tensions de recrutement. On ne passe de l’une à l’autre que par une table de correspondance officielle, profession par profession, jamais par équivalence de nom : « famille technique » ne veut pas dire « professions intermédiaires »."),

  h1("Comment le projet s’en sert"),
  ...puces([
    "**Les départs sont estimés par grande catégorie** (cadres, professions intermédiaires, employés, ouvriers). Plus on descend dans le détail, plus les effectifs sont petits et plus les résultats sont fragiles ou soumis au secret statistique. La grande catégorie donne des résultats robustes, lisibles et comparables entre territoires.",
    "**La profession détaillée sert à nommer et à contrôler.** Les tables et les cartes par profession, en France, par région et par département, disent quels métiers portent les départs. Elles complètent la lecture par grande catégorie ; elles ne la remplacent pas.",
    "**La FAP n’intervient que pour la tension.** Pour chaque métier stratégique, la profession détaillée est rattachée à sa famille professionnelle, ce qui permet de lui associer l’indicateur de tension de la Dares. Le projet ne construit jamais de correspondance entre une famille et une grande catégorie."]),

  h1("Cinq repères avant de lire un chiffre"),
  ...puces([
    "**Volume ou part ?** Un volume classe d’abord les gros territoires ; une part classe les territoires les plus exposés. Les deux cartes n’ont pas le même ordre.",
    "**Quel dénominateur ?** Les salariés concernés par le modèle, à partir de l’âge minimal, ou l’ensemble des salariés actuels. Chaque carte le précise ; les deux parts ne s’additionnent pas.",
    "**Constat ou estimation ?** La structure de l’emploi et la tension sont observées aujourd’hui ; les départs sont estimés pour 2030, avec une fourchette.",
    "**Catégorie ou métier ?** Une grande catégorie mêle des métiers très différents ; un métier se lit à la profession détaillée ou à la famille professionnelle.",
    "**Gris ou blanc ?** Une case grise est un résultat non diffusé pour secret statistique, jamais un zéro ; une case blanche est une absence de salariés dans le champ."]),

  h1("Quatre phrases à éviter"),
  grille([4394, 4394], [
    ["À éviter", "À dire"],
    ["« Les professions intermédiaires sont en tension. »", "« Tel métier, rattaché à telle famille professionnelle, est en tension en France. »"],
    ["« Ce département est en tension. »", "« Les départs de ce département portent sur des métiers en tension en France. »"],
    ["« Ce territoire est le plus touché : il a le plus de départs. »", "« Il porte le plus gros volume ; rapporté à ses effectifs, sa part est de x %. »"],
    ["« Il y aura 900 départs. »", "« Environ 900 départs sont estimés d’ici 2030, entre 850 et 950 selon l’hypothèse. » (chiffres fictifs)"]]),
  vide(160),
  texte("**Pour aller plus loin.** Note pédagogique sur les cartes d’emploi ; note de référence sur les nomenclatures (PCS, CS, FAP, ROME) ; note courte sur les départs attendus et les remplacements difficiles.", { after: 0 })
];
const doc = new Document({
  styles: { default: { document: { run: { font: FONT, size: 21, color: ENCRE } } } },
  numbering: { config: [{ reference: "puces", levels: [{ level: 0, format: LevelFormat.BULLET, text: "–", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 360, hanging: 300 } }, run: { color: GRIS } } }] }] },
  sections: [{ properties: { page: PAGE }, headers: { default: entete }, footers: { default: pied }, children: corps }] });
Packer.toBuffer(doc).then(b => { fs.writeFileSync(SORTIE, b); console.log("écrit :", SORTIE); });
