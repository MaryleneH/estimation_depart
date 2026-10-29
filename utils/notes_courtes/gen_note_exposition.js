// Note courte « Départs attendus d’ici 2030 et difficultés de recrutement » pour la décideuse (docx-js, A4 portrait, style sobre : deux à trois pages).
// Usage : node gen_note_exposition.js [fichier_sortie.docx]   (défaut : docs/note_courte_exposition_tension.docx). Chiffres FICTIFS, aucune donnée du projet.
const fs = require("fs"), path = require("path");
const { Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell, WidthType, AlignmentType, BorderStyle, ShadingType,
        TableLayoutType, VerticalAlign, Footer, Header, PageNumber, LevelFormat, HeadingLevel, TabStopType } = require("docx");
const SORTIE = process.argv[2] || path.join(__dirname, "..", "..", "docs", "note_courte_exposition_tension.docx");
const NAVY = "1F2933", ENCRE = "1F2933", TEXTE = "3E4C59", GRIS = "7A8795", FILET = "C9CED6", BLEU_CLAIR = "FFFFFF", BLANC = "FFFFFF";
const ORANGE = "1F2933", ORANGE_CLAIR = "FFFFFF", VERT = "1F2933", VERT_CLAIR = "FFFFFF";
const FONT = "Calibri";
const NONE = { style: BorderStyle.NONE, size: 0, color: BLANC }, SANS = { top: NONE, bottom: NONE, left: NONE, right: NONE };
const LARGEUR = 8788;
const PAGE = { size: { width: 11906, height: 16838 }, margin: { top: 1560, bottom: 1400, left: 1559, right: 1559, header: 700, footer: 700 } };
const PERIMETRE = process.env.PERIMETRE || "Base";
const LISTE = process.env.LISTE_METIERS || "métiers de la première sous-liste";   // libellé public de la liste de métiers (00_config TENSION_LIBELLE_LISTE)   // nom du périmètre dans le texte (en interne : PERIMETRE=BITD node …)
const run = (t, o = {}) => new TextRun({ text: t.replace(/\bBase\b(?! Tous)/g, PERIMETRE).replace(/métiers stratégiques( suivis| de la liste)?/g, LISTE).replace(/métier stratégique/g, LISTE.replace(/^métiers /, "métier ")).replace(/PCS stratégiques?/g, LISTE.replace(/^métiers? /, "PCS ")), font: FONT, size: o.size || 21, color: o.color || ENCRE, bold: o.bold, italics: o.italics, characterSpacing: o.spacing });
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
  return table(widths, lignes.map((l, k) => l.map((t, i) => cell([par(rich(t, { size: 20, color: i === 0 ? ENCRE : TEXTE }), { after: 0, line: 280 })], widths[i], { borders: B, pad: 90 })))); };
const entete = new Header({ children: [new Paragraph({ children: [run("", { size: 4 })] })] });
const pied = new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER, children: [new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 16, color: GRIS })] })] });

const corps = [
  par(run("Note de lecture", { size: 20, color: GRIS }), { after: 200 }),
  par(run("Départs attendus d’ici 2030 et difficultés de recrutement", { size: 32, bold: true, color: ENCRE }), { after: 120, line: 340 }),
  par(run("Ce que mesure l’estimation des départs, ce qu’ajoute la part des remplacements difficiles, et comment lire les résultats.", { size: 22, color: TEXTE }), { after: 360, line: 300 }),

  h1("Combien de salariés faudra-t-il remplacer d’ici 2030 ?"),
  texte("L’estimation des départs répond à cette question : parmi les salariés présents en 2024 dans les entreprises du périmètre, combien auront définitivement quitté l’emploi d’ici 2030, et devront donc avoir été remplacés ? Un « départ » est une **sortie définitive de l’emploi** : retraite ou fin de carrière, mais aussi invalidité ou décès. **Ce n’est pas un décompte des départs en retraite.**"),
  texte("**Ce que les chiffres ne contiennent pas.** Les salariés qui quitteront leur métier ou leur entreprise pour une autre raison : changer d’employeur, se reconvertir, quitter un métier pénible avant l’âge de la retraite, partir vers un autre secteur. Ces mouvements existent, ils créent eux aussi des postes à pourvoir, et **ils ne sont pas comptés**. Les départs estimés sont donc **un plancher du besoin de remplacement**, pas son total ; l’écart est d’autant plus grand que le métier est mobile ou pénible."),
  texte("**Comment elle est construite.** On part des salariés présents en 2024. À chacun est appliquée une probabilité de sortie d’ici 2030, qui dépend de son âge, de son sexe et de sa catégorie professionnelle, et qui est tirée de sources nationales (âges de sortie de l’emploi, entrées en invalidité, mortalité). La somme de ces probabilités donne le nombre de départs attendus. Le résultat est une estimation centrale, encadrée d’une fourchette basse et haute."),
  texte("**Ce que l’estimation ne fait pas.** Elle applique des règles générales et ne suit aucune trajectoire individuelle : elle ignore la carrière passée de chacun, ses trimestres acquis, sa situation familiale, ses projets, son état de santé, et les embauches à venir. Elle ne dit donc jamais qui partira, ni quand ; elle dit au minimum combien de remplacements il faut préparer, par territoire et par métier, à conditions de 2024 inchangées. Pour un salarié donné, seul un examen de sa situation personnelle permet de parler de départ en retraite."),

  h1("Ce que la note apporte"),
  texte("Cette note croise ces remplacements à préparer avec la difficulté à recruter sur les métiers concernés : **les départs portent-ils sur des métiers difficiles à recruter ?** Un remplacement dans un métier où les candidats manquent pèse plus lourd qu’un remplacement dans un métier facile à pourvoir."),
  texte("Elle répond par trois éléments : une **part des remplacements difficiles** par département, un **levier** par département, et une **liste des métiers** à surveiller en priorité."),

  h1("Comment c’est mesuré"),
  texte("**Une liste de métiers stratégiques.** Environ quatre-vingts métiers ont été retenus. Pour chacun, la Dares (ministère du Travail) publie chaque année un indicateur de tension sur le marché du travail français, de 1, métier qui recrute facilement, à 5, métier très difficile à recruter. Un métier est dit **en tension** à partir de 4. **La tension des autres métiers n’est pas observée** : tous les résultats qui suivent valent pour cette liste, et un zéro veut dire « aucun parmi les métiers suivis », pas « aucun »."),
  texte("**La part des remplacements difficiles d’un département.** Sur 100 salariés à remplacer d’ici 2030 dans les métiers stratégiques du département, combien partent de métiers où l’on peine déjà à recruter en France ? C’est la part des remplacements difficiles. Elle porte sur les seuls départs définitifs décrits plus haut : un métier pénible et en tension peut donc être sous-estimé, sur ses départs comme sur cette part. Elle est comparée à la même part calculée pour la France entière, et classée faible, modérée ou forte."),
  texte("Deux précautions. La part n’est pas interprétée quand les métiers stratégiques pèsent trop peu dans les départs du département, car elle reposerait sur trop peu de cas. Elle est marquée « à confirmer » lorsqu’un petit changement d’hypothèse la ferait changer de classe ; sinon elle est « robuste »."),
  texte("**Le levier.** La Dares décompose la tension d’un métier en sept causes possibles. Pour chaque métier en tension, la cause la plus marquée est retenue et traduite en action :"),
  grille([3300, 5488], [
    ["Manque de candidats", "attirer de nouveaux viviers de recrutement"],
    ["Formation longue et spécifique", "former tôt"],
    ["Conditions de travail", "fidéliser en améliorant les conditions de travail"],
    ["Rémunération", "agir sur la rémunération"],
    ["Éloignement géographique", "favoriser la mobilité et l’implantation"],
    ["Emplois peu stables", "stabiliser les emplois"],
    ["Embauches très nombreuses", "anticiper les recrutements"]]),
  vide(120),
  texte("Pour un département, le levier retenu est celui qui concerne **le plus grand nombre de ses départs attendus** dans les métiers en tension. Il décrit la nature dominante de la difficulté ; ce n’est pas une recommandation."),

  h1("Comment lire les résultats"),
  ...puces([
    "**La carte des profils** dit combien de salariés partent et à quel rythme. Les départements hachurés sont ceux dont la part des remplacements difficiles est forte.",
    "**Un département à la fois en renouvellement rapide et hachuré** cumule les deux difficultés : il remplace beaucoup de salariés, dans des métiers où il sera difficile de recruter. Ces départements sont nommés en tête de la fiche.",
    "**La fiche des territoires** classe les régions puis les départements par nombre de départs attendus dans les métiers suivis en tension, en rappelant le profil de chacun. Chaque chiffre est un minimum : les métiers dont la tension n’est pas connue ne sont pas comptés.",
    "**Le tableau des métiers** classe chaque métier stratégique selon ses départs, rapides ou modérés par rapport à la moyenne, et sa tension, en tension ou non. Les métiers « à départs rapides et en tension » sont ceux où l’anticipation compte le plus."]),
  ...encadre("Ce que la note ne dit pas", [
    "**L’information est partielle.** La tension n’est connue que pour les métiers stratégiques de la liste. D’autres métiers peuvent être en tension sans que nous le sachions ; la couverture, affichée pour chaque département, dit quelle part des départs est observée.",
    "Ce n’est **pas une prévision de pénurie**. Elle signale des départs qui porteront sur des métiers difficiles à recruter.",
    "La tension est celle du métier **en France entière**. Un département à forte part de remplacements difficiles n’est pas un département qui manque de candidats ; c’est un département dont les départs tombent sur des métiers tendus partout.",
    "Les départs estimés ne comptent pas les mobilités et les reconversions : le besoin réel de remplacement peut être plus élevé, surtout dans les métiers pénibles ou mobiles.",
    "Elle n’identifie aucune personne : les nombres sont des sommes de probabilités, jamais des listes de salariés.",
    "Le secret statistique s’applique : un département dont les effectifs sont trop faibles n’a aucune valeur publiée, et seules des parts et des classes sont diffusées, jamais des effectifs."], ORANGE, ORANGE_CLAIR),
  ...encadre("Un exemple fictif", [
    "Dans un département imaginaire, 300 départs sont attendus d’ici 2030 dans les métiers stratégiques, dont 210 dans des métiers en tension : part des remplacements difficiles 70 %, contre 55 % en France, classe forte.",
    "Parmi ces 210 départs, 120 concernent des métiers dont la difficulté tient d’abord au manque de candidats : le levier du département est « attirer de nouveaux viviers ».",
    "Si ce département a aussi un profil de renouvellement rapide, il figure parmi les départements qui cumulent les deux difficultés."], VERT, VERT_CLAIR),

  h1("Sources"),
  texte("Estimation des départs : modèle du projet, sur les salariés des entreprises du périmètre en 2024 ; âges de sortie de l’emploi (DREES), entrées en invalidité, mortalité (Insee). Tension des métiers : indicateurs Dares, édition à préciser, par famille professionnelle. Méthode détaillée, paramètres et règles de secret : note méthodologique jointe.", { after: 0 })
];
const doc = new Document({
  styles: { default: { document: { run: { font: FONT, size: 21, color: ENCRE } } } },
  numbering: { config: [{ reference: "puces", levels: [{ level: 0, format: LevelFormat.BULLET, text: "–", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 360, hanging: 300 } }, run: { color: GRIS } } }] }] },
  sections: [{ properties: { page: PAGE }, headers: { default: entete }, footers: { default: pied }, children: corps }] });
Packer.toBuffer(doc).then(b => { fs.writeFileSync(SORTIE, b); console.log("écrit :", SORTIE); });
