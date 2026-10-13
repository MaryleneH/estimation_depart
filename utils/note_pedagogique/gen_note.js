// Note pédagogique « PCS, grandes catégories, FAP : comment lire les cartes d'emploi de la Base ? » (docx-js, A4 portrait).
// Usage : node gen_note.js <dossier_images> <fichier_sortie.docx>
// Tous les chiffres de la note sont FICTIFS et signalés comme tels ; aucune donnée du projet n'y figure.
const fs = require("fs"), path = require("path");
const { Document, Packer, Paragraph, TextRun, ImageRun, Table, TableRow, TableCell, WidthType, AlignmentType, BorderStyle, ShadingType,
        TableLayoutType, VerticalAlign, Footer, Header, PageNumber, PageBreak, ExternalHyperlink, LevelFormat, HeadingLevel, TabStopType } = require("docx");
const DIR_IMG = process.argv[2] || path.join(__dirname, "..", "..", "docs", "assets", "note_pedagogique");
const SORTIE = process.argv[3] || path.join(__dirname, "..", "..", "docs", "note_pedagogique_PCS_FAP_cartes_Base.docx");

// --- Charte ---------------------------------------------------------------------------------
const NAVY = "1E3A5F", ENCRE = "1F2933", TEXTE = "3E4C59", GRIS = "7A8795", FILET = "D9DEE5", FOND = "F5F7FA", BLEU_CLAIR = "E3ECF6", BLANC = "FFFFFF";
const ORANGE = "C2620A", ORANGE_CLAIR = "FDF1E1", VERT = "2E7D5B", VERT_CLAIR = "E6F2EC";
const FONT = "Calibri";
const NONE = { style: BorderStyle.NONE, size: 0, color: BLANC }, SANS = { top: NONE, bottom: NONE, left: NONE, right: NONE };
const LARGEUR = 9638;   // A4 portrait, marges 2 cm
const PAGE = { size: { width: 11906, height: 16838 }, margin: { top: 1134, bottom: 1000, left: 1134, right: 1134, header: 500, footer: 500 } };

// --- Briques --------------------------------------------------------------------------------
const run = (t, o = {}) => new TextRun({ text: t, font: FONT, size: o.size || 20, color: o.color || ENCRE, bold: o.bold, italics: o.italics, characterSpacing: o.spacing });
const par = (runs, o = {}) => new Paragraph({ spacing: { before: o.before || 0, after: o.after === undefined ? 100 : o.after, line: o.line || 256 }, alignment: o.align,
  keepNext: o.keepNext, keepLines: o.keepLines, children: Array.isArray(runs) ? runs : [runs] });
// texte avec **gras** inline
const rich = (t, o = {}) => t.split(/(\*\*[^*]+\*\*)/).filter(Boolean).map(s => s.startsWith("**") ? run(s.slice(2, -2), { ...o, bold: true }) : run(s, o));
const texte = (t, o = {}) => par(rich(t, o), o);
const h1 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 280, after: 120, line: 276 }, keepNext: true,
  border: { bottom: { style: BorderStyle.SINGLE, size: 8, color: NAVY, space: 4 } }, children: [run(t, { size: 30, bold: true, color: NAVY })] });
const h2 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 200, after: 80, line: 276 }, keepNext: true, children: [run(t, { size: 24, bold: true, color: NAVY })] });
const puces = (items, o = {}) => items.map(t => new Paragraph({ numbering: { reference: "puces", level: 0 }, spacing: { before: 0, after: 50, line: 262 }, children: rich(t, o) }));
const numeros = (items, ref) => items.map(t => new Paragraph({ numbering: { reference: ref, level: 0 }, spacing: { before: 0, after: 60, line: 262 }, children: rich(t) }));
const vide = (h = 80) => new Paragraph({ spacing: { before: 0, after: 0, line: h, lineRule: "exact" }, children: [run("", { size: 4 })] });
const saut = () => new Paragraph({ children: [new PageBreak()] });
const image = (nom, wcm, o = {}) => { const [w, h] = tailles[nom]; const wpx = Math.round(wcm * 37.8), hpx = Math.round(wpx * h / w);
  return new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: o.before === undefined ? 60 : o.before, after: o.after === undefined ? 30 : o.after }, keepNext: o.keepNext !== false,
    children: [new ImageRun({ type: "png", data: fs.readFileSync(path.join(DIR_IMG, nom + ".png")), transformation: { width: wpx, height: hpx } })] }); };
const legende = (t) => par(run(t, { size: 16, color: GRIS, italics: true }), { align: AlignmentType.CENTER, after: 110 });
const cell = (children, w, o = {}) => new TableCell({ width: { size: w, type: WidthType.DXA }, borders: o.borders || SANS, verticalAlign: o.valign || VerticalAlign.TOP,
  shading: o.fill ? { type: ShadingType.CLEAR, fill: o.fill, color: "auto" } : undefined,
  margins: { top: o.pad === undefined ? 100 : o.pad, bottom: o.pad === undefined ? 100 : o.pad, left: o.padx === undefined ? 140 : o.padx, right: o.padx === undefined ? 140 : o.padx }, children });
const table = (widths, rows, o = {}) => new Table({ layout: TableLayoutType.FIXED, width: { size: widths.reduce((a, b) => a + b, 0), type: WidthType.DXA }, columnWidths: widths,
  rows: rows.map((r, i) => new TableRow({ cantSplit: true, tableHeader: o.entete && i === 0, children: r })) });
// encadré : une cellule, fond clair, filet gauche coloré, étiquette en capitales
const encadre = (etiquette, contenu, couleur = NAVY, fond = BLEU_CLAIR) => [table([LARGEUR], [[cell([
  par(run(etiquette.toUpperCase(), { size: 16, bold: true, color: couleur, spacing: 20 }), { after: 50 }),
  ...(Array.isArray(contenu) ? contenu : [texte(contenu, { size: 20, after: 0 })])], LARGEUR,
  { fill: fond, borders: { ...SANS, left: { style: BorderStyle.SINGLE, size: 24, color: couleur } }, pad: 110, padx: 200 })]]), vide(100)];
const retenir = (c) => encadre("À retenir", c, NAVY, BLEU_CLAIR);
const attention = (c, titre = "Attention à ne pas confondre") => encadre(titre, c, ORANGE, ORANGE_CLAIR);
const exemple = (c, titre = "Exemple fictif") => encadre(titre, c, VERT, VERT_CLAIR);
const citation = (t) => par(run(t, { size: 22, italics: true, color: NAVY }), { before: 40, after: 120, line: 276 });
// tableau à en-tête navy et lignes alternées
const grille = (widths, entete, lignes, o = {}) => {
  const bord = { style: BorderStyle.SINGLE, size: 4, color: FILET }, B = { top: bord, bottom: bord, left: NONE, right: NONE };
  const tete = entete.map((t, i) => cell([par(run(t, { size: 17, bold: true, color: BLANC }), { after: 0, line: 240 })], widths[i], { fill: NAVY, borders: B, pad: 80 }));
  const corps = lignes.map((l, k) => l.map((t, i) => cell([par(rich(t, { size: o.size || 18, color: i === 0 && o.premiereGras ? NAVY : ENCRE, bold: i === 0 && o.premiereGras }), { after: 0, line: 250 })], widths[i],
    { fill: k % 2 ? FOND : BLANC, borders: B, pad: 80 })));
  return table(widths, [tete, ...corps], { entete: true });
};
const lien = (t, url) => new ExternalHyperlink({ link: url, children: [new TextRun({ text: t, font: FONT, size: 18, color: "1F5FA8", underline: {} })] });
const tailles = {};   // dimensions des schémas (viewBox) lues dans les SVG
for (const f of fs.readdirSync(DIR_IMG).filter(f => f.endsWith(".svg"))) { const m = fs.readFileSync(path.join(DIR_IMG, f), "utf8").match(/viewBox="0 0 (\d+) (\d+)"/); tailles[f.replace(".svg", "")] = [+m[1], +m[2]]; }

// --- En-tête / pied --------------------------------------------------------------------------
const entete = new Header({ children: [new Paragraph({ tabStops: [{ type: TabStopType.RIGHT, position: LARGEUR }], border: { bottom: { style: BorderStyle.SINGLE, size: 4, color: FILET, space: 4 } },
  children: [run("Note pédagogique · PCS, grandes catégories, FAP et cartes d’emploi de la Base", { size: 15, color: GRIS }), run("\tTous les chiffres de cette note sont fictifs", { size: 15, color: GRIS, italics: true })] })] });
const pied = new Footer({ children: [new Paragraph({ tabStops: [{ type: TabStopType.RIGHT, position: LARGEUR }],
  children: [run("Lecture conseillée avant toute utilisation des cartes · octobre 2026", { size: 15, color: GRIS }), run("\tpage ", { size: 15, color: GRIS }),
    new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 15, color: GRIS }), run(" / ", { size: 15, color: GRIS }), new TextRun({ children: [PageNumber.TOTAL_PAGES], font: FONT, size: 15, color: GRIS })] })] });

// =============================================================================================
// PAGE 1 — La question en une minute
// =============================================================================================
const page1 = [
  par(run("NOTE PÉDAGOGIQUE · LIRE LES CARTES D’EMPLOI DE LA BASE", { size: 16, bold: true, color: GRIS, spacing: 30 }), { after: 120 }),
  par(run("PCS, grandes catégories, FAP : comment lire les cartes d’emploi de la Base ?", { size: 40, bold: true, color: NAVY }), { after: 80, line: 300 }),
  par(run("Comprendre ce que mesure chaque indicateur pour éviter les fausses comparaisons", { size: 24, color: TEXTE }), { after: 200, line: 280 }),
  texte("Les cartes du projet décrivent les salariés des entreprises de la Base de trois façons différentes : **qui occupe quel poste aujourd’hui** (la structure de l’emploi, lue dans la nomenclature PCS), **ce que cherchent les employeurs** (les métiers et les tensions, lus dans la nomenclature FAP) et **ce qui pourrait se passer d’ici 2030** (les départs estimés). Cette note explique ce que mesure chacune de ces lectures, pourquoi elles coexistent, et ce que l’on peut ou non comparer.", { after: 160 }),
  image("s1_trois_lectures", 16), legende("Schéma 1 — Trois lectures, trois questions différentes."),
  h2("Trois idées clés"),
  ...numeros(["**La PCS et la FAP ne répondent pas à la même question.** La PCS décrit d’abord la position d’une personne dans l’emploi ; la FAP rapproche les métiers du fonctionnement du marché du travail. Ce ne sont pas deux noms pour la même chose.",
    "**Un chiffre se lit avec sa maille, son dénominateur et son horizon.** Un département n’est pas un bassin d’emploi ; un volume n’est pas un taux ; une estimation à 2030 n’est pas un constat.",
    "**Plus on descend dans le détail, plus on gagne en précision et plus on perd en robustesse.** C’est pourquoi le projet estime les départs par grande catégorie, et réserve la PCS fine au détail et au contrôle."], "num1"),
  ...retenir("Ces trois lectures se complètent, mais elles ne sont pas interchangeables. Avant de rapprocher deux cartes, vérifiez qu’elles parlent du même territoire, de la même population et du même objet métier."),
  saut()];

// =============================================================================================
// 1. Les PCS
// =============================================================================================
const partie1 = [
  h1("1. Les PCS : une façon de décrire la position professionnelle"),
  texte("La nomenclature des **professions et catégories socioprofessionnelles (PCS)** est établie par l’Insee. Elle classe les personnes selon leur profession et leur place dans l’organisation du travail : cadre, technicien, employé, ouvrier… Elle ne décrit pas un métier au sens de « ce que l’on sait faire », mais une **position dans l’emploi**, combinant le type d’activité, le niveau de qualification et le statut (salarié ou indépendant, encadrant ou non)."),
  texte("Deux versions coexistent. La **PCS 2020** sert au recensement et aux enquêtes auprès des ménages. La **PCS-ESE** (Emplois salariés d’entreprise) est celle que les employeurs utilisent pour déclarer leurs salariés, notamment dans la déclaration sociale nominative. C’est la version **PCS-ESE 2017** que le projet utilise, car elle est présente dans la base de données des salariés qui alimente l’estimation. Les trois premiers caractères des deux versions sont, pour l’essentiel, communs."),
  h2("Une nomenclature emboîtée : du groupe à la profession détaillée"),
  texte("La PCS fonctionne comme un zoom. Le premier chiffre du code donne le **groupe socioprofessionnel** (la grande catégorie), les deux premiers chiffres la **catégorie**, et le code complet à quatre caractères la **profession détaillée**, que cette note appelle « PCS fine »."),
  image("s2_zoom_pcs", 13.5), legende("Schéma 2 — Plus on descend, plus la case est précise… et plus elle contient de petits effectifs."),
  texte("Le code fin apporte une information précieuse : il distingue, à l’intérieur des « professions intermédiaires », les techniciens des agents de maîtrise, puis les techniciens d’un domaine de ceux d’un autre. Mais chaque pas vers le détail divise les effectifs. Dans un département de taille moyenne, une profession détaillée peut ne compter que quelques salariés dans la Base. Le résultat devient alors **fragile** (une embauche ou un départ le fait basculer) et, en dessous des seuils de confidentialité, il n’est **pas diffusable**."),
  ...retenir("Plus on descend dans la nomenclature, plus on gagne en précision, mais plus les effectifs deviennent petits et plus les résultats deviennent fragiles ou soumis au secret statistique."),
  h2("Pourquoi le projet raisonne par grande catégorie"),
  texte("Pour estimer les départs d’ici 2030, le projet travaille au niveau du **premier chiffre** de la PCS, c’est-à-dire des grandes catégories de salariés : **cadres** (groupe 3), **professions intermédiaires** (groupe 4), **employés** (groupe 5) et **ouvriers** (groupe 6). Les agriculteurs exploitants, les artisans, commerçants et chefs d’entreprise (groupes 1 et 2) et les professions non renseignées sont hors du champ ; ils sont comptés à part, jamais mélangés aux autres."),
  texte("Ce choix tient à quatre exigences : la **robustesse statistique** (des effectifs suffisants pour que l’estimation ait un sens), la **lisibilité** (quatre catégories que tout le monde comprend), la **stabilité** (des résultats qui ne basculent pas d’une édition à l’autre pour quelques personnes) et le **respect du secret statistique** (moins de cases trop petites à masquer)."),
  image("s3_curseur", 14.5), legende("Schéma 3 — Le curseur précision / robustesse : la grande catégorie pour estimer, la PCS fine pour détailler."),
  texte("La PCS fine n’est pas abandonnée pour autant. Le projet produit aussi des tables et des cartes par profession détaillée, au niveau national, régional et départemental, avec le secret statistique appliqué case par case. Elles servent à **regarder de plus près** un territoire ou une catégorie, à **contrôler** une lecture agrégée et à repérer les métiers les plus concernés. Elles ne servent pas à remplacer la lecture par grande catégorie."),
  ...retenir(["Niveau fin = plus précis mais plus fragile. Niveau agrégé = moins précis mais plus robuste.", "Le projet estime les départs par grande catégorie ; la PCS fine sert au détail et au contrôle, pas à la décision seule."].map(t => texte(t, { after: 40 })))];

// =============================================================================================
// 2. Les FAP
// =============================================================================================
const partie2 = [
  h1("2. Les FAP : une lecture plus proche des métiers et du marché du travail"),
  texte("Les **familles professionnelles (FAP)** sont une nomenclature établie par la Dares, le service statistique du ministère du Travail. Elles regroupent les professions qui font appel à des compétences et à des gestes professionnels proches. Leur raison d’être est pratique : permettre de parler du **marché du travail** avec une logique de métier, en rapprochant dans une même grille les personnes en emploi, les offres d’emploi, les demandeurs d’emploi et les difficultés de recrutement."),
  texte("Les FAP jouent le rôle d’un **pont** entre deux mondes qui ne se parlent pas directement : la PCS de l’Insee, qui décrit les personnes en emploi, et le **ROME** de France Travail, le répertoire des métiers qui sert à décrire les offres et les demandes d’emploi. Chaque fiche ROME nomme un métier, ses appellations, ses compétences et ses mobilités possibles ; son code se compose d’une lettre et de quatre chiffres. La Dares publie des tables de passage de la PCS vers la FAP et du ROME vers la FAP. La version actuelle est la **FAP 2021**, construite pour suivre la refonte de la PCS 2020."),
  image("s4_pont", 12.5), legende("Schéma 4 — La FAP relie la PCS (personnes en emploi) et le ROME (offres et demandes d’emploi)."),
  citation("La FAP sert à parler du marché du travail avec une logique de métier, là où la PCS sert d’abord à décrire la structure de l’emploi."),
  texte("C’est dans la nomenclature FAP que sont mesurées les **tensions de recrutement** : l’indicateur de tension de la Dares et de France Travail, l’enquête sur les besoins en main-d’œuvre des employeurs et les diagnostics territoriaux de France Travail sont tous exprimés par famille professionnelle. Quand une direction opérationnelle ou France Travail signale un métier « en tension » sur un territoire, l’objet désigné est donc une FAP (ou un métier ROME), jamais une grande catégorie de la PCS."),
  ...attention([texte("**PCS ≠ FAP.** La PCS regarde la place occupée dans la structure professionnelle. La FAP regarde le type de métier exercé et sa proximité avec d’autres métiers sur le marché du travail. Les deux nomenclatures découpent le même monde selon deux logiques différentes.", { after: 60 }),
    texte("On ne peut donc pas écrire **« FAP technique = professions intermédiaires »** ni **« FAP X = PCS Y »**. Une famille professionnelle technique peut contenir des ouvriers qualifiés, des techniciens et des ingénieurs ; une grande catégorie de la PCS se répartit entre des dizaines de familles. Seule une **table de passage officielle**, au niveau détaillé, permet de relier les deux, et encore avec des approximations.", { after: 60 }),
    texte("Dans le projet, cette règle est inscrite dans le code : toute tentative de faire correspondre une FAP à une grande catégorie arrête le traitement.", { after: 0 })], "Attention : PCS ≠ FAP"),
  h2("Les trois nomenclatures en un tableau"),
  grille([1900, 2580, 2579, 2579], ["", "PCS / grande catégorie", "PCS fine", "FAP"], [
    ["Question principale", "Quelle structure d’emploi ?", "Quel type précis de profession ?", "Quels métiers, quelles tensions ?"],
    ["Qui l’établit", "Insee (déclarations des employeurs)", "Insee (même nomenclature, code complet)", "Dares, en lien avec le ROME de France Travail"],
    ["Exemple d’usage", "Décrire cadres, professions intermédiaires, employés, ouvriers", "Analyser un métier précis, repérer les professions les plus concernées", "Mesurer des tensions de recrutement, des besoins en main-d’œuvre"],
    ["Niveau de détail", "Faible à moyen (4 catégories dans le projet)", "Élevé (code à 4 caractères)", "Métier ou famille de métiers"],
    ["Atout", "Robustesse, lisibilité, stabilité", "Précision", "Lecture directe du marché du travail"],
    ["Limite", "Peu précis : des métiers très différents dans une même catégorie", "Petits effectifs, résultats fragiles, secret statistique fréquent", "Pas directement équivalente à la PCS ; maille territoriale différente"],
    ["Usage dans notre travail", "Structure de l’emploi, départs estimés, typologie des départements", "Détail et contrôle ; tops des métiers concernés", "Tensions observées, couche séparée, jamais convertie en catégorie"]], { premiereGras: true, size: 17 }),
  vide(100)];

// =============================================================================================
// 3. Géographie
// =============================================================================================
const partie3 = [
  h1("3. Les cartes ne parlent pas toujours du même territoire"),
  texte("Dans le projet, les départs sont estimés et cartographiés au niveau du **département** (et, par agrégation, de la région et de la France). C’est la maille qui concilie des effectifs suffisants, le secret statistique et une lecture possible pour tous. Les **tensions**, en revanche, sont observées par la DGA et par France Travail à une maille plus fine : le **bassin d’emploi** ou le territoire local de France Travail. Un département en contient plusieurs. Les zones d’emploi de l’Insee, construites à partir des déplacements domicile-travail, sont une autre maille infra-départementale, qui ne coïncide pas forcément avec les bassins de France Travail."),
  image("s5_departement_bassins", 12.5), legende("Schéma 5 — Un département contient plusieurs bassins ; la maille des départs n’est pas celle des tensions."),
  ...attention([texte("**Un signal observé dans un bassin ne décrit pas automatiquement tout le département.** Trois autres bassins du même département peuvent ne présenter aucune difficulté.", { after: 60 }),
    texte("**Une estimation calculée au niveau départemental ne doit pas être attribuée à un bassin.** Elle agrège des situations locales que l’on ne sait pas distinguer.", { after: 0 })], "Deux sens interdits"),
  texte("Le projet traite les tensions comme une **couche séparée** : elles ne sont jamais intégrées au profil d’un département, et la seule information retenue à l’échelle départementale est la **présence d’un signal localisé** et le nombre de bassins concernés. Aucune variable du type « département en tension » n’existe, par construction."),
  ...encadre("Règle de lecture", "Toujours regarder la maille géographique avant de comparer deux cartes. Si l’une est au département et l’autre au bassin, on peut les juxtaposer et les commenter, mais pas les additionner, les diviser ni en déduire une cause.", NAVY, BLEU_CLAIR),
  h2("Un exemple pour fixer les idées"),
  ...exemple([texte("**Département Alpha** (fictif) compte quatre bassins d’emploi. Dans un seul d’entre eux, France Travail observe une tension sur une famille professionnelle technique. Par ailleurs, à l’échelle du département, le projet estime que les professions intermédiaires de la Base ont une part de départs d’ici 2030 élevée.", { after: 80 }),
    texte("**Ce que l’on peut dire :** « Un bassin du département présente une tension sur certains métiers techniques. À l’échelle départementale, les professions intermédiaires présentent par ailleurs un enjeu de renouvellement élevé. »", { after: 80 }),
    texte("**Ce que l’on ne peut pas dire :** « Le département est en tension sur les professions intermédiaires. » (changement de maille et changement de nomenclature dans la même phrase).", { after: 80 }),
    texte("**Et encore moins :** « Les départs à la retraite expliquent la tension. » (la tension est observée aujourd’hui, les départs sont estimés pour demain ; aucune causalité n’est mesurée).", { after: 0 })], "Exemple fictif — département Alpha")];

// =============================================================================================
// 4. Volume / intensité  +  5. Structure
// =============================================================================================
const partie4 = [
  h1("4. Un grand nombre de départs n’est pas forcément un taux élevé"),
  texte("Les cartes du projet proposent deux mesures des départs : un **volume** (combien de personnes) et une **part** ou un **taux** (quelle proportion de la population concernée). Les deux sont utiles, mais elles racontent des histoires différentes et classent les territoires dans un ordre différent."),
  image("s6_volume_taux", 12.5), legende("Schéma 6 — Chiffres fictifs. A porte le plus gros volume, B la plus forte intensité."),
  ...exemple([texte("**Territoire A** : 1 000 départs estimés pour 20 000 salariés, soit un taux de 5 %. **Territoire B** : 500 départs estimés pour 2 000 salariés, soit un taux de 25 %.", { after: 60 }),
    texte("A porte le plus gros volume : c’est là que le nombre de postes à renouveler est le plus grand. B connaît l’intensité la plus forte : un salarié sur quatre pourrait partir, ce qui pèse bien plus sur l’organisation locale.", { after: 0 })]),
  ...retenir(["**Volume = combien de personnes ?** Il dépend d’abord de la taille du territoire.", "**Intensité = quelle part de la population concernée ?** Elle dépend de la pyramide des âges, pas de la taille.",
    "Un taux se lit toujours avec son dénominateur. Le projet en utilise deux : la part des **salariés du champ** (les salariés de 45 ans et plus, par défaut) susceptibles de partir, et la part de l’**emploi actuel tous âges** à remplacer. Chaque carte et chaque table indique laquelle elle affiche ; les deux ne s’additionnent pas."].map(t => texte(t, { after: 40 }))),
  h1("5. Comment lire une carte de structure d’emploi"),
  texte("La structure d’emploi décrit **qui est en poste aujourd’hui** : la répartition des salariés de la Base entre cadres, professions intermédiaires, employés et ouvriers. Le projet la résume par un qualificatif : **dominante cadres**, **dominante professions intermédiaires**, **dominante employés**, **dominante ouvrière**, ou **structure mixte**."),
  texte("Le mot « dominante » est défini par un **seuil explicite** : une catégorie est dominante si elle atteint au moins 40 % des salariés (paramètre réglable du projet, 40 % par défaut). Dominante ne veut donc pas dire majorité absolue, et arriver en tête ne suffit pas."),
  image("s9_structure_mixte", 13.5), legende("Schéma 7 — Chiffres fictifs. Les ouvriers arrivent en tête sans atteindre le seuil : la structure est mixte."),
  ...exemple([texte("Ouvriers 31 %, professions intermédiaires 30 %, cadres 25 %, employés 14 %. Aucune catégorie n’atteint 40 % : cette structure est qualifiée de **mixte**, et non de « dominante ouvrière », bien que les ouvriers soient les plus nombreux.", { after: 0 })]),
  texte("La structure décrit l’emploi actuel ; elle ne dit rien, par elle-même, des départs. Une catégorie peut dominer l’emploi sans dominer les départs, et inversement : le projet mesure séparément la **concentration des départs** sur une catégorie (au moins la moitié des départs estimés portés par une seule grande catégorie)."),
  ];

// =============================================================================================
// 6. Départs estimés  +  7. Tension
// =============================================================================================
const partie5 = [
  h1("6. Comment lire une carte de départs estimés d’ici 2030"),
  texte("Les cartes de départs représentent des **estimations**, produites par le modèle du projet, et non des départs certains, programmés ou déjà observés. Un départ y désigne une **sortie définitive de l’emploi** d’ici 2030 : retraite ou fin de carrière, invalidité ou décès. Changer d’employeur n’est pas un départ. La population suivie est celle des salariés des entreprises de la Base âgés d’au moins 45 ans (seuil réglable du projet)."),
  texte("Chaque estimation est accompagnée d’une **fourchette** : une **hypothèse basse**, une **estimation centrale** et une **hypothèse haute**, qui traduisent l’incertitude sur l’âge effectif de départ en retraite. La fourchette fait partie du résultat ; un chiffre central lu seul perd une partie de l’information."),
  image("s8_fourchette", 12), legende("Schéma 8 — Chiffres fictifs. Lire le centre avec sa fourchette."),
  texte("Sur les cartes interactives, on choisit la mesure affichée : la part des salariés susceptibles de partir (affichée par défaut), le nombre de départs estimés ou le nombre de salariés ; l’hypothèse (basse, centrale, haute) ; et, le cas échéant, la cause (retraite ou fin de carrière, invalidité, décès). Les classes de couleur ont des bornes rondes, communes à toute la catégorie affichée, pour que deux territoires de la même teinte soient réellement comparables."),
  ...attention([texte("La carte **ne dit pas** combien de recrutements seront nécessaires. Un départ peut être compensé par une réorganisation, une mobilité interne, un gain de productivité ou une baisse d’activité ; à l’inverse, une entreprise peut recruter sans qu’aucun départ n’ait lieu.", { after: 60 }),
    texte("Elle donne un **signal sur le renouvellement potentiel des effectifs** : un ordre de grandeur, avec sa fourchette, à comparer entre territoires et entre catégories.", { after: 0 })], "Ce que la carte ne dit pas"),
  texte("Deux conventions d’affichage méritent d’être connues. Une case **grise** signale un résultat non diffusé pour raison de secret statistique : trop peu de salariés (moins de 5), trop peu d’entreprises (moins de 3) ou une entreprise qui pèse trop dans la case ; la valeur n’est ni affichée ni cachée dans la page, elle n’existe pas dans les fichiers diffusés. Une case **blanche** signale l’absence de salarié observé. Les deux ne doivent jamais être lues comme un zéro."),
  h1("7. Une tension de recrutement ne signifie pas simplement « beaucoup de départs »"),
  texte("Une **tension** est une difficulté à pourvoir des postes, constatée aujourd’hui sur le marché du travail. L’indicateur de la Dares et de France Travail la mesure par famille professionnelle en combinant plusieurs informations : le rapport entre les offres d’emploi et les demandeurs, les difficultés de recrutement anticipées par les employeurs et le manque de main-d’œuvre disponible ; des indicateurs complémentaires éclairent ses causes possibles (intensité des embauches, lien entre formation et emploi, conditions de travail, durabilité des emplois, décalage géographique)."),
  image("s7_facteurs_tension", 12.5), legende("Schéma 9 — Une tension a plusieurs causes possibles ; les départs n’en sont qu’une."),
  texte("Une tension peut donc venir d’une **inadéquation entre compétences et besoins**, d’un **manque de candidats**, d’un défaut d’**attractivité** du métier ou du territoire, de la **localisation** des postes et de la **mobilité** des candidats, de la **concurrence entre employeurs**, de l’offre de **formation**, des **conditions de travail**, de la **spécialisation** du métier, et éventuellement de **départs à venir**. Ces causes se cumulent ou se compensent ; une carte de départs ne permet pas de les départager."),
  ...retenir(["**Tension actuelle ≠ vieillissement.** Une tension est un constat d’aujourd’hui, dans la nomenclature FAP et à la maille du bassin ; le vieillissement est une estimation pour demain, par grande catégorie et au département.",
    "**Départs élevés ≠ tension automatique.** Un territoire peut connaître beaucoup de départs et recruter sans difficulté, ou l’inverse."].map(t => texte(t, { after: 40 })))];

// =============================================================================================
// Page « Lire une carte en 30 secondes »
// =============================================================================================
const checklist = [
  saut(),
  par(run("PAGE À IMPRIMER", { size: 16, bold: true, color: GRIS, spacing: 30 }), { after: 60 }),
  par(run("Lire une carte en 30 secondes", { size: 36, bold: true, color: NAVY }), { after: 60 }),
  par(run("Cinq questions avant de lire, deux de plus avant de comparer.", { size: 23, color: TEXTE }), { after: 200 }),
  h2("Avant de lire une carte, poser cinq questions"),
  grille([520, 3000, 6118], ["", "La question", "Ce qu’elle change"], [
    ["1", "**Quelle est la maille géographique ?**", "Département, région, France, ou bassin d’emploi. Une couleur sur un département ne se « redescend » pas à un bassin, et un signal de bassin ne « remonte » pas au département."],
    ["2", "**Quelle population est observée ?**", "Les salariés des entreprises de la Base, à partir d’un âge minimal (45 ans par défaut), ou l’emploi actuel tous âges ? Le dénominateur d’un taux en dépend."],
    ["3", "**Quelle nomenclature est utilisée ?**", "Grande catégorie de la PCS, PCS fine à quatre caractères, ou famille professionnelle (FAP) ? On ne passe pas de l’une à l’autre sans table de passage."],
    ["4", "**Est-ce un volume ou un taux ?**", "Un nombre de personnes classe d’abord les gros territoires ; une part classe les territoires les plus exposés. Les deux cartes n’ont pas le même ordre."],
    ["5", "**Est-ce un constat actuel ou une estimation future ?**", "La structure de l’emploi et les tensions sont observées aujourd’hui ; les départs sont estimés pour 2030, avec une fourchette."]], { size: 18 }),
  vide(160),
  h2("Si deux cartes sont comparées, ajouter deux questions"),
  grille([520, 3000, 6118], ["", "La question", "Ce qu’elle change"], [
    ["6", "**Parlent-elles du même territoire ?**", "Même maille, même découpage, même année de référence. Sinon : juxtaposer et commenter, jamais additionner ni déduire."],
    ["7", "**Parlent-elles du même objet métier ?**", "Une FAP technique et les professions intermédiaires ne sont pas la même chose. Une comparaison PCS / FAP se fait au niveau détaillé, avec la table de passage officielle, ou ne se fait pas."]], { size: 18 }),
  vide(200),
  ...retenir("Toute carte se lit avec sa nomenclature, son dénominateur et sa maille. Si l’un des trois manque dans la légende, demandez-le avant de conclure."),
  saut()];

// =============================================================================================
// Erreurs à ne pas commettre + glossaire
// =============================================================================================
const erreurs = [
  h1("8. Les erreurs à ne pas commettre"),
  texte("Les formulations de gauche sont celles que l’on entend le plus souvent. Celles de droite disent la même chose sans trahir les données."),
  grille([2900, 3200, 3538], ["L’erreur", "Pourquoi c’est faux", "La formulation correcte"], [
    ["« Un bassin est en tension, donc le département est en tension. »", "Changement de maille : le bassin est une partie du département, pas un échantillon représentatif.", "« Un bassin du département est signalé en tension sur telle famille professionnelle. »"],
    ["« Beaucoup de départs, donc un fort taux. »", "Confusion entre volume et intensité : le volume suit la taille du territoire.", "« Ce territoire porte un volume élevé ; rapporté à ses effectifs, le taux est de x %. »"],
    ["« FAP technique = professions intermédiaires. »", "Deux nomenclatures, deux logiques : une famille technique mêle ouvriers, techniciens et ingénieurs.", "« Cette famille professionnelle recouvre plusieurs catégories ; voir la table de passage. »"],
    ["« Des départs élevés annoncent une pénurie. »", "Pas de causalité automatique : les départs sont un facteur parmi d’autres, et peuvent être compensés.", "« Ce territoire présente un signal de renouvellement élevé, à rapprocher des tensions observées. »"],
    ["« La PCS fine est toujours préférable. »", "Petits effectifs, résultats fragiles, secret statistique fréquent.", "« Adapter la finesse à la robustesse : la grande catégorie pour estimer, la PCS fine pour détailler. »"],
    ["« 900 salariés partiront d’ici 2030. »", "Une estimation n’est pas une certitude ; le chiffre central va avec sa fourchette.", "« Environ 900 départs sont estimés d’ici 2030, entre 850 et 950 selon l’hypothèse. » (chiffres fictifs)"],
    ["« Case grise = zéro départ. »", "Le gris signale un résultat non diffusé (secret statistique), pas une absence.", "« Le résultat n’est pas diffusable à cette maille ; voir le niveau supérieur. »"]], { size: 17 }),
  vide(160),
  h1("9. Glossaire"),
  grille([2300, 7338], ["Terme", "Définition"], [
    ["PCS", "Nomenclature des professions et catégories socioprofessionnelles de l’Insee. Elle classe les personnes selon leur profession et leur position dans l’emploi. Le projet utilise la version employeurs PCS-ESE 2017."],
    ["PCS niveau 1 / grande CS", "Premier chiffre du code PCS : le groupe socioprofessionnel. Dans le projet : cadres (3), professions intermédiaires (4), employés (5), ouvriers (6)."],
    ["PCS fine", "Code PCS complet à quatre caractères, désignant une profession détaillée. Précise, mais aux effectifs souvent petits."],
    ["FAP", "Familles professionnelles, nomenclature de la Dares qui regroupe les métiers proches pour analyser le marché du travail. Pont entre la PCS et le ROME. Version actuelle : FAP 2021."],
    ["ROME", "Répertoire opérationnel des métiers et des emplois de France Travail. Fiches métiers servant à décrire et rapprocher les offres et les demandes d’emploi."],
    ["Stock d’emploi", "Nombre de salariés en poste à une date donnée. Dans le projet, deux stocks : les salariés du champ (45 ans et plus par défaut) et l’emploi actuel tous âges, qui sert de dénominateur à la part à remplacer."],
    ["Volume", "Nombre de départs estimés. Il dépend d’abord de la taille du territoire ou de la catégorie."],
    ["Taux", "Départs estimés rapportés à un stock. Un taux doit toujours annoncer son dénominateur."],
    ["Renouvellement", "Remplacement des salariés qui quittent définitivement l’emploi. Dans le projet, l’intensité du renouvellement est la part de l’emploi actuel que les départs estimés représentent."],
    ["Tension", "Difficulté constatée à pourvoir des postes, mesurée par famille professionnelle (FAP) à partir des offres, des demandes, des difficultés anticipées et de la main-d’œuvre disponible."],
    ["Bassin d’emploi", "Territoire local de France Travail, plus petit que le département, où sont observées les tensions. Ne pas confondre avec la zone d’emploi de l’Insee, construite sur les déplacements domicile-travail."],
    ["Département", "Maille à laquelle le projet estime et cartographie les départs. Un département contient plusieurs bassins."],
    ["Secret statistique", "Règle de confidentialité : un résultat n’est pas diffusé s’il concerne moins de 5 salariés, moins de 3 entreprises, ou si une entreprise y pèse plus de 85 %. La valeur masquée n’est jamais un zéro et ne figure dans aucun fichier diffusé."]], { premiereGras: true, size: 17 })];

// =============================================================================================
// Synthèse + sources
// =============================================================================================
const synthese = [
  h1("Si vous ne retenez que six choses"),
  ...numeros(["**PCS et FAP ne répondent pas à la même question.** La PCS décrit la position dans l’emploi ; la FAP décrit les métiers du marché du travail.",
    "**Plus on descend dans le détail, plus on gagne en précision et plus on perd en robustesse.** La grande catégorie pour estimer, la PCS fine pour détailler.",
    "**Volume et taux doivent toujours être regardés ensemble.** Le premier dit combien, le second dit quelle part.",
    "**Département et bassin d’emploi ne sont pas interchangeables.** Ni dans un sens, ni dans l’autre.",
    "**Une tension observée n’est pas expliquée automatiquement par les départs.** Elle a plusieurs causes possibles, et se mesure aujourd’hui, dans une autre nomenclature.",
    "**Toute carte se lit avec sa nomenclature, son dénominateur et sa maille.** Et toute estimation se lit avec sa fourchette."], "num2"),
  vide(80),
  h1("Sources et références"),
  texte("Définitions officielles des nomenclatures et des indicateurs cités. Les exemples chiffrés de cette note sont fictifs ; les paramètres du projet (seuils de secret, seuil de dominance, âge minimal, nombre de métiers affichés) sont ceux de sa configuration et peuvent être ajustés. Les définitions propres au projet reprennent ses spécifications « Périmètre et champ », « Projection 2030 », « Secret statistique », « Restitutions territoriales » et « Typologie territoriale ».", { size: 18, color: TEXTE }),
  ...[["Insee — Nomenclatures des professions et catégories socioprofessionnelles (PCS 2020 et PCS-ESE)", "https://www.insee.fr/fr/information/2406153"],
      ["Insee — PCS-ESE 2017 : professions et catégories socioprofessionnelles des emplois salariés des employeurs privés et publics", "https://www.insee.fr/fr/information/2912545"],
      ["Insee — Définition : nomenclature des professions et catégories socioprofessionnelles", "https://www.insee.fr/fr/metadonnees/definition/c1493"],
      ["Dares — La nomenclature des familles professionnelles 2021 (FAP 2021), tables de passage PCS 2020 et ROME", "https://dares.travail-emploi.gouv.fr/donnees/la-nomenclature-des-familles-professionnelles-2021"],
      ["France Travail — Le ROME, répertoire opérationnel des métiers et des emplois (open data)", "https://www.francetravail.org/opendata/repertoire-operationnel-des-meti.html?type=article"],
      ["Dares — Les tensions sur le marché du travail (publications et méthode de l’indicateur)", "https://dares.travail-emploi.gouv.fr/publications/les-tensions-sur-le-marche-du-travail-focus"],
      ["Dares — Les tensions sur le marché du travail par métier (données)", "https://dares.travail-emploi.gouv.fr/donnees/les-tensions-sur-le-marche-du-travail-par-metier"],
      ["Dares — Note méthodologique sur l’analyse des tensions sur le marché du travail", "https://dares.travail-emploi.gouv.fr/sites/default/files/pdf/dares_note_methodologique_sur_l_analyse_des_tensions_sur_le_marche_du_travail_v2.pdf"],
      ["Insee — Définition : zone d’emploi", "https://www.insee.fr/fr/metadonnees/definition/c1361"],
      ["Insee — Base des zones d’emploi 2020 et méthode de constitution", "https://www.insee.fr/fr/information/4652957"]]
    .map(([t, u]) => new Paragraph({ numbering: { reference: "puces", level: 0 }, spacing: { before: 0, after: 40, line: 240 }, children: [run(t + " — ", { size: 18 }), lien(u.replace(/^https?:\/\//, ""), u)] })),
  ];

// =============================================================================================
const doc = new Document({
  creator: "Projet départs Base", title: "PCS, grandes catégories, FAP : comment lire les cartes d’emploi de la Base ?",
  styles: { default: { document: { run: { font: FONT, size: 21, color: ENCRE } } },
    paragraphStyles: [{ id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true, run: { font: FONT, size: 30, bold: true, color: NAVY }, paragraph: { outlineLevel: 0 } },
                      { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true, run: { font: FONT, size: 24, bold: true, color: NAVY }, paragraph: { outlineLevel: 1 } }] },
  numbering: { config: [
    { reference: "puces", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 360, hanging: 240 } }, run: { color: NAVY } } }] },
    { reference: "num1", levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 420, hanging: 300 } }, run: { bold: true, color: NAVY } } }] },
    { reference: "num2", levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 420, hanging: 300 } }, run: { bold: true, color: NAVY } } }] }] },
  sections: [{ properties: { page: PAGE }, headers: { default: entete }, footers: { default: pied },
    children: [...page1, ...partie1, ...partie2, ...partie3, ...partie4, ...partie5, ...checklist, ...erreurs, ...synthese] }] });
Packer.toBuffer(doc).then(b => { fs.mkdirSync(path.dirname(SORTIE), { recursive: true }); fs.writeFileSync(SORTIE, b); console.log("écrit :", SORTIE); });
