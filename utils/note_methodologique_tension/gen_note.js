// Note méthodologique « Exposition au recrutement difficile » (docx-js, A4 portrait).
// Usage : node gen_note.js <dossier_images> <fichier_sortie.docx>
// Tous les chiffres sont FICTIFS (exemple suivi de bout en bout). Aucune donnée du projet.
const fs = require("fs"), path = require("path");
const { Document, Packer, Paragraph, TextRun, ImageRun, Table, TableRow, TableCell, WidthType, AlignmentType, BorderStyle, ShadingType,
        TableLayoutType, VerticalAlign, Footer, Header, PageNumber, PageBreak, LevelFormat, HeadingLevel, TabStopType } = require("docx");
const DIR_IMG = process.argv[2] || path.join(__dirname, "..", "..", "docs", "assets", "methodologie_tension");
const SORTIE = process.argv[3] || path.join(__dirname, "..", "..", "docs", "note_methodologique_exposition_tension.docx");

const NAVY = "1E3A5F", ENCRE = "1F2933", TEXTE = "3E4C59", GRIS = "7A8795", FILET = "D9DEE5", FOND = "F5F7FA", BLEU_CLAIR = "E3ECF6", BLANC = "FFFFFF";
const ORANGE = "C2620A", ORANGE_CLAIR = "FDF1E1", VERT = "2E7D5B", VERT_CLAIR = "E6F2EC";
const FONT = "Calibri";
const NONE = { style: BorderStyle.NONE, size: 0, color: BLANC }, SANS = { top: NONE, bottom: NONE, left: NONE, right: NONE };
const LARGEUR = 9638;
const PAGE = { size: { width: 11906, height: 16838 }, margin: { top: 1134, bottom: 1000, left: 1134, right: 1134, header: 500, footer: 500 } };

const PERIMETRE = process.env.PERIMETRE || "Base";   // nom du périmètre dans le texte (en interne : PERIMETRE=BITD node …) ; « Base Tous salariés » jamais touché
const run = (t, o = {}) => new TextRun({ text: t.replace(/\bBase\b(?! Tous)/g, PERIMETRE), font: FONT, size: o.size || 20, color: o.color || ENCRE, bold: o.bold, italics: o.italics, characterSpacing: o.spacing });
const rich = (t, o = {}) => t.split(/(\*\*[^*]+\*\*|`[^`]+`)/).filter(Boolean).map(s => s.startsWith("**") ? run(s.slice(2, -2), { ...o, bold: true }) : s.startsWith("`") ? new TextRun({ text: s.slice(1, -1), font: "Consolas", size: (o.size || 20) - 2, color: NAVY }) : run(s, o));
const par = (runs, o = {}) => new Paragraph({ spacing: { before: o.before || 0, after: o.after === undefined ? 100 : o.after, line: o.line || 258 }, alignment: o.align, keepNext: o.keepNext, keepLines: o.keepLines, children: Array.isArray(runs) ? runs : [runs] });
const texte = (t, o = {}) => par(rich(t, o), o);
const h1 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 240, after: 110, line: 276 }, keepNext: true, border: { bottom: { style: BorderStyle.SINGLE, size: 8, color: NAVY, space: 4 } }, children: [run(t, { size: 29, bold: true, color: NAVY })] });
const h2 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 180, after: 70, line: 276 }, keepNext: true, children: [run(t, { size: 23, bold: true, color: NAVY })] });
const puces = (items, o = {}) => items.map(t => new Paragraph({ numbering: { reference: "puces", level: 0 }, spacing: { before: 0, after: 50, line: 256 }, children: rich(t, o) }));
const numeros = (items, ref = "num1") => items.map(t => new Paragraph({ numbering: { reference: ref, level: 0 }, spacing: { before: 0, after: 60, line: 256 }, children: rich(t) }));
const vide = (h = 80) => new Paragraph({ spacing: { before: 0, after: 0, line: h, lineRule: "exact" }, children: [run("", { size: 4 })] });
const tailles = {}; for (const f of fs.readdirSync(DIR_IMG).filter(f => f.endsWith(".svg"))) { const m = fs.readFileSync(path.join(DIR_IMG, f), "utf8").match(/viewBox="0 0 (\d+) (\d+)"/); tailles[f.replace(".svg", "")] = [+m[1], +m[2]]; }
const image = (nom, wcm, o = {}) => { const [w, h] = tailles[nom]; const wpx = Math.round(wcm * 37.8), hpx = Math.round(wpx * h / w);
  return new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: o.before === undefined ? 60 : o.before, after: 30 }, keepNext: true,
    children: [new ImageRun({ type: "png", data: fs.readFileSync(path.join(DIR_IMG, nom + ".png")), transformation: { width: wpx, height: hpx } })] }); };
const legende = (t) => par(run(t, { size: 16, color: GRIS, italics: true }), { align: AlignmentType.CENTER, after: 110 });
const cell = (children, w, o = {}) => new TableCell({ width: { size: w, type: WidthType.DXA }, borders: o.borders || SANS, verticalAlign: o.valign || VerticalAlign.TOP,
  shading: o.fill ? { type: ShadingType.CLEAR, fill: o.fill, color: "auto" } : undefined,
  margins: { top: o.pad === undefined ? 100 : o.pad, bottom: o.pad === undefined ? 100 : o.pad, left: o.padx === undefined ? 140 : o.padx, right: o.padx === undefined ? 140 : o.padx }, children });
const table = (widths, rows, o = {}) => new Table({ layout: TableLayoutType.FIXED, width: { size: widths.reduce((a, b) => a + b, 0), type: WidthType.DXA }, columnWidths: widths,
  rows: rows.map((r, i) => new TableRow({ cantSplit: true, tableHeader: o.entete && i === 0, children: r })) });
const encadre = (etiquette, contenu, couleur = NAVY, fond = BLEU_CLAIR) => [table([LARGEUR], [[cell([
  par(run(etiquette.toUpperCase(), { size: 16, bold: true, color: couleur, spacing: 20 }), { after: 50 }),
  ...(Array.isArray(contenu) ? contenu : [texte(contenu, { size: 20, after: 0 })])], LARGEUR,
  { fill: fond, borders: { ...SANS, left: { style: BorderStyle.SINGLE, size: 24, color: couleur } }, pad: 110, padx: 200 })]]), vide(100)];
const retenir = (c) => encadre("À retenir", c, NAVY, BLEU_CLAIR);
const attention = (c, titre = "Précaution de lecture") => encadre(titre, c, ORANGE, ORANGE_CLAIR);
const exemple = (c, titre = "Exemple fictif — le département Exemple et ses cinq métiers stratégiques") => encadre(titre, c, VERT, VERT_CLAIR);
const grille = (widths, entete, lignes, o = {}) => {
  const bord = { style: BorderStyle.SINGLE, size: 4, color: FILET }, B = { top: bord, bottom: bord, left: NONE, right: NONE };
  const tete = entete.map((t, i) => cell([par(run(t, { size: 17, bold: true, color: BLANC }), { after: 0, line: 240 })], widths[i], { fill: NAVY, borders: B, pad: 80 }));
  const corps = lignes.map((l, k) => l.map((t, i) => cell([par(rich(t, { size: o.size || 18, color: i === 0 && o.premiereGras ? NAVY : ENCRE, bold: i === 0 && o.premiereGras }), { after: 0, line: 250 })], widths[i], { fill: k % 2 ? FOND : BLANC, borders: B, pad: 80 })));
  return table(widths, [tete, ...corps], { entete: true });
};
const entete = new Header({ children: [new Paragraph({ tabStops: [{ type: TabStopType.RIGHT, position: LARGEUR }], border: { bottom: { style: BorderStyle.SINGLE, size: 4, color: FILET, space: 4 } },
  children: [run("Note méthodologique · Exposition au recrutement difficile", { size: 15, color: GRIS }), run("\ttous les chiffres de cette note sont fictifs", { size: 15, color: GRIS, italics: true })] })] });
const pied = new Footer({ children: [new Paragraph({ tabStops: [{ type: TabStopType.RIGHT, position: LARGEUR }],
  children: [run("Projet départs Base · octobre 2026", { size: 15, color: GRIS }), run("\tpage ", { size: 15, color: GRIS }), new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 15, color: GRIS }), run(" / ", { size: 15, color: GRIS }), new TextRun({ children: [PageNumber.TOTAL_PAGES], font: FONT, size: 15, color: GRIS })] })] });

// =============================================================================================
// 0. Titre et objet
// =============================================================================================
const p0 = [
  par(run("NOTE MÉTHODOLOGIQUE", { size: 16, bold: true, color: GRIS, spacing: 30 }), { after: 120 }),
  par(run("Exposition des départements au recrutement difficile", { size: 36, bold: true, color: NAVY }), { after: 80, line: 300 }),
  par(run("Comment les départs attendus d’ici 2030 sont croisés avec la tension des métiers, et comment lire les résultats.", { size: 23, color: TEXTE }), { after: 180, line: 280 }),
  texte("La typologie des territoires dit, pour chaque département, **combien** de salariés de la Base vont partir d’ici 2030 et **à quel rythme**. Elle ne dit pas si ces départs seront faciles ou difficiles à remplacer. Cette note décrit le calcul qui apporte cette information : l’**exposition au recrutement difficile**, construite à partir des indicateurs de tension de la Dares, rattachés aux métiers stratégiques de la Base."),
  texte("Elle s’adresse à qui doit comprendre, expliquer ou défendre ces résultats : ce qui est calculé, à partir de quoi, avec quelles règles, et surtout ce que les résultats ne disent pas. Chaque notion est d’abord expliquée en français courant, puis définie précisément. Un exemple fictif est suivi d’un bout à l’autre."),
  ...retenir([texte("Le résultat central tient en une phrase par département : « sur 100 départs attendus dans les métiers stratégiques, N concernent des métiers en tension en France ». La tension est celle du **métier**, mesurée en France entière ; elle ne mesure jamais la situation du département. On ne dit donc jamais qu’un département est « en tension » : on dit que ses départs portent, plus ou moins, sur des métiers difficiles à recruter.", { after: 0 })]),
  h1("1. Ce que l’analyse apporte, et ce qu’elle n’apporte pas"),
  texte("**Ce qu’elle apporte.** Deux départements peuvent attendre le même nombre de départs et se trouver dans des situations très différentes : si l’un perd surtout des comptables et l’autre surtout des techniciens de maintenance industrielle, le second aura plus de mal à remplacer. L’exposition rend cette différence visible, avec un repère national, une classe (faible, modérée, forte) et une indication de la nature de la difficulté, le **levier**."),
  texte("**Ce qu’elle n’apporte pas.** Ce n’est pas une prédiction de pénurie : elle ne dit pas si les recrutements échoueront. Ce n’est pas une mesure locale du marché du travail : la tension vient d’indicateurs nationaux par métier, pas d’une observation du département. Ce n’est pas un classement des départements à aider : c’est une lecture, à confronter à la connaissance du terrain."),
  ...attention([texte("Dans toutes les restitutions, la formule est : « ses départs portent sur des métiers en tension en France ». Les formules « département en tension », « département en pénurie » ou « bassin en difficulté » ne sont pas employées, et le code refuse toute colonne qui attribuerait une tension à un département.", { after: 0 })])];

// =============================================================================================
// 2. Sources
// =============================================================================================
const p2 = [
  h1("2. Les sources"),
  texte("Le calcul assemble deux sources déjà disponibles. Aucun départ n’est recalculé, aucun paramètre du modèle de départs n’est modifié."),
  h2("Les départs attendus par métier (projet)"),
  texte("Le modèle du projet estime, pour chaque salarié de la Base âgé de 45 ans ou plus, sa probabilité de départ définitif d’ici 2030 (retraite, invalidité, décès). Ces probabilités sont sommées par département et par **PCS** (la nomenclature des professions de l’Insee, au niveau détaillé), avec une estimation centrale et une fourchette basse et haute selon l’hypothèse d’âge de départ en retraite. C’est la table « départs par PCS » du projet (script 08c)."),
  h2("La tension des métiers (Dares)"),
  texte("La Dares publie chaque année des indicateurs de tension par **famille professionnelle (FAP)**, c’est-à-dire par regroupement de métiers proches. Pour chaque famille, un **indicateur synthétique** résume la difficulté de recrutement observée en France, et **sept composantes** en décrivent les causes possibles. Ces indicateurs sont diffusés sous forme de **rangs** : chaque famille est classée de 1 (tension la plus faible) à 5 (la plus forte) par rapport à toutes les autres. Depuis l’édition 2023, le niveau 5 est affiné en deux sous-classes, 5.1 et 5.2, la seconde étant la plus forte."),
  grille([3000, 6638], ["Composante (colonne du classeur)", "Ce qu’elle mesure"], [
    ["Intensité d’embauche (`ind_int_emb_d`)", "Le volume d’embauches rapporté à l’emploi du métier : beaucoup de recrutements à réaliser."],
    ["Manque de main-d’œuvre disponible (`ind_main_oeuvre_d`)", "Peu de demandeurs d’emploi dans le métier par rapport aux offres : les candidats manquent."],
    ["Non-durabilité de l’emploi (`ind_dur_empl_d`)", "Une part élevée de contrats courts : le métier retient peu."],
    ["Conditions de travail contraignantes (`ind_cond_travail_d`)", "Horaires, pénibilité, contraintes physiques ou organisationnelles qui freinent l’attractivité."],
    ["Inadéquation géographique (`ind_mismatch_geo_d`)", "Les offres et les candidats ne sont pas aux mêmes endroits."],
    ["Faible attractivité salariale (`ind_inattract_salariale_d`)", "Des salaires peu attractifs par rapport à des métiers comparables."],
    ["Formation spécifique au métier (`ind_specif_form_empl_d`)", "Le métier exige une formation particulière, souvent longue : on ne s’y reconvertit pas vite."]], { size: 17 }),
  vide(60),
  texte("Ces libellés sont des reformulations courantes des composantes publiées par la Dares ; la définition exacte de chaque indicateur et son millésime figurent dans la documentation de la source. Vérifier le millésime utilisé fait partie des contrôles de chaque édition.", { size: 18, color: TEXTE }),
  h2("Le classeur qui relie les deux"),
  texte("La liste des **métiers stratégiques** de la Base (environ quatre-vingts PCS) a été rattachée, PCS par PCS, à une famille professionnelle, et les rangs de tension de cette famille ont été reportés sur la ligne. Le classeur comporte aussi un **indice de confiance** du rattachement (certaines PCS correspondent nettement à une famille, d’autres se répartissent entre plusieurs) et un **poids** de la PCS dans sa famille, conservé à titre d’information."),
  texte("À la lecture, le programme vérifie que la feuille et les colonnes attendues existent, qu’aucune PCS n’apparaît deux fois, que chaque rang est bien un entier de 1 à 5 ou l’un des sous-niveaux admis (5.1, 5.2) ; une valeur hors de l’échelle arrête le calcul avec le détail des lignes fautives. Les écarts dus à la virgule flottante (par exemple 5,0999 pour 5,1) sont ramenés à une décimale et comptés en console. Une PCS de la liste absente des départs de la Base est signalée, jamais ignorée en silence.")];

// =============================================================================================
// 3. Du métier à la tension
// =============================================================================================
const p3 = [
  h1("3. Du métier à la tension : le pont par la PCS"),
  texte("Les départs sont connus par PCS ; la tension est connue par FAP. Les deux nomenclatures ne s’emboîtent pas : il faut une table de correspondance. C’est le rôle du classeur. Le pont passe donc **par le métier détaillé**, jamais par les grandes catégories (cadres, professions intermédiaires, employés, ouvriers) : une grande catégorie mélange des métiers en tension et des métiers qui ne le sont pas, et lui attribuer une tension n’aurait pas de sens. Le code refuse toute correspondance FAP → grande catégorie."),
  image("s1_pcs_fap_rangs", 15.5), legende("Schéma 1 — De la PCS stratégique à la tension de sa famille professionnelle (exemple fictif)."),
  h2("La règle « en tension »"),
  texte("Un métier est dit **en tension** lorsque le rang de l’indicateur synthétique de sa famille atteint le seuil fixé dans la configuration : **4 sur 5** par défaut (`TENSION_SEUIL_RANG`), ce qui retient les familles en tension forte et très forte. Les sous-niveaux 5.1 et 5.2 comptent comme le niveau 5 pour cette règle ; leur valeur fine n’est utilisée que pour l’affichage et pour départager des ex aequo."),
  texte("Le seuil est un choix : à 5, seuls les métiers en tension très forte comptent ; à 3, la moitié haute des familles compte. C’est pourquoi le test de robustesse (section 6) recalcule le résultat avec le seuil déplacé d’un cran dans chaque sens."),
  ...retenir("Trois états possibles pour un métier de la liste : en tension (rang ≥ seuil), hors tension (rang < seuil), tension inconnue (rang manquant). Un métier de tension inconnue n’est jamais compté comme « hors tension » : il rejoint les métiers non couverts.")];

// =============================================================================================
// 4. L'indice d'exposition
// =============================================================================================
const p4 = [
  h1("4. L’indice d’exposition"),
  texte("Pour un département, on regarde les départs attendus d’ici 2030 et on les répartit en trois parts :"),
  ...puces(["**A** : les départs des métiers stratégiques **en tension** ;", "**B** : les départs des métiers stratégiques **hors tension** ;", "**C** : tous les autres départs (métiers hors de la liste, et métiers de la liste dont la tension est inconnue)."]),
  texte("L’**exposition** est la part de A dans A + B : sur 100 départs attendus dans les métiers stratégiques, combien concernent des métiers en tension. La **couverture** est la part de A + B dans le total : quel poids les métiers stratégiques ont dans les départs du département. Le même calcul, fait sur l’ensemble des départements, donne le **repère France**."),
  image("s2_parts_abc", 15.5), legende("Schéma 2 — Les trois parts, l’exposition et la couverture (exemple fictif)."),
  ...exemple([texte("Le département Exemple attend 200 départs d’ici 2030. Ses cinq métiers stratégiques en portent 130 : technicienne de maintenance 30 (rang 5,2), soudeur 20 (rang 4), chaudronnier 10 (rang 5,1, rattachement de faible confiance), comptable 40 (rang 2), agent logistique 30 (rang 3). Part A = 30 + 20 + 10 = 60 ; part B = 40 + 30 = 70 ; part C = 70.", { after: 60 }),
    texte("**Exposition = 60 / 130 = 46 %.** Sur 100 départs des métiers stratégiques, 46 concernent des métiers en tension en France. **Couverture = 130 / 200 = 65 %** : les métiers stratégiques portent près des deux tiers des départs du département. Repère France (fictif) : 38 %.", { after: 0 })]),
  h2("Pourquoi cette forme d’indice"),
  texte("C’est un **indice d’exposition par composition**, de la même famille que les indices utilisés pour mesurer l’exposition d’un territoire à un choc (automatisation, commerce extérieur) : le territoire apporte ses poids (ses départs par métier), la caractéristique vient du métier (sa tension nationale). L’indice ne crée aucune information locale sur la tension ; il dit seulement sur quels métiers les départs du territoire se concentrent. C’est ce qui le rend défendable : chaque élément du calcul est vérifiable et provient d’une source identifiée."),
  h2("La couverture : savoir ce que l’on ne voit pas"),
  texte("La liste des métiers stratégiques ne couvre pas tous les départs. Si, dans un département, elle n’en couvre que 15 %, l’exposition porte sur une petite partie des départs et ne dit rien des 85 % restants. C’est pourquoi l’exposition **n’est pas interprétée** quand la couverture est inférieure à un minimum, **30 %** par défaut (`TENSION_COUVERTURE_MIN_PCT`) : le département reste dans les tables, avec sa couverture, mais sans classe. La couverture est toujours affichée à côté de l’exposition."),
  ...attention([texte("L’exposition est un pourcentage de départs, pas un nombre de postes. Un petit département à exposition forte peut représenter dix départs ; un grand département à exposition modérée, plusieurs centaines. La lecture se fait toujours avec le volume de départs de la typologie.", { after: 0 })])];

// =============================================================================================
// 5. Classes
// =============================================================================================
const p5 = [
  h1("5. Les classes : faible, modérée, forte"),
  texte("Pour rendre l’exposition lisible sur une carte, elle est classée en trois niveaux. Deux méthodes existent, choisies par `TENSION_METHODE_CLASSES` :"),
  ...puces(["**Terciles** (méthode par défaut, dite provisoire) : les deux bornes sont les terciles observés sur les départements retenus, de sorte qu’environ un tiers des départements tombe dans chaque classe. Les bornes dépendent donc des données de l’édition et sont **écrites dans `parametres_exposition_tension.csv`** à chaque exécution.",
    "**Seuils fixes** : deux pourcentages fixés dans la configuration (`TENSION_SEUIL_EXPO_FAIBLE`, `TENSION_SEUIL_EXPO_FORTE`), stables d’une édition à l’autre. À préférer une fois le périmètre stabilisé, après lecture des terciles de quelques éditions."]),
  texte("Dans les deux cas, une valeur égale à une borne appartient à la classe supérieure, comme pour toutes les classes de la typologie. Les départements **retenus** pour calculer les terciles sont ceux dont la couverture atteint le minimum et dont l’effectif du champ atteint le plancher de la typologie (`TYPO_SEUIL_EFFECTIF_MIN`) : un très petit département n’influence pas les bornes."),
  ...exemple([texte("Avec des bornes fictives de 35 % et 45 %, l’exposition de 46 % du département Exemple le place en classe **Forte**. Si la borne haute avait été 50 %, il aurait été en classe Modérée : c’est précisément ce que la robustesse examine.", { after: 0 })])];

// =============================================================================================
// 6. Robustesse
// =============================================================================================
const p6 = [
  h1("6. La robustesse : un résultat qui tient, ou à confirmer"),
  texte("Une classe peut dépendre de choix de méthode autant que de la réalité du département. Pour le dire honnêtement, la classe est recalculée dans **cinq situations**, toujours avec les mêmes bornes que le calcul central :"),
  ...numeros(["avec l’hypothèse de départ en retraite **basse** (fourchette basse des départs) ;", "avec l’hypothèse **haute** ;", "avec un seuil de tension **plus strict** d’un cran (rang 5 au lieu de 4) ;", "avec un seuil **plus large** d’un cran (rang 3) ;", "**sans les métiers dont le rattachement PCS → FAP est de faible confiance**, c’est-à-dire comme si l’on ignorait les rattachements incertains."]),
  texte("Si la classe ne change dans aucune des cinq situations, le résultat est dit **robuste**. Si elle change dans au moins une, il est dit **à confirmer**, et la ou les causes sont écrites : « hypothèse de départ », « seuil de tension », « rattachement PCS → FAP ». Cette mention accompagne la classe dans toutes les restitutions."),
  image("s4_robustesse", 15), legende("Schéma 3 — Les cinq variantes de robustesse sur l’exemple fictif : une seule bascule suffit."),
  h2("La faible confiance : d’où elle vient"),
  texte("Le classeur porte, pour chaque PCS, un indice de confiance du rattachement. Par défaut, le programme considère comme « de faible confiance » le **niveau le plus bas observé** dans le classeur, dès qu’il existe au moins deux niveaux ; un seuil explicite peut être donné (`TENSION_CONFIANCE_MIN`). Un indice manquant est traité comme faible. Dans la fiche, ces métiers portent la mention « à confirmer » ; dans le calcul central ils comptent normalement, et la variante 5 mesure ce qu’ils pèsent."),
  ...retenir("« À confirmer » n’est pas « faux » : c’est un résultat dont la classe est sensible à un choix. La cause indique où porter la vérification : sur les hypothèses de départ, sur le seuil de tension, ou sur la correspondance entre métiers et familles professionnelles.")];

// =============================================================================================
// 7. Le levier
// =============================================================================================
const p7 = [
  h1("7. Le levier : comment il est choisi"),
  texte("La tension d’un métier peut avoir plusieurs causes, et la réponse n’est pas la même selon la cause : on ne répond pas de la même façon à un manque de candidats, à une formation longue ou à des conditions de travail dissuasives. Les sept composantes de la Dares permettent de dire **ce qui pèse le plus** dans la tension d’un métier. Le **levier** est la traduction, en langage d’action, de cette composante dominante."),
  grille([3100, 3100, 3438], ["Composante dominante", "Levier affiché", "Ordre de priorité en cas d’égalité"], [
    ["Manque de main-d’œuvre disponible", "attirer et élargir les viviers de recrutement", "1"],
    ["Formation spécifique au métier", "former tôt : les formations sont spécifiques et longues", "2"],
    ["Conditions de travail contraignantes", "fidéliser en améliorant les conditions de travail", "3"],
    ["Faible attractivité salariale", "agir sur la rémunération", "4"],
    ["Inadéquation géographique", "favoriser la mobilité et l’implantation", "5"],
    ["Emplois peu durables", "stabiliser les emplois", "6"],
    ["Forte intensité d’embauche", "anticiper les recrutements", "7"]], { size: 17 }),
  vide(60),
  h2("Pour un métier"),
  ...numeros(["On lit les rangs des sept composantes de sa famille professionnelle.", "On retient la composante au **rang le plus élevé**.", "Elle n’est conservée que si ce rang **atteint le seuil** de tension (4 par défaut) : une composante dominante de rang 2 ne dit rien d’utile, et le métier est alors noté « aucune composante au-dessus du seuil », sans levier.", "En cas d’égalité, la **valeur fine** tranche (une composante à 5,2 l’emporte sur une à 5,1) ; s’il reste une égalité, l’**ordre du tableau** ci-dessus s’applique, et l’égalité est signalée dans la table interne."], "num2"),
  image("s3_levier", 15), legende("Schéma 4 — Du rang des composantes au levier (exemple fictif)."),
  h2("Pour un département"),
  texte("Le levier d’un département résume les leviers de ses métiers en tension. On prend les métiers de la part A, on regroupe leurs départs attendus par composante dominante, et l’on retient la composante qui **porte le plus de départs**. Un métier qui part beaucoup pèse donc plus qu’un métier qui part peu, ce qui est la logique de toute l’analyse. En cas d’égalité, le même ordre de priorité s’applique."),
  ...exemple([texte("Dans le département Exemple, les métiers en tension se répartissent ainsi : technicienne de maintenance, 30 départs, composante dominante « manque de main-d’œuvre » ; soudeur, 20 départs, « formation spécifique » ; chaudronnier, 10 départs, « conditions de travail ». La composante qui porte le plus de départs est le manque de main-d’œuvre (30 sur 60) : le levier du département est « attirer et élargir les viviers de recrutement ».", { after: 0 })]),
  ...attention([texte("Le levier est une **orientation de lecture**, pas une recommandation ni un plan d’action : il dit quelle nature de difficulté domine, d’après des indicateurs nationaux. Sa fiabilité dépend du rattachement PCS → FAP et de la qualité des composantes publiées. Il se discute avec les acteurs locaux, il ne les remplace pas.", { after: 0 })], "Ce que le levier n’est pas")];

// =============================================================================================
// 8. Les quatre situations ; 9. croisement typologie
// =============================================================================================
const p8 = [
  h1("8. Les quatre situations de métier"),
  texte("À l’échelle de la France, chaque métier stratégique est classé selon deux questions : **ses salariés partent-ils plus vite que la moyenne ?** (la part de ses salariés susceptibles de partir d’ici 2030 atteint-elle celle de l’ensemble des salariés concernés ?) et **est-il en tension ?** Les quatre combinaisons reçoivent un libellé factuel, qui décrit sans juger :"),
  grille([3200, 3219, 3219], ["Situation", "Départs", "Tension"], [
    ["**Départs rapides et métier en tension**", "au moins le repère France", "rang ≥ seuil"],
    ["**Départs rapides, métier hors tension**", "au moins le repère France", "rang < seuil"],
    ["**Départs modérés, métier en tension**", "sous le repère France", "rang ≥ seuil"],
    ["**Départs modérés, métier hors tension**", "sous le repère France", "rang < seuil"]], { size: 18 }),
  vide(60),
  texte("La fiche présente ces métiers dans un tableau, situation par situation, avec pour chacun ses départs et leur fourchette, sa part susceptible de partir, sa tension (réglette de cinq cases et valeur fine), la composante qui pèse le plus, le levier et la confiance du rattachement. Seuls les métiers **diffusables** y figurent ; les autres ne sont que des comptes (voir la section 10)."),
  h1("9. Le croisement avec la typologie"),
  texte("La typologie attribue à chaque département un profil, d’après son poids dans l’emploi, le volume et l’intensité de ses départs. L’exposition ne modifie **ni les profils, ni les règles, ni les couleurs** de la carte : elle s’y superpose."),
  ...puces(["Sur la carte des profils, les départements d’exposition **forte** sont **hachurés** ; la couleur reste celle du profil. Un département masqué par le secret n’est jamais hachuré.",
    "Le panneau de détail d’un département ajoute une rubrique « Recrutement » : exposition et repère France, classe et robustesse, couverture, levier, métiers en tension les plus concernés.",
    "Un département qui cumule un **renouvellement rapide** (profils « Pôle majeur à renouveler rapidement » et « Renouvellement rapide à surveiller ») et une **exposition forte** est signalé comme tel (`tension_cumul`). Le premier message de la fiche compte ces départements et la part des départs qu’ils concentrent."]),
  ...retenir("Lire dans cet ordre : la couleur (combien partent, à quel rythme), puis les hachures (sur quels métiers), puis le tableau des métiers et le levier (quelle nature de difficulté). Un département à renouvellement rapide et hachuré cumule les deux difficultés ; il mérite d’être regardé en premier, sans que cela constitue un classement.")];

// =============================================================================================
// 10. Secret ; 11. limites
// =============================================================================================
const p10 = [
  h1("10. Le secret statistique"),
  texte("Les départs par métier sont des données individuelles agrégées : les règles de secret de la Base Tous salariés s’appliquent (moins de 5 salariés, moins de 3 entreprises, ou une entreprise représentant plus de 85 % d’une cellule : la cellule n’est pas diffusée, avec suppression secondaire). L’exposition ajoute un agrégat nouveau, les parts A, B et C, et doit donc être protégée à son tour."),
  ...numeros(["**Publication tout ou rien** : pour un département, l’exposition n’est diffusée que si chacune des trois parts non vides respecte la règle primaire. Sinon, rien de l’exposition n’est publié pour ce département.",
    "**Garde contre la différenciation** : si une part contient exactement **une** cellule PCS masquée par ailleurs, la publier reviendrait à révéler cette cellule par soustraction. L’exposition n’est alors pas diffusée.",
    "**Cohérence avec la typologie** : un département masqué dans la diffusion de la typologie n’a aucune valeur d’exposition.",
    "**Nature des valeurs diffusées** : des pourcentages arrondis, des classes et des textes ; jamais les effectifs des parts. Les métiers cités sont des cellules PCS elles-mêmes diffusables."], "num3"),
  texte("Les tables internes (dossier `interne/`, jamais versionné) conservent tout ; les tables de diffusion (dossier `diffusion/`) sont les seules lues par les cartes et la fiche."),
  h1("11. Limites et précautions"),
  ...puces(["**Deux populations différentes.** La tension décrit tout le marché du travail d’un métier, tous secteurs et tous âges ; les départs ne concernent que les salariés de la Base âgés de 45 ans ou plus. On compare des parts et des classes, jamais des volumes entre les deux sources.",
    "**Deux temporalités.** La tension est observée à une date donnée ; les départs sont projetés à 2030. Une tension forte aujourd’hui peut s’atténuer, ou non, d’ici là.",
    "**Une liste, pas un échantillon.** Les métiers stratégiques sont une sélection. Les résultats valent pour cette liste ; la couverture dit ce qu’elle représente dans chaque département.",
    "**Des nomenclatures et des millésimes.** Les PCS de la Base, les familles professionnelles et l’édition des tensions doivent être cohérents entre eux ; le classeur de rattachement porte cette responsabilité, et l’indice de confiance en garde la trace.",
    "**Des rattachements incertains.** Une PCS répartie entre plusieurs familles reçoit la tension d’une seule ; la variante de robustesse « sans faible confiance » mesure l’effet de ces cas.",
    "**Aucune causalité.** Un métier à départs rapides et en tension n’est pas, de ce fait, un métier en pénurie dans le département : l’exposition appelle une vérification locale, elle ne la remplace pas."])];

// =============================================================================================
// Annexes
// =============================================================================================
const annexes = [
  h1("Annexe A — Glossaire"),
  grille([2500, 7138], ["Terme", "Définition"], [
    ["PCS", "Nomenclature de l’Insee des professions et catégories socioprofessionnelles ; ici au niveau détaillé (code à quatre caractères), tel qu’il figure dans la Base."],
    ["FAP", "Familles professionnelles de la Dares : regroupements de métiers proches, selon lesquels les indicateurs de tension sont publiés."],
    ["Métier stratégique", "PCS figurant dans la liste établie par le projet et rattachée à une famille professionnelle dans le classeur."],
    ["Rang de tension", "Position de la famille professionnelle parmi toutes les familles, de 1 (tension la plus faible) à 5 (la plus forte) ; 5.1 et 5.2 affinent le niveau 5 depuis 2023."],
    ["En tension", "Métier dont le rang de l’indicateur synthétique atteint le seuil configuré (4 par défaut)."],
    ["Parts A, B, C", "Départs attendus du département : métiers stratégiques en tension (A), stratégiques hors tension (B), le reste (C)."],
    ["Exposition", "A / (A + B) : part des départs des métiers stratégiques qui concernent des métiers en tension."],
    ["Couverture", "(A + B) / (A + B + C) : poids des métiers stratégiques dans les départs du département."],
    ["Repère France", "Exposition calculée sur l’ensemble des départements."],
    ["Robustesse", "Stabilité de la classe dans cinq variantes de calcul ; « robuste » ou « à confirmer » avec la cause."],
    ["Composante dominante", "Celle des sept composantes de la tension dont le rang est le plus élevé, si ce rang atteint le seuil."],
    ["Levier", "Traduction en langage d’action de la composante dominante ; pour un département, la composante qui porte le plus de départs en tension."],
    ["Cumul", "Département à renouvellement rapide (profils concernés de la typologie) et à exposition forte."]], { premiereGras: true, size: 17 }),
  vide(100),
  h1("Annexe B — Paramètres (section TENSION_* de la configuration)"),
  grille([3300, 1500, 4838], ["Paramètre", "Défaut", "Effet"], [
    ["`GENERER_EXPOSITION_TENSION`", "TRUE", "Active l’étape 08i. Classeur absent : message, jamais d’arrêt."],
    ["`FICHIER_PCS_FAP_TENSION`, `TENSION_FEUILLE`", "data/…, « Sheet1 »", "Classeur et feuille lus."],
    ["`TENSION_COLS`, `TENSION_COMPOSANTES`", "noms du classeur", "Correspondance entre les colonnes du classeur et les clés internes ; libellé et levier de chaque composante ; l’ordre départage les ex aequo."],
    ["`TENSION_RANG_MAX`", "5", "Nombre de rangs (10 pour des déciles)."],
    ["`TENSION_SOUS_NIVEAUX`", "c(1, 2)", "Sous-niveaux admis du rang maximal (5.1, 5.2) ; NULL = aucun."],
    ["`TENSION_SEUIL_RANG`", "4", "Rang à partir duquel un métier est en tension."],
    ["`TENSION_CONFIANCE_MIN`", "NULL", "Seuil de faible confiance ; NULL = plus bas niveau observé."],
    ["`TENSION_METHODE_CLASSES`", "terciles", "Bornes des classes : terciles provisoires ou seuils fixes."],
    ["`TENSION_SEUIL_EXPO_FAIBLE` / `_FORTE`", "25 / 50", "Bornes fixes des classes, en pourcentage (méthode fixes)."],
    ["`TENSION_COUVERTURE_MIN_PCT`", "30", "Couverture minimale pour interpréter l’exposition."]], { size: 17 }),
  vide(100),
  h1("Annexe C — Sorties"),
  grille([3900, 5738], ["Fichier (dossier typologie_territoriale/)", "Contenu"], [
    ["interne/exposition_tension_departements.csv", "Une ligne par département, complet : parts, exposition, couverture, classe, variantes de robustesse, levier, métiers."],
    ["interne/metiers_strategiques_tension.csv", "Une ligne par PCS de la liste : rangs, tension, composante dominante, levier, confiance, statut, situation."],
    ["interne/parametres_exposition_tension.csv", "Paramètres et bornes effectivement appliqués (dont les bornes des classes et le seuil de confiance)."],
    ["diffusion/exposition_tension_departements.csv", "Même maille, secret appliqué : pourcentages, classes, robustesse, levier, textes ; lu par les cartes."],
    ["diffusion/metiers_strategiques_tension.csv", "Métiers stratégiques au niveau France, cellules diffusables seulement."],
    ["diffusion/fiche_exposition_tension.html", "Fiche d’une page : trois messages, tableau des métiers par situation, lecture, méthode ; en annexe repliée, la matrice des métiers (un point par métier diffusable, libellé lu au survol de la souris) ; sans script."],
    ["diffusion/matrice_metiers_tension.png", "Représentation graphique des situations de métier (image à part)."],
    ["diffusion/exposition_tension.xlsx", "Classeur à quatre feuilles (Lisez-moi, Départements, Métiers, Paramètres) : le contenu de la fiche, issu des seules tables de diffusion, pour être repris, trié et transmis."]], { size: 17 })];

const doc = new Document({
  creator: "Projet départs Base", title: "Note méthodologique — Exposition des départements au recrutement difficile",
  styles: { default: { document: { run: { font: FONT, size: 20, color: ENCRE } } },
    paragraphStyles: [{ id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true, run: { font: FONT, size: 29, bold: true, color: NAVY }, paragraph: { outlineLevel: 0 } },
                      { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true, run: { font: FONT, size: 23, bold: true, color: NAVY }, paragraph: { outlineLevel: 1 } }] },
  numbering: { config: [
    { reference: "puces", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 360, hanging: 240 } }, run: { color: NAVY } } }] },
    ...["num1", "num2", "num3"].map(r => ({ reference: r, levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 420, hanging: 300 } }, run: { bold: true, color: NAVY } } }] }))] },
  sections: [{ properties: { page: PAGE }, headers: { default: entete }, footers: { default: pied },
    children: [...p0, ...p2, ...p3, ...p4, ...p5, ...p6, ...p7, ...p8, ...p10, ...annexes] }] });
Packer.toBuffer(doc).then(b => { fs.mkdirSync(path.dirname(SORTIE), { recursive: true }); fs.writeFileSync(SORTIE, b); console.log("écrit :", SORTIE); });
