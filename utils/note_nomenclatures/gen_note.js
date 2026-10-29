// Note « PCS, CS, FAP, ROME : quatre sigles, plusieurs façons de regarder un métier » (docx-js, A4 portrait).
// Usage : node gen_note.js <dossier_images> <fichier_sortie.docx>
// Codes et libellés : référentiels officiels (voir la section Sources de la note). Aucune donnée du projet.
const fs = require("fs"), path = require("path");
const { Document, Packer, Paragraph, TextRun, ImageRun, Table, TableRow, TableCell, WidthType, AlignmentType, BorderStyle, ShadingType,
        TableLayoutType, VerticalAlign, Footer, Header, PageNumber, PageBreak, ExternalHyperlink, LevelFormat, HeadingLevel, TabStopType } = require("docx");
const DIR_IMG = process.argv[2] || path.join(__dirname, "..", "..", "docs", "assets", "nomenclatures");
const SORTIE = process.argv[3] || path.join(__dirname, "..", "..", "docs", "note_comprendre_nomenclatures_PCS_FAP_ROME.docx");

const NAVY = "1E3A5F", ENCRE = "1F2933", TEXTE = "3E4C59", GRIS = "7A8795", FILET = "D9DEE5", FOND = "F5F7FA", BLEU_CLAIR = "E3ECF6", BLANC = "FFFFFF";
const ORANGE = "C2620A", ORANGE_CLAIR = "FDF1E1", VERT = "2E7D5B", VERT_CLAIR = "E6F2EC";
const FONT = "Calibri";
const NONE = { style: BorderStyle.NONE, size: 0, color: BLANC }, SANS = { top: NONE, bottom: NONE, left: NONE, right: NONE };
const LARGEUR = 9638;
const PAGE = { size: { width: 11906, height: 16838 }, margin: { top: 1134, bottom: 1000, left: 1134, right: 1134, header: 500, footer: 500 } };

const PERIMETRE = process.env.PERIMETRE || "Base";   // nom du périmètre dans le texte (en interne : PERIMETRE=BITD node …) ; « Base Tous salariés » jamais touché
const run = (t, o = {}) => new TextRun({ text: t.replace(/\bBase\b(?! Tous)/g, PERIMETRE), font: FONT, size: o.size || 20, color: o.color || ENCRE, bold: o.bold, italics: o.italics, characterSpacing: o.spacing, superScript: o.sup });
const rich = (t, o = {}) => t.split(/(\*\*[^*]+\*\*|\^\[[^\]]+\])/).filter(Boolean).map(s => s.startsWith("**") ? run(s.slice(2, -2), { ...o, bold: true }) : s.startsWith("^[") ? run(s.slice(2, -1), { ...o, sup: true, color: NAVY, size: 17, bold: true }) : run(s, o));
const par = (runs, o = {}) => new Paragraph({ spacing: { before: o.before || 0, after: o.after === undefined ? 100 : o.after, line: o.line || 258 }, alignment: o.align, keepNext: o.keepNext, keepLines: o.keepLines, children: Array.isArray(runs) ? runs : [runs] });
const texte = (t, o = {}) => par(rich(t, o), o);
const h1 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 240, after: 110, line: 276 }, keepNext: true, border: { bottom: { style: BorderStyle.SINGLE, size: 8, color: NAVY, space: 4 } }, children: [run(t, { size: 29, bold: true, color: NAVY })] });
const h2 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 180, after: 70, line: 276 }, keepNext: true, children: [run(t, { size: 23, bold: true, color: NAVY })] });
const puces = (items, o = {}) => items.map(t => new Paragraph({ numbering: { reference: "puces", level: 0 }, spacing: { before: 0, after: 50, line: 256 }, children: rich(t, o) }));
const numeros = (items, ref) => items.map(t => new Paragraph({ numbering: { reference: ref, level: 0 }, spacing: { before: 0, after: 60, line: 256 }, children: rich(t) }));
const vide = (h = 80) => new Paragraph({ spacing: { before: 0, after: 0, line: h, lineRule: "exact" }, children: [run("", { size: 4 })] });
const saut = () => new Paragraph({ children: [new PageBreak()] });
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
const attention = (c, titre = "Attention, ce n’est pas la même chose") => encadre(titre, c, ORANGE, ORANGE_CLAIR);
const filrouge = (c, titre = "Fil rouge — notre technicienne de maintenance") => encadre(titre, c, VERT, VERT_CLAIR);
const grille = (widths, entete, lignes, o = {}) => {
  const bord = { style: BorderStyle.SINGLE, size: 4, color: FILET }, B = { top: bord, bottom: bord, left: NONE, right: NONE };
  const tete = entete.map((t, i) => cell([par(run(t, { size: 17, bold: true, color: BLANC }), { after: 0, line: 240 })], widths[i], { fill: NAVY, borders: B, pad: 80 }));
  const corps = lignes.map((l, k) => l.map((t, i) => cell([par(rich(t, { size: o.size || 18, color: i === 0 && o.premiereGras ? NAVY : ENCRE, bold: i === 0 && o.premiereGras }), { after: 0, line: 250 })], widths[i], { fill: k % 2 ? FOND : BLANC, borders: B, pad: 80 })));
  return table(widths, [tete, ...corps], { entete: true });
};
const lien = (t, url) => new ExternalHyperlink({ link: url, children: [new TextRun({ text: t, font: FONT, size: 16, color: "1F5FA8", underline: {} })] });
const entete = new Header({ children: [new Paragraph({ tabStops: [{ type: TabStopType.RIGHT, position: LARGEUR }], border: { bottom: { style: BorderStyle.SINGLE, size: 4, color: FILET, space: 4 } },
  children: [run("Comprendre les nomenclatures professionnelles · PCS, CS, FAP, ROME", { size: 15, color: GRIS }), run("\tcodes et libellés issus des référentiels officiels (sources en fin de note)", { size: 15, color: GRIS, italics: true })] })] });
const pied = new Footer({ children: [new Paragraph({ tabStops: [{ type: TabStopType.RIGHT, position: LARGEUR }],
  children: [run("Guide de référence · octobre 2026", { size: 15, color: GRIS }), run("\tpage ", { size: 15, color: GRIS }), new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 15, color: GRIS }), run(" / ", { size: 15, color: GRIS }), new TextRun({ children: [PageNumber.TOTAL_PAGES], font: FONT, size: 15, color: GRIS })] })] });

// =============================================================================================
// PAGE 1 — Pourquoi plusieurs nomenclatures ?
// =============================================================================================
const page1 = [
  par(run("COMPRENDRE LES NOMENCLATURES PROFESSIONNELLES", { size: 16, bold: true, color: GRIS, spacing: 30 }), { after: 120 }),
  par(run("PCS, CS, FAP, ROME : quatre sigles, plusieurs façons de regarder un métier", { size: 38, bold: true, color: NAVY }), { after: 80, line: 300 }),
  par(run("Les repères essentiels pour comprendre les nomenclatures professionnelles.", { size: 24, color: TEXTE }), { after: 180, line: 280 }),
  texte("Prenons une technicienne de maintenance qui entretient les machines d’une usine. Selon la question que l’on se pose, on ne la classe pas de la même façon. **Quelle profession exerce-t-elle, et quelle est sa position socioprofessionnelle ?** C’est la question de la PCS. **À quel niveau de regroupement veut-on l’observer ?** C’est la question des niveaux de la PCS, du grand groupe à la profession détaillée. **À quelle famille de métiers peut-on la rapprocher ?** C’est la question des FAP. **Quel poste est recherché ou proposé sur le marché du travail ?** C’est la question du ROME."),
  image("n1_quatre_regards", 15), legende("Schéma 1 — Un même emploi, quatre questions, quatre façons de classer."),
  texte("Ces quatre regards ne sont pas quatre degrés de précision d’une même liste. Chacun a été construit par une institution différente, pour un usage différent : l’Insee décrit la société et l’emploi, la Dares analyse les métiers et le marché du travail, France Travail rapproche des offres et des candidats. C’est pourquoi aucune classification universelle ne les remplace : chacune répond bien à sa question, et mal aux autres."),
  ...retenir("Une nomenclature est une réponse à une question. Avant de lire un code, demandez-vous quelle question il sert, dans quelle nomenclature, et dans quelle version."),
  vide(120)];

// =============================================================================================
// PAGE 2 — La PCS et ses niveaux
// =============================================================================================
const page2 = [
  h1("1. La PCS, c’est un système de classement avec plusieurs niveaux de zoom"),
  texte("Quand notre technicienne répond au recensement, l’Insee ne range pas son intitulé tel quel : il le code dans la **nomenclature des professions et catégories socioprofessionnelles (PCS)**. Cette nomenclature classe les personnes selon leur profession, mais aussi selon leur statut (salarié ou indépendant), leur niveau de qualification et leur position dans l’organisation du travail (encadrant ou non, cadre, technicien, ouvrier…). Elle ne décrit pas un métier au sens des compétences ; elle décrit une **position socioprofessionnelle**.^[1]"),
  texte("La version en vigueur pour le recensement et les enquêtes auprès des ménages est la **PCS 2020**. Elle comporte quatre niveaux emboîtés : **6 groupes socioprofessionnels**, **29 catégories socioprofessionnelles**, **121 professions regroupées** et **311 professions**. Le code d’une profession a quatre caractères (deux chiffres, une lettre, un chiffre) ; son premier chiffre donne le groupe, ses deux premiers chiffres la catégorie, ses trois premiers caractères la profession regroupée.^[2]"),
  image("n2_zoom_pcs2020", 15.5), legende("Schéma 2 — Le zoom de la PCS 2020 sur l’exemple vérifié de la technicienne de maintenance."),
  ...filrouge([texte("Dans la PCS 2020, une technicienne d’installation et de maintenance d’équipements industriels relève de la profession **47D3 · Techniciens / Techniciennes d’installation et de maintenance (hors informatique)**. Ce code contient toute la hiérarchie : groupe **4 · Professions intermédiaires**, catégorie **47 · Techniciens / Techniciennes**, profession regroupée **47D · Techniciens / Techniciennes des fonctions connexes de la production**.^[3]", { after: 0 })]),
  h2("Ce que signifie « PCS niveau 1 »"),
  texte("« PCS niveau 1 » désigne le premier chiffre du code, c’est-à-dire le **groupe socioprofessionnel**. Les groupes de la PCS 2020 sont : 1 · Agriculteurs exploitants ; 2 · Artisans, commerçants et chefs d’entreprise ; **3 · Cadres et professions intellectuelles supérieures** ; **4 · Professions intermédiaires** ; **5 · Employés** ; **6 · Ouvriers**. Ces deux premiers niveaux sont restés stables depuis 1982, ce qui permet les comparaisons dans le temps ; c’est le niveau détaillé qui a été refondu en 2020 (486 professions en 2003, 311 en 2020).^[2]"),
  texte("Un tableau portant sur l’emploi **salarié** d’entreprises ne retient en général que les groupes 3 à 6 : les groupes 1 et 2 regroupent pour l’essentiel des indépendants, et une profession non renseignée ne peut être rattachée à aucun groupe. Ces exclusions sont un choix de champ, pas une propriété de la nomenclature."),
  ...attention([texte("**« Professions intermédiaires » ne veut pas dire « techniciens ».** Le groupe 4 comporte sept catégories : les techniciens (catégorie 47) n’en sont qu’une ; on y trouve aussi les instituteurs, les professions intermédiaires de la santé, du travail social, de la fonction publique, des entreprises, et les agents de maîtrise (48).^[4]", { after: 60 }),
    texte("Plus généralement, deux personnes classées dans le même groupe peuvent exercer des professions très différentes, de qualification et de compétences très différentes. Le groupe dit la position, pas le métier.", { after: 0 })])];

// =============================================================================================
// PAGE 3 — PCS, CS1, PCS fine, PCS-ESE
// =============================================================================================
const page3 = [
  h1("2. PCS, CS1, PCS fine, PCS-ESE : éviter les confusions"),
  texte("Dans les travaux statistiques, plusieurs expressions proches circulent. Elles ne désignent pas toujours le même niveau, ni la même version. Voici ce que chacune recouvre, puis ce qu’elle désigne précisément dans notre projet."),
  h2("Quatre expressions, quatre objets"),
  ...puces(["**PCS** : la nomenclature socioprofessionnelle de l’Insee dans son ensemble, avec sa logique de classement et ses niveaux emboîtés.",
    "**CS, groupe, catégorie** : des niveaux de cette arborescence. Le groupe socioprofessionnel est le niveau 1 (un chiffre) ; la catégorie socioprofessionnelle est le niveau 2 (deux chiffres). Le sigle « CS » désigne, selon les documents, l’un ou l’autre : il faut toujours vérifier.",
    "**PCS fine** ou **PCS détaillée** : le niveau le plus bas de la nomenclature. Mais lequel ? En PCS 2020, c’est la profession (311 postes, code du type 47D3). En PCS-ESE 2017, c’est la rubrique (429 postes, code du type 477b). Dire « PCS fine » sans préciser la nomenclature et la version ne suffit pas.",
    "**PCS-ESE** : la nomenclature des professions et catégories socioprofessionnelles des **emplois salariés des employeurs privés et publics**. C’est la version que les employeurs utilisent pour déclarer leurs salariés (déclaration sociale nominative). La version en vigueur est la **PCS-ESE 2017**, qui reprend la PCS-ESE 2003 en y ajoutant 17 codes propres à la fonction publique. Ses trois premiers caractères sont communs avec la PCS 2003, mais pas avec le niveau détaillé de la PCS 2020, qui a été refondu.^[5]"]),
  ...filrouge([texte("La même technicienne, déclarée par son employeur, est codée en PCS-ESE 2017 dans la rubrique **477b · Techniciens d’installation et de maintenance des équipements industriels (électriques, électromécaniques, mécaniques, hors informatique)**. Les deux premiers niveaux coïncident avec la PCS 2020 (groupe 4, catégorie 47) ; le niveau détaillé, lui, change de code et de libellé : 477b d’un côté, 47D3 de l’autre.^[6]", { after: 0 })]),
  h2("Ce que ces mots désignent dans notre projet"),
  texte("Trois objets distincts se cachent derrière le mot « PCS » dans le dépôt du projet, et ils ne doivent pas être confondus."),
  grille([2300, 1500, 1900, 3938], ["Objet", "Nomenclature", "Version", "Rôle dans le projet"], [
    ["**Code source de la base des salariés**", "PCS-ESE (nomenclature employeurs)", "Supposée 2017 ; **version source à confirmer**", "Colonne « pcs » de la base, code à 4 caractères conservé en texte. Le projet suppose la PCS-ESE sans contrôle du millésime. 2017 et 2003 ne diffèrent que par 17 codes de la fonction publique, ce qui limite l’enjeu pour des employeurs privés."],
    ["**Référentiel de libellés**", "PCS-ESE", "2017 (fichier Insee à déposer)", "Sert uniquement à afficher le libellé d’un code dans les restitutions. Aucun calcul n’en dépend ; s’il manque, le code est affiché seul. Le fichier livré avec le dépôt est un jeu fictif de démonstration."],
    ["**Regroupement analytique « cs1 »**", "Construit par le projet", "Sans objet", "Premier caractère du code PCS, traduit en quatre grandes catégories : 3 → Cadres, 4 → Professions intermédiaires, 5 → Employés, 6 → Ouvriers. Tout autre premier caractère (1, 2, codes non renseignés…) est hors champ et compté à part. « cs1 » est donc le niveau 1 de la PCS, restreint aux salariés, et non une variable officielle."]], { premiereGras: true, size: 17 }),
  vide(60),
  ...retenir("Un fichier de libellés PCS-ESE 2017 ne prouve pas que les données sources sont en PCS-ESE 2017 : il sert à nommer des codes, pas à les produire. La version effective des codes se vérifie dans la documentation de l’extraction, jamais par le nom d’un fichier.")];

// =============================================================================================
// PAGE 4 — Les FAP
// =============================================================================================
const page4 = [
  h1("3. Les FAP : regrouper les professions autrement, pour parler des métiers"),
  texte("Pourquoi ne pas utiliser directement la PCS pour analyser les métiers et les recrutements ? Parce que la PCS range notre technicienne selon sa position (technicienne, donc profession intermédiaire), tandis qu’un employeur qui recrute, ou un conseiller qui place un candidat, raisonne en **métier** : la maintenance industrielle, que l’on soit ouvrier qualifié, technicien ou agent de maîtrise. Les deux regards ne coïncident pas."),
  texte("Les **familles professionnelles (FAP)** ont été construites par la Dares pour cela. Elles regroupent des professions relevant de compétences et d’activités professionnelles proches, dans une logique d’analyse des métiers et du marché du travail. Elles ont été élaborées en rapprochant deux univers : la **PCS** de l’Insee, qui décrit les professions exercées, et le **ROME** de France Travail, qui décrit les métiers tels qu’ils s’échangent sur le marché du travail (offres, demandes, placement).^[7]"),
  image("n5_pont_pcs_fap_rome", 14.5), legende("Schéma 3 — La FAP fait le pont entre la PCS et le ROME, par des tables de passage, sans équivalence mot à mot."),
  h2("Comment les FAP sont organisées"),
  texte("La version précédente, la **FAP 2009**, comptait 22 domaines professionnels, 87 familles professionnelles regroupées et 225 familles professionnelles détaillées, chaque domaine étant repéré par une lettre.^[8] La version en vigueur, la **FAP 2021**, a été refondue pour suivre la PCS 2020 ; son arborescence (domaines, familles regroupées, familles détaillées) est publiée par la Dares, et les publications récentes sur les tensions travaillent avec 228 familles détaillées.^[7] Dans ces familles, les métiers industriels forment des ensembles reconnaissables : la **maintenance**, la **mécanique et le travail des métaux**, les **ingénieurs et cadres techniques de l’industrie**."),
  texte("Le **ROME** (répertoire opérationnel des métiers et des emplois) intervient de l’autre côté du pont. Chaque fiche ROME porte un code d’une lettre et quatre chiffres et décrit un métier par ses appellations, ses compétences et ses mobilités ; depuis mars 2023, la version en vigueur est le ROME 4.0.^[9]"),
  ...filrouge([texte("Dans la FAP 2009, dont la table officielle de correspondance FAP / PCS 2003 / ROME est publiée, notre technicienne relève du domaine **G · Maintenance** et de la famille regroupée **G1Z · Techniciens et agents de maîtrise de la maintenance**.^[8] Côté ROME, le métier correspond à la fiche **I1304 · Installation et maintenance d’équipements industriels et d’exploitation**, que France Travail présente aujourd’hui sous l’intitulé « Technicien / Technicienne de maintenance industrielle ».^[9] Pour la FAP 2021, c’est la table de passage PCS 2020 → FAP 2021 de la Dares qui fait foi : elle part du code 47D3 et, si besoin, du libellé de profession.", { after: 0 })]),
  ...attention([texte("**Une FAP n’est pas un indicateur de tension.** C’est une nomenclature de métiers. Des indicateurs sur le marché du travail (tensions, offres, demandes) peuvent ensuite être calculés et diffusés **selon** cette nomenclature, famille par famille. Dire « cette FAP est en tension » est un raccourci pour « l’indicateur de tension calculé pour cette famille est élevé ».", { after: 0 })], "Attention, ce n’est pas la même chose")];

// =============================================================================================
// PAGE 5 — Correspondances
// =============================================================================================
const page5 = [
  h1("4. Passer d’une PCS à une FAP : une correspondance, pas une traduction mot à mot"),
  texte("À l’intérieur de la PCS, remonter d’un niveau est toujours possible et sans ambiguïté : le code 47D3 contient 47D, 47 et 4. C’est une **hiérarchie emboîtée** : chaque profession a un seul parent. Entre la PCS et la FAP, il n’y a plus d’emboîtement, mais une **table de passage** : un fichier qui dit, code par code, vers quelle famille va chaque profession. Trois situations s’y rencontrent : la correspondance **un-à-un** (une profession, une famille), la correspondance **un-à-plusieurs** (une profession se répartit entre plusieurs familles), et les cas où le code seul ne suffit pas et où il faut une **information complémentaire**, en pratique le libellé précis de la profession."),
  image("n4_emboitee_vs_passage", 15.5), legende("Schéma 4 — À gauche, l’emboîtement de la PCS ; à droite, la table de passage vers les FAP."),
  h2("Le point technique à connaître : la FAP 2021"),
  texte("Avec la PCS 2003, chaque profession détaillée allait dans une seule FAP 2009. La refonte de la PCS en 2020 a changé la donne : la Dares indique qu’il **n’existe plus de correspondance unique des PCS vers les FAP**. Une même profession de la PCS 2020 peut désormais se répartir entre plusieurs familles de la FAP 2021. Pour trancher, la FAP 2021 s’appuie sur le **libellé de profession** comme brique élémentaire, et la Dares publie deux tables : l’une au niveau des codes PCS 2020, l’autre au niveau des libellés, à utiliser pour un codage sans ambiguïté.^[7] Autrement dit, connaître le code 47D3 ne suffit pas toujours ; il faut parfois savoir ce que la personne a déclaré exactement."),
  ...filrouge([texte("Même à l’intérieur de la PCS, l’intitulé « technicien de maintenance » ne détermine pas le code à lui seul. L’Insee précise que ces intitulés sont fréquemment de niveau ouvrier qualifié malgré le mot « technicien » : la profession est 47D3 si l’emploi est classé technicien dans la convention collective, et **62E5 · Ouvriers qualifiés de maintenance et d’entretien des équipements industriels** (groupe 6) s’il est classé ouvrier qualifié.^[3] Cette information de statut n’est pas dans l’intitulé : elle est dans la déclaration de l’employeur ou la réponse de la personne.", { after: 0 })]),
  image("n3_statut_deux_codes", 13.5), legende("Schéma 5 — Le même intitulé peut recevoir deux codes PCS 2020, selon le statut déclaré."),
  h2("Attention aux versions"),
  texte("Chaque nomenclature existe en plusieurs millésimes, et une table de passage relie toujours **deux versions précises** : PCS 2003 ↔ FAP 2009, ROME v3 ↔ FAP 2009, PCS 2020 ↔ FAP 2021. Réunir dans un même tableau des codes PCS 2003 (ou PCS-ESE 2017, qui en partage le niveau détaillé) et des codes PCS 2020, ou appliquer une table FAP 2009 à des données codées en PCS 2020, revient à additionner des listes différentes."),
  image("n6_versions", 14.5), legende("Schéma 6 — Les millésimes en vigueur et leurs prédécesseurs.")];

// =============================================================================================
// PAGE 6 — Pièges, glossaire, mémo
// =============================================================================================
const page6 = [
  h1("5. Ce qu’il ne faut pas confondre"),
  grille([3300, 6338], ["L’idée fausse", "La correction"], [
    ["PCS et FAP désignent deux niveaux de précision d’une même classification.", "Ce sont deux nomenclatures construites par deux institutions, selon des objectifs différents : la position socioprofessionnelle pour la PCS, les familles de métiers pour la FAP."],
    ["PCS niveau 1 et PCS fine sont deux nomenclatures indépendantes.", "Ce sont deux niveaux de regroupement d’une même nomenclature emboîtée, dont il faut préciser la version (PCS 2020 ou PCS-ESE 2017)."],
    ["Professions intermédiaires = techniciens.", "Les techniciens (catégorie 47) ne représentent qu’une des sept catégories du groupe 4."],
    ["Une PCS correspond toujours à une seule FAP.", "Cela dépend des versions et du détail disponible. Avec la PCS 2020 et la FAP 2021, une profession peut se répartir entre plusieurs familles ; le libellé de profession peut être nécessaire."],
    ["La FAP est un indicateur de tension.", "La FAP est une nomenclature de métiers. Un indicateur de tension peut être calculé ou diffusé selon cette nomenclature."],
    ["Le ROME est une subdivision de la PCS.", "Le ROME est un répertoire de France Travail, avec une finalité (placement, offres et demandes) et une organisation propres. Seule une table de passage le relie aux FAP."],
    ["PCS-ESE 2017 et PCS 2020, c’est pareil.", "Deux nomenclatures distinctes : employeurs d’un côté, ménages de l’autre. Les niveaux 1 et 2 se rapprochent ; le niveau détaillé diffère (477b contre 47D3 pour notre technicienne)."]], { size: 17 }),
  vide(100),
  h1("6. Mini-glossaire"),
  grille([2300, 7338], ["Terme", "En une phrase"], [
    ["PCS", "Nomenclature de l’Insee qui classe les personnes selon leur profession et leur position socioprofessionnelle (statut, qualification, place dans l’organisation du travail)."],
    ["Groupe socioprofessionnel", "Niveau 1 de la PCS, premier chiffre du code : six groupes, dont cadres (3), professions intermédiaires (4), employés (5), ouvriers (6)."],
    ["Catégorie socioprofessionnelle", "Niveau 2 de la PCS, deux premiers chiffres du code : 29 catégories, par exemple 47 · Techniciens."],
    ["PCS détaillée (ou fine)", "Niveau le plus fin de la nomenclature : profession à 4 caractères en PCS 2020 (311 postes), rubrique en PCS-ESE 2017 (429 postes). Toujours préciser la version."],
    ["PCS-ESE", "Version de la PCS utilisée par les employeurs pour déclarer leurs salariés ; version en vigueur : PCS-ESE 2017."],
    ["CS1 du projet", "Regroupement analytique construit dans le projet : premier caractère du code PCS, ramené à quatre grandes catégories de salariés (3, 4, 5, 6)."],
    ["FAP", "Familles professionnelles de la Dares : regroupements de professions aux compétences et activités proches, pour analyser les métiers et le marché du travail. Version en vigueur : FAP 2021."],
    ["ROME", "Répertoire opérationnel des métiers et des emplois de France Travail : fiches métiers servant à décrire et rapprocher offres et demandes d’emploi. Version en vigueur : ROME 4.0."],
    ["Table de passage", "Fichier officiel qui indique, code par code, vers quelle rubrique d’une autre nomenclature (ou d’une autre version) va chaque code. Vaut pour un couple de versions précis."],
    ["Nomenclature emboîtée", "Nomenclature dont chaque niveau fin est contenu dans un seul niveau plus agrégé, de sorte que le code détaillé contient les codes des niveaux supérieurs."]], { premiereGras: true, size: 17 }),
  vide(100),
  h1("Mémo : trois questions, trois réponses"),
  image("n7_memo", 15), legende("Schéma 7 — Quelle nomenclature pour quelle question.")];

// =============================================================================================
// Sources
// =============================================================================================
const sources = [
  h1("Sources et références"),
  texte("Les codes, libellés et effectifs de niveaux cités dans cette note proviennent des référentiels officiels ci-dessous ; les appels de note ^[n] du texte y renvoient. Les pages Insee des rubriques citées sont celles des métadonnées en ligne.", { size: 18, color: TEXTE }),
  ...[["1", "Insee — Définition : nomenclature des professions et catégories socioprofessionnelles", "https://www.insee.fr/fr/metadonnees/definition/c1493"],
      ["2", "Insee — La PCS 2020 : présentation ; niveaux et fichiers à télécharger", "https://www.insee.fr/fr/information/6205305", "https://www.insee.fr/fr/information/6051913"],
      ["3", "Insee — PCS 2020, profession 47D3 ; profession 62E5 (règle de classement selon le statut)", "https://www.insee.fr/fr/metadonnees/pcs2020/profession/47D3", "https://www.insee.fr/fr/metadonnees/pcs2020/profession/62E5"],
      ["4", "Insee — PCS 2020, groupe 4 · Professions intermédiaires ; catégorie 47 · Techniciens", "https://www.insee.fr/fr/metadonnees/pcs2020/groupeSocioprofessionnel/4", "https://www.insee.fr/fr/metadonnees/pcs2020/categorieSocioprofessionnelle/47"],
      ["5", "Insee — Nomenclatures PCS-ESE (présentation) ; PCS-ESE 2017 (niveaux et fichiers)", "https://www.insee.fr/fr/information/2497958", "https://www.insee.fr/fr/information/2912545"],
      ["6", "Insee — PCS-ESE 2017, rubrique 477b", "https://www.insee.fr/fr/metadonnees/pcsese2017/rubriqueRegroupee/477b"],
      ["7", "Dares — La nomenclature des familles professionnelles 2021 (FAP 2021), tables de passage PCS 2020 et ROME ; Les tensions sur le marché du travail en 2023 (passage à la FAP 2021, 228 familles détaillées)", "https://dares.travail-emploi.gouv.fr/donnees/la-nomenclature-des-familles-professionnelles-2021", "https://dares.travail-emploi.gouv.fr/sites/default/files/bf6c1917e9c50fc064bc43f0c1cd725d/Dares%20R%C3%A9sultats_tensions_march%C3%A9dutravail_2023.pdf"],
      ["8", "Dares — La nomenclature des familles professionnelles, version 2009 : introduction et table de correspondance FAP / PCS / ROME", "https://travail-emploi.gouv.fr/sites/travail-emploi/files/files-spip/pdf/1_b_-_fap-2009_introduction_et_table_de_correspondance_pour_publication.pdf"],
      ["9", "France Travail — Le ROME (open data, ROME 4.0) ; fiche métier I1304 (MétierScope)", "https://www.francetravail.org/opendata/repertoire-operationnel-des-meti.html?type=article", "https://candidat.francetravail.fr/metierscope/fiche-metier/I1304/technicien-technicienne-de-maintenance-industrielle"]]
    .map(([n, t, ...urls]) => new Paragraph({ spacing: { before: 0, after: 40, line: 230 }, indent: { left: 360, hanging: 360 },
      children: [run(n + "\t", { size: 16, bold: true, color: NAVY }), run(t + " — ", { size: 16 }), ...urls.flatMap((u, i) => [i ? run(" · ", { size: 17 }) : null, lien(u.replace(/^https?:\/\//, ""), u)].filter(Boolean))] })),
  vide(40),
  texte("Vérification : chaque code et libellé d’exemple a été relevé sur les pages officielles indiquées (rubriques 47D3, 62E5, 477b, catégorie 47, groupe 4 ; documents Dares et France Travail). L’affectation de 47D3 dans la FAP 2021 se lit dans la table de passage PCS 2020 → FAP 2021 de la Dares. Les éléments propres au projet proviennent de sa configuration et de sa documentation, sans consultation de données.", { size: 16, color: TEXTE, after: 0 })];

const doc = new Document({
  creator: "Projet départs Base", title: "PCS, CS, FAP, ROME : quatre sigles, plusieurs façons de regarder un métier",
  styles: { default: { document: { run: { font: FONT, size: 20, color: ENCRE } } },
    paragraphStyles: [{ id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true, run: { font: FONT, size: 29, bold: true, color: NAVY }, paragraph: { outlineLevel: 0 } },
                      { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true, run: { font: FONT, size: 23, bold: true, color: NAVY }, paragraph: { outlineLevel: 1 } }] },
  numbering: { config: [
    { reference: "puces", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 360, hanging: 240 } }, run: { color: NAVY } } }] },
    { reference: "num1", levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 420, hanging: 300 } }, run: { bold: true, color: NAVY } } }] }] },
  sections: [{ properties: { page: PAGE }, headers: { default: entete }, footers: { default: pied },
    children: [...page1, ...page2, ...page3, ...page4, ...page5, ...page6, ...sources] }] });
Packer.toBuffer(doc).then(b => { fs.mkdirSync(path.dirname(SORTIE), { recursive: true }); fs.writeFileSync(SORTIE, b); console.log("écrit :", SORTIE); });
