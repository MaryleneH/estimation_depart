# Note « PCS, CS, FAP, ROME : quatre sigles, plusieurs façons de regarder un métier »

Livrable : `docs/note_comprendre_nomenclatures_PCS_FAP_ROME.docx` (A4 portrait, 7 pages, sources comprises). Schémas dans `docs/assets/nomenclatures/` (SVG sources + PNG rendus).
Sujet exclusif : comprendre les nomenclatures professionnelles (PCS 2020, PCS-ESE 2017, niveaux et CS, FAP 2009 / 2021, ROME, tables de passage). Aucune donnée du projet ; les codes et libellés du fil rouge (technicienne de maintenance : PCS 2020 47D3 / 62E5, PCS-ESE 2017 477b, FAP 2009 G1Z, ROME I1304) proviennent des référentiels officiels cités dans la note.

## Régénérer

```bash
node utils/note_nomenclatures/gen_schemas.js docs/assets/nomenclatures                 # SVG « façon dessin à la main »
node utils/note_pedagogique/render_png.js docs/assets/nomenclatures/*.svg              # PNG nets (x3) via Chromium + playwright-core
node utils/note_nomenclatures/gen_note.js docs/assets/nomenclatures docs/note_comprendre_nomenclatures_PCS_FAP_ROME.docx
```

Prérequis et contrôle visuel : identiques à `utils/note_pedagogique/README.md` (modules `docx` et `playwright-core`, Chromium, police manuscrite Happy Monkey pour les images ; corps en Calibri ; PDF de contrôle non versionné).

En interne, pour que le texte dise « BITD » au lieu de « Base » : `PERIMETRE=BITD node …` (variable d'environnement lue par le générateur ; « Base Tous salariés » n'est jamais touché), puis re-rendre les PNG s'il y a des schémas.

## Ce qui a été vérifié dans le dépôt (sans ouvrir de données)

- Source : colonne `pcs` de la BTS, code à 4 caractères conservé en texte, supposé PCS-ESE ; aucun contrôle du millésime dans le code → « version source à confirmer » dans la note.
- `cs1` : regroupement analytique du projet (`PCS_VERS_CS1`, premier caractère → 3 Cadres, 4 Professions intermédiaires, 5 Employés, 6 Ouvriers ; tout autre premier caractère hors champ, compté à part).
- `PCS-ESE_2017_Liste.xlsx` : libellés d’affichage seulement, fichier livré fictif (`data/LISEZMOI.txt`).
- FAP / ROME : uniquement dans la couche « tensions » (gabarit `data/templates/tensions_fap_territoires_template.csv`) ; toute correspondance FAP → grande CS est refusée par le code.
