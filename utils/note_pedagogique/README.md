# Note pédagogique « PCS, grandes catégories, FAP : comment lire les cartes d’emploi de la Base ? »

Livrable : `docs/note_pedagogique_PCS_FAP_cartes_Base.docx` (A4 portrait, 10 pages). Schémas conservés dans `docs/assets/note_pedagogique/` (SVG sources + PNG rendus).
Tous les chiffres de la note sont fictifs et signalés comme tels ; aucune donnée du projet n’y figure. Les paramètres cités (seuils de secret, seuil de dominance, âge minimal, nombre de PCS affichées) sont ceux de `R/00_config_fonctions/00_config.R`.

## Régénérer

```bash
node utils/note_pedagogique/gen_schemas.js docs/assets/note_pedagogique            # SVG « façon dessin à la main »
node utils/note_pedagogique/render_png.js docs/assets/note_pedagogique/*.svg        # PNG nets (x3) via Chromium + playwright-core
node utils/note_pedagogique/gen_note.js docs/assets/note_pedagogique docs/note_pedagogique_PCS_FAP_cartes_Base.docx
```

Prérequis : Node.js avec les modules `docx` et `playwright-core` (variable `NODE_PATH` si installés ailleurs), un Chromium (chemin dans `render_png.js`), et une police manuscrite pour les schémas (`Happy Monkey`, repli `Comic Neue` puis `Humor Sans` ; sans elle, le navigateur substitue une police standard). Le corps du Word est en Calibri ; les polices manuscrites ne sont utilisées que dans les images, pour que le document s’affiche à l’identique sur tout poste.

Contrôle visuel : convertir le `.docx` en PDF (LibreOffice `--headless --convert-to pdf`) puis `pdftoppm -png -r 100` ; vérifier tableaux, encadrés (lignes insécables), schémas, numérotation. Le PDF n’est pas versionné.
