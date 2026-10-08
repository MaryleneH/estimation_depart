# Fiche pédagogique « typologie territoriale BITD » (Word, 2 pages A4 paysage)

Hors chaîne : rien ici n'est exécuté par `main.R`. Le document livré est
`sorties/typologie_territoriale/fiche_pedagogique_typologie_BITD.docx`
(et un aperçu PDF rendu par LibreOffice à côté). Tous les chiffres de la fiche
sont **fictifs** ; les règles reprennent `specs/10_typologie_territoriale.md`
§4 (dix règles ordonnées) et les titres de `PROFILS_TYPOLOGIE` (00h). Si une
règle ou un titre change dans le dépôt, mettre à jour `gen_fiche.js`.

## Régénérer

```bash
cd utils/fiche_pedagogique_typologie
npm install docx            # une fois (package.json)
node gen_svg.js             # -> images/*.svg (mini-diagrammes)
# SVG -> PNG (rendu net x3) avec n'importe quel Chrome/Chromium :
for k in poids volume intensite structure concentration ab; do
  W=$(grep -o 'data-w="[0-9]*"' images/$k.svg | tr -dc 0-9); H=$(grep -o 'data-h="[0-9]*"' images/$k.svg | tr -dc 0-9)
  chromium --headless=new --hide-scrollbars --window-size=$((W*3)),$((H*3)) --screenshot=images/$k.png "file://$PWD/images/$k.svg"
done
node gen_fiche.js images ../../sorties/typologie_territoriale/fiche_pedagogique_typologie_BITD.docx
```

Les PNG sont versionnés : l'étape Chromium n'est nécessaire que si un
diagramme change. Aperçu PDF (facultatif) : `soffice --headless --convert-to pdf <docx>`.

## Contenu

- Page 1 « Comment lit-on un territoire ? » : les cinq indicateurs (poids,
  volume, intensité, structure, concentration) avec un mini-diagramme chacun ;
  comparaison A / B volume contre intensité ; les dix règles ordonnées ; un
  AVANT / APRÈS montrant qu'un seul indicateur change le profil.
- Page 2 « Les sept situations territoriales » : une carte par profil (sens,
  critère décisif, exemple fictif, message de lecture) et le bloc « À garder en
  tête » (maille département, tensions séparées, départs ≠ pénurie, seuils
  provisoires, secret statistique).
