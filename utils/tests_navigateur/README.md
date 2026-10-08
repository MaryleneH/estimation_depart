# Tests navigateur de la carte interactive de la typologie (hors suite R)

`test_carte_typologie.js` ouvre `carte_typologie.html` dans Chromium (Playwright)
et vérifie, sur un écran de bureau (1368 px) et un écran mobile (390 px),
le scénario SPEC-TYPO-065 :

1. survol de douze départements et du département au texte le plus long :
   hauteur défilable du document, largeur, position de la carte et hauteur du
   panneau « situation » inchangées ; infobulle toujours dans l'écran ;
2. défilement à la molette, curseur sur la carte : positions de défilement
   monotones (aucun saut, aucun retour), hauteur défilable constante,
   infobulle masquée pendant le défilement ;
3. épinglage par clic, défilement, survol d'un autre département : le
   département épinglé reste affiché ; second clic = libération ;
4. filtrage par la légende (estompage des autres profils) et retour ;
5. légende sans entrée « absent » lorsque le compte est nul.

```bash
cd utils/tests_navigateur
npm install playwright-core            # une fois
CHROME=/chemin/vers/chrome node test_carte_typologie.js /chemin/absolu/sorties/typologie_territoriale/cartes/carte_typologie.html
```

`CHROME` désigne un exécutable Chrome ou Chromium ; sans variable, Playwright
utilise son navigateur par défaut s'il est installé. Le script écrit une capture
`apercu_apres.png` (ou `apercu_avant.png` si le chemin contient « avant ») et
termine avec un code de sortie non nul en cas d'échec.
