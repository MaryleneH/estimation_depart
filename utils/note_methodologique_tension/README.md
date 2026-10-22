# Note méthodologique « Exposition au recrutement difficile »

Livrable : `docs/note_methodologique_exposition_tension.docx` (A4 portrait). Schémas dans `docs/assets/methodologie_tension/` (SVG sources + PNG rendus).
Objet : expliquer ce que calcule le script 08i (specs/10 §13) et comment le lire : sources, passage des PCS aux FAP, indice d'exposition, couverture, classes, robustesse, **choix du levier**, situations de métier, croisement avec la typologie, secret statistique, limites. Tous les chiffres sont fictifs (un exemple est suivi de bout en bout) ; aucune donnée du projet n'y figure. Les paramètres cités sont ceux de `R/00_config_fonctions/00_config.R` (section `TENSION_*`).

## Régénérer

```bash
node utils/note_methodologique_tension/gen_schemas.js docs/assets/methodologie_tension      # SVG
node utils/note_pedagogique/render_png.js docs/assets/methodologie_tension/*.svg             # PNG nets (x3) via Chromium + playwright-core
node utils/note_methodologique_tension/gen_note.js docs/assets/methodologie_tension docs/note_methodologique_exposition_tension.docx
```

Prérequis et contrôle visuel : identiques à `utils/note_pedagogique/README.md` (modules `docx` et `playwright-core`, Chromium, police manuscrite Happy Monkey pour les images ; corps en Calibri ; PDF de contrôle non versionné).

À mettre à jour si la méthode change : les paramètres (`TENSION_*`), la règle du levier (`tension_metiers()` et `exposition_departements()` dans `00k_fonctions_tension.R`), les règles de secret (`appliquer_secret_exposition()`).
