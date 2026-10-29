# Notes courtes pour la décideuse

Deux notes de lecture de deux à trois pages, sobres (texte noir sur blanc, marges larges,
aucun encadré coloré), destinées à une lectrice pressée. Les notes longues restent les
annexes de référence.

| Livrable | Générateur | Contenu |
|---|---|---|
| `docs/note_courte_exposition_tension.docx` | `gen_note_exposition.js` | ce qu'est l'estimation des départs (un plancher du besoin de remplacement, pas des départs en retraite ; règles générales, aucune trajectoire individuelle), puis l'exposition au recrutement difficile : mesure, levier, lecture, limites, exemple fictif |
| `docs/note_courte_pcs_fap.docx` | `gen_note_pcs_fap.js` | PCS et FAP : deux questions, deux grilles ; comment le projet s'en sert ; cinq repères avant de lire un chiffre ; quatre phrases à éviter |

Tous les chiffres sont fictifs ; aucune donnée du projet n'y figure.

## Régénérer

```bash
node utils/notes_courtes/gen_note_exposition.js      # -> docs/note_courte_exposition_tension.docx
node utils/notes_courtes/gen_note_pcs_fap.js         # -> docs/note_courte_pcs_fap.docx
```

Prérequis : module `docx` (même installation que `utils/note_pedagogique/README.md`).
Contrôle visuel : conversion PDF avec LibreOffice, non versionnée.

À mettre à jour si la méthode change : la section « Comment c'est mesuré » (seuil de
tension, classes, levier) suit `R/00_config_fonctions/00_config.R` (section `TENSION_*`)
et la note méthodologique longue ; la section sur l'estimation des départs suit la
spécification « Périmètre et champ » et le script 04.
