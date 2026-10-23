# Renommer « Base nationale » et « Base » en « BITD » dans un dépôt de travail

Script : `utils/utils_renommer_base_bitd.R` (R de base seulement, Git facultatif). Il
sert sur un **autre** dépôt (le dépôt interne) : copiez le script, ou lancez-le avec
`--racine=<chemin du dépôt>`. Tous les « Base » ne désignent pas la BITD (« Base Tous
salariés » est la source Insee, « Base commune » a le sens courant, `_Base` est un nom
de fichier) : le script **inventorie** chaque occurrence, la **classe** d'après son
contexte, vous **décidez** dans un tableau, puis il **applique** exactement vos décisions.

## Mode d'emploi en cinq commandes

```bash
git checkout -b renommage-bitd                      # 1. une branche dédiée, tout commité
Rscript utils/utils_renommer_base_bitd.R            # 2. inventaire -> renommage_bitd.csv ; rien n'est modifié
#  ouvrir renommage_bitd.csv (Excel ou éditeur), remplir la colonne `decision`
#  des lignes « à trancher » (remplacer / conserver), corriger les autres si besoin
Rscript utils/utils_renommer_base_bitd.R --appliquer --decisions=renommage_bitd.csv   # 3. application + vérification
git diff                                            # 4. relire chaque ligne changée
#  5. suite de tests ; régénérer les images (SVG / PNG) si des générateurs ont changé
```

## Ce que fait le classement

| Classe | Règle | Décision par défaut |
|---|---|---|
| `identifiant` | collé à une lettre, un chiffre ou `_` (`_Base`, `Base64`) | jamais modifié |
| `conserver` | suivi de « Tous salariés », « de données », « de repli », « commune », « seniors », « du poids », « des zones »… | `conserver` |
| `remplacer` | précédé de périmètre, la, de, emploi, territoires, départements, typologie, présence, champ, SIREN, poids, entreprises, cartes, départs, salariés… ; ou suivi de national(e), français(e), du département… ; ou suivi d'une ponctuation ou d'une fin de ligne | `remplacer` |
| `à trancher` | tout le reste (début de ligne, « Base : … ») | vide : à remplir |

Les listes de contextes et la table `REMPLACEMENTS` (« Base nationale » → « BITD »,
« Base » → « BITD ») sont en tête du script, à compléter selon le vocabulaire du dépôt.
Motifs en casse exacte et mot entier : `base`, `BASE`, `base_poids` ne sont pas touchés.

## Garanties

- Le script, son test, ce mode d'emploi et le rapport (noms contenant `renommer_base_bitd`,
  `renommage_bitd` ou `renommer-bitd`) ne sont jamais lus : ils contiennent « Base » par nature.
- Fichiers texte versionnés seulement (`git ls-files` ; `--sans-git` = parcours du
  dossier), hors `sorties/`, `data/`, `node_modules/` (`--exclure=`), extensions
  `--extensions=` (par défaut R, Rmd, qmd, md, txt, js, json, html, svg, css, yml, yaml).
- Aucun fichier binaire ni non UTF-8 (ignorés et listés) ; BOM et fins de ligne (CRLF)
  conservés ; écriture en octets UTF-8, quelle que soit la locale de R.
- Chaque occurrence est remplacée à sa position (jamais un remplacement global).
- Refus d'appliquer si : une ligne « à trancher » est sans décision ; le dépôt a changé
  depuis l'inventaire (occurrence déplacée) ; des fichiers visés ont des modifications
  non commitées. `--force` passe outre (les « à trancher » sans décision sont conservés).
- Après application : nouvel inventaire (ne doivent rester que les conservés et les
  identifiants), signalement des « BITD BITD » et des variantes de casse.

## Limites

- Le classement est une heuristique : relisez les lignes `remplacer` du rapport avant
  d'appliquer, surtout dans un dépôt dont le vocabulaire diffère.
- Les noms de fichiers et les identifiants ne sont jamais renommés : si un livrable
  s'appelle `..._Base.docx`, renommez-le à la main et adaptez les références.
- Le CSV réenregistré par Excel en ANSI est accepté ; les colonnes `fichier`, `ligne`,
  `colonne`, `motif`, `decision` doivent rester intactes.
