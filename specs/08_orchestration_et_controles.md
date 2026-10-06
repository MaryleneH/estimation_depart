# Spec 08 — Orchestration, contrôles et robustesse

## Intention métier

La reproductibilité dépend autant de l'ordre d'exécution que des formules. Le
point d'entrée `main.R` rend cet ordre explicite et stable ; chaque étape
facultative est pilotée par un paramètre de configuration et s'écarte
proprement, jamais silencieusement.

## Ordre de référence (`main.R`)

```
00_config → 00g (arrondi) → 00c (géo) → 00d (fiches) → 00e (départs PCS/CS1) → 00f (cartes)
→ 01 BTS → 01b PCS → cs1 → 01c stock tous âges
→ 02 DREES → 02b mortalité Insee → 02c invalidité EACR
→ 03 paramètres par CSP
→ 04 projection (bts_projete)
→ 05 entreprises → 06, 06b, 06d, 06e graphiques → 07 tableau gt (si gt installé)
→ 08 seniors par territoire → 08b territoire × CS → 08c PCS → 08d CS1
→ 08e cartes (si GENERER_CARTES_DEPARTS) → 09 fiches (si GENERER_FICHES)
→ 99b BOM UTF-8 des CSV (si NORMALISER_CSV_EXCEL)
```

Le préfixe d'un répertoire de `R/` est le numéro du premier script qu'il
contient : l'ordre alphabétique des répertoires est l'ordre d'exécution. Les
noms de scripts existants ne sont jamais renommés.

```gherkin
Fonctionnalité: Exécuter la chaîne de calcul de manière reproductible

  Scénario: [SPEC-ORCH-001] Utiliser main.R comme point d'entrée
    Étant donné une session R ouverte à la racine du projet
    Quand main.R est sourcé
    Alors la chaîne complète doit être exécutée dans l'ordre de référence
    Et se terminer par le message « Chaîne exécutée. »

  Scénario: [SPEC-ORCH-002] Charger la configuration et les fonctions avant les traitements
    Quand la chaîne démarre
    Alors 00_config.R puis les fonctions communes (00g, 00c, 00d, 00e, 00f) doivent être chargés avant toute donnée
    Et aucun script de fonctions ne doit calculer ni écrire quoi que ce soit
    Et la préparation doit précéder la modélisation, qui doit précéder les restitutions

  Scénario: [SPEC-ORCH-003] Communiquer par objets de session
    Étant donné l'exécution de main.R
    Quand un script produit un objet nécessaire au suivant (bts, stock_tous_ages, fdc, table_mortalite, inval_base, param_cs, bts_projete…)
    Alors cet objet doit être transmis dans la session R
    Et chaque script doit vérifier la présence de ses prérequis et s'arrêter avec un message nommant le script à exécuter
    Et aucune étape ne doit dépendre d'un fichier intermédiaire, sauf les cartes qui lisent volontairement les CSV de diffusion

  Scénario: [SPEC-ORCH-004] Pouvoir relancer un script seul
    Étant donné une session où les objets amont existent
    Quand un script est sourcé isolément
    Alors il doit produire ses sorties sans relancer la chaîne
    Et sans modifier les objets existants

  Scénario: [SPEC-ORCH-005] Rendre gt facultatif
    Étant donné que le package gt n'est pas installé
    Quand main.R atteint le script 07
    Alors cette étape doit être ignorée avec un message indiquant le script 07b sans gt
    Et le reste de la chaîne doit continuer

  Scénario: [SPEC-ORCH-006] Piloter chaque étape facultative par la configuration
    Étant donné les paramètres STOCK_TOUS_AGES, GENERER_DEPARTS_PCS, GENERER_DEPARTS_CS1, GENERER_CARTES_DEPARTS, CARTES_PNG, GENERER_FICHES, NORMALISER_CSV_EXCEL
    Quand l'un d'eux est faux
    Alors l'étape correspondante doit être ignorée avec un message
    Et les calculs amont et les autres sorties doivent rester identiques

  Scénario: [SPEC-ORCH-007] Ne jamais échouer sur une sortie secondaire
    Étant donné une erreur lors de la production d'un PNG ou d'un PDF
    Quand elle survient
    Alors un message doit l'expliquer
    Et le HTML doit rester la restitution de référence
    Et la chaîne doit continuer

  Scénario: [SPEC-ORCH-008] Ne dépendre d'aucune ressource réseau ni d'outil externe
    Étant donné une exécution hors ligne
    Quand la chaîne est exécutée
    Alors aucun téléchargement ne doit être tenté
    Et aucun navigateur headless, LaTeX, Quarto ou Pandoc ne doit être requis
    Et les packages gt, ggrepel, plotly, shiny, pagedown, webshot, chromote, showtext, extrafont ne doivent pas être nécessaires

  Scénario: [SPEC-ORCH-009] Normaliser les CSV pour Excel sans en changer le contenu
    Étant donné NORMALISER_CSV_EXCEL vrai
    Quand la chaîne se termine
    Alors chaque CSV de sorties/ doit commencer par un BOM UTF-8, une seule fois
    Et son contenu après le BOM doit être identique octet pour octet
    Et un fichier déjà muni d'un BOM ne doit pas être touché
    Et un fichier UTF-8 invalide doit être laissé intact avec un avertissement
    Et un second passage ne doit rien changer

  Scénario: [SPEC-ORCH-010] Relire les CSV normalisés
    Étant donné un CSV muni d'un BOM
    Quand il est relu dans R
    Alors fileEncoding "UTF-8-BOM" doit être utilisé
    Et les noms de colonnes doivent être propres, les codes en texte, les décimales françaises

  Scénario: [SPEC-ORCH-011] Conserver des contrôles de non-régression
    Étant donné des références figées (instantanés par zonage, fichiers PCS, résultats du 08 en mode ZE)
    Quand une modification de code est introduite
    Alors les sorties couvertes doivent rester identiques
    Ou toute différence attendue doit être justifiée et les références refigées dans le même changement

  Scénario: [SPEC-ORCH-012] Garder une seule source de paramètres
    Étant donné un paramètre métier
    Quand il est utilisé par un script
    Alors il doit être lu depuis 00_config.R
    Et jamais défini ailleurs ni écrit en dur
```

## Doctrine de contribution

- Une fonctionnalité par branche et par PR ; les déplacements purs de fichiers
  et les changements fonctionnels vont dans des commits séparés.
- Les sorties versionnées d'exemple (`sorties/`) sont régénérées dans un commit
  distinct du code.
- Toute PR cite la non-régression : suite `testthat` complète, instantanés,
  comparaison des sorties.

## Traçabilité

Code :

- `main.R`
- `R/README.md` — organisation et ordre d'exécution
- `R/99_controles/99_controle.R` (tableaux de contrôle, à lancer à part),
  `R/99_controles/99b_normaliser_csv_excel.R` —
  `ajouter_bom_utf8_csv()`, `normaliser_csv_excel()`
- `utils/utils_comparer_sorties.R` — comparaison de deux jeux de sorties

Tests :

- `tests/testthat/test-snapshot.R` — « instantané <zonage> : aucun écart avec
  la référence figée »
- `tests/testthat/test-non-regression-ze.R`
- `tests/testthat/test-departs-pcs.R` — « NON-RÉGRESSION : les 6 fichiers
  PCS … IDENTIQUES aux références figées »
- `tests/testthat/test-normaliser-csv-excel.R` — « A. UTF-8 sans BOM : BOM
  ajouté, contenu strictement identique … » ; « B. déjà un BOM : rien n'est
  modifié » ; « C. accents … D. codes … E. 12,5 et « ; » ; F. NA » ; « G. UTF-8
  invalide : détecté, laissé intact, warning » ; « H. idempotence et
  récursivité » ; « non-régression sur les VRAIES sorties versionnées »
- `tests/testthat/test-fiches.R` — « STOCK_TOUS_AGES = FALSE : chaîne
  inchangée … » ; « cohérence avec le 08 sur la chaîne (mode test
  département) »
- `tests/testthat/test-departs-geo-cs.R` — « 8. sourcer 08b ne modifie, ne
  supprime ni ne regroupe aucun objet existant »
- SPEC-ORCH-005 (gt facultatif), SPEC-ORCH-007 (PNG non bloquant) et
  SPEC-ORCH-008 (hors ligne) : garantis par construction (`requireNamespace`,
  `tryCatch`, fond local), sans test automatique dédié.
