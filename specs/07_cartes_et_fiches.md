# Spec 07 — Cartes, tableaux de bord et fiches territoriales

## Intention métier

Les cartes, tableaux de bord et fiches sont des restitutions publiques. Elles
sont compréhensibles par un public non statisticien, autonomes (aucune
ressource réseau), et ne révèlent jamais une valeur protégée. Elles n'ajoutent
aucune interprétation causale ni prescriptive : des chiffres et des phrases
factuelles.

## Vocabulaire public

| À l'écran | Jamais à l'écran |
|---|---|
| « Salariés de 45 ans ou plus · entreprises du périmètre BITD » | « champ », « effectif », noms de colonnes R |
| « départs estimés d'ici 2030 », « pourraient partir d'ici 2030 » | « taux de départ estimé » (hors légende et sélecteur) |
| « Résultat non diffusé — secret statistique » | un chiffre masqué, même approché |
| « Aucun salarié observé » | « 0 » pour une absence de donnée |
| « Qui est concerné ? » | « diffusabilité » |

Les libellés viennent d'une seule table (`LIBELLES_UI`) ; les phrases
dépendant de l'âge sont construites à l'exécution depuis `AGE_MIN_BTS`
(`construire_libelles_public()`), le rappel du champ depuis la configuration
réelle (`construire_libelle_champ()`).

## 1. Cartes et tableaux de bord (08e)

```gherkin
Fonctionnalité: Produire des cartes diffusables

  Scénario: [SPEC-PUB-001] Alimenter les cartes uniquement avec les tables de diffusion
    Étant donné des tables internes et des tables de diffusion
    Quand les cartes sont générées
    Alors seules sorties/departs_<dim>/diffusion/*.csv doivent être lues
    Et une table sans colonne masque doit être refusée
    Et une table de diffusion absente doit produire un message, aucun fichier du tout un arrêt

  Scénario: [SPEC-PUB-002] Représenter région et département par une carte
    Étant donné une table de diffusion régionale ou départementale
    Quand la page est produite
    Alors elle doit proposer une carte choroplèthe interactive (vue « Résultats » par catégorie et indicateur)
    Et une vue « Résultats affichables » (part des catégories diffusables par territoire)
    Et un PNG de la vue initiale
    Et la métropole en Lambert-93 avec les DROM en encarts, chacun à sa propre échelle

  Scénario: [SPEC-PUB-003] Représenter la France entière sans fausse carte
    Étant donné une valeur nationale unique par catégorie
    Quand la restitution France entière est produite
    Alors elle doit prendre la forme d'un tableau de bord (départs, part, salariés, fourchette, causes)
    Et aucune carte ni PNG ne doit être produit pour ce niveau

  Scénario: [SPEC-PUB-004] Ne pas embarquer la valeur d'une cellule secrète
    Étant donné une cellule masquée
    Quand le HTML est généré
    Alors toute mesure de cette cellule doit être effacée avant sérialisation
    Et la cellule doit être embarquée sous la seule forme {"s":"m"}
    Et sa valeur ne doit apparaître ni dans le HTML, ni dans le JavaScript, ni dans les infobulles

  Scénario: [SPEC-PUB-005] Utiliser un langage public
    Étant donné une page destinée à un public non statisticien
    Quand les libellés sont construits
    Alors aucun nom de variable R ne doit apparaître à l'écran
    Et « champ », « effectif », « diffusabilité » ne doivent pas apparaître dans les grands éléments
    Et le rappel « Qui est concerné ? » doit figurer en tête de chaque page

  Scénario: [SPEC-PUB-006] Faire dériver les libellés de la configuration
    Étant donné AGE_MIN_BTS fixé à 50 dans un environnement dédié
    Quand les pages sont générées
    Alors elles doivent afficher « Salariés de 50 ans ou plus » et jamais « 45 ans »
    Et les catégories citées doivent suivre PCS_VERS_CS1, les exclusions PCS_HORS_CHAMP, l'horizon HORIZON

  Scénario: [SPEC-PUB-007] Distinguer secret et absence de donnée
    Étant donné un territoire masqué et un territoire sans salarié observé
    Quand la carte est rendue
    Alors le masqué doit être gris (COULEURS_CARTE$secret) avec le libellé du secret
    Et l'absence doit être blanche à contour pointillé avec « Aucun salarié observé »
    Et ces deux statuts doivent être distincts dans la légende, l'infobulle et le comptage

  Scénario: [SPEC-PUB-008] Produire une page autonome
    Étant donné une carte générée
    Quand son fichier HTML est ouvert hors ligne
    Alors il doit fonctionner sans script, feuille de style ni fond chargés depuis le réseau
    Et le fond de carte doit provenir de data/cartographie/ (IGN Admin Express via france-geojson, licence ouverte)

  Scénario: [SPEC-PUB-009] Rendre les frontières lisibles
    Étant donné deux territoires voisins de même classe ou tous deux masqués
    Quand la carte est rendue
    Alors leur limite doit être visible (contour gris moyen-foncé, jamais blanc)
    Et sur la carte départementale, les limites régionales doivent être tracées par-dessus, plus foncées
    Et le territoire survolé ou ayant le focus doit être redessiné au-dessus de ses voisins
    Et tous les contours doivent venir d'une seule source (STYLE_CONTOURS_CARTE), SVG et PNG compris

  Scénario: [SPEC-PUB-010] Afficher le libellé officiel des PCS
    Étant donné la nomenclature PCS-ESE 2017 locale (FICHIER_PCS_LIBELLES)
    Quand une carte ou un tableau de bord PCS est produit
    Alors une PCS présente dans la nomenclature doit s'afficher « Libellé · PCS code », et « code — Libellé » dans la liste
    Et les codes doivent être rapprochés sans tenir compte de la casse ni des espaces
    Et une PCS absente doit s'afficher « PCS code · grande catégorie »
    Et un fichier absent doit produire un message, jamais un arrêt
    Et seules les PCS de la table doivent être embarquées

  Scénario: [SPEC-PUB-011] Ordonner la liste des PCS par importance
    Étant donné les PCS d'une carte
    Quand la liste déroulante est remplie
    Alors les PCS doivent être triées par départs estimés diffusés, sommés sur les territoires de la carte, décroissants
    Et les PCS sans aucun résultat diffusé doivent venir en fin de liste
    Et les valeurs masquées ne doivent jamais compter
    Et les grandes catégories doivent garder l'ordre des codes

  Scénario: [SPEC-PUB-012] Classer avec des bornes lisibles
    Étant donné les valeurs diffusées d'une catégorie
    Quand les classes de couleur sont calculées
    Alors les bornes doivent être arrondies à des valeurs rondes
    Et l'échelle doit être commune à tous les territoires de la catégorie affichée
    Et recalculée à chaque changement de catégorie ou d'indicateur

  Scénario: [SPEC-PUB-013] Arrêter sur un code sans géométrie
    Étant donné un code de territoire absent du fond de carte
    Quand la carte est préparée
    Alors la génération doit s'arrêter avec la liste des codes manquants
    Et aucune carte partielle ne doit être produite
    Mais un territoire "inconnu" doit être exclu de la carte et compté
```

## 2. Fiches territoriales (09)

```gherkin
Fonctionnalité: Produire des fiches territoriales diffusables

  Scénario: [SPEC-PUB-014] Produire une fiche par territoire diffusable
    Étant donné FICHES_MODE "tous"
    Quand le script 09 est exécuté
    Alors chaque territoire diffusable doit avoir une fiche HTML au nom de fichier stable (<code>_<nom>.html)
    Et un index avec recherche native, une ligne par fiche et la méthode complète
    Et un journal listant les territoires écartés avec leur motif

  Scénario: [SPEC-PUB-015] Restreindre les fiches à une sélection
    Étant donné FICHES_MODE "selection" et FICHES_SELECTION (ou, à défaut, GEO_INTERET)
    Quand le script 09 est exécuté
    Alors seules les fiches des codes demandés doivent être produites
    Et un territoire absent doit être signalé sans empêcher les autres fiches

  Scénario: [SPEC-PUB-016] Rédiger des phrases factuelles
    Étant donné une fiche
    Quand le bloc « À retenir » est rédigé
    Alors il doit tenir en deux ou trois phrases factuelles
    Et ne pas répéter le chiffre clé
    Et ne contenir aucune interprétation causale ni recommandation

  Scénario: [SPEC-PUB-017] Garantir la cohérence interne d'une fiche
    Étant donné le tableau des départs par CS d'une fiche
    Quand il est affiché
    Alors la ligne « Ensemble » doit égaler la somme des catégories affichées (plus forts restes)
    Et les 55 ans et plus doivent être une sous-population du total
    Et sans senior, la valeur doit s'afficher « n.d. »

  Scénario: [SPEC-PUB-018] Afficher les effectifs tous âges quand ils existent
    Étant donné stock_tous_ages disponible
    Quand le tableau par CS est produit
    Alors il doit montrer l'effectif actuel tous âges et la part à remplacer
    Et en leur absence, la fiche doit être produite en repli sans ces colonnes

  Scénario: [SPEC-PUB-019] Conserver les codes dans les fiches
    Étant donné les codes « 01 », « 2A », « 2B », « 33 »
    Quand les fiches sont produites
    Alors ils doivent rester intacts dans l'en-tête et le nom de fichier

  Scénario: [SPEC-PUB-020] Proposer une annexe optionnelle
    Étant donné FICHES_ANNEXE
    Quand il est faux (défaut), aucune seconde page ne doit être produite
    Et quand il est vrai, une seconde page (structure par âge) doit être ajoutée
```

## Traçabilité

Code :

- `R/00_config_fonctions/00f_fonctions_cartes.R` — `charger_fond_carte()`,
  `projeter_lambert93()`, `projeter_fond()`, `preparer_carte_departs()`,
  `controler_carte_departs()`, `donnees_json_carte()`, `css_cartes()`,
  `js_cartes()`, `html_entete_champ()`, `html_selecteur_categorie()`,
  `html_carte_territoriale()`, `html_dashboard_national()`,
  `png_carte_departs()`, `charger_libelles_pcs()`, `construire_libelle_champ()`,
  `construire_libelles_public()`, `LIBELLES_UI`, `COULEURS_CARTE`,
  `STYLE_CONTOURS_CARTE`
- `R/08_territoires/08e_cartes_departs.R`
- `R/00_config_fonctions/00d_fonctions_fiches.R` — `generer_fiches()`,
  `generer_html_index()`, `selectionner_territoires()`,
  `calculer_indicateurs_territoire()`
- `R/08_territoires/09_fiches_territoriales.R`
- `data/cartographie/` (fond), `data/PCS-ESE_2017_Liste.xlsx` (libellés),
  `utils/utils_preparer_fond_carte.R` (construction du fond, hors chaîne)
- `R/00_config_fonctions/00_config.R` — `GENERER_CARTES_DEPARTS`, `CARTES_PNG`,
  `FICHIER_PCS_LIBELLES`, `PCS_LIBELLES_COLS`, `GENERER_FICHES`, `FICHES_*`

Tests :

- `tests/testthat/test-cartes-departs.R` — « fond local : 101 départements et
  18 régions, codes texte, DROM et Corse présents, aucune connexion » ; « mise
  en page : métropole dominante, Corse à droite, DROM en encarts » ;
  « FONDAMENTAL — PCS partiellement diffusables : couverture A = 50 %, B = 0 %
  (gris), C = 100 % » ; « SECRET : aucune valeur masquée dans la structure, le
  JSON, le HTML ni les infobulles ; table interne refusée » ; « jointure
  géographique : … code sans géométrie = ARRÊT avec la liste, « inconnu » hors
  carte et compté » ; « cs1 et France entière : … valeur nationale affichée » ;
  « CHAMP : source unique construite depuis la configuration … libellés UI sans
  nom de colonne » ; « 08e sur les sorties réelles du dépôt : 6 cartes HTML
  (+ PNG), comptages, fichier absent = message, aucun fichier = arrêt » ;
  « CONTOURS : source unique STYLE_CONTOURS_CARTE, trait gris (jamais blanc),
  limites régionales sur la carte départementale seulement, survol au-dessus
  des voisins, mobile » ; « LIBELLÉS PCS : nomenclature xlsx lue (codes
  normalisés), libellé affiché quand il existe, repli … » ; « ORDRE des PCS :
  départs estimés diffusés décroissants, sommés sur les territoires ; masqués
  exclus ; sans résultat en fin ; cs1 inchangé »
- `tests/testthat/test-fiches.R` — « mode « tous » : une fiche par territoire
  diffusable, index, journal » ; « mode « selection » : seulement les
  territoires demandés, noms de fichiers stables » ; « codes département : 01,
  2A, 2B, 33 intacts dans l'en-tête et le nom de fichier » ; « les fiches
  ignorent le schéma source : seul le contrat geo_* est utilisé » ; « cohérence
  interne : total = somme des CS ; 55+ = sous-population » ; « résilience :
  territoire absent signalé, autres fiches produites ; sans senior -> n.d. » ;
  « règles textuelles : position, « À retenir » en 2-3 phrases factuelles, sans
  répéter le chiffre clé » ; « tableau des départs par CS : effectifs actuels
  tous âges (01c), part à remplacer, ligne Ensemble ; repli sans stock » ;
  « annexe optionnelle : absente par défaut, seconde page sur demande » ;
  « index : recherche native, une ligne par fiche, méthode complète, champ
  paramétré »
- SPEC-PUB-012 (bornes rondes) : logique JavaScript (`classesRondes`), vérifiée
  visuellement ; pas de test automatique.
