# Spec 02 — Préparation des données

## Intention métier

Avant la projection, la BTS et les sources externes sont préparées selon un
contrat commun : variables nommées, géographie en codes texte, catégories
socioprofessionnelles dérivées de la PCS. Les sources externes absentes ne
bloquent pas la chaîne mais sont signalées fortement.

## Contrat géographique

Toute table territoriale porte trois colonnes : `geo_code` (texte, jamais
numérique : « 01 », « 09 », « 2A », « 2B », « 971 »), `geo_nom`, `geo_type`
(clé de `GEO_ZONAGES`). Le zonage d'analyse est `GEO_ANALYSE`, le zonage porté
par le fichier source `GEO_SOURCE` ; un passage (`GEO_PASSAGES`) convertit une
maille plus fine vers la maille d'analyse. Les référentiels sont locaux
(`data/ref_*.csv`, format `code;nom`), aucune ressource n'est téléchargée.

```gherkin
Fonctionnalité: Préparer des données cohérentes pour la projection

  Scénario: [SPEC-PREP-001] Charger la source BTS
    Étant donné une source BTS configurée (SOURCE_BTS = "test" ou "parquet")
    Quand le script 01 est exécuté
    Alors un objet bts doit être produit avec les colonnes id, siren, sexe, age_2024, pcs, generation
    Et il doit respecter le contrat géographique geo_code / geo_nom / geo_type
    Et il doit contenir uniquement les salariés du périmètre SIREN âgés d'au moins AGE_MIN_BTS
    Et en mode parquet, seules les colonnes déclarées dans COL_BTS doivent être lues, le filtre d'âge étant poussé au disque

  Scénario: [SPEC-PREP-002] Conserver les codes géographiques en texte
    Étant donné une colonne géographique lue comme nombre (1, 9, 33)
    Quand GEO_CODE_LARGEUR est renseigné
    Alors les codes doivent être complétés par des zéros à gauche (« 01 », « 09 », « 33 »)
    Et « 2A » et « 2B » doivent rester intacts
    Et aucun code ne doit jamais être converti en numérique dans la chaîne

  Scénario: [SPEC-PREP-003] Enrichir les libellés depuis un référentiel local
    Étant donné un référentiel code;nom, éventuellement enregistré en Windows-1252
    Quand les libellés sont complétés
    Alors les accents doivent être corrects
    Et un code absent du référentiel doit être conservé et signalé, jamais supprimé

  Scénario: [SPEC-PREP-004] Accepter plusieurs codages du sexe
    Étant donné une colonne sexe codée 1/2, H/F, M/F ou Hommes/Femmes
    Quand la BTS est préparée
    Alors le codage doit être normalisé
    Et une valeur inconnue doit provoquer un arrêt explicite, jamais des probabilités NA en 04

  Scénario: [SPEC-PREP-005] Agréger les PCS en grandes catégories
    Étant donné une PCS dont le premier caractère est 3, 4, 5 ou 6
    Quand l'agrégation PCS vers cs1 est activée (AGGREGER_PCS)
    Alors la grande catégorie correspondante doit être attribuée depuis PCS_VERS_CS1
    Et le code PCS fin doit être conservé dans la colonne pcs jusqu'aux résultats

  Scénario: [SPEC-PREP-006] Tracer les PCS hors champ
    Étant donné une PCS dont le premier caractère figure dans PCS_HORS_CHAMP
    Quand la préparation est exécutée
    Alors les salariés concernés ne doivent pas alimenter la projection
    Et leur nombre doit être affiché et tracé
    Et la table sorties/departs_pcs/pcs_hors_champ.csv doit en donner le détail, avec un total égal aux exclusions du 01b

  Scénario: [SPEC-PREP-007] Construire le stock tous âges
    Étant donné une extraction permettant d'observer les salariés sous AGE_MIN_BTS
    Quand STOCK_TOUS_AGES est vrai
    Alors les effectifs actuels doivent être agrégés par territoire et grande CS dans stock_tous_ages
    Et chaque cellule doit avoir un effectif tous âges au moins égal à l'effectif du champ, sinon arrêt
    Et cet objet doit servir de dénominateur à la part à remplacer (08b) et aux fiches (09)

  Scénario: [SPEC-PREP-008] Ne pas bloquer si le stock tous âges est indisponible
    Étant donné une extraction ne contenant aucun salarié de moins de AGE_MIN_BTS, ou STOCK_TOUS_AGES faux
    Quand le script 01c est exécuté
    Alors stock_tous_ages doit valoir NULL
    Et un avertissement explicite doit être émis dans le premier cas
    Et la projection et les restitutions doivent rester exécutables
    Et les indicateurs qui en dépendent (part à remplacer, tableau tous âges des fiches) doivent être absents ou en repli, jamais faux

  Scénario: [SPEC-PREP-009] Importer les sources externes du modèle
    Étant donné les fichiers DREES (départs en retraite par CSP), Insee (quotients de mortalité) et EACR (invalidité)
    Quand les scripts 02, 02b et 02c sont exécutés
    Alors les objets fdc, fdc_salaries, table_mortalite et inval_base doivent être produits
    Et les âges utiles doivent couvrir au minimum le champ étudié

  Scénario: [SPEC-PREP-010] Utiliser une valeur de secours documentée
    Étant donné que le classeur Insee de mortalité ou le fichier EACR d'invalidité est absent
    Quand le script correspondant est exécuté
    Alors la chaîne doit utiliser le repli prévu (Q_DECES_ANNUEL plats ; T_INVALIDITE_BASE)
    Et un message fort doit signaler que la source réelle n'a pas été utilisée
    Et la version finale ne doit pas être produite avec un repli

  Scénario: [SPEC-PREP-011] Restreindre le périmètre d'analyse sans dupliquer le paramétrage
    Étant donné GEO_INTERET renseigné (liste de codes, éventuellement nommée)
    Quand les restitutions territoriales sont produites
    Alors seuls ces territoires doivent être analysés et les hors périmètre comptés
    Et un code absent de la base doit être signalé
    Et zéro correspondance doit provoquer un arrêt
    Mais le niveau France entière ne doit jamais être réduit par GEO_INTERET

  Scénario: [SPEC-PREP-012] Refuser une configuration géographique obsolète
    Étant donné l'ancienne configuration par zone d'emploi
    Quand la configuration est chargée
    Alors une garde de migration doit refuser cette configuration avec un message explicite

  Scénario: [SPEC-PREP-013] Rattacher au territoire « inconnu » un département sans région dans la table de passage
    Étant donné une BTS au zonage département contenant un code absent de la table de passage département → région (par exemple « 99 », hors France ou non localisé)
    Et RECODER_GEO_HORS_PASSAGE vrai
    Quand le script 01d est exécuté après le 01c
    Alors les salariés concernés sont rattachés au territoire « inconnu » dans bts et dans stock_tous_ages, sans qu'aucune ligne ne soit perdue
    Et aucune région n'est inventée
    Et chaque code recodé est compté, affiché et écrit dans sorties/geo_hors_passage_<zonage>.csv
    Et les scripts 08c et 08d s'exécutent ensuite sans arrêt, « inconnu » étant compté à part et hors cartes
    Mais hors zonage département, ou avec RECODER_GEO_HORS_PASSAGE faux, rien n'est modifié et le comportement antérieur (arrêt explicite dans 08c) est conservé
```

## Traçabilité

Code :

- `R/01_preparation/01_fabriquer_donnees_test.R`, `01b_agreger_pcs.R`,
  `01c_stock_tous_ages.R`, `01d_recoder_geo_hors_passage.R`
- `R/01_preparation/02_importer_nettoyer_drees.R`,
  `02b_importer_mortalite_insee.R`, `02c_importer_invalidite_eacr.R`
- `R/00_config_fonctions/00c_fonctions_geo.R` — `normaliser_geo()`,
  `ajouter_region()`, `filtrer_geo_interet()`, `lire_passage_geo()`
- `R/00_config_fonctions/00_config.R` — `COL_BTS`, `COL_PCS`, `COL_GEO`,
  `GEO_*`, `STOCK_TOUS_AGES`, `Q_DECES_ANNUEL`, `T_INVALIDITE_BASE`

Tests :

- `tests/testthat/test-geo.R` — « mapping : une colonne source au nom arbitraire
  devient geo_code » ; « codes : toujours en texte, « 01 », « 2A », « 2B »,
  « 33 » intacts » ; « codes : une colonne numérique est complétée par des zéros
  si GEO_CODE_LARGEUR » ; « référentiel : libellés complétés, code inconnu
  conservé et signalé » ; « passage : source plus fine (commune) -> département,
  sans correspondance = inconnu » ; « GEO_INTERET : hors périmètre comptés, codes
  absents signalés, zéro match = arrêt » ; « garde de migration : l'ancienne
  configuration ZE est refusée » ; « Arrow : seules les colonnes déclarées sont
  lues, renommage générique » ; « encodage : un référentiel enregistré en
  Windows-1252 (latin1) est lu sans erreur, libellés corrects » ; « codage du
  sexe : 1/2, H/F, M/F, Hommes/Femmes acceptés ; valeur inconnue = arrêt
  explicite » ; « 04 : un sexe non apparié produit un arrêt explicite, pas des
  p_central NA »
- `tests/testthat/test-departs-pcs.R` — « fichiers : … pcs_hors_champ agrégée
  avec total = exclusions du 01b » ; « table de passage : doublon, code vide ou
  libellé manquant = arrêt explicite »
- `tests/testthat/test-fiches.R` — « STOCK_TOUS_AGES = FALSE : chaîne inchangée,
  fiches en repli, CSV 08b sans colonnes tous âges » ; « tableau des départs par
  CS : effectifs actuels tous âges (01c), part à remplacer, ligne Ensemble ;
  repli sans stock »
- `tests/testthat/test-recoder-geo-hors-passage.R` — « SPEC-PREP-013 — le code
  « 99 » sans région est recodé « inconnu », compté, tracé ; ajouter_region ne
  s'arrête plus » ; « 01d — sans objet hors zonage département, désactivable,
  et sans effet quand tout est rattaché » ; « SPEC-PREP-013 — chaîne réelle en
  mode département avec un « 99 » injecté : 08c et 08d passent … »
- SPEC-PREP-009 et SPEC-PREP-010 (sources externes et replis) : pas de test
  unitaire dédié ; la chaîne de test s'exécute avec les fichiers livrés.
