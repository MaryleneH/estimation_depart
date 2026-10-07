# Spec 10 — Typologie nationale des territoires de la BITD

## Intention métier

Caractériser chaque département selon le poids de l'emploi BITD qu'il porte,
la structure actuelle de cet emploi par grande CS, le volume des départs estimés
d'ici 2030, l'intensité du renouvellement qu'ils représentent et la concentration
éventuelle de ces départs sur une grande CS ; puis attribuer un **profil
explicable** par des règles métier simples, paramétrées et testables. Aucun score
composite, aucune pondération cachée, aucun clustering : pour chaque département,
la question « pourquoi ce profil ? » a une réponse en quelques indicateurs.

La typologie est une **couche d'analyse en aval** des départs par grande CS
(spec 06, script 08d) : aucun départ n'est recalculé, aucune définition n'est
modifiée. Les tensions DGA / France Travail, observées par FAP et par bassin,
sont une **couche séparée** qui n'entre pas dans le profil (section 5).

Dépendances : spec 03 (estimations), spec 04 (secret), spec 05 (arrondi),
spec 06 (tables département × grande CS, stock tous âges), spec 08
(orchestration).

## Sources et définitions

| Besoin | Source en session | Variable | Maille | Dénominateur | Secret |
|---|---|---|---|---|---|
| Effectif du champ et départs par grande CS | `departs_cs1$analytique$departement` (08d, exact) | `effectif_champ`, `departs_central`, `departs_bas`, `departs_haut` | département × cs1 | — | indicateurs de secret présents (interne) |
| Emploi actuel tous âges | `stock_tous_ages` (01c) | `effectif_tous_ages` | département × cs1 | — | aucun masque (effectifs) |
| Masques des cellules | `departs_cs1$diffusion$departement` (08d) | `masque`, `motif_masque` | département × cs1 | — | règle Insee + secondaire |
| Secret du département entier | `bts_projete` | `indicateurs_secret(base, "geo_code")` | département | — | règle Insee |

Indicateurs construits (tous écrits dans la table interne) :

| Indicateur | Définition | Numérateur | Dénominateur |
|---|---|---|---|
| `effectif_bitd_total` | emploi BITD du département, **base du poids** | `effectif_tous_ages` sommé sur les CS si le stock est disponible, sinon `effectif_champ` | — |
| `part_emploi_bitd_national_pct` | poids national | `effectif_bitd_total` | Σ départements (même base) |
| `part_<cs>_pct` | structure de l'emploi | effectif de la CS (même base) | `effectif_bitd_total` |
| `taux_depart_central_pct` | part des salariés **du champ** susceptibles de partir | `departs_central` | `effectif_champ` |
| `part_a_remplacer_pct` | part de l'**emploi actuel** à remplacer | `departs_central` | `effectif_tous_ages` |
| `intensite_renouvellement_pct` | **intensité du renouvellement retenue** : `part_a_remplacer_pct` quand le stock tous âges existe (`TYPO_INDICATEUR_INTENSITE`), sinon `taux_depart_central_pct` ; le nom de l'indicateur utilisé est écrit dans `indicateur_intensite` | | |
| `part_departs_pct` (par CS) | concentration | `departs_central` de la CS | `departs_central` du département |

Le terme « taux de renouvellement » n'est pas employé seul : chaque table dit
quel indicateur elle utilise. Le sens de l'intensité retenue : « sur 100
salariés actuellement en poste dans la BITD du département, combien devraient
être remplacés d'ici 2030 ».

## Paramètres (`00_config.R`, section typologie)

| Paramètre | Rôle | Valeur courante | Comportement à la borne |
|---|---|---|---|
| `GENERER_TYPOLOGIE` | activer l'étape 08f | TRUE | — |
| `TYPO_SECRET` | produire `diffusion/` | TRUE | — |
| `TYPO_INDICATEUR_INTENSITE` | intensité retenue | `part_a_remplacer_pct` | repli sur `taux_depart_central_pct` sans stock |
| `TYPO_SEUIL_DOMINANCE_CS` | structure « Dominante » / « Mixte » | 40 % | part = seuil → Dominante |
| `TYPO_SEUIL_CONCENTRATION_DEPARTS` | départs « Concentré » / « Diffus » | 50 % | part = seuil → Concentré |
| `TYPO_SEUIL_EFFECTIF_MIN` | plancher d'effectif du poids | 50 salariés | effectif < seuil → Faible ; = seuil → classé normalement |
| `TYPO_METHODE_CLASSES` | bornes des classes | `terciles` (**règle de classement statistique provisoire**) ; `fixes` = seuils ci-dessous | — |
| `TYPO_SEUIL_POIDS_FAIBLE` / `_FORT` | classes de poids (méthode fixes) | 0,5 % / 2 % | x < bas → Faible ; bas ≤ x < haut → Moyen ; x ≥ haut → Fort |
| `TYPO_SEUIL_INTENSITE_FAIBLE` / `TYPO_SEUIL_RENOUVELLEMENT_ELEVE` | classes d'intensité (méthode fixes) | 8 % / 15 % | même règle |
| `TYPO_SEUIL_VOLUME_FAIBLE` / `_ELEVE` | classes de volume (méthode fixes) | 50 / 300 départs | même règle |
| `TYPO_MARGE_FRONTIERE_PCT` | « à la frontière » d'un seuil de classe (axes poids, intensité, volume **seulement**) | 3 % relatif | \|x − seuil\| ≤ 3 % du seuil |
| `TYPO_NB_CRITERES_FRONTIERE` | nombre d'axes à la frontière déclenchant l'expertise | 2 | n ≥ 2 → Situation à expertiser |
| `TYPO_FACTEUR_INTENSITE_EXTREME` | intensité extrême sur stock faible | 2 × seuil « élevée » | ≥ → Situation à expertiser |

Les seuils de dominance et de concentration sont **informatifs** : leur
proximité est tracée (`frontiere_dominance`, `frontiere_concentration`, table
interne) mais ne déclenche jamais l'expertise, car ils ne décident pas du
profil à eux seuls.

Avec la méthode `terciles`, les bornes effectives sont calculées sur les
départements dont l'effectif atteint le plancher et **écrites dans
`parametres_typologie.csv`** à chaque exécution : elles dépendent des données et
ne sont pas des seuils métier.

## 1. Niveau géographique et sources

```gherkin
Fonctionnalité: Construire la typologie au niveau départemental, en aval des départs par grande CS

  Scénario: [SPEC-TYPO-001] Construire la typologie au niveau départemental uniquement
    Étant donné la table département × grande CS exacte du 08d
    Quand la typologie est construite
    Alors elle comporte une ligne par département, avec geo_code en texte
    Et aucune estimation n'est descendue à un bassin ni à une maille plus fine
    Et si le zonage d'analyse n'est pas le département, l'étape est ignorée avec un message et aucun fichier n'est produit

  Scénario: [SPEC-TYPO-002] Réutiliser les calculs existants sans nouvelle formule
    Étant donné les mesures du 08d (effectif_champ, departs_central, departs_bas, departs_haut) et le stock tous âges du 01c
    Quand la typologie est construite
    Alors departs_central, departs_bas et departs_haut du département sont les sommes exactes des grandes CS
    Et le poids et la structure sont calculés sur l'emploi actuel tous âges, ou sur le champ si le stock est indisponible, la base utilisée étant écrite dans base_poids
    Et aucune probabilité, aucun paramètre du modèle n'est recalculé ni modifié
```

## 2. Poids, structure, volume, intensité

```gherkin
Fonctionnalité: Caractériser chaque département par des indicateurs lisibles

  Scénario: [SPEC-TYPO-003] Identifier une structure dominante ou mixte
    Étant donné des parts de grandes CS de 31 %, 30 %, 25 % et 14 %
    Et un seuil de dominance supérieur à 31 %
    Quand la structure est déterminée
    Alors la CS principale est celle à 31 %, sa part est conservée
    Et la structure vaut « Mixte », jamais la CS à 31 %
    Et une part exactement égale au seuil donne « Dominante »
    Et deux CS à égalité donnent la première dans l'ordre métier, avec egalite_structure vrai

  Scénario: [SPEC-TYPO-004] Conserver le volume de départs et sa fourchette
    Étant donné un département
    Quand la typologie est construite
    Alors departs_central, departs_bas et departs_haut sont conservés au niveau départemental
    Et departs_bas ≤ departs_central ≤ departs_haut
    Et le volume n'est jamais confondu avec un taux : il a sa propre classe (classe_volume_departs)

  Scénario: [SPEC-TYPO-005] Définir l'intensité du renouvellement
    Étant donné departs_central et effectif_tous_ages d'un département
    Quand l'intensité est calculée
    Alors elle vaut 100 × departs_central / effectif_tous_ages (part de l'emploi actuel à remplacer)
    Et sans stock tous âges, elle vaut 100 × departs_central / effectif_champ, et indicateur_intensite le dit
    Et taux_depart_central_pct et part_a_remplacer_pct restent tous deux disponibles

  Scénario: [SPEC-TYPO-006] Distinguer volume et intensité
    Étant donné le département A avec 1 000 départs pour 20 000 salariés
    Et le département B avec 500 départs pour 2 000 salariés
    Quand la typologie est construite
    Alors A a le plus gros volume (1 000 > 500)
    Et B a la plus forte intensité (25 % > 5 %)
    Et les deux informations sont conservées simultanément

  Scénario: [SPEC-TYPO-007] Identifier séparément la CS au plus gros volume et la CS à l'intensité la plus forte
    Étant donné un département où les professions intermédiaires portent le plus de départs et où les cadres ont l'intensité la plus forte
    Quand les CS sont identifiées
    Alors cs_volume_departs_max vaut « Prof. intermediaires » et cs_taux_renouvellement_max vaut « Cadres »
    Et la table longue porte, par CS, le rang en volume et le rang en intensité

  Scénario: [SPEC-TYPO-008] Qualifier un renouvellement concentré
    Étant donné qu'une grande CS représente 60 % des départs du département
    Et un seuil de concentration de 50 %
    Quand la concentration est calculée
    Alors type_concentration vaut « Concentré » et la CS et sa part sont conservées

  Scénario: [SPEC-TYPO-009] Qualifier un renouvellement diffus
    Étant donné une répartition de 30 %, 25 %, 25 % et 20 %
    Quand la concentration est calculée
    Alors type_concentration vaut « Diffus »
    Et une part exactement égale au seuil donne « Concentré »
```

## 3. Classes et frontières

```gherkin
Fonctionnalité: Classer sans décider silencieusement

  Scénario: [SPEC-TYPO-010] Classer à trois niveaux avec la borne incluse dans la classe supérieure
    Étant donné des bornes bas = 1 et haut = 5
    Quand on classe 0,99 ; 1 ; 1,01 ; 4,99 ; 5 ; 5,01
    Alors on obtient Faible ; Moyen ; Moyen ; Moyen ; Fort ; Fort

  Scénario: [SPEC-TYPO-011] Appliquer un plancher d'effectif au poids
    Étant donné un département dont la part nationale dépasse la borne « Fort » mais dont l'effectif est inférieur au plancher
    Quand le poids est classé
    Alors sa classe est « Faible »
    Et un effectif exactement égal au plancher est classé selon sa part

  Scénario: [SPEC-TYPO-012] Qualifier la méthode des terciles de provisoire
    Étant donné TYPO_METHODE_CLASSES = « terciles »
    Quand les classes sont calculées
    Alors les bornes sont les terciles observés sur les départements retenus
    Et elles sont écrites dans parametres_typologie.csv
    Et la restitution les qualifie de règle de classement statistique provisoire

  Scénario: [SPEC-TYPO-042] Proposer des seuils fixes à partir des données, sans rien appliquer
    Étant donné la table interne des départements et un pas d'arrondi par axe (poids, intensité, volume)
    Quand proposer_seuils_typologie() est appelée
    Alors elle calcule, sur les départements au-dessus du plancher, les terciles de chaque axe et les arrondit au pas (2,37 → 2,5 au pas 0,5 ; 119 → 120 au pas 10)
    Et elle compte les départements par classe avec les bornes proposées
    Et elle imprime un bloc prêt à coller dans 00_config.R avec TYPO_METHODE_CLASSES = « fixes »
    Et les seuils métier (dominance, concentration, plancher) ne sont jamais proposés : seule leur distribution observée est affichée
    Et aucun paramètre, aucun fichier n'est modifié ; moins de 3 départements retenus = arrêt explicite
```

## 4. Profils

Chaque profil a un **titre parlant**, une **règle** en français, une
**signification** (ce que le profil dit du département) et une **consigne de
lecture** (à quoi prêter attention). Les quatre éléments sont écrits dans
`interne/profils_definitions.csv` à chaque exécution ; les clés (`enjeu`,
`emergent`, `majeur_modere`, `concentre`, `stable`, `diffus`, `expertiser`)
sont stables, les titres peuvent évoluer.

| Clé | Titre | Règle | Signification | Consigne de lecture |
|---|---|---|---|---|
| `enjeu` | Pôle majeur à renouveler rapidement | poids Fort, volume Élevé, intensité Élevée | pèse lourd, beaucoup de départs, part importante de l'emploi actuel à remplacer | priorité de renouvellement, à anticiper par des recrutements et des transmissions sur plusieurs années |
| `emergent` | Renouvellement rapide à surveiller | intensité Élevée sans réunir toutes les conditions du pôle majeur | renouvellement rapide au regard de l'emploi actuel, volume ou poids plus modestes | signal de vigilance et non prédiction de pénurie ; vérifier la capacité locale de recrutement |
| `majeur_modere` | Pôle majeur au renouvellement modéré | volume Élevé, intensité Modérée, poids Moyen ou Fort | volume élevé par la taille du département, rythme dans la moyenne | lire le volume en valeur absolue, car le nombre de postes à pourvoir est élevé |
| `concentre` | Renouvellement porté par une catégorie | ≥ moitié des départs sur une grande CS, volume et intensité non élevés | le renouvellement se joue sur une catégorie précise | regarder la catégorie et sa part à remplacer plutôt que le total |
| `stable` | Implantation stable | poids Moyen ou Fort, volume et intensité Faibles ou Modérés, départs diffus | implantation installée, renouvellement régulier et réparti | aucun signal particulier, suivi ordinaire |
| `diffus` | Implantation réduite | poids Faible (part nationale faible ou effectif sous le plancher) | présence réduite, parts et taux fragiles (petits effectifs) | ne pas sur-interpréter un taux élevé sur un petit effectif |
| `expertiser` | Situation à expertiser | signaux contradictoires ou axes à la frontière d'un seuil | les règles simples ne tranchent pas, et `motif_expertise` dit ce qui coince | examiner avec les acteurs locaux ; ne jamais classer d'office |

Les textes de signification et de consigne ne contiennent pas de deux-points,
car ils sont insérés après « Profil « … » : » et « Consigne de lecture : ».

Règles ordonnées (la première qui s'applique l'emporte) :

| Ordre | Condition | Profil | `motif_expertise` |
|---|---|---|---|
| 1 | une classe manquante | Situation à expertiser | « un indicateur de classement est manquant » |
| 2 | poids Faible et intensité ≥ facteur × borne « Élevée » | Situation à expertiser | « intensité de renouvellement extrême sur un très petit effectif » |
| 3 | poids Faible | Implantation réduite | — |
| 4 | ≥ `TYPO_NB_CRITERES_FRONTIERE` axes à la frontière d'un seuil (poids, intensité, volume **seulement**) | Situation à expertiser | « n indicateurs à la frontière d'un seuil de classe (axes) » |
| 5 | volume Élevé **et intensité Faible** (contradiction) | Situation à expertiser | « volume de départs élevé mais renouvellement lent au regard de l'emploi actuel » |
| 6 | poids Fort, volume Élevé, intensité Élevée | Pôle majeur à renouveler rapidement | — |
| 7 | intensité Élevée (poids Moyen ou Fort) | Renouvellement rapide à surveiller | — |
| 8 | volume Élevé (intensité Modérée, poids Moyen ou Fort) | Pôle majeur au renouvellement modéré | — |
| 9 | concentration « Concentré » | Renouvellement porté par une catégorie | — |
| 10 | sinon | Implantation stable | — |

Un volume Élevé avec une intensité Modérée n'est **plus** une contradiction :
c'est la situation ordinaire d'un gros département (règle 8). Seule la
combinaison volume Élevé / intensité Faible reste contradictoire (règle 5).

```gherkin
Fonctionnalité: Attribuer un profil explicable

  Scénario: [SPEC-TYPO-013] Pôle majeur à renouveler rapidement
    Étant donné un poids Fort, un volume Élevé et une intensité Élevée
    Alors le profil est « Pôle majeur à renouveler rapidement »

  Scénario: [SPEC-TYPO-014] Un faible poids ne crée jamais un pôle majeur
    Étant donné un poids Faible et une intensité Élevée
    Alors le profil est « Implantation réduite »
    Et l'intensité reste visible dans la table
    Mais si l'intensité atteint le facteur extrême, le profil est « Situation à expertiser » avec son motif

  Scénario: [SPEC-TYPO-015] Renouvellement rapide à surveiller
    Étant donné un poids Moyen ou Fort et une intensité Élevée sans les conditions du pôle majeur
    Alors le profil est « Renouvellement rapide à surveiller »
    Et ce profil est un signal de vigilance, jamais une prédiction de pénurie

  Scénario: [SPEC-TYPO-016] Renouvellement porté par une catégorie et implantation stable
    Étant donné un poids non Faible, une intensité non Élevée et un volume non Élevé
    Quand les départs sont « Concentré », le profil est « Renouvellement porté par une catégorie »
    Et quand ils sont « Diffus », le profil est « Implantation stable »

  Scénario: [SPEC-TYPO-017] Situation à expertiser sur signaux contradictoires, avec motif
    Étant donné un volume Élevé avec une intensité Faible
    Ou au moins deux axes (poids, intensité, volume) à la frontière d'un seuil
    Alors le profil est « Situation à expertiser », aucune classification n'est forcée
    Et motif_expertise dit en français ce qui empêche de trancher ; il est NA pour tout autre profil
    Et la proximité des seuils de dominance ou de concentration ne déclenche jamais l'expertise

  Scénario: [SPEC-TYPO-019] Pôle majeur au renouvellement modéré
    Étant donné un poids Moyen ou Fort, un volume Élevé et une intensité Modérée
    Alors le profil est « Pôle majeur au renouvellement modéré », et non « Situation à expertiser »

  Scénario: [SPEC-TYPO-018] Justifier chaque profil par règles
    Étant donné un département classé
    Quand la justification est construite
    Alors c'est une phrase déterministe assemblée à partir des classes et des indicateurs (poids, volume, intensité, structure, concentration, frontières)
    Et aucun texte n'est généré par un modèle de langage
```

## 8. Rédaction : dire la situation en français courant

La typologie doit permettre d'**écrire** la situation de chaque département et
de chaque profil sans retraiter les colonnes. Tous les textes sont assemblés
**par règles** à partir des indicateurs (gabarits fixes, nombres arrondis selon
la spec 05) ; aucun modèle de langage n'intervient.

Gabarit de `situation_texte` (une cellule par département, quatre phrases) :

1. **État** : « <Nom> (<code>) pèse <part> % de l'emploi BITD national
   (<effectif> salariés), un poids <classe>[, sous le plancher de <n> salariés
   retenu pour l'analyse]. Son emploi est dominé par les <CS> (<part> %). » ou
   « … se répartit entre les grandes catégories sans dominante (première :
   <CS>, <part> %). »
2. **Dynamique** : « D'ici 2030, <central> départs sont attendus (entre <bas>
   et <haut> selon l'hypothèse de départ en retraite), soit <intensité> % de
   l'emploi actuel à remplacer, <au-dessus de / en dessous de / au niveau de>
   la médiane des départements (<médiane> %) : volume de départs <classe>,
   intensité de renouvellement <classe>. » (sans stock : « des salariés du champ
   susceptibles de partir »)
3. **Concentration** : « Les <CS> portent <part> % de ces départs[, sans
   concentration marquée | : le renouvellement est concentré sur cette
   catégorie]. C'est chez les <CS> que la part à remplacer est la plus
   élevée. » (ou « C'est aussi la catégorie … » quand les deux CS coïncident)
4. **Lecture** : « Profil « <titre> » : <signification>.[ À examiner :
   <motif_expertise>.][ Point(s) d'attention : <points_attention>.] »

`points_attention` (tous profils, « ; » entre les points, NA si aucun) :

| Règle | Texte |
|---|---|
| départs « Concentré » | une seule catégorie porte <part> % des départs (<CS>) |
| poids Faible et intensité Élevée (non extrême) | renouvellement rapide mais calculé sur un petit effectif |
| (haut − bas) / central ≥ 25 % | fourchette d'estimation large (de <bas> à <haut> départs) |
| au moins un axe à la frontière, profil autre qu'« à expertiser » | proche d'un seuil de classe (<axes>) : la classe peut basculer d'une édition à l'autre |

`synthese_profils.csv` porte, par profil : `regle`, `signification`,
`consigne_lecture`, `departements` (jusqu'à douze noms, par volume de départs
décroissant), `n_a_expertiser_motifs` et `texte` : « <n> département(s)
relève(nt) du profil « <titre> » : <signification>. Ensemble, ils représentent
<x> % de l'emploi BITD et <y> % des départs attendus d'ici 2030 ; leur
intensité de renouvellement médiane est de <m> % (ensemble des départements :
<M> %). [Dans <k> cas, une seule catégorie porte au moins la moitié des
départs. ]Consigne de lecture : <consigne>. »

```gherkin
Fonctionnalité: Rédiger la situation de chaque département et de chaque profil par règles

  Scénario: [SPEC-TYPO-050] Écrire la situation d'un département
    Étant donné un département classé, avec stock tous âges
    Quand la typologie est construite
    Alors situation_texte contient, dans l'ordre, le nom et le code, le poids national, l'effectif, la structure, les trois estimations de départs, l'intensité comparée à la médiane des départements, la catégorie qui porte le plus de départs, le titre du profil et sa signification
    Et les nombres de personnes y sont entiers et les parts à une décimale avec la virgule
    Et le texte est identique d'une exécution à l'autre pour les mêmes données

  Scénario: [SPEC-TYPO-051] Expliquer ce qu'il faut examiner
    Étant donné un département « Situation à expertiser »
    Alors motif_expertise est renseigné et situation_texte contient « À examiner : <motif> »
    Et pour tout autre profil, motif_expertise est NA

  Scénario: [SPEC-TYPO-052] Signaler les points d'attention de tout profil
    Étant donné un département « Implantation stable » dont une catégorie porte 60 % des départs ou dont un axe est à la frontière d'un seuil
    Alors points_attention le dit et situation_texte se termine par « Point(s) d'attention : … »
    Et un département sans aucun point a points_attention NA et aucune mention

  Scénario: [SPEC-TYPO-053] Rédiger la synthèse par profil et publier les définitions
    Quand la synthèse est produite
    Alors chaque profil porte son titre, sa règle, sa signification, sa consigne de lecture, la liste de ses départements et un texte de synthèse
    Et interne/profils_definitions.csv contient les sept profils avec ces quatre éléments
    Et le 08f affiche en console le nombre de situations à expertiser par motif

  Scénario: [SPEC-TYPO-054] Ne rien révéler par le texte
    Étant donné la diffusion avec secret
    Quand un département est masqué, situation_texte, motif_expertise et points_attention sont NA
    Et quand une cellule CS du département est masquée, situation_texte ne cite aucune catégorie (ni structure, ni concentration) et points_attention ne mentionne aucune catégorie
    Et quand le profil est « Non diffusé (secret statistique) », situation_texte est NA
```

## 5. Tensions DGA / France Travail : maille différente, couche séparée

Les estimations sont robustes au niveau **département × grande CS**. Les tensions
sont ou seront exprimées par **FAP / métier × bassin**. Le bassin est plus petit
que le département ; une FAP n'est pas une grande CS. Un fichier
`data/tensions_fap_territoires.csv` pourra être déposé au format de
`data/templates/tensions_fap_territoires_template.csv` (lignes d'exemple
entièrement fictives). Dans cette version, la tension **n'entre pas** dans le
profil ; seule l'architecture est préparée.

```gherkin
Fonctionnalité: Ne jamais confondre les mailles ni les nomenclatures

  Scénario: [SPEC-TYPO-020] Ne pas attribuer une tension de bassin au département
    Étant donné un bassin signalé en tension dans un département
    Quand la couche de tension est jointe
    Alors seuls nb_bassins_signales et presence_signal_tension_localise sont produits
    Et aucune colonne tension_departement ni departement_en_tension n'existe nulle part
    Et le profil du département est inchangé

  Scénario: [SPEC-TYPO-021] Ne pas descendre une estimation départementale au bassin
    Étant donné une estimation calculée au niveau départemental
    Quand la couche de tension est jointe
    Alors aucun départ n'est attribué ni réparti à un bassin

  Scénario: [SPEC-TYPO-022] Refuser toute correspondance FAP → grande CS
    Étant donné un fichier de tensions portant une colonne de grande CS
    Quand il est joint
    Alors la jointure s'arrête avec un message explicite
    Car une FAP et une grande CS ne sont pas des objets équivalents et aucun référentiel ne les relie

  Scénario: [SPEC-TYPO-023] Nommer sans ambiguïté la présence locale d'un signal
    Étant donné une future jointure bassin → département
    Alors l'indicateur s'appelle presence_signal_tension_localise ou nb_bassins_signales
    Et il ne qualifie jamais le département lui-même
```

## 6. Secret et variables dérivées

Le secret existant (spec 04) n'est ni contourné ni affaibli. La table interne
reste complète ; la diffusion applique trois protections.

```gherkin
Fonctionnalité: Diffuser la typologie sans révéler une cellule protégée

  Scénario: [SPEC-TYPO-030] Masquer un département non diffusable
    Étant donné un département qui ne satisfait pas la règle Insee (salariés, entreprises, dominance) sur l'ensemble de ses salariés
    Quand la diffusion est produite
    Alors toutes ses mesures et son profil sont NA ou « Non diffusé (secret statistique) », seuls code et nom restent
    Et le secret secondaire s'applique sur le bloc national (le total France est publié par le 08d) : le plus petit département restant est masqué

  Scénario: [SPEC-TYPO-031] Suivre le masque de chaque cellule
    Étant donné une cellule département × grande CS masquée dans la diffusion du 08d
    Quand la diffusion de la typologie est produite
    Alors toute colonne par grande CS issue de cette cellule (effectif, part, départs, intensité) est NA

  Scénario: [SPEC-TYPO-032] Ne pas révéler une cellule masquée par une variable dérivée
    Étant donné un département dont au moins une cellule CS est masquée
    Quand la diffusion est produite
    Alors cs_principale, part_cs_principale_pct, structure_emploi, cs_volume_departs_max, cs_taux_renouvellement_max, part_departs_cs_max_pct et type_concentration sont NA
    Car « la CS dominante est X » ou « les départs sont concentrés sur X » borne la valeur de la cellule masquée

  Scénario: [SPEC-TYPO-033] Ne pas révéler une cellule masquée par le profil
    Étant donné un département dont une cellule CS est masquée et dont le profil dépend de la concentration (« Renouvellement porté par une catégorie », « Implantation stable »)
    Quand la diffusion est produite
    Alors le profil est « Non diffusé (secret statistique) » avec le motif « cellule cs masquée »
    Et les profils qui ne dépendent que des totaux départementaux restent publiés

  Scénario: [SPEC-TYPO-034] Conserver la table interne complète et retirer les indicateurs techniques de la diffusion
    Quand les sorties sont écrites
    Alors interne/ contient toutes les colonnes, dont les frontières et les indicateurs d'audit
    Et diffusion/ ne contient ni n_entreprises, ni part_dominante_pct, ni les indicateurs de frontière
    Et interne/ n'est jamais versionné (.gitignore)
```

## 7. Sorties et orchestration

```gherkin
Fonctionnalité: Produire des sorties contrôlées

  Scénario: [SPEC-TYPO-040] Produire les tables et la matrice
    Étant donné l'exécution du 08f après le 08d
    Alors sorties/typologie_territoriale/interne/ contient typologie_departements.csv, typologie_departement_cs.csv, synthese_profils.csv, profils_definitions.csv, parametres_typologie.csv et matrice_typologie.png
    Et diffusion/typologie_departements.csv est produit si TYPO_SECRET
    Et les nombres de personnes sont arrondis à l'entier en restitution, les parts à une décimale, les objets exacts restant intacts
    Et les objets du 08, 08b, 08c et 08d sont strictement inchangés après le 08f

  Scénario: [SPEC-TYPO-041] Contrôler avant d'écrire
    Quand la typologie est construite
    Alors Σ grandes CS = total du département (effectif et trois estimations), total national = table cs1, Σ parts nationales = 100, fourchette ordonnée, profils connus, aucune colonne de tension attribuée au département ; sinon arrêt
```

La matrice pédagogique (`matrice_typologie.png`) place chaque département en
poids (x) × intensité (y), taille du point = volume de départs, couleur =
structure de l'emploi ; les seuils de classes sont tracés et qualifiés de
provisoires ; aucune tension n'y figure.

## Limites

- Maille département : aucune lecture infra-départementale n'est possible à
  partir de ces résultats.
- Les tensions sont observées à une autre maille et dans une autre
  nomenclature : elles ne peuvent qu'être signalées comme présentes localement.
- Aucune causalité n'est établie : un profil décrit une situation, pas une
  cause ni une recommandation.
- Les bornes des classes par terciles sont provisoires et dépendent du
  périmètre ; les seuils fixes proposés restent à arbitrer.

## Traçabilité

Code :

- `R/00_config_fonctions/00h_fonctions_typologie.R` — `parametres_typologie()`,
  `seuils_classes()`, `classer_par_seuils()`, `classer_poids_bitd()`,
  `calculer_structure_cs()`, `identifier_dominance_cs()`,
  `calculer_concentration_departs()`, `identifier_cs_volume_max()`,
  `identifier_cs_taux_max()`, `attribuer_profil_typologie()` (profil + motif),
  `points_attention_typologie()`, `rediger_situation_departement()`,
  `rediger_synthese_profils()`, `profils_definitions()` (`DEFINITIONS_PROFILS`),
  `construire_justification_profil()`, `construire_typologie_departements()`,
  `controler_typologie()`, `appliquer_secret_typologie()`, `synthese_profils()`,
  `joindre_signal_tension_localise()`, `png_matrice_typologie()`,
  `proposer_seuils_typologie()` (aide au réglage, SPEC-TYPO-042)
- `R/08_territoires/08f_typologie_departements.R`
- `R/00_config_fonctions/00_config.R` — section typologie
- `data/templates/tensions_fap_territoires_template.csv`

Tests : `tests/testthat/test-typologie-territoriale.R` (chaque bloc cite ses
identifiants). Correspondance détaillée dans `09_matrice_tracabilite.md`.
