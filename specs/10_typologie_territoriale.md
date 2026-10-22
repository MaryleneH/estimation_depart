# Spec 10 — Typologie nationale des territoires de la Base

## Intention métier

Caractériser chaque département selon le poids de l'emploi Base qu'il porte,
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
| Pyramide des âges tous âges | `stock_tranches_ages` (01c) | `effectif`, `n_entreprises`, `part_dominante_pct` | département × tranche (et × cs1 en interne) | — | règle Insee + secondaire par département (§12) |
| Pyramide du champ (repli) | `bts_projete` | `age_2024` | département × tranche du champ | — | règle Insee + secondaire par département (§12) |
| Masques des cellules | `departs_cs1$diffusion$departement` (08d) | `masque`, `motif_masque` | département × cs1 | — | règle Insee + secondaire |
| Secret du département entier | `bts_projete` | `indicateurs_secret(base, "geo_code")` | département | — | règle Insee |

Indicateurs construits (tous écrits dans la table interne) :

| Indicateur | Définition | Numérateur | Dénominateur |
|---|---|---|---|
| `effectif_base_total` | emploi Base du département, **base du poids** | `effectif_tous_ages` sommé sur les CS si le stock est disponible, sinon `effectif_champ` | — |
| `part_emploi_base_national_pct` | poids national | `effectif_base_total` | Σ départements (même base) |
| `part_<cs>_pct` | structure de l'emploi | effectif de la CS (même base) | `effectif_base_total` |
| `taux_depart_central_pct` | part des salariés **du champ** susceptibles de partir | `departs_central` | `effectif_champ` |
| `part_a_remplacer_pct` | part de l'**emploi actuel** à remplacer | `departs_central` | `effectif_tous_ages` |
| `intensite_renouvellement_pct` | **intensité du renouvellement retenue** : `part_a_remplacer_pct` quand le stock tous âges existe (`TYPO_INDICATEUR_INTENSITE`), sinon `taux_depart_central_pct` ; le nom de l'indicateur utilisé est écrit dans `indicateur_intensite` | | |
| `part_departs_pct` (par CS) | concentration | `departs_central` de la CS | `departs_central` du département |

Le terme « taux de renouvellement » n'est pas employé seul : chaque table dit
quel indicateur elle utilise. Le sens de l'intensité retenue : « sur 100
salariés actuellement en poste dans la Base du département, combien devraient
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
| `AGE_BORNE_JEUNES` | borne des « jeunes » de la pyramide (§12) | 30 ans | âge < borne → jeunes |
| `TYPO_METHODE_PYRAMIDE` | bornes des classes de relève et de vague suivante | `terciles` (provisoire) ; `fixes` | — |
| `TYPO_SEUIL_RELEVE_FRAGILE` / `_SOLIDE` | classes de relève (méthode fixes) | 0,8 / 1,5 jeune pour un senior | x < bas → fragile ; x ≥ haut → solide |
| `TYPO_SEUIL_VAGUE_FAIBLE` / `_FORTE` | classes de vague suivante (méthode fixes) | 20 % / 25 % | x < bas → Faible ; x ≥ haut → Forte |

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

1. **État** : « <Nom> (<code>) pèse <part> % de l'emploi Base national
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
<x> % de l'emploi Base et <y> % des départs attendus d'ici 2030 ; leur
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
    Et interne/pyramide_ages_departement_cs.csv (département × grande CS × tranche) est produit si la pyramide complète est disponible (§12)
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

## 9. Cartes des profils (08g)

Deux pages HTML autonomes et un PNG, produits par un script séparé (08g) qui
lit **uniquement** `diffusion/typologie_departements.csv` (08f) et le fond
local ; il est relançable seul après la chaîne, depuis la racine du projet,
sans modifier aucun autre script.

| Fichier | Contenu | Contenu actif |
|---|---|---|
| `cartes/carte_typologie.html` | carte choroplèthe par profil ; survol = infobulle (nom, profil, chiffres, motif, points d'attention) + panneau « situation » rédigé ; clic = épingler ; légende cliquable pour isoler un profil ; définitions des profils ; tableau des situations | JavaScript natif inline |
| `cartes/carte_typologie_courriel.html` | mêmes couleurs écrites en attributs, infobulles natives `<title>` du SVG, tableau des situations et définitions ouverts sous la carte | **aucun** (ni `<script>`, ni `on*=`, ni `javascript:`) |
| `cartes/carte_typologie.png` | image de la carte (ggplot2), même palette, mêmes contours | — |

Statuts d'un département : `diffuse` (profil + chiffres), `profil_masque`
(chiffres diffusés mais profil « Non diffusé » : cellule CS masquée),
`masque` (département entier : seuls code et nom), `sans` (absent de la
table : blanc, contour pointillé). Couleurs des profils : `COULEURS_PROFILS`
(une par clé) ; gris `COULEURS_CARTE$secret` pour les deux statuts masqués.

```gherkin
Fonctionnalité: Cartographier les profils sans révéler une valeur protégée

  Scénario: [SPEC-TYPO-060] Alimenter la carte uniquement par la table de diffusion
    Étant donné la table de diffusion de la typologie et le fond local
    Quand la carte est préparée
    Alors une table sans colonne masque (interne) est refusée
    Et un code sans géométrie arrête la génération avec la liste, « inconnu » est exclu et compté
    Et un profil inconnu du code (autre version) arrête la génération
    Et un fichier de diffusion absent produit un message avec le chemin attendu, jamais un arrêt de la chaîne

  Scénario: [SPEC-TYPO-061] Colorer chaque département par son profil
    Quand la page interactive est produite
    Alors chaque département porte la couleur de son profil (COULEURS_PROFILS), gris s'il est masqué ou si son profil n'est pas diffusé, blanc pointillé s'il est absent
    Et la légende donne, par profil, le titre, le nombre de départements et la signification ; dans la page interactive elle est compacte (titre et nombre sur une ligne), la signification restant dans le HTML (infobulle native du bouton, panneau au clic, section des définitions)
    Et les couleurs, la légende et le détail sont écrits dans le HTML (lisibles sans script)

  Scénario: [SPEC-TYPO-062] Afficher le détail au survol
    Quand un département est survolé dans la page interactive
    Alors l'infobulle donne le nom, le profil, le poids, les départs et leur fourchette, l'intensité, les classes, la structure, la catégorie qui porte les départs, le motif d'expertise et les points d'attention
    Et le panneau « situation » affiche situation_texte ; un clic l'épingle, un second clic le libère
    Et un clic sur un profil de la légende isole ses départements et affiche dans le panneau le titre du profil, sa signification et sa consigne de lecture

  Scénario: [SPEC-TYPO-063] Produire une version sans aucun contenu actif pour le courriel
    Quand la version courriel est produite
    Alors elle ne contient ni <script>, ni attribut on*=, ni javascript:, ni bloc de données
    Et chaque département porte un <title> natif avec le nom, le profil et les mêmes lignes de détail
    Et le tableau des situations (une ligne par département diffusé, situation_texte) et les définitions des profils sont ouverts sous la carte
    Et un contenu actif détecté arrête la génération

  Scénario: [SPEC-TYPO-064] Ne rien embarquer d'un département masqué
    Étant donné un département masqué dans la table de diffusion
    Quand les pages sont produites
    Alors toute mesure et tout texte de ce département sont effacés avant sérialisation, même si la table en contenait
    Et il n'apparaît que sous son code, son nom et « Résultat non diffusé — secret statistique », en gris
    Et un département au profil non diffusé garde ses chiffres mais aucune catégorie

  Scénario: [SPEC-TYPO-065] Tout lire sans défiler ; survoler et défiler sans aucun saut ; légende sans entrée vide
    Étant donné la page interactive ouverte dans un navigateur
    Quand la fenêtre est large (plus de 900 px)
    Alors le premier écran (en-tête compact, carte entière avec l'outre-mer, panneau « situation » et légende) tient dans la hauteur de la fenêtre, avec un plancher de 560 px
    Et la page n'a pas à défiler pour survoler n'importe quel département et lire sa situation : seul le panneau défile en interne, à hauteur fixe donnée par la mise en page
    Et le texte de présentation de l'en-tête reste disponible dans un bloc « À propos de cette carte » fermé par défaut ; définitions et tableau restent sous le premier écran
    Quand la fenêtre est étroite (900 px ou moins)
    Alors la carte est au-dessus du panneau ; un toucher sur un département fait défiler jusqu'au panneau, qui porte un lien de retour vers la carte ; la hauteur du panneau est stabilisée (hauteur du texte le plus long, bornée à 60 % de la fenêtre, défilement interne au-delà)
    Quand un département est survolé puis que la page défile, le curseur restant sur la carte
    Alors l'infobulle est positionnée par rapport à la fenêtre (position fixe) et bornée à l'écran : elle n'agrandit jamais la zone défilable
    Et son contenu n'est reconstruit que lorsque le département survolé change ; seule sa position suit le curseur
    Et le panneau « situation » n'est mis à jour que si le département change, et sa hauteur ne varie jamais : la carte ne se déplace jamais sous le curseur
    Et l'infobulle est masquée pendant le défilement et réapparaît au mouvement suivant
    Et l'épinglage, le filtrage par la légende, le contour du département survolé et le secret sont inchangés
    Et la légende ne contient pas l'entrée « Département absent de la typologie » lorsque son compte est nul ; les sept profils et l'entrée du secret sont toujours présents ; un département réellement absent garde sa propre entrée, distincte du secret (HTML interactif, courriel et PNG)
```

## 10. Consulter les bornes effectives (99c, hors chaîne)

```gherkin
Fonctionnalité: Lire en console les bornes qui ont réellement servi au classement

  Scénario: [SPEC-TYPO-070] Afficher les bornes de la dernière typologie produite
    Étant donné interne/parametres_typologie.csv écrit par le 08f
    Quand source("R/99_controles/99c_afficher_bornes_typologie.R") est lancé depuis la racine, dans une session R neuve
    Alors la console donne, pour le poids, le volume et l'intensité, les deux bornes effectives et les trois classes (borne incluse dans la classe supérieure), la méthode (terciles provisoires ou seuils fixes), le plancher d'effectif et son exception, l'indicateur d'intensité retenu (repli signalé)
    Et pour la structure et la concentration, le seuil unique et les deux catégories (seuil atteint = Dominante / Concentré), sans classe faible / moyenne / forte
    Et les nombres sont en français (virgule, espaces), sans tableau brut ; rien n'est recalculé ni inventé
    Et si interne/typologie_departements.csv existe, la répartition des départements par classe déjà attribuée est ajoutée
    Et un fichier absent ou incomplet produit un message avec la marche à suivre, jamais un arrêt
```

## 11. Cartes « profils et PCS » : de la région au département (08h)

Script séparé (08h, fonctions 00j), sans modification des cartes existantes.
Deux pages dans `typologie_territoriale/cartes/` : `carte_typologie_pcs.html`
(interactive) et `carte_typologie_pcs_courriel.html` (sans aucun contenu actif).

Sources, toutes des tables de **diffusion** : `typologie_territoriale/diffusion/typologie_departements.csv`
(profils, totaux de départs publiés), `departs_pcs/diffusion/departs_pcs_{departement,region,france}.csv`
(08c, secret cellule par cellule, primaire puis secondaire par bloc territoire),
`departs_cs1/diffusion/departs_cs1_france.csv` (total France), nomenclature PCS locale.

Règle du **top des PCS** (`TYPO_PCS_TOP_N`, 5 par défaut) : classement par
`departs_central` décroissant parmi les seules cellules diffusables du
territoire, ex aequo par code PCS croissant, au plus N lignes, jamais le
complément. Garde-fou de différenciation : le total du territoire étant
publié, le top n'est diffusé que si le territoire compte zéro ou au moins
deux cellules PCS masquées (le secret secondaire du 08c le garantit ; une
seule cellule masquée = top retenu avec motif). Le nombre de PCS non
diffusées est affiché comme un compte. Parts : départs de la PCS / total
publié du territoire (département : typologie ; région : somme des totaux
départementaux publiés, NA dès qu'un département de la région est masqué ;
France : table cs1 France). Spécificité : part de la PCS dans les départs du
département face à sa part dans les départs nationaux.

Niveau régional : la typologie reste départementale. Une région est colorée
par le **profil le plus fréquent parmi ses départements** (égalité = « Profils
à égalité », gris clair), avec le compte des départements par profil et le
compte des départements non diffusés ; elle n'a jamais de profil propre.

```gherkin
Fonctionnalité: Explorer les métiers concernés par les départs, de la région au département, sans révéler une cellule protégée

  Scénario: [SPEC-TYPO-080] Classer les PCS d'un territoire
    Étant donné la table de diffusion PCS d'un territoire
    Quand le top est construit
    Alors il contient au plus N PCS diffusables, classées par départs estimés décroissants, ex aequo par code
    Et chaque ligne porte le libellé officiel, la grande catégorie, les départs (estimation centrale et fourchette), la part des salariés susceptibles de partir et la part dans les départs du territoire
    Et le nombre de PCS non diffusées est un compte, jamais une valeur

  Scénario: [SPEC-TYPO-081] Ne jamais permettre la différenciation
    Étant donné un territoire dont exactement une cellule PCS est masquée
    Quand le top est construit
    Alors aucun top n'est diffusé pour ce territoire, avec le motif « secret »
    Et aucune valeur masquée ne figure dans la page, le JSON ni les infobulles

  Scénario: [SPEC-TYPO-082] Lire une région sans lui inventer un profil
    Quand la carte régionale est produite
    Alors chaque région est colorée par le profil le plus fréquent de ses départements, ou « Profils à égalité »
    Et l'infobulle et le panneau donnent le compte des départements par profil et le compte des non diffusés
    Et un clic sur la région passe au niveau départemental, ses départements mis en évidence, avec son top de PCS

  Scénario: [SPEC-TYPO-083] Lire un département et colorer la carte par une PCS
    Quand un département est cliqué
    Alors le panneau donne son profil, ses chiffres, son top de PCS en barres et la spécificité de chaque PCS face à la France
    Et cliquer une PCS du top recolore tous les départements selon les départs de cette PCS (classes à bornes rondes, gris = non diffusé, blanc = aucun salarié observé)
    Et un bouton ramène aux profils
    Et la page suit la disposition de SPEC-TYPO-065 : sur écran large, carte, panneau et légende tiennent dans la fenêtre, seul le panneau défile en interne ; sur écran étroit, carte au-dessus, défilement vers le panneau au clic

  Scénario: [SPEC-TYPO-084] Version courriel sans contenu actif
    Quand la version courriel est produite
    Alors elle ne contient ni script, ni attribut on*=, ni javascript:, ni bloc de données
    Et les deux cartes (régions, départements) portent des infobulles natives, et chaque territoire a un bloc dépliable natif avec son top de PCS
    Et la coloration par PCS, impossible sans script, y est annoncée comme réservée à la page interactive

  Scénario: [SPEC-TYPO-085] Produire sans toucher aux autres sorties
    Étant donné les tables de diffusion présentes
    Quand 08h est lancé (chaîne ou seul depuis la racine)
    Alors les deux pages sont écrites, les objets amont sont inchangés, une table absente produit un message avec le chemin attendu, jamais un arrêt
```

## 12. Pyramide des âges : un axe descriptif (relève et vague suivante)

L'intensité du renouvellement est déjà une conséquence de l'âge : un
indicateur « part des 55 ans et plus » la compterait deux fois. L'axe âge
décrit donc ce que l'horizon 2030 ne voit pas : **la relève** (jeunes salariés
en poste face aux seniors) et **la vague suivante** (salariés qui partiront
surtout après 2030). C'est un axe **descriptif**, comme la structure par CS :
il n'entre dans aucune des dix règles, ne change aucun des sept profils ni
aucune couleur de carte ; il ajoute une classe, des points d'attention et une
phrase.

Tranches (`tranches_stock_ages()`, 00h) : quatre tranches de clés fixes, dont
les bornes viennent de la configuration, jamais écrites en dur :

| Clé | Tranche | Bornes |
|---|---|---|
| `jeunes` | moins de `AGE_BORNE_JEUNES` ans | âge < `AGE_BORNE_JEUNES` |
| `milieu` | de `AGE_BORNE_JEUNES` à `AGE_MIN_BTS − 1` ans | hors champ |
| `vague` | de `AGE_MIN_BTS` à `AGE_SENIOR − 1` ans | dans le champ |
| `seniors` | `AGE_SENIOR` ans et plus | dans le champ |

Sources (`preparer_pyramide_departements()`, 00h) :

| Situation | Source | Tranches | `pyramide_source` |
|---|---|---|---|
| Pyramide complète | `stock_tranches_ages` (01c, extraction non préfiltrée) | les quatre | `tous_ages` |
| Repli | `bts_projete` (champ seulement) | `vague`, `seniors` | `champ` |
| Aucune source | — | aucune : colonnes absentes, rien n'est inventé | — |

Indicateurs (table interne ; en diffusion après secret) :

| Indicateur | Définition | Numérateur | Dénominateur |
|---|---|---|---|
| `pyramide_<clé>_pct` | forme de la pyramide (complète seulement) | effectif de la tranche | Σ des quatre tranches du département |
| `ratio_releve` | relève : jeunes salariés pour un senior (complète seulement) | effectif `jeunes` | effectif `seniors` (0 → NA) |
| `ratio_vague_seniors` | vague suivante face aux seniors, **au sein du champ** (les deux sources) | effectif `vague` | effectif `seniors` (0 → NA) |
| `pyramide_n_<clé>` | effectifs par tranche (interne seulement) | — | — |
| `classe_releve` | Relève fragile / intermédiaire / solide | `ratio_releve` | — |
| `classe_vague_suivante` | Faible / Modérée / Forte | `pyramide_vague_pct` | — |
| `pyramide_libelles` | libellés des quatre tranches, séparés par « \| » (cartes) | — | — |

Paramètres : `STOCK_TOUS_AGES_TRANCHES` (activer la pyramide complète),
`AGE_BORNE_JEUNES` (30 ; entier, 16 ≤ borne < `AGE_MIN_BTS`),
`TYPO_METHODE_PYRAMIDE` (`terciles` provisoires ou `fixes`),
`TYPO_SEUIL_RELEVE_FRAGILE` / `_SOLIDE` (méthode fixes, jeunes pour un senior),
`TYPO_SEUIL_VAGUE_FAIBLE` / `_FORTE` (méthode fixes, %). Même règle de borne
que les autres axes (borne incluse dans la classe supérieure), mêmes
départements retenus pour les terciles (plancher `TYPO_SEUIL_EFFECTIF_MIN`).
Les bornes effectives sont écrites dans `parametres_typologie.csv`.

```gherkin
Fonctionnalité: Décrire la pyramide des âges de chaque département sans changer les profils

  Scénario: [SPEC-TYPO-090] Définir les tranches à partir de la configuration
    Étant donné AGE_BORNE_JEUNES, AGE_MIN_BTS et AGE_SENIOR
    Quand les tranches de la pyramide sont définies
    Alors elles sont « moins de AGE_BORNE_JEUNES ans », « AGE_BORNE_JEUNES à AGE_MIN_BTS − 1 ans », « AGE_MIN_BTS à AGE_SENIOR − 1 ans », « AGE_SENIOR ans et plus », de clés jeunes, milieu, vague, seniors
    Et un changement de AGE_MIN_BTS ou de AGE_SENIOR déplace les bornes sans autre modification
    Et une borne des jeunes non entière, inférieure à 16 ou supérieure ou égale à AGE_MIN_BTS arrête la configuration

  Scénario: [SPEC-TYPO-091] Calculer la pyramide complète, la relève et la vague suivante
    Étant donné la pyramide tous âges d'un département (jeunes 20, milieu 50, vague 20, seniors 10)
    Quand la typologie est construite
    Alors pyramide_jeunes_pct = 20, pyramide_milieu_pct = 50, pyramide_vague_pct = 20, pyramide_seniors_pct = 10
    Et ratio_releve = 2 (deux jeunes pour un senior) et ratio_vague_seniors = 2
    Et un département sans senior a des ratios NA, jamais infinis
    Et pyramide_source vaut « tous_ages »

  Scénario: [SPEC-TYPO-092] Se replier sur le champ sans pyramide complète
    Étant donné le stock par tranche indisponible (STOCK_TOUS_AGES_TRANCHES faux, extraction préfiltrée, stock tous âges indisponible)
    Quand la typologie est construite avec bts_projete
    Alors seul ratio_vague_seniors est calculé, au sein du champ, et pyramide_source vaut « champ »
    Et pyramide_<clé>_pct, ratio_releve, classe_releve et classe_vague_suivante sont NA
    Et la phrase rédigée le dit (« au sein du champ … pyramide complète indisponible »)
    Et sans aucune source, aucune colonne de pyramide n'est créée et les textes sont inchangés

  Scénario: [SPEC-TYPO-093] Classer la relève et la vague suivante sans changer les profils
    Quand les classes de la pyramide sont attribuées
    Alors classe_releve vaut « Relève fragile » sous la borne basse, « Relève intermédiaire » entre les bornes, « Relève solide » à partir de la borne haute
    Et classe_vague_suivante vaut « Faible », « Modérée » ou « Forte » selon la même règle
    Et les bornes (terciles provisoires sur les départements retenus, ou seuils fixes) sont écrites dans parametres_typologie.csv
    Et profil_typologie, motif_expertise, les classes de poids, d'intensité et de volume et les frontières sont identiques avec ou sans pyramide

  Scénario: [SPEC-TYPO-094] Signaler la relève fragile et la vague après 2030
    Étant donné un département à relève fragile et à intensité Élevée
    Alors ses points d'attention contiennent « relève fragile » avec le nombre de jeunes pour un senior
    Étant donné un département à vague suivante Forte et à intensité Faible
    Alors ses points d'attention contiennent « vague de départs importante après 2030 » avec la part des AGE_MIN_BTS à AGE_SENIOR − 1 ans
    Et la situation rédigée contient une phrase sur la pyramide (ou sur le champ en repli), construite par gabarit fixe

  Scénario: [SPEC-TYPO-095] Protéger la pyramide par le secret statistique
    Étant donné les cellules département × tranche, avec leur nombre d'entreprises et la part de l'entreprise dominante
    Quand la diffusion est produite
    Alors la règle Insee (primaire puis secondaire, bloc = département dont le total est publié) est appliquée aux tranches
    Et dès qu'une tranche d'un département est masquée, toute sa pyramide (parts, ratios, classes) est NA et pyramide_masquee est vrai
    Et les points d'attention et la situation ne citent alors aucune tranche
    Et les effectifs par tranche (pyramide_n_*) et le détail département × grande CS × tranche restent en interne/
    Et un département entièrement masqué n'a aucune valeur de pyramide
```

## 13. Exposition au recrutement difficile : les « deux horloges » (08i)

La typologie lit l'**horloge démographique** : combien de salariés partent, et à
quel rythme. Cette section ajoute l'**horloge du recrutement** : les départs d'un
département portent-ils sur des métiers difficiles à recruter ? Un département
n'est vraiment exposé que si les deux horloges sonnent ensemble.

**Principe.** La tension est une **propriété nationale du métier** : elle est
mesurée par FAP en France entière, sans aucune maille territoriale. Elle n'est
jamais attribuée au département. On écrit « ses départs portent sur des métiers en
tension en France », jamais « le département est en tension ». Le pont entre les
deux nomenclatures est la PCS : la liste des PCS stratégiques donne, pour chacune,
sa FAP et les indicateurs de tension de cette FAP. SPEC-TYPO-020 à 023 restent
vraies : aucun départ au bassin, aucune correspondance FAP → grande CS.

**Source** (`FICHIER_PCS_FAP_TENSION`, hors dépôt ; gabarit fictif
`data/templates/pcs_fap_tension_template.xlsx`). C'est un classeur Excel ; on lit la
feuille `TENSION_FEUILLE` (« Sheet1 »), une ligne par PCS stratégique. Colonnes
(noms réglables par `TENSION_COLS` et `TENSION_COMPOSANTES`) :
- `PCS`, `Lib PCS`, `FAP228`, `LIB_FAP228` ;
- `poids_pcs` (poids de la PCS dans sa FAP : information, jamais une pondération) ;
- `indice_confiance` (qualité du rattachement PCS → FAP) ;
- `ind_tension_d` (indicateur synthétique) ;
- sept composantes : `ind_int_emb_d`, `ind_main_oeuvre_d`, `ind_dur_empl_d`,
  `ind_cond_travail_d`, `ind_mismatch_geo_d`, `ind_inattract_salariale_d`,
  `ind_specif_form_empl_d` ;
- `emploi_fap_toussecteurs` et `emploi_pcs_défense` sont lues mais non utilisées.

Les indicateurs sont des **rangs** (quintiles, ou déciles) calculés sur toutes les
FAP. Depuis l'édition 2023, la Dares affine le niveau le plus fort en deux
sous-classes, **5.1 et 5.2** (5.2 la plus forte). Règle de lecture :
- chaque valeur est ramenée à **une décimale** avant contrôle (absorbe les
  imprécisions de virgule flottante, par exemple 5,0999 → 5.1) ; les valeurs ainsi
  corrigées sont comptées en console, jamais en silence ;
- une valeur admise est un entier de 1 à `TENSION_RANG_MAX`, ou le niveau maximal suivi
  d'une décimale de `TENSION_SOUS_NIVEAUX` (par défaut 1 et 2) ; toute autre valeur
  arrête l'étape ;
- `rang_*` est le **niveau entier** (5.1 → 5) : c'est lui qui sert au seuil, aux
  variantes de robustesse, à la composante dominante et aux classes ; `rang_*_fin`
  garde la valeur fine pour l'affichage et la matrice. Les ex aequo entre composantes
  sont départagés par la valeur fine, puis par l'ordre de `TENSION_COMPOSANTES`.

**Indicateurs** (fonctions `00k_fonctions_tension.R`) :

| Indicateur | Définition |
|---|---|
| `en_tension` (métier) | rang de `ind_tension_d` ≥ `TENSION_SEUIL_RANG` (par défaut 4 sur 5 : tension forte ou très forte) |
| composante dominante, levier (métier) | composante au rang le plus élevé, retenue seulement si ce rang atteint le seuil ; ex aequo : ordre de `TENSION_COMPOSANTES` ; chaque composante porte un levier rédigé |
| `faible_confiance` (métier) | `indice_confiance` < `TENSION_CONFIANCE_MIN` ; si ce paramètre vaut NULL, le plus bas niveau observé (s'il existe au moins deux niveaux) ; confiance manquante = faible |
| A, B, C (département) | départs (08c, exacts) des PCS stratégiques en tension (A), stratégiques hors tension (B), et de toutes les autres PCS, y compris les stratégiques de tension inconnue (C) |
| `exposition_pct` | 100 × A / (A + B) : sur 100 départs des métiers stratégiques, combien concernent des métiers en tension |
| `couverture_pct` | 100 × (A + B) / (A + B + C) : poids des métiers stratégiques dans les départs du département |
| `exposition_france_pct` | même calcul sur la France entière (repère) |
| `classe_exposition` | Faible / Modérée / Forte : bornes `TENSION_METHODE_CLASSES` (terciles provisoires sur les départements retenus, ou seuils fixes `TENSION_SEUIL_EXPO_FAIBLE` / `_FORTE`) ; borne incluse en haut ; NA si couverture < `TENSION_COUVERTURE_MIN_PCT` |
| `robustesse` | `robuste` si la classe reste la même pour les cinq variantes ; sinon `à confirmer`, avec les causes |
| levier du département | composante dominante la plus fréquente dans A, pondérée par les départs |
| `deux_horloges` | profil « Pôle majeur à renouveler rapidement » ou « Renouvellement rapide à surveiller » ET exposition Forte |

Les cinq variantes de robustesse appliquent les **mêmes bornes** que le calcul
central :
- `departs_bas` ;
- `departs_haut` ;
- seuil de rang + 1 ;
- seuil de rang − 1 ;
- sans les rattachements de faible confiance.

Les départements retenus pour les terciles sont ceux dont la couverture atteint le
minimum et l'effectif du champ atteint `TYPO_SEUIL_EFFECTIF_MIN`.

**Matrice des métiers stratégiques (France).**
- x = part des salariés du champ susceptibles de partir (08c France) ; y = rang de
  tension.
- Repère x = la même part pour l'ensemble du champ en France ; repère y = le seuil
  de rang ; y porte la valeur fine (5.1 et 5.2 graduées quand elles existent).
- Quadrants : « Métiers critiques » (départs au moins au repère et en tension),
  « Relève à organiser » (départs au moins au repère, hors tension), « Recrutement
  déjà difficile » (sous le repère, en tension), « Sous contrôle ».
- Faible confiance = symbole creux.

```gherkin
Fonctionnalité: Lire, avec la typologie, si les départs portent sur des métiers difficiles à recruter

  Scénario: [SPEC-TYPO-100] Lire et contrôler la source de tension
    Étant donné le classeur PCS → FAP avec ses indicateurs de tension
    Quand le 08i le lit
    Alors seule la feuille TENSION_FEUILLE est lue, tout en texte puis converti sous contrôle
    Et une feuille absente, une colonne absente, une PCS en double, ou un rang qui n'est ni un entier de 1 à TENSION_RANG_MAX ni un sous-niveau admis du niveau maximal (TENSION_SOUS_NIVEAUX : 5.1, 5.2) arrêtent l'étape avec un message qui nomme la colonne et les PCS en cause
    Et une valeur ramenée à une décimale (5,0999 → 5.1) est acceptée et comptée en console ; le niveau entier sert au seuil, la valeur fine à l'affichage et à la matrice
    Et une colonne cs1, grande CS ou tension départementale arrête l'étape
    Et un rang manquant donne une tension inconnue, comptée à part, jamais 0
    Et les PCS de la liste absentes des départs de la Base sont listées en console et dans la table interne

  Scénario: [SPEC-TYPO-101] Garder la tension comme propriété nationale du métier
    Quand l'exposition est calculée
    Alors aucune colonne ne qualifie la tension d'un département
    Et chaque texte dit « métiers en tension en France »
    Et aucune correspondance FAP → grande CS n'est construite

  Scénario: [SPEC-TYPO-102] Calculer l'exposition et la couverture
    Étant donné un département où A = 30, B = 70 et C = 100 départs
    Alors exposition_pct = 30 et couverture_pct = 50
    Et le repère France est calculé de la même façon sur l'ensemble des départements
    Et une couverture inférieure à TENSION_COUVERTURE_MIN_PCT donne une classe NA (« non interprétée »)
    Et aucun départ n'est recalculé : les départs sont ceux du 08c

  Scénario: [SPEC-TYPO-103] Classer l'exposition et tester sa robustesse
    Quand les classes sont attribuées
    Alors Faible, Modérée, Forte suivent les bornes de TENSION_METHODE_CLASSES, écrites dans parametres_exposition_tension.csv
    Et la classe est recalculée avec departs_bas, departs_haut, le seuil de rang ± 1 et sans les rattachements de faible confiance, sur les mêmes bornes
    Et robustesse vaut « robuste » si aucune variante ne change la classe, sinon « à confirmer » avec les causes (« hypothèse de départ », « seuil de tension », « rattachement PCS → FAP »)

  Scénario: [SPEC-TYPO-104] Placer les métiers stratégiques dans la matrice nationale
    Quand la matrice est produite
    Alors chaque métier diffusable au niveau France reçoit un quadrant parmi les quatre
    Et un métier dont la cellule France est masquée n'est compté qu'en nombre
    Et le PNG distingue les rattachements de faible confiance et ne nomme que les métiers critiques

  Scénario: [SPEC-TYPO-105] Rédiger les phrases et les leviers par gabarits fixes
    Quand les textes sont produits
    Alors chaque département diffusé reçoit une phrase : exposition, repère France, classe, robustesse, couverture, levier le plus fréquent
    Et la fiche porte trois messages (deux horloges, métiers critiques, levier dominant), « Comment lire » et « Ce que cette page ne dit pas » (ni une prédiction de pénurie, ni une tension mesurée dans le département)
    Et aucun texte n'est généré : uniquement des gabarits et des règles

  Scénario: [SPEC-TYPO-106] Protéger l'exposition par le secret statistique
    Étant donné les trois parts A, B, C d'un département, dont le total est publié
    Quand la diffusion est produite
    Alors l'exposition n'est publiée que si chaque part non vide satisfait la règle Insee (salariés, entreprises, dominance), sinon rien de l'exposition n'est publié pour ce département
    Et elle n'est pas publiée si une part contient exactement une cellule PCS masquée dans la diffusion du 08c (différenciation)
    Et un département masqué dans la diffusion de la typologie n'a aucune valeur
    Et la diffusion ne contient que des pourcentages, des classes et des textes, jamais un effectif ; les métiers cités sont des cellules PCS diffusables

  Scénario: [SPEC-TYPO-107] Montrer l'exposition sur la carte des profils, sans rien changer sans elle
    Étant donné diffusion/exposition_tension_departements.csv à côté de la table de diffusion de la typologie
    Quand le 08g produit les cartes
    Alors un département d'exposition Forte est hachuré (motif SVG, aussi dans la version courriel sans script), la couleur restant celle de son profil
    Et la légende ajoute une entrée « hachures » avec le nombre de départements, le détail ajoute une ligne et le panneau la phrase
    Et un département masqué n'est jamais hachuré et n'embarque aucune valeur d'exposition
    Et sans ce fichier, ou sans le classeur de tension (message, jamais d'arrêt), les pages et les sorties sont celles d'avant ; le PNG du 08g reste inchangé
```

Sorties (08i) :
- `interne/exposition_tension_departements.csv`, `interne/metiers_strategiques_tension.csv`,
  `interne/parametres_exposition_tension.csv` ;
- `diffusion/exposition_tension_departements.csv`, `diffusion/metiers_strategiques_tension.csv`,
  `diffusion/matrice_metiers_tension.png` ;
- `diffusion/fiche_exposition_tension.html` : une page, sans script, pour une lecture en une minute.


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
  `seuils_classes()`, `classer_par_seuils()`, `classer_poids_base()`,
  `calculer_structure_cs()`, `identifier_dominance_cs()`,
  `calculer_concentration_departs()`, `identifier_cs_volume_max()`,
  `identifier_cs_taux_max()`, `attribuer_profil_typologie()` (profil + motif),
  `points_attention_typologie()`, `rediger_situation_departement()`,
  `rediger_synthese_profils()`, `profils_definitions()` (`DEFINITIONS_PROFILS`),
  `construire_justification_profil()`, `construire_typologie_departements()`,
  `controler_typologie()`, `appliquer_secret_typologie()`, `synthese_profils()`,
  `joindre_signal_tension_localise()`, `png_matrice_typologie()`,
  `proposer_seuils_typologie()` (aide au réglage, SPEC-TYPO-042),
  `tranches_stock_ages()`, `affecter_tranche_stock()`,
  `indicateurs_secret_effectif()`, `preparer_pyramide_departements()`,
  `indicateurs_pyramide()` (pyramide des âges, SPEC-TYPO-090 à 095)
- `R/01_preparation/01c_stock_tous_ages.R` — `stock_tranches_ages`
- `R/08_territoires/08f_typologie_departements.R`
- `R/00_config_fonctions/00i_fonctions_carte_typologie.R` —
  `lire_typologie_diffusion()`, `preparer_carte_typologie()`,
  `controler_carte_typologie()`, `lignes_detail_typologie()`,
  `donnees_json_typologie()`, `generer_carte_typologie()`,
  `generer_carte_typologie_courriel()`, `png_carte_typologie()`,
  `COULEURS_PROFILS`, `LIBELLES_CARTE_TYPO` (SPEC-TYPO-060 à 064)
- `R/08_territoires/08g_carte_typologie.R` (relançable seul)
- `R/00_config_fonctions/00k_fonctions_tension.R` — `parametres_tension()`,
  `lire_pcs_fap_tension()`, `tension_metiers()`, `exposition_departements()`,
  `appliquer_secret_exposition()`, `matrice_metiers_tension()`,
  `rediger_exposition_departement()`, `messages_fiche_tension()`,
  `png_matrice_metiers_tension()`, `generer_fiche_exposition_tension()`
  (SPEC-TYPO-100 à 107) ; `R/08_territoires/08i_exposition_tension.R`
- `R/00_config_fonctions/00_config.R` — section typologie, `GENERER_CARTE_TYPOLOGIE`
- `data/templates/tensions_fap_territoires_template.csv`

Tests : `tests/testthat/test-typologie-territoriale.R` et
`tests/testthat/test-carte-typologie.R` (chaque bloc cite ses identifiants).
Correspondance détaillée dans `09_matrice_tracabilite.md`.
