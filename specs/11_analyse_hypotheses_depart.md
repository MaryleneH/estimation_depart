# 11 — Analyse « hypothèses de départ » (R/09_analyse)

Une analyse prête à être collée dans une note : un classeur Excel mis en forme, une feuille par
tableau, construit sur les **seules tables de diffusion** de la chaîne et sur des **gabarits fixes**.
Rien n'est recalculé par le modèle ; aucun texte n'est généré hors gabarit. La plage de projection est
celle du modèle : année de la photo BTS (`ANNEE_REF_GRAPHIQUE`) → année + `HORIZON`.

Quatre rubriques, dans l'ordre d'une note : hypothèses et critères, volume, répartition par métier,
répartition géographique (avec la dimension tension des métiers de la liste), puis une feuille de
priorités qui croise les trois.

```gherkin
Fonctionnalité: Produire l'analyse « hypothèses de départ » en un classeur prêt pour Word

  Scénario: [SPEC-ANA-001] Lancer l'analyse sans jamais arrêter la chaîne
    Étant donné GENERER_ANALYSE vrai et les objets bts_projete et departs_cs1$diffusion$france en mémoire
    Quand le script 09b est exécuté après le 08j
    Alors il écrit sorties/analyse_hypotheses_depart/<zonage>/ : analyse_hypotheses_depart.xlsx (si openxlsx ou writexl) et un CSV par tableau (référence)
    Et un prérequis facultatif absent (08c, 08f, 08i, 08j, synthèse 08) donne une feuille portant la raison, jamais un arrêt
    Et GENERER_ANALYSE faux ou un prérequis obligatoire absent donne un message, jamais un arrêt
    Et l'objet `analyse_hypotheses_depart` (tables, classeur, dossier) est disponible

  Scénario: [SPEC-ANA-002] Hypothèses et critères, lus dans la configuration
    Quand la feuille « 1 Hypothèses » est construite
    Alors chaque ligne porte l'hypothèse, la valeur retenue, la source, l'effet sur le résultat et ce qui la fait varier
    Et les lignes couvrent : champ (âge minimum, périmètre, année BTS), plage de projection, définition du départ, calendrier de fin de carrière (δ bas, central, haut), sas de fin de carrière (scénario), invalidité, décès, dispersion σ, secret statistique (trois seuils), et, si la tension est configurée, métiers suivis (nombre, libellé public), seuil de tension, écart à la France
    Et aucune valeur n'est saisie à la main : tout vient de 00_config et des objets du modèle (δ)

  Scénario: [SPEC-ANA-003] Volume : total, par tranche d'âge, seniors, catégorie sociale
    Quand les feuilles « 2a Volume », « 2b Par âge », « 2c Seniors », « 2d Par catégorie » sont construites
    Alors le total donne, pour les scénarios central, bas et haut : salariés du champ, départs attendus sur la plage, part susceptible de partir, départs par an en moyenne (total / HORIZON), et, au central, la décomposition retraite ou fin de carrière, invalidité, décès (mêmes mathématiques que 06 et 08)
    Et chaque ligne de tranche d'âge (BREAKS_TRANCHES), de seniors (AGE_SENIOR) et de catégorie sociale (diffusion 08d France) porte salariés, départs (central, basse, haute), part susceptible de partir, décomposition par cause, part des départs du champ
    Et chaque cellule suit la règle de secret (primaire puis secondaire, masquer_cellules) : masquée = NA et « non diffusé (secret statistique) », jamais zéro ; une tranche sans salarié est masquée comme une autre (SPEC-PUB-007)

  Scénario: [SPEC-ANA-004] Répartition par métier : volume, intensité, métiers de la liste, noyau dur
    Étant donné la diffusion France par PCS du 08c et, si elle existe, la liste de métiers du 08i
    Quand les feuilles « 3a Métiers volume », « 3b Métiers part », « 3c Métiers de la liste », « 3d Noyau dur » sont construites
    Alors « 3a » classe les ANALYSE_TOP_N métiers diffusés par départs attendus décroissants et « 3b » par part susceptible de partir décroissante parmi les métiers d'au moins ANALYSE_EFFECTIF_MIN_PART salariés ; libellé PCS depuis la nomenclature si elle est disponible, sinon « PCS <code> »
    Et « 3c » reprend la matrice du 08i : situation (quatre quadrants, puis non diffusé, tension inconnue, absente), métier, famille, tension en France, dimension principale de la tension et levier d'action (seulement en tension), confiance du rattachement, départs, part ; « 3d » n'en garde que la situation « départs rapides et métier en tension », par part décroissante
    Et aucune valeur d'un métier masqué n'apparaît

  Scénario: [SPEC-ANA-005] Répartition géographique : régions, départements, exposition, liste x régions, seniors
    Étant donné les diffusions région et département du 08d et, si elles existent, la typologie (08f), l'exposition (08i, départements et régions) et la table métier x territoire (08j)
    Quand les feuilles « 4a Régions », « 4b Départements », « 4c Exposition régions », « 4d Liste x régions », « 4e Seniors par territoire » sont construites
    Alors un territoire est totalisé sur ses catégories sociales seulement si aucune cellule n'est masquée (sinon NA et « non diffusé ») ; il porte salariés, départs (central, basse, haute), part, rang par volume et rang par part parmi les territoires diffusés, tri par part décroissante (non diffusés en fin)
    Et les départements et les régions sont enrichis, quand les objets existent, du profil de la typologie, de la classe d'exposition, de sa robustesse, de la part des remplacements difficiles et du plancher de départs dans les métiers suivis en tension ; un territoire dont l'exposition n'est pas diffusée le dit
    Et « 4d » liste, parmi les métiers de la liste, les couples métier x région « plus forte qu'en France » classés par écart décroissant (ANALYSE_TOP_N)
    Et « 4e » reprend la synthèse diffusable du 08 : part des seniors, départs et part susceptible de partir des seniors par territoire du zonage

  Scénario: [SPEC-ANA-006] Priorités : croiser métier, territoire et levier
    Étant donné le noyau dur (« 3d ») et la table métier x territoire du 08j
    Quand la feuille « 5 Priorités » est construite
    Alors chaque métier du noyau dur porte sa tension en France, la dimension principale de sa tension (composante dominante France Travail), son levier d'action, sa part et ses départs en France, et ses ANALYSE_N_REGIONS_PRIORITES premières régions diffusées par part décroissante (« Nom (part %) »)
    Et sans table du 08j, les colonnes de régions sont vides

  Scénario: [SPEC-ANA-007] Classeur prêt à coller dans Word
    Quand le classeur est écrit avec openxlsx
    Alors chaque feuille porte en ligne 1 le titre du tableau, en ligne 2 sa note de lecture, et le tableau à partir de la ligne 4 : en-tête colorée, volets figés, largeurs adaptées, sans quadrillage ; les colonnes « (%) » à une décimale, les personnes en entiers avec séparateur
    Et la feuille « Lisez-moi » rappelle la plage, le champ, la règle de secret, la liste des feuilles et la façon de coller un tableau dans Word
    Et une feuille « Définitions », placée juste après, définit chaque colonne du classeur (terme, définition, feuilles où il apparaît), les intitulés des colonnes étant explicites (« Hypothèse basse (départs retardés) », « Poids dans l'ensemble des départs (%) », « Rang par nombre de départs », « Départs attendus dans les métiers suivis en tension (minimum) »…) ; chaque note de feuille y renvoie
    Et une feuille « Paramètres » trace les paramètres utilisés
    Et sans openxlsx, writexl écrit les tableaux sans mise en forme (message) ; sans les deux, message et les CSV restent la référence

  Scénario: [SPEC-ANA-008] Garanties de diffusion
    Quand les tableaux sont assemblés
    Alors aucun effectif visible n'est inférieur à SECRET_MIN_SALARIES (contrôle bloquant)
    Et le libellé public de la liste (TENSION_LIBELLE_LISTE) est appliqué à tous les textes et en-têtes ; aucune chaîne « stratégique » ne subsiste
    Et aucune entreprise n'est nommée ; aucun fichier n'est écrit sous interne/
```

## Sorties

`sorties/analyse_hypotheses_depart/<zonage>/` : `analyse_hypotheses_depart.xlsx` ; `01_hypotheses.csv`,
`02a_volume.csv`, `02b_par_age.csv`, `02c_seniors.csv`, `02d_par_categorie.csv`, `03a_metiers_volume.csv`,
`03b_metiers_part.csv`, `03c_metiers_liste.csv`, `03d_noyau_dur.csv`, `04a_regions.csv`, `04b_departements.csv`,
`04c_exposition_regions.csv`, `04d_liste_regions.csv`, `04e_seniors_territoires.csv`, `05_priorites.csv`,
`parametres.csv`, `definitions.csv`.

Documents d'accompagnement (docs/, rédigés à la main, hors chaîne) : `guide_commentaires_analyse_hypotheses.docx`
(suggestions de commentaires selon les phénomènes observés, feuille par feuille) et
`exemple_analyse_territoire_fictif.docx` (exemple d'analyse rédigée, façon « Insee Première », sur un territoire fictif).

## Traçabilité

Code : `R/09_analyse/09a_fonctions_analyse_hypotheses.R` (fonctions pures : `parametres_analyse()`,
`table_hypotheses_analyse()`, `table_volume_total()`, `table_volume_groupe()`, `table_volume_cs1()`,
`table_metiers_classement()`, `table_metiers_liste_analyse()`, `table_noyau_dur_analyse()`,
`table_territoires_cs1()`, `enrichir_territoires_analyse()`, `table_liste_regions_analyse()`,
`table_priorites_analyse()`, `table_synthese_seniors_analyse()`, `table_definitions_analyse()`, `controler_tables_analyse()`,
`ecrire_classeur_analyse()`), `R/09_analyse/09b_analyse_hypotheses_depart.R` (script de la chaîne, après
le 08j). Paramètres : `GENERER_ANALYSE`, `ANALYSE_TOP_N`, `ANALYSE_EFFECTIF_MIN_PART`,
`ANALYSE_N_REGIONS_PRIORITES` (00_config).

Tests : `tests/testthat/test-analyse-hypotheses.R` (fixtures synthétiques) ;
`tests/testthat/test-exposition-tension.R` « SPEC-TYPO-107 — 08i sur la chaîne … » (09b sur la chaîne de test).
