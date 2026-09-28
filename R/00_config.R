# ==============================================================================
# 00_config.R — Paramètres et chemins partagés par toute la chaîne
# Rôle : centraliser ce qui se règle. Ne calcule rien.
# ==============================================================================
DIR_DATA    <- "data"
DIR_SORTIES <- "sorties"
dir.create(DIR_SORTIES, showWarnings = FALSE)

FICHIER_DREES  <- file.path(DIR_DATA, "departretraite_parcsp.csv")

# --- Source des données BTS (script 01) ---------------------------------------
# SOURCE_BTS : "test" = table simulée en mémoire (défaut, développement) ;
#              "parquet" = lecture de la vraie extraction via Arrow (lazy).
SOURCE_BTS   <- "test"
FICHIER_BTS  <- file.path(DIR_DATA, "bts_2024.parquet")   # extraction BTS
FICHIER_SIREN<- file.path(DIR_DATA, "liste_siren_bitd.txt") # périmètre BITD (1 SIREN/ligne)
# --- Âge minimal du champ étudié : SEULE SOURCE DE VÉRITÉ -----------------------
# Toute la chaîne et toutes les restitutions en dépendent : filtre Arrow poussé
# au disque (01), table test (01), tranches d'âge, dénominateurs (part des
# seniors = AGE_SENIOR+ / AGE_MIN_BTS+), titres, axes, tableaux, fiches, tests.
# Modifier UNIQUEMENT cette valeur puis relancer main.R suffit.
# Valeurs usuelles testées : 43, 44, 45. Plage autorisée dans la version actuelle :
# de 43 (borne basse des sources externes d'invalidité, TRANCHES_DEFAUT) à la
# borne haute de la première tranche d'âge (BORNES_SUP_TRANCHES[1] = 48) —
# contrôlé en fin de fichier, arrêt explicite sinon.
# La valeur 45 n'est que le défaut ACTUEL, pas une règle méthodologique.
AGE_MIN_BTS  <- 45
if (!is.numeric(AGE_MIN_BTS) || length(AGE_MIN_BTS) != 1 || is.na(AGE_MIN_BTS) ||
    AGE_MIN_BTS != round(AGE_MIN_BTS))
  stop("AGE_MIN_BTS doit être un entier unique (âge en années révolues) ; reçu : ",
       paste(deparse(AGE_MIN_BTS), collapse = ""), ". Les contrôles de plage sont en fin de fichier.")
# Libellés du champ, dérivés (ne jamais écrire l'âge en dur dans un texte) :
LIB_CHAMP       <- sprintf("%d ans et +", AGE_MIN_BTS)     # forme courante
LIB_CHAMP_LONG  <- sprintf("%d ans et plus", AGE_MIN_BTS)  # forme rédigée
LIB_CHAMP_COURT <- sprintf("%d+", AGE_MIN_BTS)            # forme compacte (axes, légendes)
# Noms des colonnes DANS LE PARQUET (à adapter au schéma réel : voir schema()).
# Elles seront renommées vers le contrat interne (siren, sexe, age_2024, pcs).
COL_BTS <- c(siren = "siren", sexe = "sexe", age = "age", pcs = "pcs")

# --- Agrégation PCS -> CS niveau 1 (script 01b) --------------------------------
# À PARAMÉTRER le jour du branchement de la vraie BTS. Le repli PCS ne dépend
# que du PREMIER chiffre du code (3=cadres, 4=prof. int., 5=employés, 6=ouvriers ;
# 1,2 = indépendants -> hors champ salarié). Les hors-champ sont EXCLUS (tracé).
AGGREGER_PCS <- TRUE    # table test désormais en PCS 4 chiffres (comme la
                        # vraie BTS) -> agrégation active. COL_PCS ci-dessous.
COL_PCS      <- "pcs"   # nom EXACT de la colonne code PCS dans votre extraction
# Table de correspondance 1er chiffre -> libellé cs1 (doit matcher les libellés
# utilisés partout ailleurs : correspondance DREES du 04, COEF_CSP_INVALIDITE).
PCS_VERS_CS1 <- c("3" = "Cadres",
                  "4" = "Prof. intermediaires",
                  "5" = "Employes",
                  "6" = "Ouvriers")
# Codes/1ers chiffres à EXCLURE explicitement (indépendants, non renseigné...).
# Tout ce qui n'est pas dans PCS_VERS_CS1 est de toute façon exclu ; cette liste
# sert surtout à documenter l'intention.
PCS_HORS_CHAMP <- c("1", "2")   # agriculteurs, artisans/commerçants/chefs d'ent.

GRAINE         <- 2024        # reproductibilité de la table test
N_TEST         <- 6000        # taille de la table test
AGE_MAX_TEST   <- 66          # âge maximal simulé (table test : AGE_MIN_BTS..AGE_MAX_TEST)
ANNEES_LISSAGE <- 2018:2020   # fenêtre de lissage DREES — à arbitrer au vu du
                              # tableau de stabilité affiché par 03 (Covid 2020)

# --- Paramètres du modèle de projection (script 04) ---------------------------
# Construction en 2 niveaux (cf. §5), au choix via SCENARIO :
#   "A" = LÉGISLATION SEULE : départs par le seul âge conjoncturel de
#         liquidation (age_conj), sans sas ni événement de vie. Socle
#         réglementaire opposable, indépendant de toute hypothèse démographique.
#   "B" = A + ÉVÉNEMENTS DE VIE : ajoute le sas (qui fait passer de age_conj à
#         l'âge de sortie d'emploi μ), l'invalidité (flux par CSP x âge) et le
#         décès (quotients Insee). C'est le scénario complet.
# Le script 04 calcule TOUJOURS les deux jeux (p_A et p_B) pour permettre le
# tableau de contribution en « pelures » ; SCENARIO fixe seulement lequel
# alimente par défaut les résultats (05) et le graphique (06).
SCENARIO <- "B"
HORIZON <- 6              # 2024 -> 2030

# Correctif réglementaire δ : les μ sont mesurés sur 2018-2020, avant la
# réforme 2023 et sa suspension (LFSS 2026). Ancrage bas = âge conjoncturel
# tous régimes le plus récent : 63,6 ans (chiffre actualisé ; l'édition 2025
# du panorama DREES donnait 62 ans et 9 mois fin 2023). Ce paramètre pilote
# δ = 63,6 − âge conjoncturel DREES 2018-2020 : le relever RETARDE les
# sorties et baisse les départs (~-10 % entre 62,75 et 63,6).
AGE_CONJ_TOUS_REGIMES_2023 <- 63.6
# Montée résiduelle attendue d'ici 2030 (calendrier suspendu : générations
# 1965-1969 passant de 63 à 64 ans + effets de comportement) — hypothèse haute.
MONTEE_RESIDUELLE_2030 <- 1.0

SIGMA_SORTIE   <- 2.5     # dispersion (écart-type, en années) des âges de
                          # sortie autour de μ ; à défaut de distribution
                          # publiée par CSP, fixé à 2,5 ans (sorties étalées
                          # ~55-67 ans) et testé en sensibilité.

# --- Invalidité : flux propre par CSP x âge, sur tout l'horizon ----------------
# Choix méthodologique (cf. §5) : l'invalidité est modélisée comme un flux
# distinct à TOUT âge, et non repliée dans μ. Pour éviter le double-compte avec
# la durée hors emploi DREES (qui inclut de l'invalidité), on RETIRE de μ la
# part de sas imputable à l'invalidité — voir script 04 (mu_sortie_corrige).
# Invalidité : construite comme base(âge, sexe) x coefficient(CSP).
#  - base(âge, sexe) : lue sur DONNÉES RÉELLES 2024 par le script 02c (EACR,
#    flux d'entrées en invalidité par âge et sexe). Repli ci-dessous si absent.
#  - coefficient(CSP) : seul paramètre à dire d'expert. Gradient ~1 à 3, calé
#    sur l'EIR-invalidité 2020 par diplôme (proxy CSP) et l'analogie du chômage
#    par CSP (Insee). Centré sur les professions intermédiaires (= 1,0).
COEF_CSP_INVALIDITE <- c("Cadres"               = 0.5,
                         "Prof. intermediaires" = 1.0,
                         "Employes"             = 1.3,
                         "Ouvriers"             = 1.9)
# Base de REPLI par tranche d'âge x sexe (niveau "moyen tous CSP"), utilisée
# NB : la première tranche commence à 43 ans = borne de la SOURCE (tables
# EIR/EACR et population active), PAS le champ : son taux s'applique aux
# premiers âges du champ (AGE_MIN_BTS à 49 ans). Ne pas l'aligner sur
# AGE_MIN_BTS (cela changerait la largeur du dénominateur reconstruit, donc le
# taux — modification méthodo). C'est aussi la borne BASSE admise pour AGE_MIN_BTS.
# UNIQUEMENT si le fichier EACR-invalidité est absent (mode dégradé). Les
# tranches ci-dessous servent aussi de tranches PAR DÉFAUT quand aucun fichier
# de population active n'est fourni (bornes incluses : borne_inf..borne_sup).
# (femmes > hommes : invalidité plus féminine, EIR 2020 ; croissante avec l'âge)
TRANCHES_DEFAUT <- tibble::tribble(
  ~borne_inf, ~borne_sup,
  43,         49,
  50,         54,
  55,         59,
  60,         64,
  65,         72
)
T_INVALIDITE_BASE <- tibble::tribble(
  ~sexe, ~borne_inf, ~taux,
  "H",   43, 0.0023,  "H", 50, 0.0036,  "H", 55, 0.0054,  "H", 60, 0.0068,  "H", 65, 0.0068,
  "F",   43, 0.0029,  "F", 50, 0.0046,  "F", 55, 0.0069,  "F", 60, 0.0086,  "F", 65, 0.0086
)
# Fichier EACR-invalidité (table I) au format natif, lu par le script 02c.
FICHIER_INVALIDITE  <- file.path(DIR_DATA, "incidence_invalidite.xlsx")
INVAL_ANNEE         <- 2024                    # millésime du flux d'entrées
INVAL_CAISSE        <- "CNAM, yc indépendants" # champ salariés privé + indép.
INVAL_CHAMP         <- "ddir"                  # droits directs (= somme cat1/2/3)
AGE_PLEIN_INVALIDITE <- 61   # au-delà : flux d'invalidité gelé (bascule retraite
                             # pour inaptitude à l'âge légal -> quasi-zéros EACR)

# --- Population active de référence = DÉNOMINATEUR des taux d'invalidité -------
# Principe (cf. §3) : on rapporte les entrées en invalidité à la population
# réellement EXPOSÉE au risque, c.-à-d. les ACTIFS (occupés + chômeurs), et non
# à la population totale. C'est le champ qui pouvait entrer en invalidité.
# IDÉAL : fichier Insee enquête Emploi 2024 -> déposez data/pop_active_insee.csv
#         (colonnes  age;sexe;actifs , sexe H/F), il primera automatiquement.
FICHIER_POP_ACTIVE <- file.path(DIR_DATA, "pop_active_insee.csv")
# À DÉFAUT : reconstruction approchée (cohorte x taux d'activité). PROVISOIRE,
# à remplacer par le fichier Insee pour la version finale.
COHORTE_PAR_SEXE   <- 410000     # taille approx. d'une génération / sexe (générations de la BTS)
# Taux d'activité par tranche d'âge et sexe (Insee 2024, ordres de grandeur) :
TAUX_ACTIVITE <- tibble::tribble(
  ~sexe, ~a_43_49, ~a_50_54, ~a_55_59, ~a_60_61, ~a_62plus,
  "H",    0.90,     0.88,     0.76,     0.50,     0.22,
  "F",    0.86,     0.84,     0.73,     0.48,     0.22
)

# Mortalité : classeur Insee « Quotients de mortalité par sexe et âge »
# (https://www.insee.fr/fr/statistiques/8560675). Format natif : 4 onglets
# {FR,FM}-{Femmes,Hommes}, en table large (1 ligne = 1 année, 1 col = 1 âge),
# quotients pour 100 000, 2 lignes de titre. Lu tel quel par le script 02b.
FICHIER_MORTALITE  <- file.path(DIR_DATA, "3_Quotients_mortalite.xlsx")
CHAMP_MORTALITE    <- "FR"    # "FR" = France entière ; "FM" = métropolitaine
ANNEE_MORTALITE    <- 2022    # millésime retenu (2022-2024 = provisoires "(p)")
# Secours si le fichier est absent (quotients plats — chaîne en mode dégradé) :
Q_DECES_ANNUEL <- c("H" = 0.0045, "F" = 0.0025)

# --- Paramètres de restitution graphique (script 06) --------------------------
ANNEE_REF_GRAPHIQUE <- 2024   # année d'affichage = millésime de la photo BTS
                              # (tout aligné sur 2024 : cohérence heatmap/barres)
# Tranches d'âge de restitution : la PREMIÈRE commence au bas du champ
# (AGE_MIN_BTS) ; seules les bornes HAUTES internes se règlent ici. Breaks et
# libellés en dérivent (« <AGE_MIN_BTS>-48 ans », « 49-54 ans », « 55-60 ans »,
# « 61 ans et + »). Un âge sous le champ tomberait en NA (visible).
BORNES_SUP_TRANCHES <- c(48, 54, 60)
BREAKS_TRANCHES <- c(AGE_MIN_BTS - 1, BORNES_SUP_TRANCHES, Inf)
LABELS_TRANCHES <- c(sprintf("%d-%d ans", c(AGE_MIN_BTS, head(BORNES_SUP_TRANCHES, -1) + 1),
                             BORNES_SUP_TRANCHES),
                     sprintf("%d ans et +", tail(BORNES_SUP_TRANCHES, 1) + 1))
# Discrétisation de p_central en classes de lecture (convention de restitution)
SEUIL_CERTAIN  <- 0.75        # p >= 0.75  -> « Départ certain d'ici 2030 »
SEUIL_PROBABLE <- 0.25        # 0.25-0.75  -> « Départ probable / envisageable »
MODE_GRAPHIQUE <- "attendu"   # "attendu" : parts espérées par cause (défaut)
                              # "classes" : classes individuelles par seuils

# --- Analyse des seniors (55+) par territoire (script 08) ----------------------
AGE_SENIOR <- 55              # borne basse de la population « senior » étudiée
# Secret statistique : une cellule (territoire x CS) portant moins de
# SEUIL_DIFFUSION salariés n'est pas diffusée (convention statistique publique).
SEUIL_DIFFUSION <- 20

# --- Fiches « chiffres clés » territoriales (script 09) ------------------------
# Une page HTML autonome par territoire (aucune dépendance : HTML/CSS écrits
# par R, barres en CSS). Lecture en une minute par un décideur non statisticien.
GENERER_FICHES   <- TRUE          # FALSE = étape 09 ignorée, chaîne inchangée
FICHES_MODE      <- "tous"        # "tous"      : tous les territoires du périmètre analysé
                                  # "selection" : les codes de FICHES_SELECTION
FICHES_SELECTION <- NULL          # codes (ex. c("03", "18", "33")) ; NULL en mode
                                  # "selection" = les codes de GEO_INTERET.
                                  # Doctrine : GEO_INTERET fixe le PÉRIMÈTRE d'analyse
                                  # (médianes, quadrant) ; FICHES_SELECTION dit seulement
                                  # pour QUI on édite une fiche.
FICHES_DIR       <- NULL          # NULL = sorties/fiches_<suffixe du zonage>/
FICHES_SEUIL_PROCHE <- NULL       # NULL = position « au-dessus / en dessous » de la
                                  # médiane (même règle que le quadrant) ; sinon un
                                  # nombre de POINTS (ex. 2) sous lequel on affiche
                                  # « proche de la médiane » — règle imprimée en pied de fiche

# ==============================================================================
# GÉOGRAPHIE — trois couches, sur le modèle de COL_BTS (fonctions : R/00c)
#   1. schéma SOURCE  : COL_GEO = noms RÉELS des colonnes du fichier reçu
#   2. zonage         : GEO_SOURCE (porté par le fichier) / GEO_ANALYSE (restitué)
#   3. contrat INTERNE: geo_code (texte), geo_nom, geo_type — seul connu de 01/04/08
# Workflow à réception d'une nouvelle table :
#   1. source("R/00_config.R"); source("R/00c_fonctions_geo.R")
#   2. inspecter_schema(FICHIER_BTS)   -> colonnes, types, exemples de valeurs
#   3. identifier la (les) colonne(s) géographique(s)
#   4. renseigner le BLOC « À ADAPTER » ci-dessous
#   5. source("main.R")
# ==============================================================================

# --- BLOC À ADAPTER à chaque extraction ---------------------------------------
GEO_ANALYSE <- "ze"           # zonage des restitutions : clé de GEO_ZONAGES
GEO_SOURCE  <- "ze"           # zonage porté par le fichier (= GEO_ANALYSE, sauf
                              # géographie plus fine, ex. "commune" -> passage)
COL_GEO <- c(                 # noms RÉELS dans le fichier (voir inspecter_schema)
  code = "ze",                # colonne du CODE (obligatoire sauf cas « nom seul »)
  nom  = NA                   # colonne du LIBELLÉ si elle existe, sinon NA
)                             # Exemple département à venir :
                              #   GEO_ANALYSE <- "departement"; GEO_SOURCE <- "departement"
                              #   COL_GEO <- c(code = "<à renseigner après inspection>", nom = NA)
GEO_CODE_LARGEUR <- NA        # zéros à gauche : 2 pour un département lu en
                              # numérique (« 1 » -> « 01 »), NA = aucun complément
# Périmètre de restitution : CODES des territoires retenus (NULL = tous). Un
# vecteur NOMMÉ  code = libellé  sert AUSSI de table de libellés, sans fichier :
#   GEO_INTERET <- c("8401" = "Val-des-Montagnes", "2402" = "Porte-de-Berry")
# Les observations hors liste sont écartées AVEC décompte ; tout code absent
# des données est signalé ; aucune correspondance = arrêt.
GEO_INTERET <- NULL

# --- Paramètres PAR ZONAGE : stables, à compléter seulement pour un nouveau
#     zonage (région, EPCI...) — jamais à chaque extraction ---------------------
GEO_ZONAGES <- list(
  ze          = list(libelle = "Zone d'emploi", un = "une zone d'emploi",
                     pluriel = "zones d'emploi", suffixe = "ze"),
  departement = list(libelle = "Département",   un = "un département",
                     pluriel = "départements",  suffixe = "departement"),
  region      = list(libelle = "Région",        un = "une région",
                     pluriel = "régions",       suffixe = "region"),
  commune     = list(libelle = "Commune",       un = "une commune",
                     pluriel = "communes",      suffixe = "commune")
)
# Référentiels LOCAUX  code;nom  (séparateur ';', codes en texte) : complètent
# geo_nom quand le fichier n'a que des codes. Facultatifs (repli : code affiché).
GEO_REFERENTIELS <- list(
  ze          = file.path(DIR_DATA, "ref_ze.csv"),
  departement = file.path(DIR_DATA, "ref_departement.csv"),
  region      = file.path(DIR_DATA, "ref_region.csv")
)
# Tables de passage LOCALES  code_source;code_cible[;nom_cible]  quand la
# source est plus fine que l'analyse. Obligatoires dans ce cas.
GEO_PASSAGES <- list(
  "commune->ze"          = file.path(DIR_DATA, "passage_commune_ze.csv"),
  "commune->departement" = file.path(DIR_DATA, "passage_commune_departement.csv")
)

# ==============================================================================
# CONTRÔLES DE COHÉRENCE DES PARAMÈTRES D'ÂGE — arrêt explicite, jamais un
# résultat incohérent. Un message = la règle + comment la lever.
# ==============================================================================
# (le type d'AGE_MIN_BTS est contrôlé dès sa définition, en tête de fichier)
if (!is.numeric(AGE_SENIOR) || length(AGE_SENIOR) != 1 || is.na(AGE_SENIOR) ||
    AGE_SENIOR != round(AGE_SENIOR))
  stop("AGE_SENIOR doit être un entier unique ; reçu : ",
       paste(deparse(AGE_SENIOR), collapse = ""), ".")
# Borne basse : les taux d'invalidité (EIR/EACR, population active) ne sont
# connus qu'à partir de la première tranche source ; en dessous, le 02c
# rabattrait les âges sur cette tranche sans le dire -> refus.
if (AGE_MIN_BTS < min(TRANCHES_DEFAUT$borne_inf))
  stop(sprintf(paste0("AGE_MIN_BTS = %d en dessous de la première tranche des sources ",
                      "d'invalidité (%d ans, TRANCHES_DEFAUT) : aucun taux ne couvrirait ",
                      "les %d-%d ans. Choisissez AGE_MIN_BTS >= %d ou fournissez des ",
                      "sources couvrant ces âges."),
               AGE_MIN_BTS, min(TRANCHES_DEFAUT$borne_inf), AGE_MIN_BTS,
               min(TRANCHES_DEFAUT$borne_inf) - 1, min(TRANCHES_DEFAUT$borne_inf)))
# Borne haute : la première tranche d'âge doit contenir au moins un âge.
if (AGE_MIN_BTS > BORNES_SUP_TRANCHES[1])
  stop(sprintf(paste0("AGE_MIN_BTS = %d incompatible avec les tranches configurées ",
                      "(première tranche jusqu'à %d ans). Adaptez BORNES_SUP_TRANCHES."),
               AGE_MIN_BTS, BORNES_SUP_TRANCHES[1]))
if (any(diff(BREAKS_TRANCHES) <= 0))
  stop("BORNES_SUP_TRANCHES doit être strictement croissant et > AGE_MIN_BTS.")
# Le champ doit contenir les seniors : sinon « part des AGE_SENIOR+ dans le
# champ » change de sens (toujours 100 %) et le quadrant n'a plus d'objet.
if (AGE_MIN_BTS >= AGE_SENIOR)
  stop(sprintf(paste0("AGE_MIN_BTS = %d >= AGE_SENIOR = %d : le champ doit commencer ",
                      "sous la borne senior (part des %d+ = %d+ / %d+)."),
               AGE_MIN_BTS, AGE_SENIOR, AGE_SENIOR, AGE_SENIOR, AGE_MIN_BTS))
if (AGE_SENIOR > BORNES_SUP_TRANCHES[length(BORNES_SUP_TRANCHES)])
  stop("AGE_SENIOR au-delà de la dernière borne des tranches : les fiches ne ",
       "sauraient plus quelles tranches sont « seniors ». Adaptez BORNES_SUP_TRANCHES.")
if (AGE_MIN_BTS >= AGE_MAX_TEST)
  stop(sprintf("AGE_MIN_BTS = %d >= AGE_MAX_TEST = %d : la table test serait vide.",
               AGE_MIN_BTS, AGE_MAX_TEST))