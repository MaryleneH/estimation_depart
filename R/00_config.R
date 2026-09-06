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
SOURCE_BTS   <- "parquet"
FICHIER_BTS  <- file.path(DIR_DATA, "bts_2024.parquet")   # extraction BTS
FICHIER_SIREN<- file.path(DIR_DATA, "liste_siren_bitd.txt") # périmètre BITD (1 SIREN/ligne)
AGE_MIN_BTS  <- 43        # borne d'âge du champ de l'étude (filtre poussé au disque)
# Noms des colonnes DANS LE PARQUET (à adapter au schéma réel : voir schema()).
# Elles seront renommées vers le contrat interne (siren, sexe, age_2024, pcs).
COL_BTS <- c(siren = "siren", sexe = "sexe", age = "age", pcs = "pcs")

# --- Agrégation PCS -> CS niveau 1 (script 01b) --------------------------------
# À PARAMÉTRER le jour du branchement de la vraie BTS. Le repli PCS ne dépend
# que du PREMIER chiffre du code (3=cadres, 4=prof. int., 5=employés, 6=ouvriers ;
# 1,2 = indépendants -> hors champ salarié). Les hors-champ sont EXCLUS (tracé).
AGGREGER_PCS <- TRUE    # table test désormais en PCS 4 chiffres (comme la
                        # vraie BTS) -> agrégation active. COL_PCS ci-dessous.
COL_PCS      <- "pcs"   # nom EXACT de la colonne code PCS dans BTS2024
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
# réforme 2023 et sa suspension (LFSS 2026). Ancrage bas : l'âge conjoncturel
# tous régimes atteint 62 ans et 9 mois fin 2023 (DREES, panorama éd. 2025).
AGE_CONJ_TOUS_REGIMES_2023 <- 62.75
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
COHORTE_PAR_SEXE   <- 410000     # taille approx. d'une génération / sexe (43-64)
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
ANNEE_REF_GRAPHIQUE <- 2026   # année d'affichage des âges (photo BTS : 2024)
BREAKS_TRANCHES <- c(-Inf, 48, 54, 60, Inf)
LABELS_TRANCHES <- c("43-48 ans", "49-54 ans", "55-60 ans", "61 ans et +")
# Discrétisation de p_central en classes de lecture (convention de restitution)
SEUIL_CERTAIN  <- 0.75        # p >= 0.75  -> « Départ certain d'ici 2030 »
SEUIL_PROBABLE <- 0.25        # 0.25-0.75  -> « Départ probable / envisageable »
MODE_GRAPHIQUE <- "attendu"   # "attendu" : parts espérées par cause (défaut)
                              # "classes" : classes individuelles par seuils
