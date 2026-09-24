# Helper testthat : localise la racine du projet et source la couche géo.
# Les tests ne lancent PAS la chaîne (sauf test-non-regression) : ils testent
# les fonctions pures de R/00c_fonctions_geo.R sur des tables en mémoire.
racine_projet <- function() {
  d <- normalizePath(getwd())
  for (i in 1:4) {
    if (file.exists(file.path(d, "main.R"))) return(d)
    d <- dirname(d)
  }
  stop("Racine du projet introuvable (main.R) depuis ", getwd())
}
RACINE <- racine_projet()
DIR_SORTIES <- tempdir()
GEO_ZONAGES <- list(
  ze          = list(libelle = "Zone d'emploi", un = "une zone d'emploi",
                     pluriel = "zones d'emploi", suffixe = "ze"),
  departement = list(libelle = "Département",   un = "un département",
                     pluriel = "départements",  suffixe = "departement")
)
GEO_ANALYSE <- "departement"
# local = TRUE : les fonctions vivent dans l'environnement de test, où les
# paramètres GEO_* ci-dessus sont visibles (pas dans l'environnement global)
source(file.path(RACINE, "R", "00c_fonctions_geo.R"), local = TRUE)

# Petit référentiel département écrit dans un fichier temporaire
ecrire_ref_dep <- function() {
  f <- tempfile(fileext = ".csv")
  writeLines(c("code;nom", "01;Ain", "2A;Corse-du-Sud", "2B;Haute-Corse",
               "33;Gironde", "09;Ariège"), f)
  f
}
