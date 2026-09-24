# Lance les tests depuis la RACINE du projet :
#   source("tests/testthat.R")      ou      testthat::test_dir("tests/testthat")
# Les tests unitaires (test-geo.R) ne lancent pas la chaîne ; le test de
# non-régression (test-non-regression-ze.R) la rejoue en mode test (~30 s).
library(testthat)
test_dir("tests/testthat")
