# ==============================================================================
# 00b_fonts.R — Rend disponibles les polices manuscrites DEPUIS LE PROJET
# ------------------------------------------------------------------------------
# Objectif : que les familles "Caveat" et "Kalam" soient utilisables par le
# device ragg des graphiques, SANS installation système — les .ttf vivent dans
# assets/fonts/ (versionnés avec le projet, portables : Onyxia, collègues...).
# Sourcé par les scripts graphiques (06, 06b, 06c, 99).
# PRODUIT : variables globales FONT_TITRE / FONT_CORPS (noms de familles, ou
#           "sans" en repli). Robuste : gère le cas où la police est déjà
#           connue du système (register_font refuserait le doublon).
# ==============================================================================
FONT_TITRE <- "sans"; FONT_CORPS <- "sans"

.dir_fonts <- "assets/fonts"
if (exists("DIR_DATA") && dir.exists(file.path(dirname(DIR_DATA), "assets", "fonts")))
  .dir_fonts <- file.path(dirname(DIR_DATA), "assets", "fonts")

# Une famille est-elle DÉJÀ rendable par systemfonts (installée OU enregistrée) ?
.font_dispo <- function(fam) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) return(FALSE)
  info <- tryCatch(systemfonts::match_font(fam), error = function(e) NULL)
  !is.null(info) && grepl(tolower(substr(fam, 1, 3)), tolower(info$path), fixed = TRUE)
}
# Rendre une famille disponible : si déjà là -> rien ; sinon register depuis .ttf
.assure_font <- function(fam, plain, bold = plain) {
  if (.font_dispo(fam)) return(TRUE)
  if (!requireNamespace("systemfonts", quietly = TRUE) || !file.exists(plain)) return(FALSE)
  tryCatch({
    systemfonts::register_font(name = fam, plain = plain,
                               bold = if (file.exists(bold)) bold else plain)
    TRUE
  }, error = function(e) .font_dispo(fam))  # doublon éventuel -> retester
}

if (.assure_font("Caveat", file.path(.dir_fonts, "Caveat-Regular.ttf")))
  FONT_TITRE <- "Caveat"
if (.assure_font("Kalam", file.path(.dir_fonts, "Kalam-Regular.ttf"),
                 file.path(.dir_fonts, "Kalam-Bold.ttf")))
  FONT_CORPS <- "Kalam"

message("00b fonts : titre=", FONT_TITRE, " corps=", FONT_CORPS)