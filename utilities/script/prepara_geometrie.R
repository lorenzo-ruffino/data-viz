# utilities/script/prepara_geometrie.R
#
# Genera / aggiorna i file .rds in utilities/data/.
# Da rilanciare solo quando GISCO pubblica una nuova versione delle geometrie
# o quando Istat rilascia nuove geometrie regionali.
#
# Uso:
#   cd "/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/script"
#   Rscript prepara_geometrie.R

suppressPackageStartupMessages({
  library(giscoR)
  library(sf)
  library(dplyr)
})

UTIL_ROOT <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities"
DATA_DIR <- file.path(UTIL_ROOT, "data")
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

source(file.path(UTIL_ROOT, "R", "mappe.R"))

# --- Sfondo Europa --------------------------------------------------------

cat("Scarico geometrie GISCO Europa...\n")
# Non filtriamo per `region = "Europe"` perché GISCO classifica Cipro come Asia
# e Kosovo non rientra (non è uno stato ISO sovrano: in GISCO è solo NUTS-0).
# Strategia: scarichiamo tutti i paesi e filtriamo per CNTR_ID, poi aggiungiamo
# manualmente il Kosovo prendendolo da gisco_get_nuts(level = 0).
geo_paesi <- gisco_get_countries(year = "2020", resolution = "10") |>
  filter(CNTR_ID %in% paesi_europa_mappa) |>
  select(CNTR_ID, NAME_ENGL, ISO3_CODE)

geo_kosovo <- gisco_get_nuts(country = "XK", nuts_level = 0,
                             year = "2024", resolution = "10") |>
  transmute(CNTR_ID = "XK",
            NAME_ENGL = "Kosovo",
            ISO3_CODE = "XKO")

geo_europa <- bind_rows(geo_paesi, geo_kosovo) |>
  st_transform(crs = 3035)

mancanti <- setdiff(paesi_europa_mappa, geo_europa$CNTR_ID)
if (length(mancanti) > 0) {
  stop("Mancano dalle geometrie GISCO: ", paste(mancanti, collapse = ", "))
}
saveRDS(geo_europa, file.path(DATA_DIR, "geo_europa.rds"))
cat("  Salvato: ", file.path(DATA_DIR, "geo_europa.rds"),
    " (", nrow(geo_europa), " paesi)\n", sep = "")

# --- Regioni Italia -------------------------------------------------------

cat("Scarico geometrie NUTS-2 Italia (= regioni)...\n")
geo_italia_regioni <- gisco_get_nuts(country = "IT",
                                     nuts_level = 2,
                                     year = "2024",
                                     resolution = "10") |>
  st_transform(crs = 3035) |>
  select(NUTS_ID, NAME_LATN, NUTS_NAME)

saveRDS(geo_italia_regioni, file.path(DATA_DIR, "geo_italia_regioni.rds"))
cat("  Salvato: ", file.path(DATA_DIR, "geo_italia_regioni.rds"),
    " (", nrow(geo_italia_regioni), " regioni)\n", sep = "")

cat("\nFatto.\n")
