# utilities/R/mappe.R
#
# Helper condiviso per le mappe del repo data-viz.
# Si usa con:
#
#   source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
#
# Espone:
#   - paesi_europa_mappa   : vettore CNTR_ID inclusi nello sfondo Europa
#   - bbox_europa          : bbox EPSG:3035 stretto a est (no RU/UA/BY/TR)
#   - load_geo_europa()    : sf con i paesi Europa, EPSG:3035, cached in data/
#   - load_geo_italia_regioni() : sf con le regioni Italia, cached in data/
#   - theme_map()          : tema minimal per mappe choropleth
#
# I file .rds sono generati una tantum da utilities/script/prepara_geometrie.R.

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
})

.UTIL_ROOT <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities"

# Sfondo Europa: EU27 + EFTA + UK + Balcani non-UE.
# Esclusi esplicitamente: RU (Russia), BY (Bielorussia), UA (Ucraina),
# TR (Turchia), MD (Moldova).
paesi_europa_mappa <- c(
  # EU27
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES",
  "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
  "NL", "PL", "PT", "RO", "SE", "SI", "SK",
  # EFTA
  "CH", "NO", "IS", "LI",
  # UK
  "UK",
  # Balcani non-UE
  "AL", "BA", "ME", "MK", "RS", "XK"
)

# bbox EPSG:3035 (ETRS89-LAEA Europe) stretto a est.
# Stessi valori usati storicamente nelle mappe Europa del repo.
bbox_europa <- c(xmin = 2400000, ymin = 1380000,
                 xmax = 6200000, ymax = 5500000)

# Colore "no data" per i paesi sullo sfondo senza valore.
# Coincide con il default consigliato per `na.value` in scale_fill_stepsn.
COL_NA_MAPPA <- "#CCCCCC"

load_geo_europa <- function() {
  rds_path <- file.path(.UTIL_ROOT, "data", "geo_europa.rds")
  if (!file.exists(rds_path)) {
    stop("Geometrie Europa non trovate: ", rds_path,
         "\nLancia prima: Rscript ", file.path(.UTIL_ROOT, "script", "prepara_geometrie.R"))
  }
  readRDS(rds_path)
}

load_geo_italia_regioni <- function() {
  rds_path <- file.path(.UTIL_ROOT, "data", "geo_italia_regioni.rds")
  if (!file.exists(rds_path)) {
    stop("Geometrie regioni Italia non trovate: ", rds_path,
         "\nLancia prima: Rscript ", file.path(.UTIL_ROOT, "script", "prepara_geometrie.R"))
  }
  readRDS(rds_path)
}

# mainland_centroid(): centroide del poligono più grande che cade dentro il
# bbox Europa. Necessario per i paesi con territori d'oltremare (Francia con
# Guyana francese, Spagna con Canarie, Portogallo con Azzorre/Madeira,
# Norvegia con Svalbard): senza questo, st_centroid() sul multipolygon
# completo restituisce un punto fuori dall'Europa (es. il centroide della
# Francia con la Guyana cade in Spagna nord).
mainland_centroid <- function(geom) {
  parts <- suppressWarnings(sf::st_cast(geom, "POLYGON"))
  if (length(parts) <= 1) return(sf::st_centroid(geom))
  bbs <- lapply(parts, sf::st_bbox)
  in_eu <- vapply(bbs, function(b) {
    b["xmin"] >= bbox_europa["xmin"] && b["xmax"] <= bbox_europa["xmax"] &&
    b["ymin"] >= bbox_europa["ymin"] && b["ymax"] <= bbox_europa["ymax"]
  }, logical(1))
  parts_in <- if (any(in_eu)) parts[in_eu] else parts
  areas <- sf::st_area(parts_in)
  sf::st_centroid(parts_in[which.max(areas)])
}

# mainland_centroids(): vettorizzazione di mainland_centroid su un oggetto sf.
# Restituisce un sfc di POINT (uno per riga del sf input).
mainland_centroids <- function(sf_obj) {
  geoms <- sf::st_geometry(sf_obj)
  pts   <- lapply(seq_along(geoms), function(i) mainland_centroid(geoms[i]))
  do.call(c, pts)
}

# Tema mappa minimale: assi spenti, legenda dentro il pannello in alto a destra.
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      legend.position = c(1, 0.95),
      legend.justification = c(1, 1),
      legend.key.height = unit(1.2, "cm"),
      legend.key.width = unit(0.45, "cm"),
      legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}
