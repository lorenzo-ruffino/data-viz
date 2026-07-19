# Mappa dell'Europa (griglia ERA5-Land 0,1° riproiettata in EPSG:3035) con
# l'anomalia della temperatura media di giugno 2026 rispetto a giugno 1991-2020.
# Richiede i dati scaricati con:
#   python3 01_scarica_era5land.py --preset europa-giugno
# Finché giugno 2026 è incompleto, il confronto usa la stessa finestra di giorni.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
library(tidyverse)
library(ncdf4)
library(sf)
library(terra)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

files <- list.files("input/nc/europa", pattern = "^era5land_t2m_mean_.*\\.nc$",
                    full.names = TRUE)
stopifnot(length(files) > 0)

leggi_nc <- function(path) {
  nc <- nc_open(path)
  on.exit(nc_close(nc))
  lon <- as.numeric(ncvar_get(nc, "longitude"))
  lat <- as.numeric(ncvar_get(nc, "latitude"))
  tm  <- as.vector(ncvar_get(nc, "valid_time"))
  un  <- ncatt_get(nc, "valid_time", "units")$value
  origine <- as.Date(sub("days since ", "", un))
  list(lon = lon, lat = lat, date = origine + tm, t2m = ncvar_get(nc, "t2m"))
}

# finestra di giorni disponibile per giugno 2026
fin <- 30L
f26 <- "input/nc/europa/era5land_t2m_mean_2026-06.nc"
if (file.exists(f26)) fin <- max(as.integer(format(leggi_nc(f26)$date, "%d")))
cat("Finestra giugno: 1-", fin, "\n", sep = "")

# media di giugno (finestra) per cella e anno, accumulata su tutti i file
somme <- NULL
medie_anno <- list()
for (f in files) {
  d <- leggi_nc(f)
  sel_g <- as.integer(format(d$date, "%d")) <= fin
  anni <- as.integer(format(d$date, "%Y"))
  for (a in unique(anni)) {
    sel <- sel_g & anni == a
    m <- apply(d$t2m[, , sel, drop = FALSE], c(1, 2), mean) - 273.15
    medie_anno[[as.character(a)]] <- m
  }
  if (is.null(somme)) { lon <- d$lon; lat <- d$lat }
}

anni_disp <- sort(as.integer(names(medie_anno)))
anni_base <- intersect(1991:2020, anni_disp)
cat("Anni disponibili:", length(anni_disp), "| baseline 1991-2020:", length(anni_base), "anni\n")
stopifnot("2026" %in% names(medie_anno), length(anni_base) >= 25)

baseline <- Reduce(`+`, medie_anno[as.character(anni_base)]) / length(anni_base)
anomalia <- medie_anno[["2026"]] - baseline

# raster 4326 -> proiezione EPSG:3035 -> maschera sui paesi delle mappe europee
df <- expand_grid(lat = lat, lon = lon)   # lon varia più veloce
df$valore <- as.vector(anomalia)          # t2m[lon, lat]: lon più veloce
r <- rast(df |> select(lon, lat, valore) |> as.data.frame(),
          type = "xyz", crs = "EPSG:4326")
geo <- load_geo_europa()
r3035 <- project(r, "EPSG:3035", res = 9000)
r3035 <- mask(r3035, vect(geo))
tiles <- as.data.frame(r3035, xy = TRUE, na.rm = TRUE) |> rename(valore = 3)

cat("Celle in mappa:", nrow(tiles), "\n")
print(round(quantile(tiles$valore, c(0, 0.02, 0.1, 0.5, 0.9, 0.98, 1)), 2))

# ---- Bin discreti (0,5 °C, aperti agli estremi) -----------------------------

bin_levels <- c("sotto 0", "da 0 a 0,5", "da 0,5 a 1", "da 1 a 1,5", "da 1,5 a 2",
                "da 2 a 2,5", "da 2,5 a 3", "da 3 a 3,5", "da 3,5 a 4",
                "da 4 a 4,5", "da 4,5 a 5", "5 e oltre")
bin_colours <- c(
  "sotto 0"    = "#A1C6EE",
  "da 0 a 0,5" = "#FDF1F3",
  "da 0,5 a 1" = "#FCE4E7",
  "da 1 a 1,5" = "#F8C0C7",
  "da 1,5 a 2" = "#F49BA5",
  "da 2 a 2,5" = "#F2707D",
  "da 2,5 a 3" = "#F12938",
  "da 3 a 3,5" = "#D42430",
  "da 3,5 a 4" = "#B01D28",
  "da 4 a 4,5" = "#8E1622",
  "da 4,5 a 5" = "#6E1019",
  "5 e oltre"  = "#4A0A10"
)

tiles <- tiles |>
  mutate(bin = factor(case_when(
    valore < 0   ~ "sotto 0",
    valore < 0.5 ~ "da 0 a 0,5",
    valore < 1   ~ "da 0,5 a 1",
    valore < 1.5 ~ "da 1 a 1,5",
    valore < 2   ~ "da 1,5 a 2",
    valore < 2.5 ~ "da 2 a 2,5",
    valore < 3   ~ "da 2,5 a 3",
    valore < 3.5 ~ "da 3 a 3,5",
    valore < 4   ~ "da 3,5 a 4",
    valore < 4.5 ~ "da 4 a 4,5",
    valore < 5   ~ "da 4,5 a 5",
    TRUE         ~ "5 e oltre"
  ), levels = bin_levels))

print(table(tiles$bin))

# confini: interni (tra paesi) in bianco, perimetro esterno (coste) in nero
contorno <- st_union(geo)

# i bin estremi senza celle non vanno in legenda
bin_presenti <- bin_levels[bin_levels %in% unique(as.character(tiles$bin))]

sotto_periodo <- if (fin >= 30) {
  "la temperatura media di giugno 2026 e la media di giugno"
} else {
  sprintf("la temperatura media dei primi %d giorni di giugno 2026 e la media\ndegli stessi giorni nel", fin)
}

p <- ggplot() +
  geom_tile(data = tiles, aes(x, y, fill = bin), width = 9000, height = 9000) +
  geom_sf(data = geo, fill = NA, color = "white", linewidth = 0.3) +
  geom_sf(data = contorno, fill = NA, color = "#1C1C1C", linewidth = 0.22) +
  scale_fill_manual(values = bin_colours, drop = FALSE, name = NULL,
                    breaks = bin_presenti) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.5, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0))) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")],
           ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.98, 0.80),
        legend.justification = c(1, 1)) +
  labs(
    title = "Dove giugno 2026 è stato più caldo del normale",
    subtitle = paste0("Differenza in gradi tra ", sotto_periodo,
                      " 1991-2020, celle di circa 9 km, Europa"),
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land"
  )

ggsave("output/mappa_giugno_2026_europa.png", p,
       width = 9, height = 9, units = "in", dpi = 300, bg = "white")
cat("Salvata output/mappa_giugno_2026_europa.png\n")
