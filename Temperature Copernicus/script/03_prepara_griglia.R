# Costruisce la tabella di corrispondenza tra le celle della griglia ERA5-Land
# (0,1° ~ 9 km) e il territorio italiano:
#   - regione di appartenenza (quella con la sovrapposizione maggiore)
#   - superficie italiana dentro la cella (km², peso per le medie "geografiche")
#   - popolazione residente 2021 dentro la cella (peso per le medie "percepite",
#     da griglia Istat 1 km del censimento, cartella "Densità Popolazione Italia")
#
# Output (cache riusabile): input/geo/celle_griglia.csv
# Va rilanciato solo se cambia il bounding box del download.

library(tidyverse)
library(sf)
library(ncdf4)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

POP_GRID <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Densità Popolazione Italia/GrigliaPop2021_Ind_ITA.json"

# 1) Griglia di riferimento da un nc già scaricato --------------------------

nc  <- nc_open("input/nc/italia/era5land_t2m_mean_2026-06.nc")
lon <- as.numeric(round(ncvar_get(nc, "longitude"), 1))
lat <- as.numeric(round(ncvar_get(nc, "latitude"), 1))
nc_close(nc)

celle <- expand_grid(lon = lon, lat = lat) |>
  mutate(ilon = as.integer(round(lon * 10)),
         ilat = as.integer(round(lat * 10)))

mezza <- 0.05
poligono_cella <- function(x, y) {
  st_polygon(list(rbind(
    c(x - mezza, y - mezza), c(x - mezza, y + mezza),
    c(x + mezza, y + mezza), c(x + mezza, y - mezza),
    c(x - mezza, y - mezza))))
}

celle_sf <- st_sf(celle,
                  geometry = st_sfc(map2(celle$lon, celle$lat, poligono_cella),
                                    crs = 4326)) |>
  st_transform(32632)

# 2) Intersezione con le regioni Istat 2025 ---------------------------------

regioni <- read_sf("input/geo/Reg01012025_g_WGS84.json") |>
  st_make_valid() |>
  st_transform(32632) |>
  select(COD_REG, DEN_REG)

message("Intersezione celle x regioni...")
inters <- st_intersection(celle_sf, regioni) |>
  mutate(area_kmq = as.numeric(st_area(geometry)) / 1e6) |>
  st_drop_geometry() |>
  group_by(ilon, ilat, lon, lat) |>
  summarise(regione  = DEN_REG[which.max(area_kmq)],
            cod_reg  = COD_REG[which.max(area_kmq)],
            area_kmq = sum(area_kmq),
            .groups = "drop")

# 3) Popolazione 2021 per cella ---------------------------------------------

message("Lettura griglia popolazione Istat (1-2 minuti)...")
pop_grid <- read_sf(POP_GRID)
xy <- st_coordinates(suppressWarnings(st_centroid(st_geometry(pop_grid))))

pop_celle <- tibble(ilon = as.integer(round(xy[, 1] * 10)),
                    ilat = as.integer(round(xy[, 2] * 10)),
                    pop  = pop_grid$T) |>
  group_by(ilon, ilat) |>
  summarise(pop = sum(pop), .groups = "drop")

celle_finali <- inters |>
  left_join(pop_celle, by = c("ilon", "ilat")) |>
  mutate(pop = replace_na(pop, 0))

write_csv(celle_finali, "input/geo/celle_griglia.csv")

# 4) Controlli ---------------------------------------------------------------

cat("Celle assegnate:", nrow(celle_finali), "\n")
cat("Superficie totale (km2):", round(sum(celle_finali$area_kmq)), "(attesa ~302.000)\n")
cat("Popolazione totale:", format(sum(celle_finali$pop), big.mark = "."), "(attesa ~59 mln)\n")
celle_finali |>
  group_by(regione) |>
  summarise(celle = n(), area_kmq = round(sum(area_kmq)),
            pop = sum(pop), .groups = "drop") |>
  arrange(desc(pop)) |>
  print(n = 25)
