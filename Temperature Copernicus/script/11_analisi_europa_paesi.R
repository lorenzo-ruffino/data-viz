# Anomalia di giugno 2026 rispetto alla media di giugno 1991-2020 per ogni
# paese europeo (dai dati ERA5-Land scaricati con --preset europa-giugno).
# Media pesata per l'area della cella (coseno della latitudine), celle
# assegnate al paese del proprio centro.
#
# Output: output/analisi_europa_paesi.csv + classifica a video.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
library(tidyverse)
library(ncdf4)
library(sf)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

files <- list.files("input/nc/europa", pattern = "^era5land_t2m_mean_.*\\.nc$",
                    full.names = TRUE)

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

fin <- 30L
f26 <- "input/nc/europa/era5land_t2m_mean_2026-06.nc"
if (file.exists(f26)) fin <- max(as.integer(format(leggi_nc(f26)$date, "%d")))
cat("Finestra giugno: 1-", fin, "\n", sep = "")

medie_anno <- list()
for (f in files) {
  d <- leggi_nc(f)
  sel <- as.integer(format(d$date, "%d")) <= fin
  a <- format(d$date[1], "%Y")
  medie_anno[[a]] <- apply(d$t2m[, , sel, drop = FALSE], c(1, 2), mean) - 273.15
}
lon <- d$lon; lat <- d$lat

anni_base <- intersect(1991:2020, as.integer(names(medie_anno)))
baseline <- Reduce(`+`, medie_anno[as.character(anni_base)]) / length(anni_base)
t2026 <- medie_anno[["2026"]]

# assegnazione cella -> paese (centro cella dentro il poligono)
celle <- expand_grid(lat = lat, lon = lon)          # lon varia più veloce
celle$baseline <- as.vector(baseline)
celle$t2026    <- as.vector(t2026)
celle <- celle |> filter(!is.na(baseline), !is.na(t2026))

geo <- load_geo_europa()
punti <- st_as_sf(celle, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(st_crs(geo))
dentro <- st_within(punti, geo)
idx <- vapply(dentro, function(i) if (length(i)) i[1] else NA_integer_, integer(1))
celle$CNTR_ID <- geo$CNTR_ID[idx]
celle <- celle |> filter(!is.na(CNTR_ID))

nomi_paesi <- c(
  AL = "Albania", AT = "Austria", BA = "Bosnia ed Erzegovina", BE = "Belgio",
  BG = "Bulgaria", CH = "Svizzera", CY = "Cipro", CZ = "Cechia",
  DE = "Germania", DK = "Danimarca", EE = "Estonia", EL = "Grecia",
  ES = "Spagna", FI = "Finlandia", FR = "Francia", HR = "Croazia",
  HU = "Ungheria", IE = "Irlanda", IS = "Islanda", IT = "Italia",
  LI = "Liechtenstein", LT = "Lituania", LU = "Lussemburgo", LV = "Lettonia",
  ME = "Montenegro", MK = "Macedonia del Nord", MT = "Malta",
  NL = "Paesi Bassi", NO = "Norvegia", PL = "Polonia", PT = "Portogallo",
  RO = "Romania", RS = "Serbia", SE = "Svezia", SI = "Slovenia",
  SK = "Slovacchia", UK = "Regno Unito", XK = "Kosovo"
)

paesi <- celle |>
  mutate(peso = cos(lat * pi / 180)) |>
  group_by(CNTR_ID) |>
  summarise(celle = n(),
            media_1991_2020 = weighted.mean(baseline, peso),
            t_2026 = weighted.mean(t2026, peso),
            .groups = "drop") |>
  mutate(anomalia = t_2026 - media_1991_2020,
         paese = nomi_paesi[CNTR_ID],
         giorni = paste0("1-", fin)) |>
  select(paese, CNTR_ID, giorni, celle, media_1991_2020, t_2026, anomalia) |>
  arrange(desc(anomalia))

write_csv(paesi, "output/analisi_europa_paesi.csv")

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")
cat("\n=== Giugno 2026 vs media giugno 1991-2020, per paese (°C) ===\n")
for (i in seq_len(nrow(paesi))) {
  r <- paesi[i, ]
  cat(sprintf("%-22s %s°C (2026: %s | 91-20: %s)\n",
              r$paese, sprintf("%+s", fmt(r$anomalia)),
              fmt(r$t_2026, 1), fmt(r$media_1991_2020, 1)))
}
