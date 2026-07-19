# Esporta i materiali per le versioni interattive delle due mappe (es. Flourish)
# nella sottocartella "grafici interattivi/":
#   - griglia_italia_025.geojson  celle 0,25° ritagliate sull'Italia, WGS84, con id
#   - dati_italia_giugno2026.csv  valori per cella (id, regione, 2026, baseline, anomalia)
#   - griglia_europa_025.geojson  celle 0,25° ritagliate sui 38 paesi, WGS84, con id
#   - dati_europa_giugno2026.csv  valori per cella (id, paese, 2026, baseline, anomalia)
#   - regioni_italia.geojson      confini regionali Istat 2025, WGS84
#   - paesi_europa.geojson        confini nazionali (set delle mappe europee), WGS84
#
# Gli id sono costruiti dalle coordinate del centro cella (lon/lat x100),
# identici tra geojson e csv: es. "it_775_4525" = cella centrata su 7,75E 45,25N.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
library(tidyverse)
library(sf)
library(ncdf4)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")
dir.create("grafici interattivi", showWarnings = FALSE)

PASSO <- 0.25

quadrato <- function(x, y, mezzo = PASSO / 2) {
  st_polygon(list(rbind(c(x - mezzo, y - mezzo), c(x - mezzo, y + mezzo),
                        c(x + mezzo, y + mezzo), c(x + mezzo, y - mezzo),
                        c(x - mezzo, y - mezzo))))
}

griglia_blocchi <- function(bb) {
  expand_grid(
    bx = seq(floor(bb["xmin"] / PASSO) * PASSO, bb["xmax"] + PASSO, by = PASSO),
    by = seq(floor(bb["ymin"] / PASSO) * PASSO, bb["ymax"] + PASSO, by = PASSO))
}

riempi_vicini <- function(dominio, valori) {
  buchi <- which(is.na(dominio$anomalia))
  for (i in buchi) {
    vic <- valori |>
      filter(abs(bx - dominio$bx[i]) <= PASSO + 1e-6,
             abs(by - dominio$by[i]) <= PASSO + 1e-6)
    dominio$anomalia[i] <- mean(vic$anomalia, na.rm = TRUE)
    dominio$t_2026[i]   <- mean(vic$t_2026, na.rm = TRUE)
    dominio$baseline[i] <- mean(vic$baseline, na.rm = TRUE)
  }
  dominio |> filter(!is.na(anomalia))
}

scrivi_geojson <- function(obj, path) {
  if (file.exists(path)) file.remove(path)
  st_write(obj, path, driver = "GeoJSON",
           layer_options = "COORDINATE_PRECISION=4", quiet = TRUE)
}

# ---- ITALIA -----------------------------------------------------------------

message("Italia...")
griglia <- read_csv("output/griglia_mensile.csv.gz", show_col_types = FALSE)
celle   <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)

dati_celle <- griglia |>
  filter(stat == "mean", mese == 6, finestra == "finestra_giu2026") |>
  group_by(ilon, ilat, lon, lat) |>
  summarise(baseline = mean(valore[anno %in% 1991:2020]),
            n_base   = sum(anno %in% 1991:2020),
            t_2026   = mean(valore[anno == 2026]), .groups = "drop") |>
  filter(!is.na(t_2026), n_base >= 25) |>
  left_join(celle |> select(ilon, ilat, area_kmq, regione), by = c("ilon", "ilat"))

blocchi_it <- dati_celle |>
  mutate(bx = round(lon / PASSO) * PASSO,
         by = round(lat / PASSO) * PASSO) |>
  group_by(bx, by) |>
  summarise(t_2026   = weighted.mean(t_2026, area_kmq),
            baseline = weighted.mean(baseline, area_kmq),
            regione  = regione[which.max(area_kmq)], .groups = "drop") |>
  mutate(anomalia = t_2026 - baseline)

regioni <- read_sf("input/geo/Reg01012025_g_WGS84.json") |> st_make_valid()
italia  <- st_union(regioni)

bb_it <- st_bbox(italia)
tutti_it <- griglia_blocchi(bb_it)
tutti_it_sf <- st_sf(tutti_it,
                     geometry = st_sfc(map2(tutti_it$bx, tutti_it$by, quadrato),
                                       crs = 4326))
suppressWarnings(dominio_it <- st_intersection(tutti_it_sf, italia))
dominio_it <- dominio_it[as.numeric(st_area(dominio_it)) > 0, ] |>
  left_join(blocchi_it, by = c("bx", "by"))

# regione per i blocchi riempiti dai vicini: quella del poligono che li contiene
dominio_it <- riempi_vicini(dominio_it, blocchi_it)
manca_reg <- which(is.na(dominio_it$regione))
if (length(manca_reg) > 0) {
  idx <- st_nearest_feature(st_centroid(st_geometry(dominio_it[manca_reg, ])),
                            st_transform(regioni, 4326))
  dominio_it$regione[manca_reg] <- regioni$DEN_REG[idx]
}

dominio_it <- dominio_it |>
  mutate(id = sprintf("it_%d_%d", round(bx * 100), round(by * 100)),
         across(c(t_2026, baseline, anomalia), ~ round(.x, 2)))

scrivi_geojson(dominio_it |> select(id, geometry),
               "grafici interattivi/griglia_italia_025.geojson")
dominio_it |>
  st_drop_geometry() |>
  select(id, lon = bx, lat = by, regione, t_2026,
         media_1991_2020 = baseline, anomalia) |>
  write_csv("grafici interattivi/dati_italia_giugno2026.csv")

cat("Italia:", nrow(dominio_it), "celle\n")

# ---- EUROPA -----------------------------------------------------------------

message("Europa...")
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

medie_anno <- list()
for (f in files) {
  d <- leggi_nc(f)
  a <- format(d$date[1], "%Y")
  medie_anno[[a]] <- apply(d$t2m, c(1, 2), mean) - 273.15
}
anni_base <- intersect(1991:2020, as.integer(names(medie_anno)))
baseline_eu <- Reduce(`+`, medie_anno[as.character(anni_base)]) / length(anni_base)
t26_eu <- medie_anno[["2026"]]

celle_eu <- expand_grid(lat = d$lat, lon = d$lon)
celle_eu$baseline <- as.vector(baseline_eu)
celle_eu$t_2026   <- as.vector(t26_eu)
celle_eu <- celle_eu |> filter(!is.na(baseline), !is.na(t_2026))

blocchi_eu <- celle_eu |>
  mutate(bx = round(lon / PASSO) * PASSO,
         by = round(lat / PASSO) * PASSO,
         peso = cos(lat * pi / 180)) |>
  group_by(bx, by) |>
  summarise(t_2026   = weighted.mean(t_2026, peso),
            baseline = weighted.mean(baseline, peso), .groups = "drop") |>
  mutate(anomalia = t_2026 - baseline)

geo <- load_geo_europa()
europa_3035 <- st_union(geo)

bb_eu <- st_bbox(st_transform(geo, 4326))
tutti_eu <- griglia_blocchi(bb_eu)
tutti_eu_sf <- st_sf(tutti_eu,
                     geometry = st_sfc(map2(tutti_eu$bx, tutti_eu$by, quadrato),
                                       crs = 4326)) |>
  st_transform(3035)
suppressWarnings(dominio_eu <- st_intersection(tutti_eu_sf, europa_3035))
dominio_eu <- dominio_eu[as.numeric(st_area(dominio_eu)) > 0, ] |>
  left_join(blocchi_eu, by = c("bx", "by"))
dominio_eu <- riempi_vicini(dominio_eu, blocchi_eu)

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
idx <- st_nearest_feature(st_centroid(st_geometry(dominio_eu)), geo)
dominio_eu$paese <- nomi_paesi[geo$CNTR_ID[idx]]

dominio_eu <- dominio_eu |>
  st_transform(4326) |>
  mutate(id = sprintf("eu_%d_%d", round(bx * 100), round(by * 100)),
         across(c(t_2026, baseline, anomalia), ~ round(.x, 2)))

scrivi_geojson(dominio_eu |> select(id, geometry),
               "grafici interattivi/griglia_europa_025.geojson")
dominio_eu |>
  st_drop_geometry() |>
  select(id, lon = bx, lat = by, paese, t_2026,
         media_1991_2020 = baseline, anomalia) |>
  write_csv("grafici interattivi/dati_europa_giugno2026.csv")

cat("Europa:", nrow(dominio_eu), "celle\n")

# ---- CONFINI ----------------------------------------------------------------

scrivi_geojson(regioni |> select(cod_reg = COD_REG, regione = DEN_REG),
               "grafici interattivi/regioni_italia.geojson")
scrivi_geojson(geo |> st_transform(4326) |>
                 mutate(paese = nomi_paesi[CNTR_ID]) |>
                 select(cntr_id = CNTR_ID, paese),
               "grafici interattivi/paesi_europa.geojson")

cat("Confini esportati. Tutto in 'grafici interattivi/'\n")
