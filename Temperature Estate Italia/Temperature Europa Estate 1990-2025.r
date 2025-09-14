library(ecmwfr)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(purrr)
library(sf)
library(rnaturalearthhires)
library(rnaturalearth)

# Autenticazione ECMWF
wf_set_key(user = "...",
           key  = "...")

# Output
output_dir = "~/Documents/data-viz/Temperature Estate Italia/Europa"


years <- c(2005:2020, 2025)        # Anni da scaricare
months <- c("06", "07", "08")      # Giugno, Luglio, Agosto
area_europa <- c(72, -25, 34, 45)  # Nord, Ovest, Sud, Est (grossa parte Europa)


for (year in years) {
  cat("Scarico estate:", year, "\n")
  
  req = list(
    "dataset_short_name" = "derived-era5-land-daily-statistics",
    "variable"           = "2m_temperature",
    "year"               = as.character(year),
    "month"              = months,     # Tre mesi insieme
    "day"                = sprintf("%02d", 1:31),  # Tutti i giorni (CDS gestisce quelli non validi)
    "daily_statistic"    = "daily_mean",
    "time_zone"          = "utc+00:00",
    "frequency"          = "1_hourly",
    "area"               = area_europa,
    "format"             = "netcdf",
    "target"             = paste0("ERA5L_T2m_Europe_", year, "_summer.nc")
  )
  
  try({
    wf_request(
      user     = "...",
      request  = req,
      transfer = TRUE,
      path     = path.expand(output_dir)
    )
  })
}





process_nc = function(file) {
  df = leggi_nc(file) %>%
    mutate(year = year(date)) %>%
    select(lon, lat, year, t2m)
  
  df %>%
    filter(year >= 1991 & year <= 2020 | year == 2025) %>%
    mutate(periodo = ifelse(year == 2025, "2025", "1991-2020")) %>%
    group_by(lon, lat, periodo) %>%
    summarise(temperatura_media = mean(t2m, na.rm = TRUE), .groups = "drop")
}

results_list = map(files, process_nc)

df_all_summary = bind_rows(results_list)

griglia_average = df_all_summary %>%
  pivot_wider(
    names_from = periodo,
    values_from = temperatura_media,
    values_fn = mean  
  ) %>%
  mutate(
    aumento = `2025` - `1991-2020`,
    tile_id = paste0(round(lon, 1), "_", round(lat, 1))
  )


cell_size = 0.1

# 🔹 1. Crea i poligoni dei tile a partire da griglia_average
grid_tiles = griglia_average %>%
  rowwise() %>%
  mutate(
    geometry = list(st_polygon(list(rbind(
      c(lon - cell_size / 2, lat - cell_size / 2),
      c(lon - cell_size / 2, lat + cell_size / 2),
      c(lon + cell_size / 2, lat + cell_size / 2),
      c(lon + cell_size / 2, lat - cell_size / 2),
      c(lon - cell_size / 2, lat - cell_size / 2)
    ))))
  ) %>%
  ungroup()

sf_grid_tiles = st_as_sf(grid_tiles, crs = 4326)

europe_states <- ne_countries(continent = "Europe", returnclass = "sf") %>%
  filter(!admin %in% c("Russia", "Turkey"))

sf_tiles_with_states <- st_join(sf_grid_tiles, europe_states["admin"])%>%
  filter(!is.na(admin))

griglia_with_states <- sf_tiles_with_states %>%
  st_drop_geometry()

write.csv(griglia_with_states, "Output/griglia_average_states.csv", row.names = FALSE)

output_path_states = "Output/griglia_tiles_states.geojson"
st_write(sf_tiles_with_states, output_path_states, driver = "GeoJSON")




media_per_stato <- sf_tiles_with_states %>%
  st_drop_geometry() %>%
  group_by(admin) %>%
  summarise(
    variazione_media = mean(aumento, na.rm = TRUE),
    `2025` =  mean(`2025`, na.rm = TRUE),
    `1991-2020`=  mean(`1991-2020`, na.rm = TRUE),
    n_tile = n()
  ) %>%
  arrange(desc(variazione_media))


write.csv(media_per_stato, "Output/media_per_stato.csv", row.names = FALSE)