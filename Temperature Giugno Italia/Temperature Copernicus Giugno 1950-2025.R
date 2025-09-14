library(ecmwfr)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(purrr)
library(sf)
library(rnaturalearthhires)
library(rnaturalearth)

<<<<<<< Updated upstream
setwd("~/Documenti/data-viz/Temperature Estate 2025 Italia")
=======
setwd("~/Documents/data-viz/Temperature Estate Italia")
>>>>>>> Stashed changes

wf_set_key(user = "53371aae-64d6-4215-ba84-fb15384f77bf",
           key  = "075797c7-d3d8-434e-a0d4-8f0667467827")

<<<<<<< Updated upstream
wf_set_key(user = "53371aae-64d6-4215-ba84-fb15384f77bf",
           key  = "075797c7-d3d8-434e-a0d4-8f0667467827")

output_dir = "~/Documenti/data-viz/Temperature Estate 2025 Italia/Dati"
=======
output_dir = "~/Documents/data-viz/Temperature Estate Italia/Dati"
>>>>>>> Stashed changes


## Download dei dati

<<<<<<< Updated upstream
start_years = seq(1950, 2025, by = 5)

for (start in start_years) {
  end = min(start + 4, 2025)  
  cat("Blocco:", start, "-", end, "\n")
  
  for (mese in c("06","07","08")) {
    req = list(
      "dataset_short_name" = "derived-era5-land-daily-statistics",
      "variable"           = "2m_temperature",
      "year"               = as.character(start:end),
      "month"              = mese,
      "day"                = sprintf("%02d", 1:31),   # max 31 giorni
      "daily_statistic"    = "daily_mean",
      "time_zone"          = "utc+02:00",
      "frequency"          = "1_hourly",
      "area"               = c(47, 6, 36, 19),
      "format"             = "netcdf",
      "target"             = paste0("ERA5L_T2m_Mean_Italy_", mese, "_", start, "_", end, ".nc")
    )
    
    try({
      wf_request(
        user     = "53371aae-64d6-4215-ba84-fb15384f77bf",
        request  = req,
        transfer = TRUE,
        path     = path.expand(output_dir)
=======
for (y in 1950:2025) {
  for (mese in c("06","07","08")) {
    req <- list(
      dataset_short_name = "derived-era5-land-daily-statistics",
      variable           = "2m_temperature",
      year               = as.character(y),
      month              = mese,
      day                = sprintf("%02d", 1:ifelse(mese=="06",30,31)),
      daily_statistic    = "daily_mean",
      time_zone          = "utc+02:00",
      frequency          = "1_hourly",
      area               = c(47, 6, 36, 19),
      format             = "netcdf",
      target             = paste0("ERA5L_T2m_Mean_Italy_", mese, "_", y, ".nc")
    )
    try({
      wf_request(
        request  = req,
        transfer = TRUE,
        path     = path.expand(output_dir),
        user     = "53371aae-64d6-4215-ba84-fb15384f77bf",
        time_out = 60*60*4   # 4 ore: eviti timeout senza bloccare per giorni
>>>>>>> Stashed changes
      )
    })
  }
}

### Lettura dei file

leggi_nc = function(file) {
  nc = nc_open(file)
  
  lon = ncvar_get(nc, "longitude")
  lat = ncvar_get(nc, "latitude")
  time = ncvar_get(nc, "valid_time") 
  temp = ncvar_get(nc, "t2m")        
  
  time_units = ncatt_get(nc, "valid_time", "units")$value
  origin_date = as.Date(sub("days since ", "", time_units))
  dates = origin_date + time
  
  nc_close(nc)
  
  df = expand.grid(
    lon = lon,
    lat = lat,
    date = dates
  )
  
  df$t2m = as.vector(temp) - 273.15  # Da Kelvin a Celsius
  
  return(df)
}


files = list.files(path = output_dir, pattern = "\\.nc$", full.names = TRUE)

df_all = map_dfr(files, leggi_nc)



### Tieni solo i tile/punti dentro i confini dell'Italia

points_unique = df_all %>%
  distinct(lon, lat)

sf_points = st_as_sf(points_unique, coords = c("lon", "lat"), crs = 4326)
sf_points$lon = points_unique$lon
sf_points$lat = points_unique$lat

italy_prov = ne_states(country = "Italy", returnclass = "sf")

italy_regions = italy_prov %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

points_with_regions = st_join(sf_points, italy_regions) %>%
  st_drop_geometry()

df_labeled = df_all %>%
  inner_join(points_with_regions, by = c("lon", "lat")) %>%
  mutate(
    year = year(date),
    day  = yday(date)
  ) %>%
  select(region, lon, lat, date, year, day, t2m)%>%
  filter(!is.na(region))%>%
  mutate(region = case_when(region == 'Apulia' ~ 'Puglia',
                            region == 'Sicily' ~ 'Sicilia',
                            T ~ region))



### Elaborazioni

italy_average = df_labeled %>%
  group_by(year)%>%
  summarise(temp_media = mean(t2m, na.rm=T))

write.csv(italy_average, file="Output/italy_average.csv", row.names = F)



df_labeled %>%
  filter(year %in% c(1961:1990, 1991:2020, 2003, 2025)) %>%
  mutate(periodo = case_when(
    year >= 1961 & year <= 1990 ~ "1961-1990",
    year >= 1991 & year <= 2020 ~ "1991-2020", 
    year == 2025 ~ "2025"
  )) %>%
  group_by(periodo) %>%
  summarise(temperatura_media = mean(t2m, na.rm = TRUE)) %>%
  pivot_wider(names_from = periodo, values_from = temperatura_media) %>%
  mutate(aumento = `2025` - `1991-2020`) 


italy_average_daily =  df_labeled %>%
  mutate(day = lubridate::day(date)) %>%
  filter(year %in% c(1961:1990, 1991:2020, 2003, 2025)) %>%
  mutate(periodo = case_when(
    year >= 1961 & year <= 1990 ~ "1961-1990",
    year >= 1991 & year <= 2020 ~ "1991-2020", 
    year == 2025 ~ "2025"
  )) %>%
  group_by(periodo, day) %>%
  summarise(temp_media = mean(t2m, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = periodo, values_from = temp_media)

write.csv(italy_average_daily, file="Output/italy_average_daily.csv", row.names = F)



region_average = df_labeled %>%
  filter(year >= 1991 & year <= 2020 | year == 2025) %>%
  mutate(periodo = ifelse(year == 2025, "2025", "1991-2020")) %>%
  group_by(region, periodo) %>%
  summarise(temperatura_media = mean(t2m, na.rm = TRUE)) %>%
  pivot_wider(names_from = periodo, values_from = temperatura_media) %>%
  mutate(aumento = `2025` - `1991-2020`,
         var_pct = (`2025`/  `1991-2020`)-1) %>%
  select(baseline = `1991-2020`, temp_2025 = `2025`, aumento, var_pct)


write.csv(region_average, file="Output/region_average.csv", row.names = F)



macro_region_average = df_labeled %>%
  mutate(
    macro_area = case_when(
      region %in% c("Piemonte", "Valle d'Aosta", "Lombardia", "Liguria") ~ "Nord-Ovest",
      region %in% c("Trentino-Alto Adige", "Veneto", "Friuli-Venezia Giulia", "Emilia-Romagna") ~ "Nord-Est",
      region %in% c("Toscana", "Marche", "Umbria", "Lazio") ~ "Centro",
      region %in% c("Abruzzo", "Molise", "Puglia", "Campania", "Sardegna", "Basilicata", "Calabria", "Sicilia") ~ "Mezzogiorno",
      TRUE ~ "Altro"
    )
  )%>%
  filter(year >= 1991 & year <= 2020 | year == 2025) %>%
  mutate(periodo = ifelse(year == 2025, "2025", "1991-2020")) %>%
  group_by(macro_area, periodo) %>%
  summarise(temperatura_media = mean(t2m, na.rm = TRUE)) %>%
  pivot_wider(names_from = periodo, values_from = temperatura_media) %>%
  mutate(aumento = `2025` - `1991-2020`,
         var_pct = (`2025`/  `1991-2020`)-1) %>%
  select(macro_area, baseline = `1991-2020`, temp_2025 = `2025`, aumento, var_pct)



#### Mappa

cell_size = 0.1

grid_points = df_all %>%
  distinct(lon, lat) %>%
  mutate(
    tile_id = paste0(round(lon, 1), "_", round(lat, 1))
  )

grid_tiles = grid_points %>%
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

grid_sf = st_sf(grid_tiles, crs = 4326)

output_path = "Output/griglia_tile.geojson"
if (file.exists(output_path)) file.remove(output_path)
st_write(grid_sf, output_path, driver = "GeoJSON")


griglia_average = df_labeled %>%
  filter(year >= 1991 & year <= 2020 | year == 2025) %>%
  mutate(periodo = ifelse(year == 2025, "2025", "1991-2020")) %>%
  group_by(lon, lat, region, periodo) %>%
  summarise(temperatura_media = mean(t2m, na.rm = TRUE)) %>%
  pivot_wider(names_from = periodo, values_from = temperatura_media) %>%
  mutate(aumento = `2025` - `1991-2020`,
         var_pct = (`2025`/  `1991-2020`)-1) %>%
  mutate(
    tile_id = paste0(round(lon, 1), "_", round(lat, 1))
  )


write.csv(griglia_average, file="Output/griglia_average.csv", row.names = F)
