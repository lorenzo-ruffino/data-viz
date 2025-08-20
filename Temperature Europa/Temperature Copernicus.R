library(ecmwfr)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(purrr)
library(sf)
library(rnaturalearthhires)
library(rnaturalearth)
library(showtext)
library(scales)

setwd("~/Documenti/data-viz/Temperature Europa")

# Autenticazione ECMWF
wf_set_key(user = "...",
           key  = "...")

# Output
output_dir = "~/Documenti/data-viz/Temperature Europa/Dati"

# ========================
# PARAMETRI PERSONALIZZABILI
# ========================

years <- c(1991:2020, 2025)     # Anni da scaricare
target_month <- 7              # Mese da scaricare (numero)
area_europa <- c(72, -25, 34, 45)  # Nord, Ovest, Sud, Est (grossa parte Europa)

# ========================
# DOWNLOAD DATI
# ========================

for (year in years) {
  cat("Scarico:", year, "mese:", target_month, "\n")
  
  # Giorni del mese
  days_in_month <- days_in_month(ymd(sprintf("%d-%02d-01", year, target_month)))
  day_vec <- sprintf("%02d", 1:days_in_month)
  
  req = list(
    "dataset_short_name" = "derived-era5-land-daily-statistics",
    "variable"           = "2m_temperature",
    "year"               = as.character(year),
    "month"              = sprintf("%02d", target_month),
    "day"                = day_vec,
    "daily_statistic"    = "daily_mean",
    "time_zone"          = "utc+00:00",
    "frequency"          = "1_hourly",
    "area"               = area_europa,
    "format"             = "netcdf",
    "target"             = paste0("ERA5L_T2m_Europe_", year, "_", sprintf("%02d", target_month), ".nc")
  )
  
  try({
    wf_request(
      user     = "53371aae-64d6-4215-ba84-fb15384f77bf",
      request  = req,
      transfer = TRUE,
      path     = path.expand(output_dir)
    )
  })
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

df_all = map_dfr(files, leggi_nc) %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  select(lon, lat, year, t2m)



tile_means = df_all %>%
  mutate(periodo = ifelse(year == 2025, "2025", "1991-2020")) %>%
  group_by(lon, lat, periodo) %>%
  summarise(t2m = mean(t2m, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = t2m) %>%
  mutate(variazione = `2025` - `1991-2020`)



# Crea sf dei tile
tile_points = tile_means %>%
  select(lon, lat) %>%
  distinct()

sf_tile_points = st_as_sf(tile_points, coords = c("lon", "lat"), crs = 4326)

# Stati europei esclusa Russia e Turchia
europe_states = ne_countries(continent = "Europe", returnclass = "sf") %>%
  filter(!admin %in% c("Russia", "Turkey"))

# Join spaziale
points_in_europe = st_join(sf_tile_points, europe_states) %>%
  filter(!is.na(admin)) %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  distinct(lon, lat)



tile_means_europe = tile_means %>%
  semi_join(points_in_europe, by = c("lon", "lat"))



cell_size = 0.25  # puoi aumentare per velocizzare il plot

# Crea i poligoni dei tile
tile_polygons = tile_means_europe %>%
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
  ungroup() %>%
  st_as_sf(crs = 4326)

# Confini europei
europe_borders <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(
    continent == "Europe",
    !admin %in% c("Russia", "Turkey")
  )

font_add_google("Source Sans Pro")
showtext_auto()



theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = c(0.07, 0.05),
      legend.justification = c("left", "bottom"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.5, 0, 0.5, 0.5), "cm"),
      legend.key.height = unit(1.8, 'cm'),
      legend.key.width = unit(0.7, 'cm'),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.text = element_text(
        size = 55, color = "#1C1C1C", hjust = 0,
        margin = margin(b = 0, t = 0, l = 0.2, r=0, unit = "cm")
      ),
      plot.caption = element_text(
        size = 50, color = "#1C1C1C", hjust = 1,
        margin = margin(b = 0, t = 1, l = 0, r = 1, unit = "cm")
      ),
      plot.title = element_text(
        size = 90, color = "#1C1C1C", hjust = 0,
        margin = margin(b = 0, t = 0, l = 0, r = 0, unit = "cm")
      ),
      plot.subtitle = element_text(
        size = 50, color = "#1C1C1C", hjust = 0,
        margin = margin(b = 0, t = 0.2, l = 0, unit = "cm")
      ),
      ...
    )
}





robin_proj <- "+proj=robin +datum=WGS84"

tile_polygons_proj <- st_transform(tile_polygons, crs = robin_proj)
europe_borders_proj <- st_transform(europe_borders, crs = robin_proj)

png("Temperature_Europa_2025_07", width = 10, height = 9, units="in", res=300)
ggplot() +
  geom_sf(data = tile_polygons_proj, aes(fill = variazione), color = NA) +
  geom_sf(data = europe_borders_proj, fill = NA, color = "black", size = 0.3) +
  scale_fill_stepsn(
    breaks = seq(-1, 5, by = 0.5),
    labels = ifelse(
      seq(-1, 5, by = 0.5) %% 1 == 0,
      ifelse(seq(-1, 5, by = 0.5) > 0, paste0("+", seq(-1, 5, by = 0.5)), as.character(seq(-1, 5, by = 0.5))),
      ""
    ),
    values = rescale(c(-1, 0, 5)),
    limits = c(-1, 5),
    colours = c("#0478EA", "white", "#F12938"),
    space = "Lab",
    na.value = "white",
    guide = "coloursteps",
    aesthetics = "fill",
    oob = scales::squish
  ) +
  labs(
    title = "Quanto caldo ha fatto a luglio",
    subtitle = "Differenza tra la temperatura media di luglio 2025 e la media del periodo 1991–2020",
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus (ERA5-Land Daily)"  ) +
  coord_sf(xlim = c(-1744017, 2907441), ylim = c(3836430,  7437934),
           expand = FALSE,
           crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_map()
dev.off()






tile_with_country <- st_join(tile_polygons_proj, europe_borders_proj["admin"], join = st_intersects) %>%
  filter(!is.na(admin))  

# Calcola la media della variazione per Stato
media_per_stato <- tile_with_country %>%
  st_drop_geometry() %>%
  group_by(admin) %>%
  summarise(
    variazione_media = mean(variazione, na.rm = TRUE),
    `2025` =  mean(`2025`, na.rm = TRUE),
    `1991-2020`=  mean(`1991-2020`, na.rm = TRUE),
    n_tile = n()
  ) %>%
  arrange(desc(variazione_media))


write.csv(media_per_stato, "media_per_stato.csv", row.names = F)