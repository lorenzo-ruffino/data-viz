library(tidyverse)
library(terra)
library(giscoR)
library(tidyterra)
library(showtext)

# Fonte originale codice: https://github.com/milos-agathon/map-population-change/blob/main/R/main.r

urls <- c(
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E1990_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0.zip",
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.zip"
)

options(timeout = 300)

for (url in urls) {
  download.file(
    url = url,
    path = getwd(),
    destfile = basename(url)
  )
}

lapply(
  basename(urls),
  unzip
)



file_names <- list.files(
  path = getwd(),
  pattern = "tif$",
  full.names = T
)

pop_rasters <- lapply(
  file_names,
  terra::rast
)


get_country_borders <- function() {
  country <- giscoR::gisco_get_countries(
    country = "IT",
    resolution = "1"
  )
  
  return(country)
}

country <- get_country_borders()


country_pop_rasters <- lapply(
  pop_rasters,
  function(x) {
    terra::crop(
      x,
      terra::vect(country),
      snap = "in",
      mask = T
    )
  }
)


crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

pop_change <- (
  country_pop_rasters[[2]] - country_pop_rasters[[1]]
) |>
  terra::project(crs_lambert)


get_categories <- function(x){
  terra::ifel(
    pop_change == 0, 0,
    terra::ifel(
      pop_change > 0, 1,
      terra::ifel(
        pop_change < 0, -1, pop_change
      )
    )
  )
}

pop_change_cats <- get_categories(pop_change) |>
  as.factor()


regioni = read_sf("https://raw.githubusercontent.com/lorenzo-ruffino/data-viz/main/Indicatori%20demografici%202023/Input/regioni.json")


font_add_google("Source Sans Pro")
showtext_auto()


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="top",
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank() ,
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title=element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.key.height = unit(0.5, 'cm'), 
      legend.key.width = unit(2, 'cm'),
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0.5, t = -1, l = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 1, l = 0, r = 1, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, t = 0.25, l = 0, unit = "cm")),
      ...
    )
}


png("Popolazione_Italia_1990_2020.png", width = 9.5, height = 10, units="in", res=300)
ggplot() +
  tidyterra::geom_spatraster(
    data = pop_change_cats
  ) +
  geom_sf(data = regioni, mapping=aes(geometry = geometry), color = "#1C1C1C",  lwd = .2, fill="white", alpha=0)+
  scale_fill_manual(
    name = "",
    values = c("#F12938","grey80","#0478EA"),
    labels = c("Calo", "Zone inabitate","Crescita"),
    na.translate = FALSE
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      label.position = "bottom",
      label.hjust = .5,
      label.vjust = 0,
      nrow = 1,
      byrow = T,
      drop = T
    )
  ) +
  coord_sf(crs = crs_lambert) +
  theme_map()+
  labs(x = NULL, 
       y = NULL, 
       title = "Com'è cambiata la popolazione in 30 anni", 
       subtitle = "Variazione della popolazione ogni tra 1990 e 2020", 
       caption =  "Elaborazione di Lorenzo Ruffino | Fonte dati: Global Human Settlement Layer")
dev.off()
