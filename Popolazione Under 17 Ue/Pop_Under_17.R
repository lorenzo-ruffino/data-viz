library(eurostat)
library(tidyverse)
library(sf)
library(data.table)
library(showtext)
library(scales)

data = get_eurostat("demo_r_pjanind2")%>%
  filter(freq == "A"
         & indic_de   == "PC_Y0_17"
         & TIME_PERIOD == "2023-01-01"
         & !geo %in% c("ES70", "FRY1", "FRY2", "FRY3", "FRY4", "FRY5"))

ue = get_eurostat("demo_pjanind")%>%
  filter(freq == "A"
         & indic_de   == "PC_Y0_17"
         & TIME_PERIOD == "2023-01-01"
         & geo == "EU27_2020")

geo = read_sf("nuts_2021.json")%>%
  filter(LEVL_CODE == 2
         &  CNTR_CODE %in% c('AT',	'BE',	'BG',	'HR',	'CY',	'CZ',	'DK',	'EE',	'FI',	'FR',	'DE',	'EL',		'HU',	'IE',	'IT',		'LV',	'LT',	'LU',	'MT',	'NL',	'PL',	'PT',	'RO',	'SK',	'SI',	'ES',	'SE'))

data = inner_join(geo,data, by = c("NUTS_ID"="geo"))

geo_0 <- data %>%
  group_by(CNTR_CODE) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_as_sf()


font_add_google("Source Sans Pro")
showtext_auto()

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
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
      plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "cm"),
      legend.key.height = unit(2.2, 'cm'), 
      legend.key.width = unit(0.7, 'cm'),
      legend.position = c(0.9, 0.8),  # Posizione della legenda (x, y)
      legend.justification = c(0, 1),  # Punto di ancoraggio della legenda
      legend.direction = "vertical",
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, t = 1, l = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 0, l = 0, r = 0, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 0.25, l = 0, unit = "cm")),
      ...
    )
}


robinson_proj <- st_crs("+proj=robin")

data <- st_transform(data, robinson_proj)
geo_0 <- st_transform(geo_0, robinson_proj)
  
png("Pop_Under_17.png", width = 9.5, height = 9, units="in", res=300)
ggplot()  +
  theme_map()+
  geom_sf(data = data, mapping=aes(geometry = geometry, fill=values), color = "#1C1C1C",  lwd = 0.01)+
  geom_sf(data = geo_0, mapping = aes(geometry = geometry), color = "#1C1C1C", lwd = 0.3, fill = NA, alpha=1) +
  scale_fill_stepsn(
    breaks= seq(12, 24, by=2),
    values=rescale(c(12, 18, 24)),
    limits = c(12, 24),
    colours = c("#F12938", "white", "#0478EA"),
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    oob = scales::squish,
    labels = function(x) paste0(round(x/1, 1), ifelse(x==18, "% = Ue", "%")),
    guide_colourbar( label.position = "left", direction = "vertical", barheight = 100, barwidth = 40, frame.colour = "black", ticks.colour = "black"))+
  labs(x = NULL, 
       y = NULL, 
       title = "Quanti giovani ci sono nell'Ue", 
       subtitle = "Perceentuale di popolazione tra gli 0 e i 17 anni nel 2023", 
       caption =  "Elaborazione di Lorenzo Ruffino | Fonte dati: Eurostat")
dev.off()
