library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(showtext)

font_add_google("Source Sans Pro")
showtext_auto()

data <- data.frame(
  country = c(
    "Rest of the World", "Australia", "Azerbaijan", "Belgium", "France", "Germany",
    "Luxembourg", "Netherlands", "Portugal", "Spain", "Sweden", "Switzerland", "UK",
    "Cyprus", "Czech Republic", "Finland", "Ireland", "Norway", "San Marino",
    "Denmark", "Italy",
    "Albania", "Austria", "Georgia", "Greece", "Latvia", "Montenegro",
    "Slovenia",
    "Malta",
    "Iceland",
    "Lithuania",
    "Estonia", "Serbia",
    "Ukraine", "Armenia", "Croatia", "Poland"
  ),
  points = c(
    12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12,
    10, 10, 10, 10, 10, 10,
    8, 8,
    7, 7, 7, 7, 7, 7,
    6,
    5,
    4,
    3,
    2, 2,
    1, 0, 0, 0
  )
)




data_long <- data %>%
  mutate(category = case_when(
    points == 12 ~ "Più votato",
    points == 10 ~ "Secondo più votato",
    points == 8 ~ "Terzo più votato",
    points == 0 ~ "Nessun punto",
    TRUE ~ "Altre posizioni"
  )) %>%
  select(country, category)

europe_map <- map_data("world") %>%
  filter(region %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                       "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein", "Lithuania", 
                       "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia", 
                       "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "UK", "Vatican"))

europe_map <- europe_map %>%
  filter(!(subregion %in% c("Svalbard", "Jan Mayen")))

map_data <- left_join(europe_map, data_long, by = c("region" = "country"))


map_data$category <- factor(map_data$category, levels=c("Più votato", "Secondo più votato", "Terzo più votato", "Altre posizioni", "Nessun punto"))


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
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      legend.key.height = unit(0.6, 'cm'), 
      legend.key.width = unit(0.6, 'cm'),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0.5, t = 0.5, l = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 0, l = 0, r = 1, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, t = 0.25, l = 0, unit = "cm")),
      ...
    )
}

png("Eurovision.png", width = 9.5, height = 10, units = "in", res = 300)

ggplot() +
  geom_map(data = europe_map, map = europe_map,
           aes(long, lat, map_id = region),
           fill = "lightgray", color = "white", size = 0.1) +
  geom_map(data = map_data, map = map_data,
           aes(long, lat, map_id = region, fill = category),
           color = "white", size = 0.1, na.rm = TRUE) +
  scale_fill_manual(
    values = c(
      "Più votato" = "#0044A6",
      "Secondo più votato" = "#0175C8",
      "Terzo più votato" = "#29ABCB",
      "Altre posizioni" = "#FB77C6",
      "Nessun punto" = "#F25268"
    ),
    drop = TRUE,
    na.translate = FALSE
  ) +
  labs(
    x = NULL, 
    y = NULL, 
    title = "Il televoto a Israele all'Eurovision", 
    subtitle = "Posizione ottenuta da Israele al televoto dell'Eurovision del 2025", 
    caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Eurovision"
  ) +
  theme_map()

dev.off()
