library(tidyverse)
library(sf)
library(maps)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

# --- Dati: televoto a Israele all'Eurovision 2026 ---------------------------
# Punti assegnati a Israele dal pubblico (televoto) di ciascun paese.

data <- tibble(
  country = c(
    "Azerbaijan", "Portugal", "Switzerland", "Germany", "France", "Finland",
    "Georgia", "Albania", "UK",
    "Belgium", "Moldova", "Cyprus", "Italy",
    "Armenia", "Malta", "Sweden", "Austria",
    "Rest of the World", "Romania",
    "Greece", "Ukraine", "Czech Republic", "Bulgaria", "Norway",
    "Latvia", "Montenegro", "Serbia",
    "San Marino",
    "Denmark", "Poland",
    "Estonia", "Luxembourg",
    "Australia", "Croatia", "Lithuania"
  ),
  points = c(
    12, 12, 12, 12, 12, 12,
    10, 10, 10,
    8, 8, 8, 8,
    7, 7, 7, 7,
    6, 6,
    5, 5, 5, 5, 5,
    4, 4, 4,
    3,
    2, 2,
    1, 1,
    0, 0, 0
  )
)

data_long <- data %>%
  mutate(category = case_when(
    points == 12 ~ "Più votato",
    points == 10 ~ "Secondo più votato",
    points == 8  ~ "Terzo più votato",
    points == 0  ~ "Nessun punto",
    TRUE         ~ "Altre posizioni"
  )) %>%
  select(country, category)

write_csv(data, "../output/eurovision_2026.csv")

# --- Geometrie Europa -------------------------------------------------------
# Polygoni dal pacchetto `maps`, ritagliati alla finestra europea (esclude
# le Svalbard) e proiettati in EPSG:3035 (azimutale equivalente: niente
# ingrandimento del nord). coord_sf(expand = FALSE) fa aderire la mappa al
# pannello senza margini bianchi.

regioni <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark",
             "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein", "Lithuania",
             "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia",
             "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "UK", "Vatican")

sf_use_s2(FALSE)

geo <- maps::map("world", regions = regioni, fill = TRUE, plot = FALSE) %>%
  st_as_sf() %>%
  st_set_crs(4326) %>%
  st_crop(xmin = -25, ymin = 34, xmax = 51, ymax = 72) %>%
  mutate(country = sub(":.*", "", ID)) %>%
  group_by(country) %>%
  summarise(.groups = "drop") %>%
  left_join(data_long, by = "country") %>%
  mutate(category = factor(
    if_else(is.na(category), "Non ha partecipato", category),
    levels = c("Più votato", "Secondo più votato", "Terzo più votato",
               "Altre posizioni", "Nessun punto", "Non ha partecipato")
  )) %>%
  st_transform(3035)

bb <- st_bbox(geo)

# --- Tema mappa -------------------------------------------------------------

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      legend.justification = "left",
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
      legend.margin = margin(b = 0.15, t = 0.1, unit = "cm"),
      legend.key.height = unit(0.45, "cm"),
      legend.key.width = unit(0.45, "cm"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.text = element_text(size = 9, color = "#1C1C1C", hjust = 0),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.3, unit = "cm")),
      plot.title = element_text(size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.2, t = 0.1, unit = "cm")),
      ...
    )
}

# --- Grafico ----------------------------------------------------------------

p <- ggplot(geo) +
  geom_sf(aes(fill = category), color = "white", linewidth = 0.12) +
  scale_fill_manual(
    values = c(
      "Più votato"         = "#0044A6",
      "Secondo più votato" = "#0175C8",
      "Terzo più votato"   = "#29ABCB",
      "Altre posizioni"    = "#FB77C6",
      "Nessun punto"       = "#F12938",
      "Non ha partecipato" = "#E4E4E4"
    ),
    drop = FALSE
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
           ylim = c(bb["ymin"], bb["ymax"]),
           expand = FALSE) +
  labs(
    title = "Il televoto a Israele all'Eurovision",
    subtitle = "Posizione di Israele tra i paesi votati al televoto, Eurovision 2026",
    caption = "Elaborazione di Lorenzo Ruffino su dati Eurovision"
  ) +
  theme_map()

# Dimensioni figura calibrate sull'aspect ratio della mappa, così il
# pannello aderisce alla mappa: ~2 pollici extra in altezza per
# titolo + legenda + caption.
map_aspect <- as.numeric((bb["xmax"] - bb["xmin"]) / (bb["ymax"] - bb["ymin"]))
fig_w <- 7
panel_h <- (fig_w - 0.24) / map_aspect
fig_h <- panel_h + 2.0

ggsave("../output/eurovision_2026.png", p,
       width = fig_w, height = fig_h, units = "in", dpi = 300, bg = "white")

cat("map_aspect:", round(map_aspect, 3), " figura:", fig_w, "x", round(fig_h, 2), "\n")
