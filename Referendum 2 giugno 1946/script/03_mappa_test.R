suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
})

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum 2 giugno 1946"
g <- st_read(file.path(BASE, "output", "referendum_1946_su_confini_1991_simplified.geojson"),
             quiet = TRUE)

cat("Comuni con %SI:\n")
summary(g$PERC_SI)
cat("Geometrie vuote:", sum(st_is_empty(g)), "\n")

# Mappa di test
p <- ggplot(g) +
  geom_sf(aes(fill = PERC_SI), color = NA) +
  scale_fill_gradient2(low = "#762a83", mid = "#f7f7f7", high = "#1b7837",
                       midpoint = 50, na.value = "grey80",
                       name = "% Repubblica", limits = c(0, 100)) +
  coord_sf(crs = 4326) +
  theme_void(base_family = "Helvetica") +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.position = "right") +
  labs(title = "Referendum 2 giugno 1946 - % voti per la Repubblica",
       subtitle = "Risultati comunali (Min. Interno) su confini Istat 1991",
       caption = "Fonte: Ministero dell'Interno, Istat | Cartografia: Istat 1991")

ggsave(file.path(BASE, "output", "mappa_test_referendum_1946.png"),
       p, width = 8, height = 10, dpi = 150)
cat("Salvata: output/mappa_test_referendum_1946.png\n")
