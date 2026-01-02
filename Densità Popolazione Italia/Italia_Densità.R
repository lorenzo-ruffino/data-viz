library(tidyverse)
library(sf)
library(ggtext)
library(showtext)
library(viridis)
library(scales)

#Fonte: https://www.istat.it/notizia/statistiche-sulla-popolazione-per-griglia-regolare/

geo = read_sf("~/Downloads/GrigliaPop2021_Ind_ITA.json")

geo$pop = geo$'T'

regioni = read_sf("~/Downloads/Reg01012025_g_WGS84.json")


font_add_google("Source Sans Pro")
showtext_auto()


theme_map2 <- function(...) {
  theme_minimal() +
    theme(
      plot.margin = unit(c(b = .8, l = .8, t = .8, r=.8), "cm"),
      axis.line = element_blank(),
      text=element_text(family="Source Sans Pro"),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.background = element_blank(), 
      legend.background = element_blank(), 
      legend.box.background = element_blank(), 
      legend.key =element_blank(), 
      legend.position = c(0.9, 0.72),
      legend.text = element_text(size = 15, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title = element_text(size = 30, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_markdown(size = 15, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0.2, r=0, unit = "cm")),
      plot.caption = element_text(size = 15, color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = -2, r=0, unit = "cm")),
      ...
    )
}

png("Italia_Densità.png", width = 9.5, height = 10, units="in", res=300)
ggplot(geo %>%
         filter(pop!=0)) +
  geom_sf(mapping=aes(fill = pop, geometry = geometry),
          color = "transparent",
          size = 2) +
  scale_fill_viridis(
    alpha = 1,
    begin = 0,
    end = 1,
    direction = -1,
    oob = scales::squish,
    option = "G",
    breaks = c(1, 10, 100, 1e3, 1e4, 3e4, 1e5),
    labels = scales::number_format(big.mark = " "),
    trans = "pseudo_log",
    name = NULL,
    aesthetics = "fill") +
  guides(fill = guide_colorbar(direction = "vertical", 
                               barheight = unit(9, "cm"), 
                               barwidth = unit(0.6, "cm"), 
                               frame.colour = "white", 
                               ticks.colour = "white")) +
  geom_sf(data = regioni, mapping=aes(geometry = geometry), fill = "white", color = "black", size = 0.3, alpha=0) +
  annotate("label", x = 13, y = 39.5, 
           label = "Le zone bianche\nnon hanno popolazione", 
           size = 5, 
           family = "Source Sans Pro",
           color = "#1C1C1C") +
  theme_map2() +
  labs(x = NULL,
       y = NULL,
       legend = NULL,
       title = "La densità abitativa in Italia",
       subtitle = "Popolazione per chilometro quadrato con scala logaritmica",
       caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Istat")
dev.off()