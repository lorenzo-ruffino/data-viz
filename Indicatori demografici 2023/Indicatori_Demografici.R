library(tidyverse)
library(sf)
library(janitor)
library(readxl)
library(svglite)
library(scales)


province = read_sf("C:/Users/loren_afkjpxa/Downloads/province.json")
regioni = read_sf("C:/Users/loren_afkjpxa/OneDrive/Documenti/DataVIz_YouTrend/2022_11_21_Aborto/Dati/regioni.json")

dati = read_excel("Tavole_indicatori_demografici.xlsx", sheet = "Foglio1")

data = left_join(province, dati, by=c("COD_PROV" = "Codice_Prov"))


library(showtext)
font_add_google("Source Sans Pro")
showtext_auto()

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
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
      legend.key.height = unit(2.2, 'cm'), 
      legend.key.width = unit(0.8, 'cm'),
       plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0.5, t = -1, l = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 1, l = 0, r = 1, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = -0.2, t = 0.25, l = 0, unit = "cm")),
      ...
    )
}



### Variazione Popolazione nel 2023

min = min(data$Tasso_Variazione_Per_Mille)
max = max(data$Tasso_Variazione_Per_Mille)


#svglite(filename = paste0("Indicatori_Dem_Variazione_pop", ".svg"), width = 10, height = 9.5,standalone = TRUE,id = NULL, fix_text_size = TRUE, scaling = 1, always_valid = FALSE)
png("Indicatori_Dem_Variazione_pop.png", width = 9.5, height = 10, units="in", res=300)
ggplot()  +
  theme_map()+
  geom_sf(data = data, mapping=aes(geometry = geometry, fill=Tasso_Variazione_Per_Mille), color = "#a6a6a6",  lwd = 0.4 )+
  geom_sf(data = regioni, mapping=aes(geometry = geometry), color = "#1C1C1C",  lwd = 10, fill="white", alpha=0)+
  scale_fill_stepsn(
    breaks= seq(-10, 6, by=2),
    values=rescale(c(-10, 0, 6)),
    limits = c(-10, 6),
    colours = c("#F12938", "white", "#0478EA"),
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    oob = scales::squish,
    labels = function(x) paste0(round(x/1, 1), ""),
    guide_colourbar( label.position = "left", direction = "vertical", barheight = 100, barwidth = 40, frame.colour = "black", ticks.colour = "black"))+
  labs(x = NULL, 
       y = NULL, 
       title = "Com'è cambiata la popolazione nel 2023", 
       subtitle = "Variazione della popolazione ogni 1.000 abitanti nel corso del 2023", 
       caption =  "Elaborazione di Lorenzo Ruffino | Fonte dati: Istat")
dev.off()
