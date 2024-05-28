library(tidyverse)
library(sf)
library(janitor)
library(data.table)
library(showtext)

comuni = read_sf("Input/comuni.json")
dati = fread("Input/pop_comuni.csv")%>%
  filter(et_a == 999)

data = left_join(comuni, dati, by=c("PRO_COM" = "codice_comune"))%>%
  mutate(value = is.na(comune))

data%>%
  st_drop_geometry(data_all)%>%
  group_by(COD_REG)%>%
  count(value)%>%
  mutate(pct = n / sum(n))%>%
  filter(value==FALSE)%>%
  arrange(desc(pct))

sf_use_s2(FALSE)
          
regioni = comuni %>%
  st_make_valid()%>%
  group_by(COD_REG) %>%
  summarise(geometry = st_union(geometry))%>%
  st_as_sf()


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
      plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "cm"),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0,   t = 0,    l = 0, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0,   t = -1,   l = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0,   t = 0,    l = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0,   t = 0.25, l = 0, r=0, unit = "cm")),
      ...
    )
}



png("Comuni_Meno_Cinquecento_Abitanti.png", width = 9.5, height = 10, units="in", res=300)
ggplot()  +
  theme_map()+
  geom_sf(data = data, mapping=aes(geometry = geometry, fill=value), color="#e1e1e1", lwd = 0, show.legend = F )+
  geom_sf(data = regioni, mapping=aes(geometry = geometry), fill="white", alpha=0, color="#1C1C1C", lwd = 0.2, show.legend = F )+
  scale_fill_manual(values = c("TRUE" = "#e1e1e1" , "FALSE" = "#F12938"))+
  labs(x = NULL, 
       y = NULL, 
       title = "I comuni con meno di 500 abitanti", 
       subtitle = "Popolazione residente a inizio 2024", 
       caption =  "Elaborazione di Lorenzo Ruffino | Fonte dati: Istat")
dev.off()
