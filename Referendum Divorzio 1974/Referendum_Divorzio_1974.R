library(data.table)
library(tidyverse)
library(sf)
library(stringr)
library(showtext)
library(scales)

referendum = as.data.frame(fread("Input/referendum-19740512.txt"))%>%
  mutate(macro_regione = case_when(
    REGIONE %in% c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "LIGURIA") ~ "Nord-Ovest",
    REGIONE %in% c("TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA", "EMILIA-ROMAGNA") ~ "Nord-Est",
    REGIONE %in% c("TOSCANA", "UMBRIA", "MARCHE", "LAZIO") ~ "Centro",
    REGIONE %in% c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA")  ~ "Sud",
    REGIONE %in% c("SICILIA", "SARDEGNA") ~ "Isole"))


data_prov = referendum %>%
  group_by(REGIONE, PROVINCIA)%>%
  summarise(tot_si = sum(NUMVOTISI, na.rm = T),
            tot_no = sum(NUMVOTINO, na.rm = T))%>%
  mutate(pct_no = (tot_no / (tot_no + tot_si))*100)%>%
  mutate(PROVINCIA = tolower(trimws(PROVINCIA)),
         PROVINCIA = case_when(
                               PROVINCIA == "forli'" ~ "forli",
                               PROVINCIA == "reggio emilia" ~ "reggio nell'emilia",
                               PROVINCIA == "pesaro e urbino" ~ "pesaro urbino",
                               PROVINCIA == "massa-carrara" ~ "massa-carrara",
                               PROVINCIA == "bolzano" ~ "bolzano - bozen",
                               PROVINCIA == "reggio calabria" ~ "reggio di calabria",
                               PROVINCIA == "aosta" ~ "valle d'aosta",
                               TRUE ~ PROVINCIA))


prov <- st_read("Input/Province_1971/Province_1971.shp") %>%
  st_set_geometry(NULL) %>% 
  mutate(DEN_PROV = iconv(DEN_PROV, from = "latin1", to = "UTF-8"),
         DEN_PROV = tolower(trimws(DEN_PROV)),
         DEN_PROV = if_else(grepl("for", DEN_PROV), "forli", DEN_PROV)) %>%
  st_set_geometry(st_geometry(prov)) 

data = left_join(prov, data_prov, by=c("DEN_PROV"="PROVINCIA"))

regioni = data %>%
  group_by(REGIONE) %>%
  summarise(geometry = st_union(geometry))



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
      plot.caption.position =  "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0.5, t = -1, l = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 1, l = 0, r = 1, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = -0.2, t = 0.25, l = 0, unit = "cm")),
      ...
    )
}




png("Referendum_Divorzio_1974.png", width = 9.5, height = 10, units="in", res=300)
ggplot()  +
  theme_map()+
  geom_sf(data = data, mapping=aes(geometry = geometry, fill=pct_no), color = "#a6a6a6",  lwd = 0.4 )+
  geom_sf(data = regioni, mapping=aes(geometry = geometry), color = "#1C1C1C",  lwd = 10, fill="white", alpha=0)+
  scale_fill_stepsn(
    breaks= seq(35, 75, by=5),
    values=rescale(c(35, 50, 75)),
    limits = c(35, 75),
    colours = c("#F12938", "white", "#0478EA"),
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    oob = scales::squish,
    labels = function(x) paste0(round(x/1, 1), "%"),
    guide_colourbar( label.position = "left", direction = "vertical", barheight = 100, barwidth = 40, frame.colour = "black", ticks.colour = "black"))+
  labs(x = NULL, 
       y = NULL, 
       title = "Il referendum sul divorzio del 1974", 
       subtitle = "% di voti contrari all'abolizione del divorzio al referendum del 1974", 
       caption =  "Elaborazione di Lorenzo Ruffino | Fonte dati: Ministero dell'Interno")
dev.off()



referendum %>%
  group_by(macro_regione)%>%
  summarise(tot_si = sum(NUMVOTISI, na.rm = T),
            tot_no = sum(NUMVOTINO, na.rm = T))%>%
  mutate(pct_no = (tot_no / (tot_no + tot_si))*100)

