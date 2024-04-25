library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(scales)
library(showtext)


link = "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2022.zip?d=1615465800"

download(link, dest="mef.zip", mode="wb") 
unzip ("mef.zip", exdir = "Input")

data = as.data.frame(fread("Input/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2022.csv"))%>%
  mutate(across(8:ncol(.), ~ as.numeric(gsub("\\.", "", .))))%>%
  clean_names() %>%
  mutate(sigla_provincia = ifelse(is.na(sigla_provincia), "NA", sigla_provincia))%>%
  group_by(sigla_provincia)%>%
  summarise(reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = T),
            contribuenti = sum(reddito_imponibile_frequenza , na.rm = T))%>%
  ungroup()%>%
  mutate(media = reddito / contribuenti,
         media_italia = sum(reddito)/sum(contribuenti))


province = read_sf("province.json")
regioni = read_sf("regioni.json")


data = left_join(province, data, by=c("SIGLA" = "sigla_provincia"))


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
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      legend.key.height = unit(1.9, 'cm'), 
      legend.key.width = unit(0.8, 'cm'),
      plot.title.position = "plot",
      legend.text =    element_text(size = 40,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0.5, t = -1, l = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 0.25, l = 0, r = 0, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 0.25, l = 0, unit = "cm")),
      ...
    )
}


min = min(data$media)
max = max(data$media)


png("Reddito_Imponibile_Provincia.png", width = 9.5, height = 10, units="in", res=300)
ggplot()  +
  theme_map()+
  geom_sf(data = data, mapping=aes(geometry = geometry, fill=media), color = "#a6a6a6",  lwd = 0.4 )+
  geom_sf(data = regioni, mapping=aes(geometry = geometry), color = "#1C1C1C",  lwd = 10, fill="white", alpha=0)+
  scale_fill_stepsn(
    breaks= seq(16000, 32000, by=2000),
    values=rescale(c(16000, 22800, 32000)),
    limits = c(16000, 32000),
    colours = c("#F12938", "white", "#0478EA"),
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    oob = scales::squish,
    labels = function(x) paste0(round(x/1000, 0), " mila"),
    guide_colourbar( label.position = "left", direction = "vertical", barheight = 100, barwidth = 40, frame.colour = "black", ticks.colour = "black"))+
  annotate(geom = "richtext", y = 39.49931, x = 12.67170, label = "In <span style='color:#0478EA'>blu</span> le aree con un<br>dato maggiore della <b>media italiana</b>,<br>in <span style='color:#F12938'>rosso</span> quelle con un dato minore<br>e in bianco quelle simili alla media.",
           size = 14, fill = NA, label.color = NA, lineheight = 0.5)+
  labs(x = NULL, 
       y = NULL, 
       title = "Il reddito medio nel 2022", 
       subtitle = "Reddito medio imponibile dichiarato nel 2023 per il 2022", 
       caption =  "Elaborazione di Lorenzo Ruffino | Fonte dati: Ministero dell'Economia")
dev.off()

