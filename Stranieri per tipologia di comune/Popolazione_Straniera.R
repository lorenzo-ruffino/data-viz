library(data.table)
library(tidyverse)
library(janitor)
library(downloader)
library(showtext)


# Dati stranieri

link_1 = "https://demo.istat.it/data/strasa/STRASA_2024_it_Comuni.zip"

download(link_1, dest="stranieri_data.zip", mode="wb") 
unzip ("stranieri_data.zip", exdir = "Input")

comuni_stranieri = as.data.frame(fread("Input/STRASA_2024_it_Comuni.csv"))


# Dati popolazione

link_2 = "https://demo.istat.it/data/posas/POSAS_2024_it_Comuni.zip"

download(link_2, dest="pop_data.zip", mode="wb") 
unzip ("pop_data.zip", exdir = "Input")

comuni_pop = as.data.frame(fread("Input/POSAS_2024_it_Comuni.csv"))


# Dati per tipologia di comune

link_3 = "https://www.istat.it/storage/codici-unita-amministrative/Elenco-codici-statistici-e-denominazioni-delle-unita-territoriali.zip"

download(link_3, dest="comuni_data.zip", mode="wb") 
unzip ("comuni_data.zip", exdir = "Input")

comuni_data = as.data.frame(fread("Input/Elenco-codici-statistici-e-denominazioni-delle-unit…-territoriali/Codici-statistici-e-denominazioni-al-22_01_2024.csv"))



# Pulizia

data_pop = comuni_pop %>%
  clean_names()%>%
  filter(et_a == 999)%>%
  select(codice_comune, comune, totale)%>%
  rename(totale_pop = totale)

data_str = comuni_stranieri %>%
  clean_names()%>%
  filter(et_a == 999)%>%
  mutate(totale = maschi + femmine)%>%
  select(codice_comune, totale)%>%
  rename(totale_str = totale)

nrow(data_pop) == nrow(data_str)

data_0 = inner_join(data_pop, data_str, by="codice_comune")



comuni = comuni_data %>%
  clean_names()%>%
  mutate(flag_capoluogo_regione = ifelse( denominazione_in_italiano %in% c(
    "Aosta", "Torino", "Genova", "Milano", "Trento", "Venezia",  "Trieste",
    "Bologna", "Firenze", "Ancona", "Perugia", "Roma", "L'Aquila", "Campobasso",
    "Napoli", "Bari", "Potenza", "Catanzaro", "Palermo", "Cagliari"), 1, 0))%>%
  mutate(categoria = case_when(flag_capoluogo_regione == 1 ~ "Capoluoghi Regione",
                               flag_comune_capoluogo_di_provincia_citta_metropolitana_libero_consorzio == 1 ~ "Capoluoghi Provincia",
                               TRUE ~ "Altri comuni"))%>%
  select(codice_comune_formato_alfanumerico, ripartizione_geografica, categoria)%>%
  rename(codice_comune = codice_comune_formato_alfanumerico)%>%
  mutate(ripartizione_geografica = ifelse(ripartizione_geografica %in% c("Sud", "Isole"), "Sud-Isole", ripartizione_geografica))



data_ita = inner_join(data_0, comuni, by="codice_comune")%>%
  group_by(categoria)%>%
  summarise(totale_pop = sum(totale_pop),
            totale_str = sum(totale_str))%>%
  mutate(perc = totale_str / totale_pop * 100,
         ripartizione_geografica = "Italia")

data = inner_join(data_0, comuni, by="codice_comune")%>%
  group_by(categoria, ripartizione_geografica)%>%
  summarise(totale_pop = sum(totale_pop),
            totale_str = sum(totale_str))%>%
  mutate(perc = totale_str / totale_pop * 100)%>%
  bind_rows(data_ita)

write.csv(data, "stranieri_categoria_comune.csv")


data$categoria <- factor(data$categoria, levels=c( "Capoluoghi Regione", "Capoluoghi Provincia", "Altri comuni"))

data$ripartizione_geografica <- factor(data$ripartizione_geografica, levels=c("Italia", "Sud-Isole", "Centro", "Nord-est", "Nord-ovest"))


# Tema e grafico

#font_add_google("Source Sans Pro")
showtext_auto()


theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.y = element_text(size = 50,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm"), lineheight = 0.35),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      strip.text = element_text(size = 50,  color = "#1C1C1C", hjust = 0.1),
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
      plot.margin = unit(c(b = 1, l = 1.5, t = 1, r=1.5), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 1, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 1, t = 0.2, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 1, r=0, unit = "cm")),
      ...
    )
}


png("Stranieri.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=perc, y=ripartizione_geografica, group=ripartizione_geografica, fill=categoria, label = paste0(round(perc, 1), "%"))) +
  theme_barchart()+
  facet_wrap(~categoria, nrow=1)+#, scales = "free_x")+
  geom_col(show.legend = F)+
  geom_text(data, mapping=aes(x=perc-.5, y=ripartizione_geografica, group=ripartizione_geografica, label = paste0(round(perc, 1), "%")),
             size=14, hjust=1, colour="white", family="Source Sans Pro")+
  scale_fill_manual(values =c("Capoluoghi Regione"="#F12938", "Capoluoghi Provincia"="#0478EA", "Altri comuni "="#565679" ))+
  #scale_x_continuous(limits=c(0,18), breaks=seq(0, 18, by=2), labels = function(x) paste0(x, "%"), expand = c(0.01, 0))+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 5)) +
  labs(x = NULL,
       y = NULL,
       title = "Dove vivono gli stranieri in Italia",
       subtitle = "Peso della popolazione straniera nel 2023 per tipologia di comune (autoescludenti)", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Istat"))
dev.off()
