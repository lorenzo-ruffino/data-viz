library(data.table)
library(tidyverse)
library(janitor)
library(readxl)
library(downloader)
library(ggrepel)
library(showtext)

link = "https://www.istat.it/it/files//2018/07/Appendice-statistica_new.zip"

download(link, dest="bes.zip", mode="wb") 
unzip ("bes.zip", exdir = "Input")

# Occupazione per titolo di studio

data = read_xlsx("Input/Appendice statistica_new/Indicatori_per_titolo_studio.xlsx")%>%
  clean_names()%>%
  filter(indicatore == "Tasso di occupazione (20-64 anni)"
         & sesso == "Totale"
         & eta == "35-54")%>%
  mutate(categoria = case_when(titolo_studio == "Licenza media/Elementare/Nessun titolo (ISCED 0, 1, 2)" ~ "Medie o meno",
                               titolo_studio == "Diploma superiore (ISCED 3, 4)" ~ "Superiori",
                               titolo_studio == "Accademia/Diploma universitario/Laurea/Specializzazione/Dottorato (ISCED 5, 6, 7, 8)" ~ "Laurea o più",
                               TRUE ~ "Totale"),
         colore = categoria == "Totale")


data$categoria <- factor(data$categoria, levels=c( "Medie o meno", "Superiori", "Laurea o più", "Totale"))



font_add_google("Source Sans Pro")
showtext_auto()


theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.x = element_text(size = 55,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
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
      plot.margin = unit(c(b = 1, l = 1, t = 1, r=1), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = 0.25, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 1, r=0, unit = "cm")),
      ...
    )
}



png("Bes_Occupazione_Titolo_Studio.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=categoria, y=x2023, fill=colore, label = paste0(round(x2023,1), "%"))) +
  theme_barchart()+
  geom_col(show.legend = F)+
  scale_fill_manual(values =c("TRUE"="#F12938", "FALSE"="#0478EA"))+
  geom_text(size=18, hjust=0.5, vjust=1.8, show.legend = FALSE, family="Source Sans Pro", colour="white")+
  scale_y_continuous(limits=c(0,90), expand = c(0.01, 0))+
  labs(x = NULL,
       y = NULL,
       title = "L'occupazione tra i laureati è maggiore", 
       subtitle = "Tasso di occupazione tra i 35 e i 54 anni nel 2023 per titolo di studio", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Istat (Bes)"))
dev.off()


#Sicurezza Perceputa


data = read_xlsx("Input/Appendice statistica_new/Indicatori_per_titolo_studio.xlsx")%>%
  clean_names()%>%
  filter(indicatore == "Percezione di sicurezza camminando da soli quando è buio"
         & titolo_studio == "Totale"
         & eta == "Totale")%>%
  gather(categoria, valore, x2010:x2023)%>%
  mutate(categoria = as.numeric(gsub("x", "", categoria)),
         label = if_else(categoria == max(categoria), as.character(sesso), NA_character_))



theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      axis.line = element_line(linewidth = 0.6),
      axis.text = element_text(size = 55,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.title = element_blank(),
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
      plot.margin = unit(c(b = 1, l = 1, t = 1, r=1), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0.7, l = 0, t = 0.25, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 0.7, r=0, unit = "cm")),
      ...
    )
}




png("Bes_SicurezzaPercepita.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=categoria, y=valore, color=sesso)) +
  theme_linechart()+
  geom_line(linewidth=1.3, show.legend = F)+
  scale_color_manual(values =c("Femmine"="#F12938", "Maschi"="#0478EA", "Totale"="#34A853"))+
  #geom_text(size=18, hjust=0.5, vjust=1.8, show.legend = FALSE, family="Source Sans Pro", colour="white")+
  scale_y_continuous(limits=c(30, 80), breaks=seq(30, 80, by=10), expand = c(0.01, 0), labels = function(x) paste0(x, "%"))+
  scale_x_continuous(limits=c(2010, 2024),  breaks=seq(2011, 2023, by=2), expand = c(0.01, 0))+
  geom_text_repel(aes(label = label), size=17, family="Source Sans Pro",
                   nudge_x = 0.2,
                   na.rm = TRUE, show.legend = F)+
  labs(x = NULL,
       y = NULL,
       title = "Migliora la percezione di sicurezza", 
       subtitle = "Percezione di sicurezza camminando da soli quando è buio, anni 2010-2023", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Istat (Bes)"))
dev.off()



# obesità



data = read_xlsx("Input/Appendice statistica_new/Indicatori_per_eta_sesso.xlsx")%>%
  clean_names()%>%
  filter(indicatore == "Eccesso di peso (tassi grezzi)"
         & sesso == "Totale")%>%
  mutate(colore = eta == "Totale",
         x2023 = as.numeric(x2023))


data$eta <- factor(data$eta, levels=c('18-19',
                                      '20-24',
                                      '25-34',
                                      '35-44',
                                      '45-54',
                                      '55-59',
                                      '60-64',
                                      '65-74',
                                      '75 e più',
                                      'Totale'))




theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.x = element_text(size = 55,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
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
      plot.margin = unit(c(b = 1, l = 1, t = 1, r=1), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = 0.25, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 1, r=0, unit = "cm")),
      ...
    )
}



png("Bes_Obesità.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=eta, y=x2023, fill=colore, label = paste0(round(x2023,1), "%"))) +
  theme_barchart()+
  geom_col(show.legend = F)+
  scale_fill_manual(values =c("TRUE"="#F12938", "FALSE"="#0478EA"))+
  geom_text(size=18, hjust=0.5, vjust=1.8, show.legend = FALSE, family="Source Sans Pro", colour="white")+
  scale_y_continuous(limits=c(0,60), expand = c(0.01, 0))+
  labs(x = NULL,
       y = NULL,
       title = "Quanto è diffusa l'obesità", 
       subtitle = "Persone di 18 anni e più in sovrappeso o obese in Italia, anno 2023", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Istat (Bes)"))
dev.off()





