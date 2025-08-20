library(tidyverse)
library(rvest)
library(showtext)
library(janitor)

"https://it.wikipedia.org/wiki/Governi_italiani_per_durata" %>%
  read_html() ->
  wikipage

tables = wikipage  %>%
  html_nodes("table")  %>%
  html_table(na.strings = "_")

data = tables[[1]]%>%
  clean_names()%>%
  select(giorni_effettivi_2, governo)%>%
  mutate(giorni = as.numeric(gsub("\\[5\\]|\\[4\\]", "", giorni_effettivi_2)),
         colore = governo == "Meloni",
         governo = trimws(governo))%>%
  arrange(desc(giorni))%>%
  slice(1:30)

data$governo <- reorder(data$governo, data$giorni)


font_add_google("Source Sans Pro")
showtext_auto()

theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.y = element_text(size = 40,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.text.x = element_blank(),
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
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = 0.1, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 0.5, r=0, unit = "cm")),
      ...
    )
}



png("Durata_Governi.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=giorni, y=governo, fill=colore, label = giorni)) +
  theme_barchart()+
  geom_col(show.legend = F)+
  scale_fill_manual(values =c("TRUE"="#F12938", "FALSE"="#0478EA"))+
  geom_text(size=15, hjust=-0.2, show.legend = FALSE, family="Source Sans Pro")+
  scale_x_continuous(limits=c(0,1500), expand = c(0.01, 0))+
  labs(x = NULL,
       y = NULL,
       title = "La durata dei governi", 
       subtitle = "Primi 30 governi per durata effettiva | Dato del governo Meloni aggiornato al 6 agosto 2024", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Wikipedia"))
dev.off()
