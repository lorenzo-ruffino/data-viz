library(tidyverse)
library(showtext)
library(lubridate)
library(eurostat)

data = get_eurostat("ilc_pw01")%>%
  filter(geo %in% c("IT")
         & freq == "A"
         & unit == "RTG"
         & isced11 == "TOTAL"
         & sex == "T"
         & TIME_PERIOD == "2022-01-01"
         & age %in% c("Y16-24", "Y25-34", "Y35-49", "Y50-64", "Y65-74", "Y_GE75", "Y_GE16"))%>%
  mutate(age = gsub("_GE16", "Totale", gsub("_GE75", "75+", gsub("Y", "", age))),
         colore = age == "Totale")

data$age <- factor(data$age, levels=rev(c("Totale", "75+", "65-74", "50-64", "35-49", "25-34", "16-24")))


# Tema e grafico

#font_add_google("Source Sans Pro")
showtext_auto()


theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.x = element_text(size = 60,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0.1, l = 0, unit = "cm"), lineheight = 0.35),
      axis.text.y = element_blank(),
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
      plot.margin = unit(c(b = 0.8, l = 0.8, t = 0.8, r=0.8), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 0, t = 0.15, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 1, r=0, unit = "cm")),
      ...
    )
}


png("Soddisfazione_Vita.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=age, y=values, group=age, fill=colore, label = values)) +
  theme_barchart()+
  geom_col(show.legend = F)+
  geom_text(data, mapping=aes(x=age, y=values+.2, group=age, colour=colore, label = values),
            size=18, hjust=.5, family="Source Sans Pro", fontface="bold", show.legend = F)+
  scale_fill_manual(values =c("TRUE"="#F12938", "FALSE"="#0478EA"))+
  scale_colour_manual(values =c("TRUE"="#F12938", "FALSE"="#0478EA"))+
  scale_y_continuous(limits=c(0,8), expand = c(0, 0))+
  scale_x_discrete(expand = c(0, 0))+
  labs(x = NULL,
       y = NULL,
       title = "I giovani sono più soddisfatti degli anziani",
       subtitle = "Valutazione media su una scala da 0 a 10 della soddisfazione generale per la vita nel 2022 in Italia", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Eurostat"))
dev.off()