library(tidyverse)
library(showtext)
library(showtext)

data = data.frame(
                  tipologia = c("Non dimessi", "Dimessi"),
                  valore = c((73-13), 13)
                  )


font_add_google("Source Sans Pro")
showtext_auto()

theme_donut <- function(...) {
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
      plot.title =     element_text(size = 85,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 1, l = 0, r = 0, unit = "cm")),
      plot.subtitle =  element_text(size = 55,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 0.2, l = 0, unit = "cm")),
      ...
    )
}


png("Eurodeputati_Dimessi.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x = "", y=valore, fill=tipologia)) +
  theme_donut()+
  geom_bar(width = 1, stat = "identity", color = "white", show.legend = F)+
  scale_fill_manual(values =c("Dimessi"="#F12938", "Non dimessi"="#0478EA" ))+
  coord_polar(theta = "y", direction = -1) +
  annotate("text",
           label = "Il 18%\ndegli eurodeputati\nsi è dimesso\nper un altro incarico",
           family = "Source Sans Pro",
           #fontface = "bold",
           color = "#F12938",
           size = 25,
           x = -2,
           y = 0,
           lineheight = 0.35)+
  labs(x = NULL,
       y = NULL,
       title = "Quanti eurodeputati hanno lasciato il Parlamento Ue",
       subtitle = "% di europarlamentari eletti nel 2019 che si sono dimessi per un altro incarico", 
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Wikipedia"))
dev.off()
