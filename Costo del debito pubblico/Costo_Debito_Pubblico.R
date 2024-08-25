library(eurostat)
library(tidyverse)
library(svglite)
library(showtext)
library(ggrepel)

data_interessi = get_eurostat("gov_10dd_edpt1") %>%
  filter(unit == "MIO_EUR"
         & geo == "IT"
         & sector == "S13"
         & na_item == "D41PAY"
         & TIME_PERIOD >= as.Date("2000-01-01"))%>%
  mutate(values = values / 1000)



data_inflazione = get_eurostat("prc_hicp_aind") %>%
  filter(unit == "INX_A_AVG"
         & geo == "IT"
         & coicop  == "CP00"
         & TIME_PERIOD >= as.Date("2000-01-01"))%>%
  mutate(values = values)%>%
  mutate(indice = round((values / last(values[TIME_PERIOD == "2023-01-01"])),1))%>%
  select(TIME_PERIOD, indice)

data = left_join(data_interessi, data_inflazione, by ="TIME_PERIOD")%>%
  mutate(interessi_riv = values / indice)



font_add_google("Source Sans Pro")
showtext_auto()

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_line(linewidth = 0.5),
      axis.text = element_text(size = 50,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
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
      plot.margin = unit(c(b = 0.7, l = 0.7, t = 0.7, r=0.7), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_text(size = 55, color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = 0.05, r = 0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = .2, r=0, unit = "cm")),
      ...
    )
}


png("Costo_Debito_Pubblico.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=year(TIME_PERIOD), y=interessi_riv, label = paste0(round(interessi_riv, 0)))) +
  theme_linechart()+
  geom_col(fill="#F12938", show.legend = F) + 
  geom_text(size=14, vjust=2, color="white" , family="Source Sans Pro", show.legend = F)+
  scale_y_continuous(limits=c(0, 140), breaks=seq(25, 150, by=25), labels = function(x) paste0(x, " mld"), expand = c(0, 0))+
  scale_x_continuous(limits=c(1999, 2024), breaks=seq(2000, 2023, by=5), labels = function(x) paste0(x, ""), expand = c(0, 0))+
  labs(x = NULL,
       y = NULL,
       title = "Il costo del debito pubblico italiano",
       subtitle = "Interessi pagati sul debito pubblico in Italia. Dati espressi in euro del 2023",
       caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Eurostat")
dev.off()


data%>%
  group_by(1)%>%
  summarise(mean = mean(interessi_riv))