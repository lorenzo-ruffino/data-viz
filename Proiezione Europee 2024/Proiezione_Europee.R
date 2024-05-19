library(tidyverse)
library(showtext)


european_elections <- data.frame(
  gruppo = c("EPP", "S&D", "RE", "G/EFA", "ID", "ECR", "LEFT", "NI"),
  anno_2019 = c(182, 144, 91, 63, 73, 58, 40, 27),
  anno_2019_percent = c(26.8, 21.2, 13.4, 9.3, 10.8, 8.5, 5.9, 4.0),
  anno_2024 = c(182, 134, 85, 54, 83, 83, 43, 56),
  anno_2024_percent = c(25.3, 18.6, 11.8, 7.5, 11.5, 11.5, 6.0, 7.8)
)

european_elections_sorted <- european_elections %>%
  mutate(ordine = ifelse(gruppo == "NI", 0, anno_2024_percent))%>%
  arrange((ordine)) %>%
  mutate(gruppo = factor(gruppo, levels = gruppo))

european_elections_plot <- european_elections_sorted %>%
  select(gruppo, anno_2019_percent, anno_2024_percent) %>%
  gather(anno, valore, anno_2019_percent:anno_2024_percent)

european_elections_plot$anno <- factor(european_elections_plot$anno, levels=c( "anno_2024_percent", "anno_2019_percent"))


font_add_google("Source Sans Pro")
showtext_auto()

theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.y = element_text(size = 45,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
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
      plot.subtitle =  element_text(size = 55,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0.2, l = 0, t = 0, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 0.5, r=0, unit = "cm")),
      ...
    )
}



png("Proiezione_Europee.png", width = 9.5, height = 10, units="in", res=300)
ggplot(european_elections_plot, aes(x=valore, y=gruppo, group=anno, fill=gruppo, label = paste0(valore, "%"))) +
  theme_barchart() +
  geom_bar(stat="identity", position = position_dodge(width = 0.5), show.legend = F,
           alpha = ifelse(european_elections_plot$anno == "anno_2019_percent", 0.4, 1)) +
  scale_fill_manual(values = c("LEFT" = "#8C2443", "G/EFA" = "#61BB46", "S&D" = "#F02D32", "RE" = "#F8EC26", "EPP" = "#3B5DA9", "ECR" = "#36BFE2", "ID" = "#03849E", "NI" = "#CCCCCC")) +
  geom_text(size = 15, color = "black", hjust = ifelse(european_elections_plot$anno == "anno_2019_percent", -0.2, 1.1),
            position = position_dodge(width = 0.5),
            show.legend = FALSE, family = "Source Sans Pro") +
  scale_x_continuous(limits = c(0,30), expand = c(0, 0)) +
  labs(x = NULL,
       y = NULL,
       title = "La proiezione dei seggi per europee",
       subtitle = "Confronto tra i seggi del 2019 (Regno Unito escluso) e la proiezione per il 2024",
       caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Europe Elects e Parlamento Europeo")

dev.off()