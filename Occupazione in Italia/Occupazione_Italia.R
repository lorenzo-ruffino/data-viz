library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)

df <- data.frame(
  segment = c("15-24", "25-34", "35-49", "50-64"),
  segpct = c(5812, 6162, 11455, 13755),
  OCCUPATI = c(1191, 4247, 8795, 8842),
  DISOCCUPATI = c(299, 447, 634, 451),
  INATTIVI = c(4322, 1468, 2026, 4462)
)

df <- df[order(df$segment, decreasing = TRUE), ]

df$ymax <- cumsum(df$segpct) / sum(df$segpct)
df$ymin <- df$ymax - (df$segpct/sum(df$segpct))
df$segpct <- NULL
dfm <- pivot_longer(df, cols = c("OCCUPATI", "DISOCCUPATI", "INATTIVI"), names_to = "variable", values_to = "value")

dfm1 <- dfm %>%
  group_by(segment) %>%
  mutate(pct = value / sum(value) * 100,
         xmax = cumsum(pct),
         xmin = xmax - pct,
         value = round(value / 1000, 1))

dfm1$ytext <- with(dfm1, ymin + (ymax - ymin) / 2)
dfm1$xtext <- with(dfm1, xmin + (xmax - xmin) / 2)


font_add_google("Source Sans Pro")
showtext_auto()

theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "right",
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(b = 1, l = 1, t = 1, r = 1), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 60, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
      plot.title = element_text(size = 90, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
      plot.subtitle = element_text(size = 60, color = "#1C1C1C", hjust = 0, margin = margin(b = 0.4, l = 0, t = 0.25, r = 0, unit = "cm")),
      plot.caption = element_text(size = 50, color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 0.5, r = 0, unit = "cm")),
      ...
    )
}

png("Occupazione_Italia.png", width = 10, height = 9.5, units = "in", res = 300)
ggplot(dfm1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = variable, colour=variable)) +
  geom_rect(colour = I("white"), alpha = 0.7, show.legend = F) +
  geom_text(aes(x = xtext, y = 1.02, label = ifelse(segment == "15-24", variable, "")), size = 15,  family="Source Sans Pro", fontface="bold", show.legend = F) +
  geom_text(aes(x = xtext, y = ytext, label = paste(round(value, 1), "M", " (", round(pct, 1), "%)", sep = "")), colour="black", size = 14, family="Source Sans Pro", show.legend = F) +
  geom_text(aes(x = -7, y = ytext, label = ifelse(variable == "OCCUPATI", segment, "")), size = 18, hjust=0, colour="black", family="Source Sans Pro")+
  theme_barchart() +
  scale_fill_manual(values = c("OCCUPATI" = "#0478EA", "DISOCCUPATI" = "#F12938", "INATTIVI" = "lightgrey"))+
  scale_colour_manual(values = c("OCCUPATI" = "#0478EA", "DISOCCUPATI" = "#F12938", "INATTIVI" = "darkgrey"))+
  scale_y_continuous(limits=c(0, 1.03), expand = c(0, 0))+
  scale_x_continuous(limits=c(-7, 100), expand = c(0, 0))+
  labs(x = NULL,
       y = NULL,
       title = "L'occupazione in Italia", 
       subtitle = "Condizione lavorativa tra i 15 e i 64 anni, marzo 2024",
       caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Istat")
dev.off()