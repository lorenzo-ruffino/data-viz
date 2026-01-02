library(tidyverse)
library(showtext)
library(ggtext)

df <- data.frame(
  eta = c("18 - 22", "23 - 27", "28 - 32", "33 - 37", "38 - 42",
          "43 - 47", "48 - 52", "53 - 57", "58 - 62", "63 - 67", "68 e più"),
  femmine = c(1215, 1732, 2287, 3096, 3293, 3570, 4664, 5592, 7180, 7554, 9802),
  maschi = c(1951, 2927, 4082, 5611, 6098, 6109, 7239, 8306, 10762, 11981, 19677)
)

data = df %>%
  select(eta, femmine, maschi) %>%
  gather(categoria, valore, femmine : maschi)

data$eta <- factor(data$eta, levels = c("68 e più", "63 - 67", "58 - 62", "53 - 57", "48 - 52",
                                        "43 - 47", "38 - 42", "33 - 37", "28 - 32", "23 - 27", "18 - 22"),
                   ordered = TRUE)

font_add_google("Source Sans Pro")
showtext_auto()


theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text = element_blank(),
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
      plot.margin = unit(c(b = .8, l = .8, t = .8, r=.8), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 20, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title = element_text(size = 30, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_markdown(size = 15, color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 0, t = 0.2, r=0, unit = "cm")),
      plot.caption = element_text(size = 15, color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 0.5, r=0, unit = "cm")),
      ...
    )
}

max_val <- max(data$valore)




png("Firme_Referendum_Giustizia.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x = eta, color = categoria)) +
  geom_segment(data = subset(data, categoria == "maschi"),
               aes(xend = eta, y = 1500, yend = valore+1500),
               size = 15, lineend = "butt", show.legend = F) +
  geom_segment(data = subset(data, categoria == "femmine"),
               aes(xend = eta, y = -1500, yend = -valore-1500),
               size = 15, lineend = "butt", show.legend = F) +
  geom_text(data = data[data$categoria == "maschi", ], aes(y = 0, label = eta),
            hjust = .5, vjust = 0.5, size = 4.7, show.legend = F, color="black",
            family="Source Sans Pro") +
  geom_text(data = subset(data, categoria == "maschi"),
            aes(y = valore+1500-200, label = format(round(valore, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE)),
            hjust = 1, vjust = .5, size = 4, color = "white", show.legend = F,
            family="Source Sans Pro") +
  geom_text(data = subset(data, categoria == "femmine" & !(eta %in% c("18 - 22", "23 - 27"))),
            aes(y = -valore-1500+200, label = format(round(valore, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE)),
            hjust = 0, vjust = .5, size = 4, color = "white", show.legend = F,
            family="Source Sans Pro") +
  geom_text(data = subset(data, categoria == "femmine" & eta %in% c("18 - 22", "23 - 27")),
            aes(y = -valore-1600, label = format(round(valore, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE)),
            hjust = 1, vjust = .5, size = 4, color = "#F12938", show.legend = F,
            family="Source Sans Pro") +
  coord_flip(ylim = c(-max_val-1500, max_val+1500)) +
  scale_color_manual(values = c("maschi" = "#0478EA" , "femmine" = "#F12938"),
                     labels = c("Femmine", "Maschi")) +
  scale_y_continuous(expand = c(0, 0))+
  labs(x = NULL,
       y = NULL,
       title = "Le firme per il referendum sulla giustizia",
       subtitle = "Numero di <span style='color:#F12938;'><b>donne</b></span> e <span style='color:#0478EA;'><b>uomini</b></span> per fascia anagrafica che hanno firmato per il referendum costituzionale<br>sulla riforma della giustizia al 30 dicembre 2025",
       caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Ministero della Giustizia - Referendum e iniziative popolari") +
  theme_barchart()
dev.off()