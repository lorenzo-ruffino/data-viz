library(tidyverse)
library(spatstat)
library(showtext)
library(ggtext)

df <- data.frame(
  eta = c("<19", "20 - 24", "25 - 29", "30 - 34", "35 - 39",
          "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", ">65"),
  privato = c(334483, 1370396, 1703256, 1808279, 1773947, 1888956, 2153403, 2081369, 1773591, 921659, 285450),
  pubblico = c(1022, 63041, 167568, 248861, 298071, 398012, 502671, 602079, 689403, 507508, 149041)
) %>%
  mutate(privato_pct = (privato / sum(privato))*100,
         pubblico_pct = (pubblico / sum(pubblico))*100)

df$Eta_centrale <- c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 70)

eta_mediana_privato <- weighted.median(df$Eta_centrale, df$privato)
eta_mediana_pubblico <- weighted.median(df$Eta_centrale, df$pubblico)

data = df %>%
  select(eta, privato_pct, pubblico_pct) %>%
  gather(categoria, valore, privato_pct : pubblico_pct)


data$eta <- factor(data$eta, levels =c(">65", "60 - 64", "55 - 59", "50 - 54", "45 - 49",
                                       "40 - 44", "35 - 39", "30 - 34", "25 - 29", "20 - 24", "<19"),
                   ordered = TRUE)

#font_add_google("Source Sans Pro")
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
      legend.text = element_text(size = 50, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title = element_text(size = 90, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_markdown(size = 60, color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 0, t = 0, r=0, unit = "cm")),
      plot.caption = element_text(size = 50, color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 1, r=0, unit = "cm")),
      ...
    )
}

png("Dipendenti_Privati_Pubblici_Fascia_Anagrafica.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x = eta, color = categoria)) +
  geom_segment(data = subset(data, categoria == "pubblico_pct"),
               aes(xend = eta, y = 2, yend = valore+2),
               size = 15, lineend = "butt", show.legend = F) +
  geom_segment(data = subset(data, categoria == "privato_pct"),
               aes(xend = eta, y = -2, yend = -valore-2),
               size = 15, lineend = "butt", show.legend = F) +
  geom_text(data = data[data$categoria == "pubblico_pct", ], aes(y = 0, label = eta),
            hjust = .5, vjust = 0.5, size = 15, show.legend = F, color="black",
            family="Source Sans Pro") +
  geom_text(data = subset(data, categoria == "pubblico_pct"),
            aes(y = valore+1.77, label = paste0(round(valore, 1), "%")),
            hjust = 1, vjust = .5, size = 11, color = "white", show.legend = F,
            family="Source Sans Pro") +
  geom_text(data = subset(data, categoria == "privato_pct"),
            aes(y = -valore-1.7, label = paste0(round(valore, 1), "%")),
            hjust = 0, vjust = .5, size = 11, color = "white", show.legend = F,
            family="Source Sans Pro") +
  coord_flip(ylim = c(-22, 22)) +
  scale_color_manual(values = c("privato_pct" = "#0478EA" , "pubblico_pct" = "#F12938"),
                     labels = c("Privato", "Pubblico")) +
  scale_y_continuous(expand = c(0, 0))+
  labs(x = NULL,
       y = NULL,
       title = "L'età dei dipendenti pubblici e privati",
       subtitle = "Dipendenti <span style='color:#0478EA;'><b>privati</b></span> e <span style='color:#F12938;'><b>pubblici</b></span> per fascia anagrafica nel 2022",
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: INPS")) +
  theme_barchart()
dev.off()