# Serie giornaliera nazionale, 1 maggio - 30 giugno 2026: decessi osservati
# (corretti per completezza) e attesi senza caldo anomalo con intervallo di
# previsione al 95% (da 22_eccesso.R); sotto, anomalia della temperatura media
# (pesata per popolazione) rispetto agli stessi giorni 2011-2025.
#
# Output: output/grafico_serie_osservati_attesi.png

library(tidyverse)
library(patchwork)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Mortalità caldo 2026")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

gg <- read_csv("output/eccesso_italia_giornaliero.csv", show_col_types = FALSE) |>
  filter(data >= as.Date("2026-05-01"))

ondate <- tibble(inizio = as.Date(c("2026-05-21", "2026-06-17")),
                 fine = as.Date(c("2026-05-29", "2026-06-30")))

tema <- theme_minimal(base_family = "Source Sans Pro") +
  theme(text = element_text(color = "#1C1C1C"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.3),
        axis.text = element_text(size = 10, color = "#1C1C1C"),
        axis.title = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 17.5),
        plot.subtitle = element_text(size = 11, lineheight = 1.3, margin = margin(b = 8)),
        plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 8)),
        legend.position = "none")

fondo <- geom_rect(data = ondate, aes(xmin = inizio - 0.5, xmax = fine + 0.5, ymin = -Inf, ymax = Inf),
                   inherit.aes = FALSE, fill = "#F5F5F5")
asse_x <- scale_x_date(date_breaks = "1 week", date_labels = "%d/%m", expand = c(0, 0),
                       limits = as.Date(c("2026-04-30", "2026-07-01")))

p1 <- ggplot(gg, aes(data)) +
  fondo +
  geom_ribbon(aes(ymin = att_inf, ymax = att_sup), fill = "#BCD3EA") +
  geom_line(aes(y = atteso), color = "#4F7FB8", linewidth = 0.7) +
  geom_line(aes(y = oss), color = "#F12938", linewidth = 0.8) +
  annotate("text", x = as.Date("2026-05-03"), y = 2000, label = "osservati", hjust = 0,
           family = "Source Sans Pro", color = "#F12938", size = 3.8) +
  annotate("text", x = as.Date("2026-05-03"), y = 1440, label = "attesi senza caldo anomalo",
           hjust = 0, family = "Source Sans Pro", color = "#4F7FB8", size = 3.8) +
  asse_x +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  coord_cartesian(ylim = c(1400, 2050)) +
  tema +
  labs(title = "Morti in più nei giorni di caldo, in meno subito dopo",
       subtitle = paste0("Decessi giornalieri in Italia, osservati e attesi (con intervallo al 95%), ",
                         "maggio-giugno 2026;\nin grigio i periodi di caldo anomalo; ",
                         "sotto, anomalia della temperatura media in °C"))

p2 <- ggplot(gg, aes(data, anomalia)) +
  fondo +
  geom_col(aes(fill = anomalia > 0), width = 0.8) +
  scale_fill_manual(values = c(`TRUE` = "#F12938", `FALSE` = "#4F7FB8")) +
  asse_x +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "°")) +
  tema +
  labs(caption = "Elaborazione di Lorenzo Ruffino su dati Istat (provvisori) e Copernicus ERA5-Land")

p <- p1 / p2 + plot_layout(heights = c(3, 1.2))
ggsave("output/grafico_serie_osservati_attesi.png", p, width = 9, height = 6.4, dpi = 220, bg = "white")
