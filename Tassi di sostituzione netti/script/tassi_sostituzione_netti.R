library(tidyverse)
library(showtext)
library(ggrepel)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO <- "#1C1C1C"; COL_BLU <- "#0478EA"; COL_VIOLA <- "#A82DE3"
COL_ROSSO <- "#F12938"; COL_GIALLO <- "#F2A900"; COL_GRIGIO <- "#9A9A9A"
COL_VERDE <- "#1B9E77"; COL_GRIGIO_SCURO <- "#5A5A5A"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = "#1C1C1C", hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0),
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP <- "Elaborazione di Lorenzo Ruffino su dati Ragioneria generale dello Stato"

# RGS, "Le tendenze di medio-lungo periodo del sistema pensionistico e
# socio-sanitario - Aggiornamento 2026": Tab. 6.3.a (p. 178) e Tab. 6.4 (p. 181).
# Tassi di sostituzione netti, dipendente privato, scenario nazionale base.
anni <- c(2010, 2020, 2030, 2040, 2050, 2060, 2070)
dati <- bind_rows(
  tibble(serie = "Con 38 anni di contributi e il fondo pensione",
         valore = c(82.7, 88.4, 87.4, 80.2, 78.5, 75.1, 74.8)),
  tibble(serie = "Con 38 anni di contributi",
         valore = c(82.7, 81.5, 77.5, 67.9, 66.4, 64.9, 64.4))
) |>
  group_by(serie) |> mutate(anno = anni) |> ungroup()

write_csv(dati, "../output/tassi_sostituzione_netti.csv")

colori <- c("Con 38 anni di contributi e il fondo pensione" = COL_VERDE,
            "Con 38 anni di contributi" = COL_BLU)

fmt_pct <- function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")

ultimi <- dati |> filter(anno == 2070)
etichette <- tibble(serie = names(colori),
                    anno = c(2041, 2041), valore = c(84.2, 62.3))

p <- ggplot(dati, aes(anno, valore, colour = serie)) +
  geom_vline(xintercept = 2025, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2025.8, y = 93, label = "Proiezioni", hjust = 0,
           colour = COL_GRIGIO_SCURO, size = 3.4, family = "Source Sans Pro") +
  annotate("text", x = 2010, y = 82.7, label = "82,7%", vjust = 2.8, hjust = 0.3,
           colour = COL_NERO, size = 3.6, fontface = "bold", family = "Source Sans Pro") +
  geom_line(linewidth = 1.0) +
  geom_point(size = 1.8) +
  geom_text(data = ultimi, aes(label = fmt_pct(valore)), hjust = 0, nudge_x = 1.2,
            size = 3.6, fontface = "bold", family = "Source Sans Pro") +
  geom_text(data = etichette, aes(label = serie), hjust = 0,
            size = 3.6, fontface = "bold", family = "Source Sans Pro") +
  scale_colour_manual(values = colori) +
  scale_x_continuous(breaks = anni, expand = c(0, 0)) +
  scale_y_continuous(limits = c(50, 95), breaks = seq(60, 90, 10),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(xlim = c(2008, 2076), clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "Quanto varranno le pensioni in futuro?",
       subtitle = "Prima pensione netta in percentuale dell'ultimo stipendio netto, dipendente privato, Italia, 2010-2070",
       caption = CAP)

ggsave("../output/tassi_sostituzione_netti.png", p, width = 9, height = 6.5, dpi = 220, bg = "white")
print(ultimi)
