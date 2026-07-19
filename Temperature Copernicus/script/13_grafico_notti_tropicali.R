# Notti tropicali (minima >= 20°C) a giugno per l'italiano medio, 1961-2026:
# barre grigie, 2026 in rosso, medie dei due trentenni come riferimento.
# Dati da 12_analisi_articolo.R (output/soglie_giugno_italia_pop.csv).

library(tidyverse)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

fmt1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")

dati <- read_csv("output/soglie_giugno_italia_pop.csv", show_col_types = FALSE) |>
  select(anno, notti = n_tmin20)

m6190 <- mean(dati$notti[dati$anno %in% 1961:1990])
m9120 <- mean(dati$notti[dati$anno %in% 1991:2020])

evidenzia <- dati |>
  filter(anno %in% c(2003, 2022, 2025, 2026)) |>
  mutate(scosta = case_when(anno == 2025 ~ -2.1, anno == 2026 ~ 2.1, TRUE ~ 0))

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = "#1C1C1C", hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

p <- ggplot(dati, aes(anno, notti)) +
  geom_col(fill = "#D4D4D4", width = 0.75) +
  geom_col(data = dati |> filter(anno == 2026), fill = COL_ROSSO, width = 0.75) +
  geom_segment(aes(x = 1961, xend = 1990, y = m6190, yend = m6190),
               colour = COL_NERO, linewidth = 0.5, linetype = "dashed") +
  geom_segment(aes(x = 1991, xend = 2020, y = m9120, yend = m9120),
               colour = COL_NERO, linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 1975.5, y = m6190 + 0.55,
           label = paste0("media 1961-1990: ", fmt1(m6190)),
           family = "Source Sans Pro", size = 3.1, color = COL_NERO) +
  annotate("text", x = 2005.5, y = m9120 + 0.55,
           label = paste0("media 1991-2020: ", fmt1(m9120)),
           family = "Source Sans Pro", size = 3.1, color = COL_NERO) +
  geom_text(data = evidenzia,
            aes(x = anno + scosta, label = paste0(anno, ": ", fmt1(notti))),
            vjust = -0.5, family = "Source Sans Pro", fontface = "bold",
            size = 3.2, color = c(COL_NERO, COL_NERO, COL_NERO, COL_ROSSO)) +
  scale_x_continuous(limits = c(1959.8, 2031.2), breaks = seq(1970, 2020, 10),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 12, 2), limits = c(0, 12.4),
                     expand = c(0, 0)) +
  theme_linechart() +
  labs(
    title = "Da meno di una notte tropicale a giugno a undici",
    subtitle = "Numero medio per abitante di notti di giugno con temperatura minima sopra i 20°C, Italia, 1961-2026",
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land e Istat"
  )

ggsave("output/grafico_notti_tropicali.png", p,
       width = 8, height = 6.5, units = "in", dpi = 300, bg = "white")
cat("Salvato output/grafico_notti_tropicali.png\n")
