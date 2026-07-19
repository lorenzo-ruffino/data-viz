# Temperatura media giornaliera in Italia, maggio-giugno: 2026 (rosso), 2003
# e le medie 1961-1990 e 1991-2020, giorno per giorno.

library(tidyverse)
library(ggrepel)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

serie <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data),
         giorno = lubridate::mday(data)) |>
  filter(mese %in% c(5, 6))

fin <- max(serie$giorno[serie$anno == 2026 & serie$mese == 6])

giorni <- serie |>
  group_by(mese, giorno) |>
  summarise(
    `2026`      = mean(t_area_mean[anno == 2026]),
    `2003`      = mean(t_area_mean[anno == 2003]),
    `1961-1990` = mean(t_area_mean[anno %in% 1961:1990]),
    `1991-2020` = mean(t_area_mean[anno %in% 1991:2020]),
    .groups = "drop") |>
  mutate(x = as.Date(sprintf("2026-%02d-%02d", mese, giorno))) |>
  pivot_longer(c(`2026`, `2003`, `1961-1990`, `1991-2020`),
               names_to = "serie", values_to = "t") |>
  filter(!is.nan(t))

colori <- c("2026" = COL_ROSSO, "2003" = COL_NERO,
            "1991-2020" = COL_BLU, "1961-1990" = COL_GRIGIO)

etichette <- giorni |>
  group_by(serie) |>
  slice_max(x, n = 1) |>
  ungroup() |>
  mutate(nome = ifelse(serie %in% c("2026", "2003"), serie,
                       paste("Media", serie)))

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

p <- ggplot(giorni, aes(x, t, color = serie)) +
  geom_vline(xintercept = as.numeric(as.Date("2026-06-01")),
             colour = "#9A9A9A", linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(linewidth = serie == "2026")) +
  geom_text_repel(data = etichette,
                  aes(label = nome), hjust = 0, nudge_x = 2.5,
                  direction = "y", size = 3.3, fontface = "bold",
                  family = "Source Sans Pro", segment.colour = NA,
                  min.segment.length = 0, box.padding = 0.15, seed = 1) +
  scale_color_manual(values = colori) +
  scale_linewidth_manual(values = c(`TRUE` = 1.0, `FALSE` = 0.65)) +
  scale_x_date(limits = c(as.Date("2026-05-01"), as.Date("2026-07-14")),
               breaks = as.Date(c("2026-05-01", "2026-05-15", "2026-06-01",
                                  "2026-06-15", "2026-06-30")),
               labels = c("1 maggio", "15 maggio", "1 giugno", "15 giugno", "30 giugno"),
               expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(10, 28, 2),
                     labels = function(x) paste0(x, "°"),
                     expand = c(0.02, 0.02)) +
  theme_linechart() +
  labs(
    title = "A fine giugno il 2026 ha superato anche il 2003",
    subtitle = paste0("Temperatura media giornaliera in Italia: ",
                      if (fin >= 30) "2026" else paste0("2026 (fino al ", fin, " giugno)"),
                      ", 2003 e medie 1961-1990 e 1991-2020, maggio e giugno"),
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land"
  )

ggsave("output/grafico_giornaliero_maggio_giugno.png", p,
       width = 8, height = 6.5, units = "in", dpi = 300, bg = "white")
cat("Salvato output/grafico_giornaliero_maggio_giugno.png\n")
