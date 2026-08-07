library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#C8C8C8"

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
      panel.border = element_blank(),
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

CAP <- "Elaborazione di Lorenzo Ruffino su dati dell'Osservatorio Meteorologico dell'Università di Torino"

# --- Dati (riuso del CSV già scaricato, solo giugno) ------------------------

dati <- read_csv("../output/temperature_maggio_giugno_torino.csv",
                 show_col_types = FALSE) %>%
  mutate(data_generica = as.Date(data_generica)) %>%
  filter(mese == 6)

# Media giornaliera 2005-2025 delle minime
media <- dati %>%
  filter(anno >= 2005, anno <= 2025) %>%
  group_by(data_generica) %>%
  summarise(Tmin = mean(Tmin, na.rm = TRUE), .groups = "drop")

# --- Etichette inline (ultimo punto di ciascuna serie) ----------------------

lab_2026 <- dati %>%
  filter(anno == 2026) %>%
  slice_max(data_generica, n = 1) %>%
  transmute(data_generica, Tmin, label = "2026", color = COL_ROSSO)

lab_media <- media %>%
  slice_max(data_generica, n = 1) %>%
  transmute(data_generica, Tmin, label = "Media\n2005-2025", color = COL_BLU)

# --- Grafico ----------------------------------------------------------------

p <- ggplot(dati, aes(x = data_generica, y = Tmin)) +
  geom_line(data = filter(dati, anno < 2026),
            aes(group = anno), color = COL_GRIGIO, linewidth = 0.3) +
  geom_line(data = media, color = COL_BLU, linewidth = 0.9) +
  geom_line(data = filter(dati, anno == 2026),
            color = COL_ROSSO, linewidth = 1.1) +
  geom_text(data = bind_rows(lab_2026, lab_media),
            aes(label = label, color = color),
            hjust = 0, nudge_x = 1, vjust = 0.4,
            size = 3.6, fontface = "bold", lineheight = 0.9,
            family = "Source Sans Pro") +
  scale_color_identity() +
  scale_x_date(
    breaks = as.Date(c("2024-06-01", "2024-06-08", "2024-06-15",
                       "2024-06-22", "2024-06-29")),
    date_labels = "%d %b",
    limits = as.Date(c("2024-06-01", "2024-07-05")),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(breaks = seq(10, 25, 5),
                     labels = function(x) paste0(x, "°")) +
  theme_linechart() +
  labs(
    title = "A Torino le minime di giugno 2026 sono sopra la media",
    subtitle = "Temperatura minima giornaliera dell'aria a Torino, in grigio i singoli anni dal 2005 al 2025",
    caption = CAP
  )

ggsave("../output/temperature_min_giugno_torino.png", p,
       width = 8, height = 6.5, dpi = 300, bg = "white")
