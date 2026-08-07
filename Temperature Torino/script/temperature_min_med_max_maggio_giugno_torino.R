library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"

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

# --- Dati (riuso del CSV già scaricato) -------------------------------------

dati <- read_csv("../output/temperature_maggio_giugno_torino.csv",
                 show_col_types = FALSE) %>%
  mutate(data_generica = as.Date(data_generica))

d2026 <- dati %>% filter(anno == 2026)

# Media giornaliera 2005-2025 di minime, medie e massime
media <- dati %>%
  filter(anno >= 2005, anno <= 2025) %>%
  group_by(data_generica) %>%
  summarise(across(c(Tmin, Tmed, Tmax), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

# --- Etichette inline (non in grassetto) ------------------------------------

# Trio della media (blu), all'estremità destra (fine giugno)
fine_media <- media %>% slice_max(data_generica, n = 1)
lab_media <- tibble(
  data_generica = fine_media$data_generica,
  y     = c(fine_media$Tmax, fine_media$Tmed, fine_media$Tmin),
  label = c("Massima\n2005-2025", "Media\n2005-2025", "Minima\n2005-2025"),
  color = COL_BLU
)

# Etichette del 2026 (rosso), all'ultimo giorno disponibile
fine_2026 <- d2026 %>% slice_max(data_generica, n = 1)
lab_2026 <- tibble(
  data_generica = fine_2026$data_generica,
  y     = c(fine_2026$Tmax, fine_2026$Tmed, fine_2026$Tmin),
  label = c("Massima 2026", "Media 2026", "Minima 2026"),
  color = COL_ROSSO
)

# --- Grafico ----------------------------------------------------------------

p <- ggplot() +
  # Riferimento: inizio di giugno
  geom_vline(xintercept = as.Date("2024-06-01"),
             linetype = "dashed", color = "#9A9A9A", linewidth = 0.4) +
  # Fasce sfumate tra minima e massima
  geom_ribbon(data = media, aes(data_generica, ymin = Tmin, ymax = Tmax),
              fill = COL_BLU, alpha = 0.10) +
  geom_ribbon(data = d2026, aes(data_generica, ymin = Tmin, ymax = Tmax),
              fill = COL_ROSSO, alpha = 0.10) +
  # Media 2005-2025 (blu): media continua, min/max tratteggiate
  geom_line(data = media, aes(data_generica, Tmax),
            color = COL_BLU, linetype = "dashed", linewidth = 0.7) +
  geom_line(data = media, aes(data_generica, Tmin),
            color = COL_BLU, linetype = "dashed", linewidth = 0.7) +
  geom_line(data = media, aes(data_generica, Tmed),
            color = COL_BLU, linewidth = 1) +
  # 2026 (rosso): media continua, min/max tratteggiate
  geom_line(data = d2026, aes(data_generica, Tmax),
            color = COL_ROSSO, linetype = "dashed", linewidth = 0.7) +
  geom_line(data = d2026, aes(data_generica, Tmin),
            color = COL_ROSSO, linetype = "dashed", linewidth = 0.7) +
  geom_line(data = d2026, aes(data_generica, Tmed),
            color = COL_ROSSO, linewidth = 1.1) +
  # Etichette: rosse a fine 2026 (19 giu), blu a fine giugno
  geom_text(data = lab_2026,
            aes(data_generica, y, label = label, color = color),
            hjust = 0, nudge_x = 1, vjust = 0.4,
            size = 3.1, family = "Source Sans Pro") +
  geom_text(data = lab_media,
            aes(data_generica, y, label = label, color = color),
            hjust = 0, nudge_x = 1, vjust = 0.5, lineheight = 0.85,
            size = 3.1, family = "Source Sans Pro") +
  scale_color_identity() +
  scale_x_date(
    breaks = as.Date(c("2024-05-01", "2024-05-11", "2024-05-21",
                       "2024-05-31", "2024-06-10", "2024-06-20",
                       "2024-06-30")),
    date_labels = "%d %b",
    limits = as.Date(c("2024-05-01", "2024-07-11")),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(breaks = seq(10, 35, 5),
                     labels = function(x) paste0(x, "°")) +
  theme_linechart() +
  labs(
    title = "Le temperature a Torino di maggio e giugno",
    subtitle = "Temperatura giornaliera dell'aria a Torino: in rosso il 2026, in blu la media 2005-2025.\nLinea continua = temperatura media, tratteggiata = minima e massima",
    caption = CAP
  )

ggsave("../output/temperature_min_med_max_maggio_giugno_torino.png", p,
       width = 8, height = 6.5, dpi = 300, bg = "white")
