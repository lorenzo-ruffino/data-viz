# Temperatura media giornaliera in Italia, 1 maggio-31 agosto: 2026 (rosso),
# 2003 e 2022 (più leggeri) e le medie 1961-1990 e 1991-2020, giorno per giorno.
# Gemello di 10_grafico_giornaliero_maggio_luglio.R.

library(tidyverse)
library(ggrepel)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO    <- "#1C1C1C"
COL_BLU     <- "#0478EA"
COL_ROSSO   <- "#F12938"
COL_GRIGIO  <- "#9A9A9A"
COL_ARANCIO <- "#E07700"

mesi_it <- c("maggio", "giugno", "luglio", "agosto")

serie <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data),
         giorno = lubridate::mday(data)) |>
  filter(mese %in% 5:8, !is.na(t_area_mean))

ultimo_2026 <- max(serie$data[serie$anno == 2026])

# media di un trentennio: NA se per quel giorno mancano troppi anni
# (agosto 1961-2019 ancora in download), così la linea non è fatta da 1-2 anni
media_trentennio <- function(x, min_anni = 20) {
  if (sum(!is.na(x)) < min_anni) NA_real_ else mean(x, na.rm = TRUE)
}

giorni <- serie |>
  group_by(mese, giorno) |>
  summarise(
    `2026`      = mean(t_area_mean[anno == 2026]),
    `2003`      = mean(t_area_mean[anno == 2003]),
    `1961-1990` = media_trentennio(t_area_mean[anno %in% 1961:1990]),
    `1991-2020` = media_trentennio(t_area_mean[anno %in% 1991:2020]),
    .groups = "drop") |>
  mutate(x = as.Date(sprintf("2026-%02d-%02d", mese, giorno))) |>
  pivot_longer(c(`2026`, `2003`, `1961-1990`, `1991-2020`),
               names_to = "serie", values_to = "t") |>
  filter(!is.nan(t), !is.na(t))

# Titolo dai dati: striscia più lunga di giorni consecutivi del 2026 sopra la
# media 1991-2020 (maggio-agosto)
confronto <- giorni |>
  filter(serie %in% c("2026", "1991-2020")) |>
  pivot_wider(names_from = serie, values_from = t) |>
  filter(!is.na(`2026`), !is.na(`1991-2020`)) |>
  arrange(x) |>
  mutate(sopra = `2026` > `1991-2020`)
r <- rle(confronto$sopra)
i_max <- which(r$values)[which.max(r$lengths[r$values])]
n_striscia <- r$lengths[i_max]
inizio <- confronto$x[sum(r$lengths[seq_len(i_max - 1)]) + 1]
fine   <- inizio + n_striscia - 1
cat("Striscia più lunga sopra la media 1991-2020: ", n_striscia, " giorni, dal ",
    format(inizio, "%d/%m"), " al ", format(fine, "%d/%m"), "\n", sep = "")
titolo <- "Com'è andata l'estate 2026 giorno per giorno"

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
      axis.text = element_text(size = 11, color = "#1C1C1C", hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(size = 17.5, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.0,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

testo_2026 <- if (format(ultimo_2026, "%m-%d") >= "08-31") "2026" else
  paste0("2026 (fino al ", as.integer(format(ultimo_2026, "%d")), " ",
         mesi_it[lubridate::month(ultimo_2026) - 4], ")")

p <- ggplot(giorni, aes(x, t, color = serie)) +
  geom_vline(xintercept = as.numeric(as.Date(c("2026-06-01", "2026-07-01", "2026-08-01"))),
             colour = "#9A9A9A", linewidth = 0.4, linetype = "dashed") +
  geom_line(data = giorni |> filter(serie != "2026"),
            aes(alpha = serie), linewidth = 0.6) +
  geom_line(data = giorni |> filter(serie == "2026"), linewidth = 1.0) +
  geom_text_repel(data = etichette,
                  aes(label = nome), hjust = 0, nudge_x = 2.5,
                  direction = "y", size = 4.1, fontface = "bold",
                  family = "Source Sans Pro", segment.colour = NA,
                  min.segment.length = 0, box.padding = 0.15, seed = 1) +
  scale_color_manual(values = colori) +
  scale_alpha_manual(values = c("2003" = 0.7,
                                "1991-2020" = 1, "1961-1990" = 1)) +
  scale_x_date(limits = c(as.Date("2026-05-01"), as.Date("2026-09-18")),
               breaks = as.Date(c("2026-05-01", "2026-06-01", "2026-07-01",
                                  "2026-08-01", "2026-08-31")),
               labels = c("1 maggio", "1 giugno", "1 luglio", "1 agosto", "31 agosto"),
               expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(10, 30, 2),
                     labels = function(x) paste0(x, "°"),
                     expand = c(0.02, 0.02)) +
  theme_linechart() +
  labs(
    title = titolo,
    subtitle = paste0("Temperatura media giornaliera in Italia: ", testo_2026,
                      ", 2003 e medie 1961-1990 e 1991-2020, da maggio ad agosto"),
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land"
  )

ggsave("output/grafico_giornaliero_maggio_agosto.png", p,
       width = 8, height = 6.5, units = "in", dpi = 300, bg = "white")
cat("Salvato output/grafico_giornaliero_maggio_agosto.png\n")
