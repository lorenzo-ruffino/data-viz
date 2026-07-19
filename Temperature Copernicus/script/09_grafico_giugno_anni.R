# Serie storica di giugno in Italia (1961-2026): temperatura media e media
# delle massime e delle minime giornaliere per ogni anno, 2026 in rosso.
# Per confrontare il 2026 (incompleto) tutti gli anni usano la stessa finestra
# di giorni disponibile (es. 1-26).

library(tidyverse)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO    <- "#1C1C1C"
COL_ROSSO   <- "#F12938"
COL_GRIGIO  <- "#9A9A9A"
COL_ARANCIO <- "#E07700"
COL_BLU     <- "#0478EA"
colori_serie <- c(massime = COL_ARANCIO, media = COL_ROSSO, minime = COL_BLU)

serie <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data),
         giorno = lubridate::mday(data)) |>
  filter(mese == 6)

fin <- max(serie$giorno[serie$anno == 2026])

annuale <- serie |>
  filter(giorno <= fin) |>
  group_by(anno) |>
  summarise(media   = mean(t_area_mean),
            massime = mean(t_area_max),
            minime  = mean(t_area_min), .groups = "drop")

classifica <- annuale |> arrange(desc(media))
cat("Giugno più caldi (media, giorni 1-", fin, "):\n", sep = "")
print(head(classifica, 5))

posto <- which(classifica$anno == 2026)
ordinali <- c("", "il secondo", "il terzo", "il quarto", "il quinto",
              "il sesto", "il settimo", "l'ottavo", "il nono", "il decimo")
titolo <- if (posto == 1) {
  "Giugno 2026 è il più caldo almeno dal 1961"
} else {
  paste0("Giugno 2026 è ", ordinali[posto], " più caldo dal 1961")
}

lunga <- annuale |>
  pivot_longer(-anno, names_to = "serie", values_to = "t") |>
  mutate(serie = factor(serie, levels = c("massime", "media", "minime")))

etichette <- tibble(
  serie = factor(c("massime", "media", "minime"), levels = levels(lunga$serie)),
  nome  = c("Media massime", "Media", "Media minime"),
  anno  = max(annuale$anno),
  t     = c(annuale$massime[annuale$anno == 2026],
            annuale$media[annuale$anno == 2026],
            annuale$minime[annuale$anno == 2026])
)

p2026 <- lunga |> filter(anno == 2026)
fmt1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")

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

p <- ggplot(lunga, aes(anno, t, group = serie)) +
  geom_smooth(method = "lm", se = FALSE, color = COL_GRIGIO,
              linewidth = 0.45, linetype = "dashed") +
  geom_line(data = lunga |> filter(serie != "media"),
            aes(color = serie), linewidth = 0.55) +
  geom_line(data = lunga |> filter(serie == "media"),
            color = COL_ROSSO, linewidth = 0.9) +
  geom_point(data = p2026, aes(color = serie), size = 2.2) +
  geom_text(data = p2026,
            aes(label = fmt1(t), color = serie), fontface = "bold",
            size = 3.4, vjust = -1.1, family = "Source Sans Pro") +
  geom_text(data = etichette,
            aes(label = nome, color = serie), hjust = 0, nudge_x = 1.5,
            size = 3.3, fontface = "bold", family = "Source Sans Pro") +
  scale_color_manual(values = colori_serie, guide = "none") +
  scale_x_continuous(limits = c(1961, 2041), breaks = seq(1970, 2020, 10),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(10, 28, 2),
                     labels = function(x) paste0(x, "°"),
                     expand = c(0.02, 0.02)) +
  theme_linechart() +
  labs(
    title = titolo,
    subtitle = if (fin >= 30) {
      "Temperatura media di giugno, medie di massime e minime e tendenze lineari, Italia, 1961-2026"
    } else {
      paste0("Temperatura media di giugno, medie di massime e minime (giorni 1-", fin,
             " per tutti gli anni), Italia, 1961-2026")
    },
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land"
  )

ggsave("output/grafico_giugno_annuale.png", p,
       width = 8, height = 6.5, units = "in", dpi = 300, bg = "white")
cat("Salvato output/grafico_giugno_annuale.png\n")
