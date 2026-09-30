# Serie storica dell'estate (giugno-luglio-agosto) in Italia (1961-2026):
# temperatura media e media delle massime e delle minime giornaliere per ogni
# estate, 2026 in evidenza. Gemello di 09_grafico_giugno_anni.R e
# 18_grafico_luglio_anni.R.
# Se agosto 2026 è incompleto, tutti gli anni usano la stessa finestra di
# giorni (1 giugno - ultimo giorno disponibile del 2026), come fa 09.
# Gli anni con copertura incompleta (agosto ancora in download) vengono
# segnalati a video e calcolati sui giorni disponibili.

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
  filter(mese %in% 6:8, !is.na(t_area_mean))

# finestra comune: ultimo giorno disponibile dell'estate 2026
ultimo_2026 <- max(serie$data[serie$anno == 2026])
fin_mese   <- lubridate::month(ultimo_2026)
fin_giorno <- lubridate::mday(ultimo_2026)
completa   <- fin_mese == 8 && fin_giorno == 31
n_attesi   <- as.integer(ultimo_2026 - as.Date("2026-06-01")) + 1

annuale <- serie |>
  filter(mese < fin_mese | (mese == fin_mese & giorno <= fin_giorno)) |>
  group_by(anno) |>
  summarise(media   = mean(t_area_mean),
            massime = mean(t_area_max),
            minime  = mean(t_area_min),
            giorni  = n(), .groups = "drop")

incompleti <- annuale$anno[annuale$giorni < n_attesi]
if (length(incompleti) > 0) {
  cat("ATTENZIONE: anni con copertura incompleta (", n_attesi, " giorni attesi): ",
      paste(incompleti, collapse = ", "), "\n", sep = "")
}

classifica <- annuale |> arrange(desc(media))
cat("Estati più calde (media", if (completa) "" else paste0(", fino al ", fin_giorno, " agosto"), "):\n", sep = "")
print(head(classifica |> select(-giorni), 5))

posto <- which(classifica$anno == 2026)
ordinali <- c("", "la seconda", "la terza", "la quarta", "la quinta",
              "la sesta", "la settima", "l'ottava", "la nona", "la decima")
titolo <- if (posto == 1) {
  "L'estate 2026 è la più calda da quando abbiamo i dati"
} else if (posto <= length(ordinali)) {
  paste0("L'estate 2026 è ", ordinali[posto], " più calda dal 1961")
} else {
  paste0("L'estate 2026 è al ", posto, "° posto tra le più calde dal 1961")
}

lunga <- annuale |>
  select(-giorni) |>
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

sottotitolo <- "Temperatura media dell'estate (giugno-agosto), medie di massime e minime e tendenze lineari, Italia, 1961-2026"
if (!completa) {
  sottotitolo <- paste0(sottotitolo, "\nPer tutti gli anni sono considerati i giorni dal 1 giugno al ",
                        fin_giorno, " agosto, ultimo dato disponibile del 2026")
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
            size = 4.2, vjust = -1.1, family = "Source Sans Pro") +
  geom_text(data = etichette,
            aes(label = nome, color = serie), hjust = 0, nudge_x = 1.5,
            size = 4.1, fontface = "bold", family = "Source Sans Pro") +
  scale_color_manual(values = colori_serie, guide = "none") +
  scale_x_continuous(limits = c(1961, 2041), breaks = seq(1970, 2020, 10),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(12, 32, 2),
                     labels = function(x) paste0(x, "°"),
                     expand = expansion(mult = c(0.02, 0.07))) +
  theme_linechart() +
  labs(
    title = titolo,
    subtitle = sottotitolo,
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land"
  )

ggsave("output/grafico_estate_annuale.png", p,
       width = 8, height = 6.5, units = "in", dpi = 300, bg = "white")
cat("Salvato output/grafico_estate_annuale.png\n")
