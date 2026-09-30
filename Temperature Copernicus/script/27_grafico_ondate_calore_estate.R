# Giorni in ondata di calore per estate (giugno-luglio-agosto) in Italia,
# 1961-2026: barre grigie, 2026 in rosso, medie dei due trentenni, etichette su
# 2003, 2022 e 2026. Stesso stile del grafico delle notti tropicali.
# Ondata di calore: massima nazionale (t_area_max) sopra il 90° percentile
# delle massime JJA 1991-2020 per almeno 3 giorni consecutivi (come 12).
# Dati: output/ondate_calore_estate.csv (prodotto da 20_analisi_estate.R).
# Se il file definitivo non esiste, li calcola in via provvisoria dalla serie
# giornaliera e li salva in output/ondate_calore_estate_provv.csv.
#   - output/grafico_ondate_calore_estate.png

library(tidyverse)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

FILE_DEF   <- "output/ondate_calore_estate.csv"
FILE_PROVV <- "output/ondate_calore_estate_provv.csv"

fmt1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
fmt0 <- function(x) formatC(x, format = "f", digits = 0, decimal.mark = ",")

# ---- Dati -------------------------------------------------------------------

serie <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data)) |>
  filter(mese %in% 6:8, !is.na(t_area_max))

# soglia (serve anche per il sottotitolo)
p90 <- quantile(serie$t_area_max[serie$anno %in% 1991:2020], 0.9)
cat("90° percentile delle massime JJA 1991-2020:", fmt1(p90), "°C\n")

if (file.exists(FILE_DEF)) {
  cat("Uso il file definitivo", FILE_DEF, "\n")
  ondate <- read_csv(FILE_DEF, show_col_types = FALSE)
  # il file di 20 ha una riga per peso (area = massima nazionale, pop = pesata
  # per popolazione): qui serve la massima nazionale
  if ("peso" %in% names(ondate)) ondate <- ondate |> filter(peso == "area")
  if ("soglia_p90" %in% names(ondate)) p90 <- ondate$soglia_p90[1]
} else {
  cat("File definitivo assente: calcolo provvisorio dalla serie giornaliera...\n")
  ondate <- serie |>
    group_by(anno) |>
    arrange(data, .by_group = TRUE) |>
    summarise(sopra = list(rle(t_area_max > p90)), n_giorni = n(), .groups = "drop") |>
    mutate(
      giorni_sopra  = map_int(sopra, ~ sum(.x$lengths[.x$values])),
      episodi_3g    = map_int(sopra, ~ sum(.x$values & .x$lengths >= 3)),
      giorni_ondata = map_int(sopra, ~ sum(.x$lengths[.x$values & .x$lengths >= 3])),
      striscia_max  = map_int(sopra, ~ ifelse(any(.x$values), max(.x$lengths[.x$values]), 0L))
    ) |>
    select(anno, giorni_sopra, episodi_3g, giorni_ondata, striscia_max, n_giorni)
  write_csv(ondate, FILE_PROVV)
  cat("Salvato", FILE_PROVV, "(PROVVISORIO: anni con meno di 92 giorni sono incompleti)\n")
}

dati <- ondate |> filter(!is.na(giorni_ondata)) |> select(anno, giorni = giorni_ondata)

m6190 <- mean(dati$giorni[dati$anno %in% 1961:1990])
m9120 <- mean(dati$giorni[dati$anno %in% 1991:2020])
a2026 <- dati$giorni[dati$anno == 2026]

cat("Giorni in ondata di calore per estate — 61-90:", fmt1(m6190),
    "| 91-20:", fmt1(m9120), "| 2003:", dati$giorni[dati$anno == 2003],
    "| 2022:", dati$giorni[dati$anno == 2022], "| 2026:", a2026, "\n")
cat("Top 5 anni:\n")
print(dati |> arrange(desc(giorni)) |> head(5))

# ---- Grafico ----------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_ROSSO <- "#F12938"

evidenzia <- dati |>
  filter(anno %in% c(2003, 2022, 2026)) |>
  arrange(anno) |>
  mutate(scosta = 0)
for (i in seq_len(nrow(evidenzia))) {
  vicino <- abs(evidenzia$anno - evidenzia$anno[i]) <= 4 & seq_len(nrow(evidenzia)) != i
  if (any(vicino)) {
    evidenzia$scosta[i] <- ifelse(evidenzia$anno[i] < mean(evidenzia$anno[vicino | seq_len(nrow(evidenzia)) == i]), -2.6, 2.6)
  }
}

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

y_max <- max(dati$giorni) * 1.15
passo <- if (y_max > 30) 10 else if (y_max > 15) 5 else 2
off   <- y_max * 0.03

titolo <- "Quanti giorni di ondata di calore ci sono stati quest'estate"

p <- ggplot(dati, aes(anno, giorni)) +
  geom_col(fill = "#D4D4D4", width = 0.75) +
  geom_col(data = dati |> filter(anno == 2026), fill = COL_ROSSO, width = 0.75) +
  annotate("segment", x = 1961, xend = 1990, y = m6190, yend = m6190,
           colour = COL_NERO, linewidth = 0.5, linetype = "dashed") +
  annotate("segment", x = 1991, xend = 2020, y = m9120, yend = m9120,
           colour = COL_NERO, linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 1975.5, y = m6190 + off,
           label = paste0("media 1961-1990: ", fmt1(m6190)),
           family = "Source Sans Pro", size = 3.9, color = COL_NERO) +
  annotate("text", x = 2005.5, y = m9120 + off,
           label = paste0("media 1991-2020: ", fmt1(m9120)),
           family = "Source Sans Pro", size = 3.9, color = COL_NERO) +
  # l'etichetta del 2026 e' allineata a destra e finisce sul bordo della barra,
  # cosi' l'asse puo' chiudersi subito dopo il 2026 senza spazio bianco
  geom_text(data = evidenzia,
            aes(x = ifelse(anno == 2026, anno + 0.375, anno + scosta),
                label = paste0(anno, ": ", giorni),
                hjust = ifelse(anno == 2026, 1, 0.5)),
            vjust = -0.5, family = "Source Sans Pro", fontface = "bold",
            size = 4.0,
            color = ifelse(evidenzia$anno == 2026, COL_ROSSO, COL_NERO)) +
  scale_x_continuous(limits = c(1958.5, 2027.3), breaks = seq(1970, 2020, 10),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, passo), limits = c(0, y_max),
                     expand = c(0, 0)) +
  theme_linechart() +
  labs(
    title = titolo,
    subtitle = paste0("Giorni per estate (giugno-agosto) con la massima media nazionale sopra il 90° percentile delle massime 1991-2020\n",
                      "(", fmt1(p90), "°C) per almeno tre giorni consecutivi, Italia, 1961-2026"),
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land"
  )

ggsave("output/grafico_ondate_calore_estate.png", p,
       width = 8, height = 6.5, units = "in", dpi = 300, bg = "white")
cat("Salvato output/grafico_ondate_calore_estate.png\n")
