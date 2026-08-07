# Notti tropicali (minima >= 20°C) a LUGLIO per l'italiano medio, 1961-2026.
# Calcola i conteggi per cella dagli orari (giugno+luglio concatenati, così il
# 1° luglio recupera la mezzanotte dal file di giugno), pesa per la popolazione
# e produce grafico + CSV:
#   - output/soglie_luglio_italia_pop.csv
#   - output/grafico_notti_tropicali_luglio.png

library(tidyverse)
library(ncdf4)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

celle <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)

leggi_tempo <- function(nc) {
  tm <- as.vector(ncvar_get(nc, "valid_time"))
  un <- ncatt_get(nc, "valid_time", "units")$value
  as.POSIXct(sub("seconds since ", "", un), tz = "UTC") + tm
}

estrai_matrice <- function(nc_path) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc))
  lon <- as.numeric(round(ncvar_get(nc, "longitude"), 1))
  lat <- as.numeric(round(ncvar_get(nc, "latitude"), 1))
  tempo <- leggi_tempo(nc)
  t2m <- ncvar_get(nc, "t2m")
  m <- matrix(t2m, nrow = length(lon) * length(lat), ncol = length(tempo)) - 273.15
  chiave <- paste(rep(as.integer(round(lon * 10)), times = length(lat)),
                  rep(as.integer(round(lat * 10)), each  = length(lon)))
  list(m = m[match(paste(celle$ilon, celle$ilat), chiave), , drop = FALSE],
       tempo = tempo)
}

soglie_luglio <- map_dfr(1961:2026, function(anno) {
  paths <- sprintf("input/nc/italia_orari/era5land_t2m_orario_%d-0%d.nc", anno, 6:7)
  paths <- paths[file.exists(paths)]
  dd <- map(paths, estrai_matrice)
  m <- do.call(cbind, map(dd, "m"))
  tempo <- do.call(c, map(dd, "tempo"))
  ord <- order(tempo); m <- m[, ord, drop = FALSE]; tempo <- tempo[ord]
  dloc <- as.Date(tempo + 3600, tz = "UTC")
  conte <- table(dloc)
  giorni <- as.Date(names(conte)[conte >= 23])
  giorni <- giorni[format(giorni, "%m") == "07"]
  tmin <- vapply(giorni, function(g) do.call(pmin, asplit(m[, dloc == g, drop = FALSE], 2)),
                 numeric(nrow(m)))
  tmax <- vapply(giorni, function(g) do.call(pmax, asplit(m[, dloc == g, drop = FALSE], 2)),
                 numeric(nrow(m)))
  valide <- rowSums(is.na(tmin)) == 0
  tibble(anno = anno,
         n_tmin20 = weighted.mean(rowSums(tmin[valide, , drop = FALSE] >= 20), celle$pop[valide]),
         n_tmin25 = weighted.mean(rowSums(tmin[valide, , drop = FALSE] >= 25), celle$pop[valide]),
         n_tmax30 = weighted.mean(rowSums(tmax[valide, , drop = FALSE] >= 30), celle$pop[valide]),
         n_tmax35 = weighted.mean(rowSums(tmax[valide, , drop = FALSE] >= 35), celle$pop[valide]),
         giorni = length(giorni))
})
write_csv(soglie_luglio, "output/soglie_luglio_italia_pop.csv")

fmt1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
m6190 <- mean(soglie_luglio$n_tmin20[soglie_luglio$anno %in% 1961:1990])
m9120 <- mean(soglie_luglio$n_tmin20[soglie_luglio$anno %in% 1991:2020])
a2026 <- soglie_luglio$n_tmin20[soglie_luglio$anno == 2026]

cat("Notti tropicali a luglio per abitante — 61-90:", fmt1(m6190),
    "| 91-20:", fmt1(m9120), "| 2003:", fmt1(soglie_luglio$n_tmin20[soglie_luglio$anno == 2003]),
    "| 2026:", fmt1(a2026), "\n")
cat("Top 5 anni:\n")
print(soglie_luglio |> arrange(desc(n_tmin20)) |> select(anno, n_tmin20) |> head(5) |>
        mutate(n_tmin20 = round(n_tmin20, 1)))

# ---- Grafico ----------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_ROSSO <- "#F12938"

dati <- soglie_luglio |> select(anno, notti = n_tmin20)

# etichette: i 4 anni con più notti, con scostamenti anti-sovrapposizione
evidenzia <- dati |>
  slice_max(notti, n = 4) |>
  arrange(anno) |>
  mutate(scosta = 0)
for (i in seq_len(nrow(evidenzia))) {
  vicino <- abs(evidenzia$anno - evidenzia$anno[i]) <= 3 & seq_len(nrow(evidenzia)) != i
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
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

y_max <- ceiling(max(dati$notti)) + 1.4

p <- ggplot(dati, aes(anno, notti)) +
  geom_col(fill = "#D4D4D4", width = 0.75) +
  geom_col(data = dati |> filter(anno == 2026), fill = COL_ROSSO, width = 0.75) +
  annotate("segment", x = 1961, xend = 1990, y = m6190, yend = m6190,
           colour = COL_NERO, linewidth = 0.5, linetype = "dashed") +
  annotate("segment", x = 1991, xend = 2020, y = m9120, yend = m9120,
           colour = COL_NERO, linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 1975.5, y = m6190 + 0.65,
           label = paste0("media 1961-1990: ", fmt1(m6190)),
           family = "Source Sans Pro", size = 3.9, color = COL_NERO) +
  annotate("text", x = 2005.5, y = m9120 + 0.65,
           label = paste0("media 1991-2020: ", fmt1(m9120)),
           family = "Source Sans Pro", size = 3.9, color = COL_NERO) +
  geom_text(data = evidenzia,
            aes(x = anno + scosta, label = paste0(anno, ": ", fmt1(notti))),
            vjust = -0.5, family = "Source Sans Pro", fontface = "bold",
            size = 4.0,
            color = ifelse(evidenzia$anno == 2026, COL_ROSSO, COL_NERO)) +
  scale_x_continuous(limits = c(1958.5, 2034), breaks = seq(1970, 2020, 10),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, y_max),
                     expand = c(0, 0)) +
  theme_linechart() +
  labs(
    title = "A luglio ventidue notti su trentuno sopra i 20 gradi",
    subtitle = "Numero medio per abitante di notti di luglio con temperatura minima sopra i 20°C, Italia, 1961-2026",
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land e Istat"
  )

ggsave("output/grafico_notti_tropicali_luglio.png", p,
       width = 8, height = 6.5, units = "in", dpi = 300, bg = "white")
cat("Salvato output/grafico_notti_tropicali_luglio.png\n")
