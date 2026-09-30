# Notti tropicali (minima >= 20°C) nell'ESTATE (giugno-luglio-agosto) per
# l'italiano medio, 1961-2026. Gemello di 13_grafico_notti_tropicali.R e
# 17_notti_tropicali_luglio.R.
# Dati: output/soglie_estate_italia_pop.csv (prodotto da 20_analisi_estate.R).
# Se il file definitivo non esiste, li calcola in via provvisoria dagli orari
# con il metodo di 17 (giugno+luglio+agosto concatenati, pesati per popolazione)
# e li salva in output/soglie_estate_italia_pop_provv.csv.
#   - output/grafico_notti_tropicali_estate.png

library(tidyverse)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

FILE_DEF   <- "output/soglie_estate_italia_pop.csv"
FILE_PROVV <- "output/soglie_estate_italia_pop_provv.csv"

# ---- Dati -------------------------------------------------------------------

if (file.exists(FILE_DEF)) {
  cat("Uso il file definitivo", FILE_DEF, "\n")
  soglie <- read_csv(FILE_DEF, show_col_types = FALSE)
  # se il file di 20 ha una riga per peso, qui serve quella per popolazione
  if ("peso" %in% names(soglie)) soglie <- soglie |> filter(peso == "pop")
} else {
  cat("File definitivo assente: calcolo provvisorio dagli orari (metodo di 17)...\n")
  library(ncdf4)
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

  soglie <- map_dfr(1961:2026, function(anno) {
    paths <- sprintf("input/nc/italia_orari/era5land_t2m_orario_%d-0%d.nc", anno, 6:8)
    paths <- paths[file.exists(paths)]
    if (length(paths) == 0) return(NULL)
    dd <- map(paths, estrai_matrice)
    m <- do.call(cbind, map(dd, "m"))
    tempo <- do.call(c, map(dd, "tempo"))
    ord <- order(tempo); m <- m[, ord, drop = FALSE]; tempo <- tempo[ord]
    dloc <- as.Date(tempo + 3600, tz = "UTC")
    conte <- table(dloc)
    giorni <- as.Date(names(conte)[conte >= 23])
    giorni <- giorni[format(giorni, "%m") %in% c("06", "07", "08")]
    tmin <- vapply(giorni, function(g) do.call(pmin, asplit(m[, dloc == g, drop = FALSE], 2)),
                   numeric(nrow(m)))
    tmax <- vapply(giorni, function(g) do.call(pmax, asplit(m[, dloc == g, drop = FALSE], 2)),
                   numeric(nrow(m)))
    valide <- rowSums(is.na(tmin)) == 0
    cat("  ", anno, ": ", length(giorni), " giorni\n", sep = "")
    tibble(anno = anno,
           n_tmin20 = weighted.mean(rowSums(tmin[valide, , drop = FALSE] >= 20), celle$pop[valide]),
           n_tmin25 = weighted.mean(rowSums(tmin[valide, , drop = FALSE] >= 25), celle$pop[valide]),
           n_tmax30 = weighted.mean(rowSums(tmax[valide, , drop = FALSE] >= 30), celle$pop[valide]),
           n_tmax35 = weighted.mean(rowSums(tmax[valide, , drop = FALSE] >= 35), celle$pop[valide]),
           giorni = length(giorni))
  })
  write_csv(soglie, FILE_PROVV)
  cat("Salvato", FILE_PROVV, "(PROVVISORIO: anni con meno di 92 giorni sono incompleti)\n")
}

fmt1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
fmt0 <- function(x) formatC(x, format = "f", digits = 0, decimal.mark = ",")

dati <- soglie |> filter(!is.na(n_tmin20)) |> select(anno, notti = n_tmin20)

m6190 <- mean(dati$notti[dati$anno %in% 1961:1990])
m9120 <- mean(dati$notti[dati$anno %in% 1991:2020])
a2026 <- dati$notti[dati$anno == 2026]

cat("Notti tropicali in estate per abitante — 61-90:", fmt1(m6190),
    "| 91-20:", fmt1(m9120), "| 2003:", fmt1(dati$notti[dati$anno == 2003]),
    "| 2022:", fmt1(dati$notti[dati$anno == 2022]), "| 2026:", fmt1(a2026), "\n")
cat("Top 5 anni:\n")
print(dati |> arrange(desc(notti)) |> head(5) |> mutate(notti = round(notti, 1)))

# ---- Grafico ----------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_ROSSO <- "#F12938"

# etichette su 2003, 2022 e 2026, con scostamenti anti-sovrapposizione
evidenzia <- dati |>
  filter(anno %in% c(2003, 2022, 2026)) |>
  arrange(anno) |>
  mutate(scosta = 0)
for (i in seq_len(nrow(evidenzia))) {
  vicino <- abs(evidenzia$anno - evidenzia$anno[i]) <= 4 & seq_len(nrow(evidenzia)) != i
  if (any(vicino)) {
    evidenzia$scosta[i] <- ifelse(evidenzia$anno[i] < mean(evidenzia$anno[vicino | seq_len(nrow(evidenzia)) == i]), -3, 3)
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

y_max  <- max(dati$notti) * 1.12
passo  <- if (y_max > 30) 10 else if (y_max > 15) 5 else 2
off    <- y_max * 0.03

titolo <- "Quante notti tropicali abbiamo avuto quest'estate"

p <- ggplot(dati, aes(anno, notti)) +
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
                label = paste0(anno, ": ", fmt1(notti)),
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
    subtitle = "Numero medio per abitante di notti estive (giugno-agosto) con temperatura minima sopra i 20°C, Italia, 1961-2026",
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land e Istat"
  )

ggsave("output/grafico_notti_tropicali_estate.png", p,
       width = 8, height = 6.5, units = "in", dpi = 300, bg = "white")
cat("Salvato output/grafico_notti_tropicali_estate.png\n")
