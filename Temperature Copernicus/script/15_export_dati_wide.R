# Esporta in "grafici interattivi/" i dati dei grafici non-mappa in formato
# wide (una riga per valore dell'asse X, una colonna per serie):
#   - giugno_annuale_wide.csv        anno | media | media_massime | media_minime
#   - giornaliero_maggio_giugno_wide.csv  data | etichetta | t_2026 | t_2003 |
#                                         media_1961_1990 | media_1991_2020
#   - notti_tropicali_wide.csv       anno | notti | media_1961_1990 | media_1991_2020
#     (le due colonne-media sono valorizzate solo nei rispettivi trentenni, per
#      disegnare i segmenti di riferimento)

library(tidyverse)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

serie <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data),
         giorno = lubridate::mday(data))

# ---- 1. Serie annuale di giugno ---------------------------------------------

annuale <- serie |>
  filter(mese == 6) |>
  group_by(anno) |>
  summarise(media         = round(mean(t_area_mean), 2),
            media_massime = round(mean(t_area_max), 2),
            media_minime  = round(mean(t_area_min), 2), .groups = "drop")
write_csv(annuale, "grafici interattivi/giugno_annuale_wide.csv")

# ---- 2. Giornaliero maggio-giugno -------------------------------------------

mesi_it <- c("maggio", "giugno")
giorni <- serie |>
  filter(mese %in% c(5, 6)) |>
  group_by(mese, giorno) |>
  summarise(
    t_2026          = round(mean(t_area_mean[anno == 2026]), 2),
    t_2003          = round(mean(t_area_mean[anno == 2003]), 2),
    media_1961_1990 = round(mean(t_area_mean[anno %in% 1961:1990]), 2),
    media_1991_2020 = round(mean(t_area_mean[anno %in% 1991:2020]), 2),
    .groups = "drop") |>
  mutate(data = sprintf("2026-%02d-%02d", mese, giorno),
         etichetta = paste(giorno, mesi_it[mese - 4])) |>
  arrange(data) |>
  select(data, etichetta, t_2026, t_2003, media_1961_1990, media_1991_2020)
write_csv(giorni, "grafici interattivi/giornaliero_maggio_giugno_wide.csv")

# ---- 3. Notti tropicali per abitante ----------------------------------------

soglie <- read_csv("output/soglie_giugno_italia_pop.csv", show_col_types = FALSE)
m6190 <- mean(soglie$n_tmin20[soglie$anno %in% 1961:1990])
m9120 <- mean(soglie$n_tmin20[soglie$anno %in% 1991:2020])

notti <- soglie |>
  transmute(anno,
            notti = round(n_tmin20, 1),
            media_1961_1990 = ifelse(anno %in% 1961:1990, round(m6190, 1), NA),
            media_1991_2020 = ifelse(anno %in% 1991:2020, round(m9120, 1), NA))
write_csv(notti, "grafici interattivi/notti_tropicali_wide.csv", na = "")

cat("Esportati:\n  giugno_annuale_wide.csv:", nrow(annuale), "righe\n",
    " giornaliero_maggio_giugno_wide.csv:", nrow(giorni), "righe\n",
    " notti_tropicali_wide.csv:", nrow(notti), "righe\n")
