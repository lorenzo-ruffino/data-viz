# Split giorno/notte dai dati ORARI ERA5-Land (scaricati con 02_scarica_orari.py).
# Definizione (ora locale solare, UTC+1):
#   giorno = ore 07:00-18:59, notte = ore 19:00-06:59
# La notte è attribuita alla data in cui finisce (la notte "del 15" è quella
# tra il 14 e il 15). Medie Italia pesate per superficie e popolazione 2021.
#
# Output: output/giorno_notte_italia.csv + confronto giugno 2026 vs 2003 a video.

library(tidyverse)
library(ncdf4)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

ORA_INIZIO_GIORNO <- 7L    # incluso
ORA_FINE_GIORNO   <- 19L   # escluso
FUSO_ORE          <- 1L    # UTC+1, ora solare italiana

celle <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)

files <- list.files("input/nc/italia_orari", pattern = "^era5land_t2m_orario_.*\\.nc$",
                    full.names = TRUE)
stopifnot(length(files) > 0)

elabora_orario <- function(path) {
  nc <- nc_open(path)
  on.exit(nc_close(nc))
  lon <- as.numeric(round(ncvar_get(nc, "longitude"), 1))
  lat <- as.numeric(round(ncvar_get(nc, "latitude"), 1))
  tm  <- as.vector(ncvar_get(nc, "valid_time"))
  un  <- ncatt_get(nc, "valid_time", "units")$value
  if (grepl("seconds since", un)) {
    origine <- as.POSIXct(sub("seconds since ", "", un), tz = "UTC")
    tempo <- origine + tm
  } else {
    origine <- as.POSIXct(paste(sub("hours since ", "", un)), tz = "UTC")
    tempo <- origine + tm * 3600
  }
  t2m <- ncvar_get(nc, "t2m")

  n_lon <- length(lon); n_lat <- length(lat)
  m <- matrix(t2m, nrow = n_lon * n_lat, ncol = length(tempo)) - 273.15
  chiave <- paste(rep(as.integer(round(lon * 10)), times = n_lat),
                  rep(as.integer(round(lat * 10)), each  = n_lon))
  idx <- match(paste(celle$ilon, celle$ilat), chiave)
  sub <- m[idx, , drop = FALSE]
  valide <- rowSums(is.na(sub)) == 0
  sv <- sub[valide, , drop = FALSE]
  w_a <- celle$area_kmq[valide]
  w_p <- celle$pop[valide]

  locale <- tempo + FUSO_ORE * 3600
  ora <- as.integer(format(locale, "%H"))
  di_giorno <- ora >= ORA_INIZIO_GIORNO & ora < ORA_FINE_GIORNO
  # la notte appartiene alla data in cui finisce: le ore serali (>= 19) slittano
  # al giorno successivo
  data_rif <- as.Date(locale, tz = "UTC") + ifelse(!di_giorno & ora >= ORA_FINE_GIORNO, 1L, 0L)

  tibble(data = data_rif,
         fascia = ifelse(di_giorno, "giorno", "notte"),
         t_area = as.vector(colSums(sv * w_a) / sum(w_a)),
         t_pop  = as.vector(colSums(sv * w_p) / sum(w_p))) |>
    group_by(data, fascia) |>
    summarise(t_area = mean(t_area), t_pop = mean(t_pop), n_ore = n(), .groups = "drop")
}

message("Elaborazione di ", length(files), " file orari...")
serie <- map_dfr(files, elabora_orario) |>
  filter(n_ore == 12) |>          # scarta notti/giorni monchi ai bordi del file
  distinct(data, fascia, .keep_all = TRUE) |>
  arrange(data, fascia)

write_csv(serie, "output/giorno_notte_italia.csv")

fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")

confronto <- serie |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data),
         giorno = lubridate::mday(data)) |>
  filter(mese %in% c(5, 6))

fin_giu <- confronto |> filter(anno == 2026, mese == 6) |> pull(giorno) |> max()

# medie annuali per fascia (giugno sulla finestra disponibile nel 2026)
annuale <- confronto |>
  filter(mese == 5 | giorno <= fin_giu) |>
  group_by(anno, mese, fascia) |>
  summarise(t_area = mean(t_area), t_pop = mean(t_pop), .groups = "drop")

riepilogo <- annuale |>
  pivot_longer(c(t_area, t_pop), names_to = "peso", values_to = "t") |>
  mutate(peso = sub("t_", "", peso)) |>
  group_by(mese, fascia, peso) |>
  summarise(
    n_anni_6190     = sum(anno %in% 1961:1990),
    n_anni_9120     = sum(anno %in% 1991:2020),
    media_1961_1990 = mean(t[anno %in% 1961:1990]),
    media_1991_2020 = mean(t[anno %in% 1991:2020]),
    t_2003          = mean(t[anno == 2003]),
    t_2026          = mean(t[anno == 2026]),
    .groups = "drop") |>
  mutate(vs_1961_1990 = t_2026 - media_1961_1990,
         vs_1991_2020 = t_2026 - media_1991_2020,
         vs_2003      = t_2026 - t_2003)

write_csv(riepilogo, "output/giorno_notte_riepilogo.csv")

if (any(riepilogo$n_anni_9120 < 30) || any(riepilogo$n_anni_6190 < 30)) {
  cat("\nATTENZIONE: baseline oraria incompleta (61-90:", min(riepilogo$n_anni_6190),
      "anni; 91-20:", min(riepilogo$n_anni_9120),
      "): scaricare i mesi mancanti con 02_scarica_orari.py\n")
}

cat("\n=== Giorno/notte, media Italia ===\n")
cat("(giorno = ore 7-19, notte = 19-7 ora solare; giugno sui giorni 1-", fin_giu, ")\n\n", sep = "")
for (i in seq_len(nrow(riepilogo))) {
  r <- riepilogo[i, ]
  cat(sprintf("%-8s %-7s peso %-11s 2026: %s°C | 61-90: %s (%+s) | 91-20: %s (%+s) | 2003: %s (%+s)\n",
              ifelse(r$mese == 5, "maggio", "giugno"), r$fascia,
              ifelse(r$peso == "area", "superficie", "popolazione"),
              fmt(r$t_2026),
              fmt(r$media_1961_1990), fmt(r$vs_1961_1990),
              fmt(r$media_1991_2020), fmt(r$vs_1991_2020),
              fmt(r$t_2003), fmt(r$vs_2003)))
}
