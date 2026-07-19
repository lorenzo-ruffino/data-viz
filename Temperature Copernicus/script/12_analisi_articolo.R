# Batteria di analisi per l'articolo su giugno 2026 (focus Italia):
#   1. notti tropicali (Tmin >= 20) e super-tropicali (>= 25), giorni caldi
#      (Tmax >= 30) e molto caldi (>= 35) per cella e anno, giugno 1961-2026
#      -> medie pesate per popolazione, quote di popolazione esposta, capoluoghi
#   2. ondate di calore sulla serie nazionale (>= 3 giorni consecutivi con
#      massima nazionale sopra il 90° percentile delle massime di giugno 1991-2020)
#   3. trend di riscaldamento: nazionale (media/min/max), per cella e per regione,
#      giorno vs notte
#   4. esposizione della popolazione per fascia di anomalia
#   5. record giornalieri e classifiche regionali del giugno 2026
#
# Output CSV in output/ + log completo a video (base per findings_giugno_2026.md).

library(tidyverse)
library(ncdf4)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")
celle <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)

leggi_tempo <- function(nc, var = "valid_time") {
  tm <- as.vector(ncvar_get(nc, var))
  un <- ncatt_get(nc, var, "units")$value
  if (grepl("days since", un)) {
    as.Date(sub("days since ", "", un)) + tm
  } else {
    as.POSIXct(sub("seconds since ", "", un), tz = "UTC") + tm
  }
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

# ============ 1. SOGLIE PER CELLA (giugno, 1961-2026) ========================

message("1. Notti tropicali e giorni caldi per cella...")

minmax_giugno <- function(anno) {
  fd_min <- sprintf("input/nc/italia/era5land_t2m_min_%d-06.nc", anno)
  fd_max <- sprintf("input/nc/italia/era5land_t2m_max_%d-06.nc", anno)
  if (file.exists(fd_min) && file.exists(fd_max)) {
    return(list(tmin = estrai_matrice(fd_min)$m, tmax = estrai_matrice(fd_max)$m))
  }
  paths <- sprintf("input/nc/italia_orari/era5land_t2m_orario_%d-0%d.nc", anno, 5:6)
  paths <- paths[file.exists(paths)]
  dd <- map(paths, estrai_matrice)
  m <- do.call(cbind, map(dd, "m"))
  tempo <- do.call(c, map(dd, "tempo"))
  ord <- order(tempo); m <- m[, ord, drop = FALSE]; tempo <- tempo[ord]
  dloc <- as.Date(tempo + 3600, tz = "UTC")
  conte <- table(dloc)
  giorni <- as.Date(names(conte)[conte >= 23])
  giorni <- giorni[format(giorni, "%m") == "06"]
  list(tmin = vapply(giorni, function(g) do.call(pmin, asplit(m[, dloc == g, drop = FALSE], 2)),
                     numeric(nrow(m))),
       tmax = vapply(giorni, function(g) do.call(pmax, asplit(m[, dloc == g, drop = FALSE], 2)),
                     numeric(nrow(m))))
}

soglie <- map_dfr(1961:2026, function(anno) {
  mm <- minmax_giugno(anno)
  valide <- rowSums(is.na(mm$tmin)) == 0
  tibble(ilon = celle$ilon[valide], ilat = celle$ilat[valide], anno = anno,
         n_tmin20 = rowSums(mm$tmin[valide, , drop = FALSE] >= 20),
         n_tmin25 = rowSums(mm$tmin[valide, , drop = FALSE] >= 25),
         n_tmax30 = rowSums(mm$tmax[valide, , drop = FALSE] >= 30),
         n_tmax35 = rowSums(mm$tmax[valide, , drop = FALSE] >= 35))
})
write_csv(soglie, "output/soglie_giugno_celle.csv.gz")

sg <- soglie |>
  left_join(celle |> select(ilon, ilat, regione, pop, area_kmq), by = c("ilon", "ilat"))

# medie nazionali pesate per popolazione (l'esperienza dell'italiano medio)
naz_pop <- sg |>
  group_by(anno) |>
  summarise(across(starts_with("n_"), ~ weighted.mean(.x, pop)), .groups = "drop")
write_csv(naz_pop, "output/soglie_giugno_italia_pop.csv")

riass_soglie <- naz_pop |>
  summarise(across(starts_with("n_"),
                   list(m6190 = ~ mean(.x[anno %in% 1961:1990]),
                        m9120 = ~ mean(.x[anno %in% 1991:2020]),
                        a2003 = ~ .x[anno == 2003],
                        a2026 = ~ .x[anno == 2026])))
cat("\n=== Notti tropicali e giorni caldi a giugno (media per abitante) ===\n")
for (v in c("n_tmin20", "n_tmin25", "n_tmax30", "n_tmax35")) {
  cat(sprintf("%-9s 61-90: %s | 91-20: %s | 2003: %s | 2026: %s\n", v,
              fmt(riass_soglie[[paste0(v, "_m6190")]], 1),
              fmt(riass_soglie[[paste0(v, "_m9120")]], 1),
              fmt(riass_soglie[[paste0(v, "_a2003")]], 1),
              fmt(riass_soglie[[paste0(v, "_a2026")]], 1)))
}

# quote di popolazione esposta alle notti tropicali
pop_tot <- sum(celle$pop)
quote <- sg |>
  filter(anno %in% c(2003, 2022, 2025, 2026) | anno %in% 1991:2020) |>
  mutate(periodo = ifelse(anno %in% 1991:2020 & !anno %in% c(2003), "media 91-20", as.character(anno))) |>
  group_by(periodo, anno) |>
  summarise(pop_1  = sum(pop[n_tmin20 >= 1]),
            pop_7  = sum(pop[n_tmin20 >= 7]),
            pop_15 = sum(pop[n_tmin20 >= 15]), .groups = "drop") |>
  group_by(periodo) |>
  summarise(across(starts_with("pop_"), mean), .groups = "drop") |>
  mutate(across(starts_with("pop_"), ~ .x / pop_tot * 100))
cat("\n=== Quota di popolazione con notti tropicali a giugno (%) ===\n")
for (i in seq_len(nrow(quote))) {
  r <- quote[i, ]
  cat(sprintf("%-12s almeno 1: %s%% | almeno 7: %s%% | almeno 15: %s%%\n",
              r$periodo, fmt(r$pop_1, 0), fmt(r$pop_7, 0), fmt(r$pop_15, 0)))
}

# capoluoghi: notti tropicali nella cella della città
capoluoghi <- tribble(
  ~citta, ~lon, ~lat,
  "Torino", 7.686, 45.070, "Milano", 9.190, 45.464, "Venezia", 12.316, 45.440,
  "Genova", 8.934, 44.407, "Bologna", 11.343, 44.494, "Firenze", 11.256, 43.770,
  "Roma", 12.496, 41.903, "Napoli", 14.268, 40.852, "Bari", 16.871, 41.117,
  "Palermo", 13.361, 38.116, "Cagliari", 9.110, 39.223, "Ancona", 13.518, 43.617
) |>
  mutate(ilon = as.integer(round(lon * 10)), ilat = as.integer(round(lat * 10)))

# le città costiere possono cadere su celle-acqua ERA5-Land: si usa la cella
# valida più vicina
celle_valide <- sg |> filter(anno == 2026) |> distinct(ilon, ilat)
capoluoghi <- capoluoghi |>
  rowwise() |>
  mutate(idx = which.min((celle_valide$ilon - ilon)^2 + (celle_valide$ilat - ilat)^2),
         ilon = celle_valide$ilon[idx],
         ilat = celle_valide$ilat[idx]) |>
  ungroup() |>
  select(-idx)

citta_notti <- capoluoghi |>
  left_join(sg, by = c("ilon", "ilat")) |>
  group_by(citta) |>
  summarise(m6190 = mean(n_tmin20[anno %in% 1961:1990]),
            m9120 = mean(n_tmin20[anno %in% 1991:2020]),
            a2003 = n_tmin20[anno == 2003][1],
            a2026 = n_tmin20[anno == 2026][1], .groups = "drop") |>
  arrange(desc(a2026))
write_csv(citta_notti, "output/notti_tropicali_capoluoghi.csv")
cat("\n=== Notti tropicali a giugno nei capoluoghi (cella della città) ===\n")
for (i in seq_len(nrow(citta_notti))) {
  r <- citta_notti[i, ]
  cat(sprintf("%-9s 2026: %2.0f | 2003: %2.0f | media 91-20: %s | media 61-90: %s\n",
              r$citta, r$a2026, r$a2003, fmt(r$m9120, 1), fmt(r$m6190, 1)))
}

# ============ 2. ONDATE DI CALORE (serie nazionale) ==========================

serie <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data),
         giorno = lubridate::mday(data)) |>
  filter(mese == 6)

p90 <- quantile(serie$t_area_max[serie$anno %in% 1991:2020], 0.9)
cat("\n=== Ondate di calore (Tmax nazionale > 90° percentile 91-20 = ",
    fmt(p90, 1), "°C, episodi di 3+ giorni) ===\n", sep = "")

ondate_anno <- serie |>
  group_by(anno) |>
  arrange(giorno, .by_group = TRUE) |>
  summarise(sopra = list(rle(t_area_max > p90)), .groups = "drop") |>
  mutate(
    giorni_sopra   = map_int(sopra, ~ sum(.x$lengths[.x$values])),
    episodi_3g     = map_int(sopra, ~ sum(.x$values & .x$lengths >= 3)),
    giorni_ondata  = map_int(sopra, ~ sum(.x$lengths[.x$values & .x$lengths >= 3])),
    striscia_max   = map_int(sopra, ~ ifelse(any(.x$values), max(.x$lengths[.x$values]), 0L))
  ) |>
  select(-sopra)
write_csv(ondate_anno, "output/ondate_calore_giugno.csv")

oc <- ondate_anno |>
  summarise(m6190_g = mean(giorni_ondata[anno %in% 1961:1990]),
            m9120_g = mean(giorni_ondata[anno %in% 1991:2020]))
for (a in c(2003, 2022, 2025, 2026)) {
  r <- ondate_anno |> filter(anno == a)
  cat(sprintf("%d: %d episodi, %d giorni in ondata, striscia più lunga %d giorni\n",
              a, r$episodi_3g, r$giorni_ondata, r$striscia_max))
}
cat(sprintf("Media 61-90: %s giorni in ondata | media 91-20: %s\n",
            fmt(oc$m6190_g, 1), fmt(oc$m9120_g, 1)))
top_str <- ondate_anno |> slice_max(striscia_max, n = 3, with_ties = TRUE)
cat("Strisce più lunghe dal 1961:",
    paste0(top_str$anno, " (", top_str$striscia_max, " gg)", collapse = ", "), "\n")
s26 <- serie |> filter(anno == 2026) |> arrange(giorno)
cat("Giorni sopra il 90° percentile nel 2026:",
    paste(s26$giorno[s26$t_area_max > p90], collapse = ", "), "\n")

# ============ 3. TREND DI RISCALDAMENTO ======================================

cat("\n=== Trend giugno Italia (°C per decennio, 1961-2026) ===\n")
annuale <- serie |>
  group_by(anno) |>
  summarise(media = mean(t_area_mean), massime = mean(t_area_max),
            minime = mean(t_area_min),
            media_pop = mean(t_pop_mean), .groups = "drop")
for (v in c("media", "massime", "minime", "media_pop")) {
  sl <- coef(lm(annuale[[v]] ~ annuale$anno))[2] * 10
  cat(sprintf("%-10s %+s °C/decennio\n", v, fmt(sl)))
}
sl_recente <- coef(lm(media ~ anno, data = annuale |> filter(anno >= 1991)))[2] * 10
cat(sprintf("media (solo 1991-2026): %+s °C/decennio\n", fmt(sl_recente)))

# per cella (da griglia mensile, giugno mese intero)
griglia <- read_csv("output/griglia_mensile.csv.gz", show_col_types = FALSE) |>
  filter(stat == "mean", mese == 6, finestra == "mese_intero")
trend_celle <- griglia |>
  group_by(ilon, ilat, lon, lat, regione) |>
  summarise(trend_dec = cov(anno, valore) / var(anno) * 10,
            n = n(), .groups = "drop") |>
  filter(n >= 60)
write_csv(trend_celle, "output/trend_giugno_celle.csv")

cat("\nDistribuzione trend per cella (°C/decennio): ")
print(round(quantile(trend_celle$trend_dec, c(0.02, 0.25, 0.5, 0.75, 0.98)), 2))

trend_reg <- trend_celle |>
  group_by(regione) |>
  summarise(trend_dec = mean(trend_dec), .groups = "drop") |>
  arrange(desc(trend_dec))
write_csv(trend_reg, "output/trend_giugno_regioni.csv")
cat("\n=== Trend per regione (°C/decennio, media delle celle) ===\n")
for (i in seq_len(nrow(trend_reg))) {
  cat(sprintf("%-22s %+s\n", trend_reg$regione[i], fmt(trend_reg$trend_dec[i])))
}

# giorno vs notte (dagli orari, giugno)
gn <- read_csv("output/giorno_notte_italia.csv", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data)) |>
  filter(mese == 6) |>
  group_by(anno, fascia) |>
  summarise(t = mean(t_area), .groups = "drop")
cat("\n=== Trend giorno vs notte (giugno, °C/decennio) ===\n")
for (f in c("giorno", "notte")) {
  d <- gn |> filter(fascia == f)
  cat(sprintf("%-7s %+s\n", f, fmt(coef(lm(d$t ~ d$anno))[2] * 10)))
}

# ============ 4. ESPOSIZIONE DELLA POPOLAZIONE ===============================

anomalia_celle <- griglia |>
  group_by(ilon, ilat) |>
  summarise(baseline = mean(valore[anno %in% 1991:2020]),
            t26 = mean(valore[anno == 2026]), .groups = "drop") |>
  filter(!is.na(t26)) |>
  mutate(anomalia = t26 - baseline) |>
  left_join(celle |> select(ilon, ilat, pop), by = c("ilon", "ilat"))

esp <- anomalia_celle |>
  summarise(
    pop_tot   = sum(pop),
    `oltre +2`   = sum(pop[anomalia >= 2]) / pop_tot * 100,
    `oltre +3`   = sum(pop[anomalia >= 3]) / pop_tot * 100,
    `oltre +4`   = sum(pop[anomalia >= 4]) / pop_tot * 100,
    `oltre +4,5` = sum(pop[anomalia >= 4.5]) / pop_tot * 100)
cat("\n=== Popolazione per fascia di anomalia di giugno 2026 vs 91-20 (%) ===\n")
cat(sprintf("oltre +2: %s%% | oltre +3: %s%% | oltre +4: %s%% | oltre +4,5: %s%%\n",
            fmt(esp$`oltre +2`, 0), fmt(esp$`oltre +3`, 0),
            fmt(esp$`oltre +4`, 1), fmt(esp$`oltre +4,5`, 1)))
cat(sprintf("(equivalenti a %s, %s, %s e %s milioni di persone)\n",
            fmt(esp$`oltre +2` * esp$pop_tot / 100 / 1e6, 1),
            fmt(esp$`oltre +3` * esp$pop_tot / 100 / 1e6, 1),
            fmt(esp$`oltre +4` * esp$pop_tot / 100 / 1e6, 1),
            fmt(esp$`oltre +4,5` * esp$pop_tot / 100 / 1e6, 1)))

# aree assolute più calde del 2026 (livelli, non anomalie)
top_calde <- griglia |>
  filter(anno == 2026) |>
  left_join(celle |> select(ilon, ilat, pop), by = c("ilon", "ilat")) |>
  slice_max(valore, n = 12) |>
  select(regione, lon, lat, valore, pop)
cat("\n=== Le 12 celle più calde in assoluto (T media giugno 2026) ===\n")
for (i in seq_len(nrow(top_calde))) {
  r <- top_calde[i, ]
  cat(sprintf("%-15s (%.1f, %.1f)  %s°C\n", r$regione, r$lon, r$lat, fmt(r$valore, 1)))
}

# ============ 5. RECORD GIORNALIERI E CLASSIFICHE REGIONALI ==================

record_giorni <- serie |>
  group_by(giorno) |>
  summarise(record_2026 = t_area_mean[anno == 2026][1] >= max(t_area_mean),
            .groups = "drop")
cat("\nGiorni di giugno 2026 che sono il più caldo mai registrato per quella data:",
    sum(record_giorni$record_2026), "su 30\n")
cat("Quali:", paste(record_giorni$giorno[record_giorni$record_2026], collapse = ", "), "\n")

regioni_serie <- read_csv("output/serie_giornaliera_regioni.csv.gz", show_col_types = FALSE) |>
  mutate(anno = lubridate::year(data), mese = lubridate::month(data)) |>
  filter(mese == 6) |>
  group_by(regione, anno) |>
  summarise(t = mean(t_area_mean), .groups = "drop")

rank_reg <- regioni_serie |>
  group_by(regione) |>
  summarise(posto_2026 = rank(-t)[anno == 2026],
            record = t[anno == 2026] >= max(t), .groups = "drop") |>
  arrange(posto_2026)
write_csv(rank_reg, "output/classifica_regioni_2026.csv")
cat("\n=== Posizione del giugno 2026 nella storia di ogni regione (dal 1961) ===\n")
for (i in seq_len(nrow(rank_reg))) {
  cat(sprintf("%-22s %d°%s\n", rank_reg$regione[i], rank_reg$posto_2026[i],
              ifelse(rank_reg$record[i], " (RECORD)", "")))
}

cat("\nFatto: CSV salvati in output/\n")
