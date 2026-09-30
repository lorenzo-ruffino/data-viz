# Soglie per cella sull'ESTATE (giugno+luglio+agosto) 1961-2026 dai dati ORARI
# ERA5-Land, gemello stagionale del punto 1 di 12_analisi_articolo.R e di
# 17_notti_tropicali_luglio.R:
#   - notti tropicali (Tmin >= 20) e super-tropicali (Tmin >= 25)
#   - giorni caldi (Tmax >= 30) e molto caldi (Tmax >= 35)
# I file orari maggio-agosto dello stesso anno vengono concatenati (così il
# 1° giugno recupera la mezzanotte dal file di maggio); si tengono i giorni
# locali (UTC+1) con almeno 23 ore.
#
# Finestra omogenea: finché agosto 2026 è parziale (es. 1-30) si contano per
# tutti gli anni solo i giorni di agosto fino a quel giorno. Gli anni senza
# agosto (download storico non ancora arrivato) restano nel CSV con `giorni`
# < attesi e vengono esclusi da medie, quote e classifiche.
#
# Output (in output/):
#   soglie_estate_celle.csv.gz        per cella e anno (+ giorni contati)
#   soglie_estate_italia_pop.csv      medie per abitante per anno
#   soglie_estate_mesi_pop.csv        medie per abitante per anno e mese
#   quote_popolazione_soglie_estate.csv  quote di popolazione con >= N notti/giorni
#   notti_tropicali_capoluoghi_estate.csv  capoluoghi 2026|2003|2022|91-20|61-90

suppressPackageStartupMessages({ library(tidyverse); library(ncdf4) })

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")
t_inizio <- Sys.time()

fmt  <- function(x, d = 2) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = d, decimal.mark = ","))
fmts <- function(x, d = 2) ifelse(is.na(x), "n.d.", formatC(x, format = "f", digits = d, decimal.mark = ",", flag = "+"))
sep  <- function(titolo) cat("\n", strrep("=", 78), "\n", titolo, "\n", strrep("=", 78), "\n", sep = "")

ANNO  <- 2026L
BASE1 <- 1961:1990
BASE2 <- 1991:2020
celle <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)

leggi_tempo <- function(nc) {
  tm <- as.vector(ncvar_get(nc, "valid_time"))
  un <- ncatt_get(nc, "valid_time", "units")$value
  if (grepl("seconds since", un)) as.POSIXct(sub("seconds since ", "", un), tz = "UTC") + tm
  else as.POSIXct(sub("hours since ", "", un), tz = "UTC") + tm * 3600
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
  list(m = m[match(paste(celle$ilon, celle$ilat), chiave), , drop = FALSE], tempo = tempo)
}

# min/max giornaliere per cella dagli orari di un anno (mesi 5-8 concatenati)
minmax_anno <- function(anno) {
  paths <- sprintf("input/nc/italia_orari/era5land_t2m_orario_%d-0%d.nc", anno, 5:8)
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
  if (length(giorni) == 0) return(NULL)
  list(giorni = giorni,
       tmin = vapply(giorni, function(g) do.call(pmin, asplit(m[, dloc == g, drop = FALSE], 2)), numeric(nrow(m))),
       tmax = vapply(giorni, function(g) do.call(pmax, asplit(m[, dloc == g, drop = FALSE], 2)), numeric(nrow(m))))
}

# ---- Finestra di agosto disponibile nel 2026 --------------------------------
mm26 <- minmax_anno(ANNO)
stopifnot(!is.null(mm26))
fin_ago <- max(c(0L, as.integer(format(mm26$giorni[format(mm26$giorni, "%m") == "08"], "%d"))))
n_attesi <- 30L + 31L + fin_ago
in_finestra <- function(giorni) format(giorni, "%m") != "08" | as.integer(format(giorni, "%d")) <= fin_ago

sep("SOGLIE PER CELLA, ESTATE 1961-2026 (dagli orari)")
cat("Finestra: giugno 1-30, luglio 1-31, agosto 1-", fin_ago, " (", n_attesi, " giorni)\n", sep = "")
if (fin_ago < 31) cat("ATTENZIONE: agosto 2026 PARZIALE, conteggi su finestra omogenea per tutti gli anni.\n")

conta_soglie <- function(anno, mm) {
  sel <- in_finestra(mm$giorni)
  tmin <- mm$tmin[, sel, drop = FALSE]; tmax <- mm$tmax[, sel, drop = FALSE]
  mese <- as.integer(format(mm$giorni[sel], "%m"))
  valide <- rowSums(is.na(tmin)) == 0
  per_mese <- map_dfr(sort(unique(mese)), function(mm_) {
    s <- mese == mm_
    tibble(ilon = celle$ilon[valide], ilat = celle$ilat[valide], anno = anno, mese = mm_,
           n_tmin20 = rowSums(tmin[valide, s, drop = FALSE] >= 20),
           n_tmin25 = rowSums(tmin[valide, s, drop = FALSE] >= 25),
           n_tmax30 = rowSums(tmax[valide, s, drop = FALSE] >= 30),
           n_tmax35 = rowSums(tmax[valide, s, drop = FALSE] >= 35),
           giorni = sum(s))
  })
  per_mese
}

message("Lettura orari 1961-", ANNO, " (4 file per anno)...")
soglie_mesi <- map_dfr(1961:ANNO, function(anno) {
  mm <- if (anno == ANNO) mm26 else minmax_anno(anno)
  if (is.null(mm)) { message("  ", anno, ": nessun file"); return(NULL) }
  out <- conta_soglie(anno, mm)
  message(sprintf("  %d: %d giorni (%s)", anno, sum(out$giorni[out$ilon == out$ilon[1] & out$ilat == out$ilat[1]]),
                  paste(sort(unique(out$mese)), collapse = ",")))
  out
})

soglie <- soglie_mesi |>
  group_by(ilon, ilat, anno) |>
  summarise(across(c(n_tmin20, n_tmin25, n_tmax30, n_tmax35, giorni), sum), .groups = "drop") |>
  mutate(completo = giorni == n_attesi)
write_csv(soglie |> select(-completo), "output/soglie_estate_celle.csv.gz")

anni_completi <- soglie |> filter(completo) |> distinct(anno) |> pull(anno)
cat("\nAnni con estate completa sulla finestra:", length(anni_completi),
    "| incompleti (esclusi da medie/quote):", length(setdiff(1961:ANNO, anni_completi)), "\n")
cat(sprintf("Anni completi nelle baseline: 61-90 %d su 30 | 91-20 %d su 30 | 2003: %s | 2022: %s\n",
            sum(anni_completi %in% BASE1), sum(anni_completi %in% BASE2),
            ifelse(2003 %in% anni_completi, "sì", "NO"), ifelse(2022 %in% anni_completi, "sì", "NO")))

sg <- soglie |> left_join(celle |> select(ilon, ilat, regione, pop, area_kmq), by = c("ilon", "ilat"))

# ---- Medie nazionali per abitante ------------------------------------------
naz_pop <- sg |>
  group_by(anno) |>
  summarise(across(c(n_tmin20, n_tmin25, n_tmax30, n_tmax35), ~ weighted.mean(.x, pop)),
            giorni = first(giorni), completo = first(completo), .groups = "drop")
write_csv(naz_pop, "output/soglie_estate_italia_pop.csv")

naz_mesi <- soglie_mesi |>
  left_join(celle |> select(ilon, ilat, pop), by = c("ilon", "ilat")) |>
  group_by(anno, mese) |>
  summarise(across(c(n_tmin20, n_tmin25, n_tmax30, n_tmax35), ~ weighted.mean(.x, pop)),
            giorni = first(giorni), .groups = "drop")
write_csv(naz_mesi, "output/soglie_estate_mesi_pop.csv")

etic <- c(n_tmin20 = "notti Tmin>=20", n_tmin25 = "notti Tmin>=25", n_tmax30 = "giorni Tmax>=30", n_tmax35 = "giorni Tmax>=35")
sep("Medie per abitante, estate (giorni nella finestra)")
nc_ <- naz_pop |> filter(completo)
for (v in names(etic)) {
  cat(sprintf("%-16s 61-90: %s (%d anni) | 91-20: %s (%d anni) | 2003: %s | 2022: %s | 2026: %s | posto 2026: %d su %d\n", etic[v],
              fmt(mean(nc_[[v]][nc_$anno %in% BASE1]), 1), sum(nc_$anno %in% BASE1),
              fmt(mean(nc_[[v]][nc_$anno %in% BASE2]), 1), sum(nc_$anno %in% BASE2),
              fmt(nc_[[v]][nc_$anno == 2003][1], 1), fmt(nc_[[v]][nc_$anno == 2022][1], 1),
              fmt(nc_[[v]][nc_$anno == ANNO][1], 1),
              sum(nc_[[v]] > nc_[[v]][nc_$anno == ANNO][1]) + 1L, nrow(nc_)))
  top <- nc_ |> arrange(desc(.data[[v]])) |> head(5)
  cat("   top 5:", paste0(top$anno, " (", fmt(top[[v]], 1), ")", collapse = ", "), "\n")
}
cat("\nPer mese, 2026 (per abitante): \n")
m26 <- naz_mesi |> filter(anno == ANNO)
for (i in seq_len(nrow(m26))) {
  r <- m26[i, ]
  b <- naz_mesi |> filter(mese == r$mese, anno %in% BASE2)
  cat(sprintf("  mese %d (%d giorni): notti>=20 %s (91-20: %s, %d anni) | notti>=25 %s | giorni>=30 %s (91-20: %s) | giorni>=35 %s (91-20: %s)\n",
              r$mese, r$giorni, fmt(r$n_tmin20, 1), fmt(mean(b$n_tmin20), 1), nrow(b), fmt(r$n_tmin25, 1),
              fmt(r$n_tmax30, 1), fmt(mean(b$n_tmax30), 1), fmt(r$n_tmax35, 1), fmt(mean(b$n_tmax35), 1)))
}

# ---- Quote di popolazione esposta ------------------------------------------
pop_tot <- sum(celle$pop)
quote <- sg |>
  filter(completo, anno %in% c(2003, 2022, ANNO) | anno %in% BASE1 | anno %in% BASE2) |>
  mutate(periodo = case_when(anno %in% c(2003, 2022, ANNO) ~ as.character(anno),
                             anno %in% BASE1 ~ "media 61-90", TRUE ~ "media 91-20")) |>
  group_by(periodo, anno) |>
  summarise(notti_1  = sum(pop[n_tmin20 >= 1]),  notti_15 = sum(pop[n_tmin20 >= 15]),
            notti_30 = sum(pop[n_tmin20 >= 30]), notti_45 = sum(pop[n_tmin20 >= 45]),
            notti_60 = sum(pop[n_tmin20 >= 60]),
            super_1  = sum(pop[n_tmin25 >= 1]),  super_5 = sum(pop[n_tmin25 >= 5]),
            caldi35_1 = sum(pop[n_tmax35 >= 1]), caldi35_10 = sum(pop[n_tmax35 >= 10]), caldi35_20 = sum(pop[n_tmax35 >= 20]),
            .groups = "drop") |>
  group_by(periodo) |>
  summarise(n_anni = n(), across(-anno, mean), .groups = "drop") |>
  mutate(across(-c(periodo, n_anni), ~ .x / pop_tot * 100)) |>
  arrange(match(periodo, c("media 61-90", "media 91-20", "2003", "2022", as.character(ANNO))))
write_csv(quote, "output/quote_popolazione_soglie_estate.csv")

sep("Quota di popolazione (%) per numero di notti tropicali (Tmin>=20) nell'estate")
for (i in seq_len(nrow(quote))) {
  r <- quote[i, ]
  cat(sprintf("%-12s (%2d anni) almeno 1: %s%% | 15: %s%% | 30: %s%% | 45: %s%% | 60: %s%%  || super-tropicali >=1: %s%%, >=5: %s%% || Tmax>=35 almeno 1: %s%%, 10: %s%%, 20: %s%%\n",
              r$periodo, r$n_anni, fmt(r$notti_1, 0), fmt(r$notti_15, 0), fmt(r$notti_30, 0), fmt(r$notti_45, 0), fmt(r$notti_60, 0),
              fmt(r$super_1, 0), fmt(r$super_5, 0), fmt(r$caldi35_1, 0), fmt(r$caldi35_10, 0), fmt(r$caldi35_20, 0)))
}
cat(sprintf("(popolazione totale nelle celle: %s milioni; 1%% = %s mila persone)\n", fmt(pop_tot / 1e6, 1), fmt(pop_tot / 1e5, 0)))

# ---- Capoluoghi (stessa lista di 12_analisi_articolo.R) ---------------------
capoluoghi <- tribble(
  ~citta, ~lon, ~lat,
  "Torino", 7.686, 45.070, "Milano", 9.190, 45.464, "Venezia", 12.316, 45.440,
  "Genova", 8.934, 44.407, "Bologna", 11.343, 44.494, "Firenze", 11.256, 43.770,
  "Roma", 12.496, 41.903, "Napoli", 14.268, 40.852, "Bari", 16.871, 41.117,
  "Palermo", 13.361, 38.116, "Cagliari", 9.110, 39.223, "Ancona", 13.518, 43.617
) |>
  mutate(ilon = as.integer(round(lon * 10)), ilat = as.integer(round(lat * 10)))

# le città costiere possono cadere su celle-acqua: si usa la cella valida più vicina
celle_valide <- sg |> filter(anno == ANNO) |> distinct(ilon, ilat)
capoluoghi <- capoluoghi |>
  rowwise() |>
  mutate(idx = which.min((celle_valide$ilon - ilon)^2 + (celle_valide$ilat - ilat)^2),
         ilon = celle_valide$ilon[idx], ilat = celle_valide$ilat[idx]) |>
  ungroup() |> select(-idx)

riass_citta <- function(v) {
  capoluoghi |>
    left_join(sg |> filter(completo), by = c("ilon", "ilat")) |>
    group_by(citta) |>
    summarise("{v}_2026" := .data[[v]][anno == ANNO][1],
              "{v}_2003" := .data[[v]][anno == 2003][1],
              "{v}_2022" := .data[[v]][anno == 2022][1],
              "{v}_m9120" := mean(.data[[v]][anno %in% BASE2]),
              "{v}_m6190" := mean(.data[[v]][anno %in% BASE1]),
              "{v}_posto_2026" := sum(.data[[v]] > .data[[v]][anno == ANNO][1]) + 1L,
              .groups = "drop")
}
citta <- reduce(map(c("n_tmin20", "n_tmin25", "n_tmax30", "n_tmax35"), riass_citta), left_join, by = "citta") |>
  arrange(desc(n_tmin20_2026))
write_csv(citta, "output/notti_tropicali_capoluoghi_estate.csv")

sep("Capoluoghi (cella della città): notti tropicali e giorni >= 35 nell'estate, 2026 | 2003 | 2022 | media 91-20 | media 61-90")
for (i in seq_len(nrow(citta))) {
  r <- citta[i, ]
  cat(sprintf("%-9s notti>=20: %2.0f | %s | %s | %s | %s (posto %d)   notti>=25: %2.0f | %s | %s | %s   giorni>=35: %2.0f | %s | %s | %s | %s\n",
              r$citta, r$n_tmin20_2026, fmt(r$n_tmin20_2003, 0), fmt(r$n_tmin20_2022, 0), fmt(r$n_tmin20_m9120, 1), fmt(r$n_tmin20_m6190, 1), r$n_tmin20_posto_2026,
              r$n_tmin25_2026, fmt(r$n_tmin25_2003, 0), fmt(r$n_tmin25_2022, 0), fmt(r$n_tmin25_m9120, 1),
              r$n_tmax35_2026, fmt(r$n_tmax35_2003, 0), fmt(r$n_tmax35_2022, 0), fmt(r$n_tmax35_m9120, 1), fmt(r$n_tmax35_m6190, 1)))
}

# ---- Regioni: notti tropicali per abitante ---------------------------------
reg <- sg |> filter(completo) |>
  group_by(regione, anno) |>
  summarise(n_tmin20 = weighted.mean(n_tmin20, pop), n_tmax35 = weighted.mean(n_tmax35, pop), .groups = "drop") |>
  group_by(regione) |>
  summarise(notti_2026 = n_tmin20[anno == ANNO][1], notti_2003 = n_tmin20[anno == 2003][1], notti_2022 = n_tmin20[anno == 2022][1],
            notti_m9120 = mean(n_tmin20[anno %in% BASE2]), notti_m6190 = mean(n_tmin20[anno %in% BASE1]),
            caldi35_2026 = n_tmax35[anno == ANNO][1], caldi35_m9120 = mean(n_tmax35[anno %in% BASE2]), .groups = "drop") |>
  arrange(desc(notti_2026))
write_csv(reg, "output/notti_tropicali_regioni_estate.csv")
sep("Regioni: notti tropicali per abitante nell'estate, 2026 | 2003 | 2022 | 91-20 | 61-90  (giorni >= 35: 2026 | 91-20)")
for (i in seq_len(nrow(reg))) {
  r <- reg[i, ]
  cat(sprintf("%-22s %s | %s | %s | %s | %s   (%s | %s)\n", r$regione, fmt(r$notti_2026, 1), fmt(r$notti_2003, 1), fmt(r$notti_2022, 1),
              fmt(r$notti_m9120, 1), fmt(r$notti_m6190, 1), fmt(r$caldi35_2026, 1), fmt(r$caldi35_m9120, 1)))
}

cat(sprintf("\nFatto in %s minuti. CSV salvati in output/.\n", fmt(as.numeric(difftime(Sys.time(), t_inizio, units = "mins")), 1)))
