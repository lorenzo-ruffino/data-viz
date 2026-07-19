# Legge i dati ERA5-Land scaricati e produce le serie pronte per analisi e grafici.
#
# Due sorgenti equivalenti, in ordine di preferenza per ogni mese:
#   1. statistiche giornaliere già calcolate dal CDS (input/nc/italia/, 3 file per mese)
#   2. dati orari (input/nc/italia_orari/) da cui min/media/max giornaliere vengono
#      calcolate qui, sui giorni in fuso UTC+1 — stessa definizione del dataset
#      derivato del CDS. (Unica differenza: il primo giorno del mese ha 23 ore
#      invece di 24 perché l'ora di mezzanotte sta nel file del mese precedente;
#      trascurabile e uguale per tutti gli anni.)
#
# Output:
#   - output/serie_giornaliera_italia.csv     media Italia per giorno, pesata per
#                                             superficie e per popolazione 2021
#   - output/serie_giornaliera_regioni.csv.gz media per regione e giorno
#   - output/griglia_mensile.csv.gz           media mensile per cella (per le mappe);
#                                             per giugno anche la versione sulla
#                                             finestra di giorni disponibile nel 2026
#
# Rieseguibile in ogni momento: elabora tutti i file presenti.

library(tidyverse)
library(ncdf4)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

celle <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)

files_daily <- list.files("input/nc/italia", pattern = "^era5land_t2m_.*\\.nc$",
                          full.names = TRUE)
files_orari <- list.files("input/nc/italia_orari", pattern = "^era5land_t2m_orario_.*\\.nc$",
                          full.names = TRUE)

# mesi già coperti dalle statistiche giornaliere CDS (tutte e tre le statistiche)
mesi_daily <- tibble(f = basename(files_daily)) |>
  mutate(ym = str_match(f, "_(\\d{4}-\\d{2})\\.nc$")[, 2]) |>
  filter(!is.na(ym)) |>
  count(ym) |>
  filter(n >= 3) |>
  pull(ym)

orari_da_derivare <- files_orari[
  !str_match(basename(files_orari), "_(\\d{4}-\\d{2})\\.nc$")[, 2] %in% mesi_daily]

leggi_tempo <- function(nc, var = "valid_time") {
  tm <- as.vector(ncvar_get(nc, var))
  un <- ncatt_get(nc, var, "units")$value
  if (grepl("days since", un)) {
    as.Date(sub("days since ", "", un)) + tm
  } else if (grepl("seconds since", un)) {
    as.POSIXct(sub("seconds since ", "", un), tz = "UTC") + tm
  } else {
    as.POSIXct(sub("hours since ", "", un), tz = "UTC") + tm * 3600
  }
}

# matrice [cella x tempo] allineata a `celle`, in °C
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

# Finestra di giorni disponibile per giugno 2026 (per confronti omogenei)
giu26_max_giorno <- 30L
f_giu26 <- "input/nc/italia/era5land_t2m_mean_2026-06.nc"
if (file.exists(f_giu26)) {
  nc <- nc_open(f_giu26); giu26_max_giorno <- max(as.integer(format(leggi_tempo(nc), "%d"))); nc_close(nc)
}
cat("Finestra giugno 2026: giorni 1-", giu26_max_giorno, "\n", sep = "")

# ---- Aggregazione comune (da matrice giornaliera [cella x giorno]) ---------

aggrega <- function(sub, date, stat) {
  valide <- rowSums(is.na(sub)) == 0
  sv  <- sub[valide, , drop = FALSE]
  ck  <- celle[valide, ]
  w_a <- ck$area_kmq
  w_p <- ck$pop

  italia <- tibble(
    data = date, stat = stat,
    t_area = as.vector(colSums(sv * w_a) / sum(w_a)),
    t_pop  = as.vector(colSums(sv * w_p) / sum(w_p))
  )

  num_a <- rowsum(sv * w_a, ck$regione)
  den_a <- tapply(w_a, ck$regione, sum)
  num_p <- rowsum(sv * w_p, ck$regione)
  den_p <- tapply(w_p, ck$regione, sum)
  reg_a <- num_a / as.vector(den_a[rownames(num_a)])
  reg_p <- num_p / as.vector(den_p[rownames(num_p)])
  colnames(reg_a) <- colnames(reg_p) <- as.character(date)
  regioni <- as_tibble(reg_a, rownames = "regione") |>
    pivot_longer(-regione, names_to = "data", values_to = "t_area") |>
    left_join(as_tibble(reg_p, rownames = "regione") |>
                pivot_longer(-regione, names_to = "data", values_to = "t_pop"),
              by = c("regione", "data")) |>
    mutate(data = as.Date(data), stat = stat)

  mesi <- format(date, "%Y-%m")
  griglia <- map_dfr(unique(mesi), function(mm) {
    sel <- mesi == mm
    out <- tibble(ilon = ck$ilon, ilat = ck$ilat,
                  anno = as.integer(substr(mm, 1, 4)),
                  mese = as.integer(substr(mm, 6, 7)),
                  stat = stat, finestra = "mese_intero",
                  valore = rowMeans(sv[, sel, drop = FALSE]))
    if (unique(out$mese) == 6) {
      sel_f <- sel & as.integer(format(date, "%d")) <= giu26_max_giorno
      out <- bind_rows(out, out |>
        mutate(finestra = "finestra_giu2026",
               valore = rowMeans(sv[, sel_f, drop = FALSE])))
    }
    out
  })

  list(italia = italia, regioni = regioni, griglia = griglia)
}

# ---- Percorso 1: statistiche giornaliere CDS --------------------------------

elabora_daily <- function(path) {
  stat <- str_match(basename(path), "era5land_t2m_(mean|min|max)_")[, 2]
  d <- estrai_matrice(path)
  aggrega(d$m, d$tempo, stat)
}

# ---- Percorso 2: statistiche derivate dagli orari ---------------------------
# I file orari dello stesso anno vengono concatenati, così il giorno 1 di un
# mese recupera l'ora di mezzanotte dal file del mese precedente. Si tengono
# solo i giorni con almeno 23 delle 24 ore.

elabora_orario_anno <- function(paths) {
  dd <- map(paths, estrai_matrice)
  m <- do.call(cbind, map(dd, "m"))
  tempo <- do.call(c, map(dd, "tempo"))
  ord <- order(tempo)
  m <- m[, ord, drop = FALSE]
  tempo <- tempo[ord]

  data_loc <- as.Date(tempo + 3600, tz = "UTC")   # giorno locale UTC+1
  conteggio <- table(data_loc)
  giorni <- as.Date(names(conteggio)[conteggio >= 23])

  per_giorno <- function(fun_riduci) {
    vapply(giorni, function(g) {
      cols <- which(data_loc == g)
      fun_riduci(m[, cols, drop = FALSE])
    }, numeric(nrow(m)))
  }
  mat <- list(
    mean = per_giorno(rowMeans),
    min  = per_giorno(function(x) do.call(pmin, asplit(x, 2))),
    max  = per_giorno(function(x) do.call(pmax, asplit(x, 2)))
  )
  ris <- imap(mat, ~ aggrega(.x, giorni, .y))
  list(italia  = map_dfr(ris, "italia"),
       regioni = map_dfr(ris, "regioni"),
       griglia = map_dfr(ris, "griglia"))
}

# ---- Esecuzione --------------------------------------------------------------

gruppi_anno <- split(orari_da_derivare,
                     str_match(basename(orari_da_derivare), "_(\\d{4})-")[, 2])
message("File giornalieri CDS: ", length(files_daily),
        " | mesi da derivare dagli orari: ", length(orari_da_derivare),
        " (", length(gruppi_anno), " anni)")
risultati <- c(map(files_daily, elabora_daily),
               map(gruppi_anno, elabora_orario_anno))

serie_italia <- map_dfr(risultati, "italia") |>
  distinct(data, stat, .keep_all = TRUE) |>
  pivot_wider(names_from = stat, values_from = c(t_area, t_pop)) |>
  arrange(data)

serie_regioni <- map_dfr(risultati, "regioni") |>
  distinct(regione, data, stat, .keep_all = TRUE) |>
  pivot_wider(names_from = stat, values_from = c(t_area, t_pop)) |>
  arrange(regione, data)

griglia_mensile <- map_dfr(risultati, "griglia") |>
  distinct(ilon, ilat, anno, mese, stat, finestra, .keep_all = TRUE) |>
  left_join(celle |> select(ilon, ilat, lon, lat, regione), by = c("ilon", "ilat"))

write_csv(serie_italia, "output/serie_giornaliera_italia.csv")
write_csv(serie_regioni, "output/serie_giornaliera_regioni.csv.gz")
write_csv(griglia_mensile, "output/griglia_mensile.csv.gz")

cat("Giorni in serie Italia:", nrow(serie_italia), "dal", format(min(serie_italia$data)),
    "al", format(max(serie_italia$data)), "\n")
cat("Anni-mese in griglia:", n_distinct(paste(griglia_mensile$anno, griglia_mensile$mese)), "\n")
