# Temperature giornaliere (ERA5-Land, 0,1°) per provincia e per comune, pesate
# per la popolazione residente 2021.
#
# Pesi: ogni cella della griglia Istat di popolazione 1 km (censimento 2021) è
# assegnata, tramite il suo centroide, sia alla cella ERA5-Land che lo contiene
# (stessa regola di Temperature Copernicus/script/03_prepara_griglia.R) sia al
# comune in cui cade (confini Istat 1/1/2026). Una cella ERA5 a cavallo di due
# comuni o province contribuisce a entrambi in proporzione alla popolazione.
#   - centroidi fuori da ogni poligono comunale (costa): comune più vicino;
#   - centroidi in celle ERA5 senza dati (mare per la maschera ERA5-Land):
#     cella valida più vicina;
#   - comuni senza popolazione nella griglia: cella del punto interno al comune.
# I codici comunali sono riportati alla classificazione dei dati sui decessi
# (31/3/2026): Lirio (018082) incorporato in Montalto Pavese (018094) dal
# 31/1/2026; Castegnero (024027) e Nanto (024071) fusi in Castegnero Nanto (024129).
#
# Temperature: dagli orari t2m, per ogni cella minima/media/massima del giorno
# locale (UTC+1, giorni con almeno 23 ore, come in 04_elabora_dati.R), poi
# media pesata delle celle. tmin e tmax sono quindi medie pesate delle minime e
# massime di cella.
#
# Output (input/derivati/):
#   - pesi_celle_comune.parquet     cod_comune x cella (ilon, ilat = lon/lat*10) -> pop, w
#   - pesi_celle_provincia.parquet  cod_prov x cella -> pop, w
#   - temperatura_provincia.parquet cod_prov x data -> tmean, tmin, tmax (tutti i giorni)
#   - temperatura_comune.parquet    cod_comune x data -> tmean, tmin, tmax (tutti i giorni)
# I pesi sono una cache: per ricalcolarli cancellare i due file pesi_*.

library(data.table)
library(arrow)
library(sf)
library(ncdf4)
library(Matrix)

RADICE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz"
setwd(file.path(RADICE, "Mortalità caldo 2026"))

DIR_NC     <- file.path(RADICE, "Temperature Copernicus/input/nc/italia_orari")
CELLE_CSV  <- file.path(RADICE, "Temperature Copernicus/input/geo/celle_griglia.csv")
SERIE_IT   <- file.path(RADICE, "Temperature Copernicus/output/serie_giornaliera_italia.csv")
POP_GRID   <- file.path(RADICE, "Densità Popolazione Italia/GrigliaPop2021_Ind_ITA.json")
COMUNI_GEO <- file.path(RADICE, "Referendum_Giustizia_2026/utilities/Com01012026_g_WGS84.json")

F_PESI_COM  <- "input/derivati/pesi_celle_comune.parquet"
F_PESI_PROV <- "input/derivati/pesi_celle_provincia.parquet"

comuni <- fread("input/derivati/comuni.csv", encoding = "Latin-1",
                colClasses = list(character = "cod_comune"))

# variazioni tra 1/1/2026 (confini) e 31/3/2026 (decessi)
variazioni <- data.table(da = c("018082", "024027", "024071"),
                         a  = c("018094", "024129", "024129"))

files_nc <- sort(list.files(DIR_NC, pattern = "^era5land_t2m_orario_\\d{4}-\\d{2}\\.nc$",
                            full.names = TRUE))

leggi_tempo <- function(nc) {
  tm <- as.vector(ncvar_get(nc, "valid_time"))
  un <- ncatt_get(nc, "valid_time", "units")$value
  stopifnot(grepl("seconds since 1970-01-01", un))
  as.POSIXct(tm, origin = "1970-01-01", tz = "UTC")
}

# ---- 1) Pesi -----------------------------------------------------------------

if (!file.exists(F_PESI_COM) || !file.exists(F_PESI_PROV)) {

  # celle ERA5-Land con dati (la maschera terra/mare è fissa)
  nc <- nc_open(files_nc[length(files_nc)])
  lon <- as.integer(round(as.numeric(ncvar_get(nc, "longitude")) * 10))
  lat <- as.integer(round(as.numeric(ncvar_get(nc, "latitude")) * 10))
  x1 <- ncvar_get(nc, "t2m", start = c(1, 1, 1), count = c(-1, -1, 1))
  nc_close(nc)
  griglia <- data.table(ilon = rep(lon, times = length(lat)),
                        ilat = rep(lat, each = length(lon)),
                        valida = !is.na(as.vector(x1)))
  valide <- griglia[valida == TRUE]

  cella_valida_vicina <- function(ilon, ilat) {
    vapply(seq_along(ilon), function(i) {
      d <- ((valide$ilon - ilon[i]) * cos(ilat[i] / 10 * pi / 180))^2 +
           (valide$ilat - ilat[i])^2
      which.min(d)
    }, integer(1))
  }

  message("Lettura griglia popolazione 1 km (1-2 minuti)...")
  pop_grid <- read_sf(POP_GRID)
  pop_grid <- pop_grid[pop_grid$T > 0, "T"]
  cen <- suppressWarnings(st_centroid(st_geometry(pop_grid)))
  xy <- st_coordinates(cen)
  punti <- data.table(id = seq_len(nrow(xy)), pop = pop_grid$T,
                      ilon = as.integer(round(xy[, 1] * 10)),
                      ilat = as.integer(round(xy[, 2] * 10)))
  rm(pop_grid); gc()

  message("Assegnazione dei punti ai comuni...")
  com_geo <- read_sf(COMUNI_GEO) |>
    st_transform(32632) |>
    st_make_valid()
  pt <- st_transform(cen, 32632)
  dentro <- st_intersects(pt, com_geo)
  idx_com <- vapply(dentro, function(z) if (length(z)) z[1] else NA_integer_, integer(1))
  fuori <- which(is.na(idx_com))
  idx_com[fuori] <- st_nearest_feature(pt[fuori], com_geo)
  punti[, cod_comune := com_geo$PRO_COM_T[idx_com]]
  cat("Punti con popolazione:", nrow(punti), "| fuori dai confini comunali:",
      length(fuori), "(pop", sum(punti$pop[fuori]), ") -> comune più vicino\n")

  # celle senza dati ERA5 -> cella valida più vicina
  punti[, cella_ok := paste(ilon, ilat) %in% paste(valide$ilon, valide$ilat)]
  cat("Popolazione in celle ERA5 senza dati:", sum(punti[cella_ok == FALSE, pop]),
      "su", sum(punti$pop), "-> spostata sulla cella valida più vicina\n")
  da_spostare <- unique(punti[cella_ok == FALSE, .(ilon, ilat)])
  j <- cella_valida_vicina(da_spostare$ilon, da_spostare$ilat)
  da_spostare[, `:=`(nlon = valide$ilon[j], nlat = valide$ilat[j])]
  punti[da_spostare, on = .(ilon, ilat), `:=`(ilon = i.nlon, ilat = i.nlat)]

  # codici alla classificazione dei decessi
  punti[variazioni, on = .(cod_comune = da), cod_comune := i.a]
  stopifnot(all(punti$cod_comune %in% comuni$cod_comune))

  pesi_com <- punti[, .(pop = sum(pop)), by = .(cod_comune, ilon, ilat)]
  pesi_com[, fonte := "popolazione"]

  # comuni senza popolazione nella griglia: cella del punto interno
  senza <- setdiff(comuni$cod_comune, pesi_com$cod_comune)
  if (length(senza)) {
    com_geo$cod_dec <- com_geo$PRO_COM_T
    m <- match(com_geo$cod_dec, variazioni$da)
    com_geo$cod_dec[!is.na(m)] <- variazioni$a[m[!is.na(m)]]
    g <- com_geo[com_geo$cod_dec %in% senza, ]
    pin <- st_coordinates(st_transform(st_point_on_surface(st_geometry(g)), 4326))
    agg <- data.table(cod_comune = g$cod_dec,
                      ilon = as.integer(round(pin[, 1] * 10)),
                      ilat = as.integer(round(pin[, 2] * 10)))
    agg <- unique(agg, by = "cod_comune")
    j <- cella_valida_vicina(agg$ilon, agg$ilat)
    agg[, `:=`(ilon = valide$ilon[j], ilat = valide$ilat[j], pop = 0, fonte = "centroide")]
    pesi_com <- rbind(pesi_com, agg)
    cat("Comuni senza popolazione nella griglia (cella del centroide):",
        paste(comuni[cod_comune %in% senza, paste0(comune, " (", cod_comune, ")")],
              collapse = ", "), "\n")
  }
  stopifnot(setequal(pesi_com$cod_comune, comuni$cod_comune))
  pesi_com[, w := if (sum(pop) > 0) pop / sum(pop) else 1 / .N, by = cod_comune]
  setorder(pesi_com, cod_comune, ilon, ilat)

  pesi_prov <- pesi_com[pop > 0][comuni[, .(cod_comune, cod_prov)], on = "cod_comune",
                                  nomatch = NULL][, .(pop = sum(pop)), by = .(cod_prov, ilon, ilat)]
  pesi_prov[, w := pop / sum(pop), by = cod_prov]
  setorder(pesi_prov, cod_prov, ilon, ilat)

  write_parquet(pesi_com, F_PESI_COM)
  write_parquet(pesi_prov, F_PESI_PROV)
  rm(punti, com_geo, cen, pt); gc()
}

pesi_com  <- as.data.table(read_parquet(F_PESI_COM))
pesi_prov <- as.data.table(read_parquet(F_PESI_PROV))
stopifnot(uniqueN(pesi_prov$cod_prov) == uniqueN(comuni$cod_prov))

# ---- 2) Matrici dei pesi (unità x cella) -------------------------------------

celle_usate <- unique(rbind(pesi_com[, .(ilon, ilat)], pesi_prov[, .(ilon, ilat)]))
setorder(celle_usate, ilon, ilat)
celle_usate[, k := .I]
chiave_usate <- paste(celle_usate$ilon, celle_usate$ilat)

# replica della media Italia di 04_elabora_dati.R (pesi = pop delle celle, celle con dati)
celle_ref <- fread(CELLE_CSV)[pop > 0]
celle_ref <- celle_ref[paste(ilon, ilat) %in% chiave_usate]

matrice_pesi <- function(unita, ilon, ilat, w) {
  u <- sort(unique(unita))
  sparseMatrix(i = match(unita, u), j = match(paste(ilon, ilat), chiave_usate),
               x = w, dims = c(length(u), nrow(celle_usate)),
               dimnames = list(as.character(u), NULL))
}
W_com  <- matrice_pesi(pesi_com$cod_comune, pesi_com$ilon, pesi_com$ilat, pesi_com$w)
W_prov <- matrice_pesi(pesi_prov$cod_prov, pesi_prov$ilon, pesi_prov$ilat, pesi_prov$w)
W_ita  <- matrice_pesi(rep("ref", nrow(celle_ref)), celle_ref$ilon, celle_ref$ilat,
                       celle_ref$pop / sum(celle_ref$pop))

# ---- 3) Temperature ----------------------------------------------------------

# matrice [cella usata x ora] in °C
leggi_orari <- function(path) {
  nc <- nc_open(path)
  on.exit(nc_close(nc))
  lon <- as.integer(round(as.numeric(ncvar_get(nc, "longitude")) * 10))
  lat <- as.integer(round(as.numeric(ncvar_get(nc, "latitude")) * 10))
  tempo <- leggi_tempo(nc)
  t2m <- ncvar_get(nc, "t2m")
  chiave <- paste(rep(lon, times = length(lat)), rep(lat, each = length(lon)))
  riga <- match(chiave_usate, chiave)
  stopifnot(!anyNA(riga))
  m <- matrix(t2m, nrow = length(lon) * length(lat))[riga, , drop = FALSE] - 273.15
  list(m = m, tempo = tempo)
}

in_lungo <- function(mat, giorni, nome_unita) {
  out <- data.table(unita = rep(rownames(mat), times = ncol(mat)),
                    data = rep(giorni, each = nrow(mat)),
                    valore = as.vector(mat))
  setnames(out, "unita", nome_unita)
  out
}

elabora_anno <- function(paths) {
  dd <- lapply(paths, leggi_orari)
  m <- do.call(cbind, lapply(dd, `[[`, "m"))
  tempo <- do.call(c, lapply(dd, `[[`, "tempo"))
  rm(dd)
  ord <- order(tempo)
  m <- m[, ord, drop = FALSE]
  tempo <- tempo[ord]
  stopifnot(!anyNA(m), !anyDuplicated(tempo))

  data_loc <- as.IDate(tempo + 3600)              # giorno locale UTC+1
  n_ore <- table(data_loc)
  giorni <- as.IDate(names(n_ore)[n_ore >= 23])
  ore_per_giorno <- split(seq_along(data_loc), data_loc)[as.character(giorni)]

  riduci <- function(f) vapply(ore_per_giorno, function(cols)
    do.call(f, asplit(m[, cols, drop = FALSE], 2)), numeric(nrow(m)))
  cella <- list(
    tmean = vapply(ore_per_giorno, function(cols) rowMeans(m[, cols, drop = FALSE]),
                   numeric(nrow(m))),
    tmin  = riduci(pmin),
    tmax  = riduci(pmax)
  )

  pesa <- function(W, nome) {
    tab <- lapply(names(cella), function(s) {
      x <- in_lungo(as.matrix(W %*% cella[[s]]), giorni, nome)
      setnames(x, "valore", s)
      x
    })
    Reduce(function(a, b) a[b, on = c(nome, "data")], tab)
  }
  list(prov = pesa(W_prov, "cod_prov"), com = pesa(W_com, "cod_comune"), ita = pesa(W_ita, "ref"))
}

anni <- substr(basename(files_nc), 21, 24)
ris <- lapply(split(files_nc, anni), function(p) {
  message("Anno ", substr(basename(p[1]), 21, 24), " (", length(p), " mesi)")
  elabora_anno(p)
})

temp_prov <- rbindlist(lapply(ris, `[[`, "prov"))
temp_prov[, cod_prov := as.integer(cod_prov)]
temp_com  <- rbindlist(lapply(ris, `[[`, "com"))
temp_ita_ref <- rbindlist(lapply(ris, `[[`, "ita"))[, ref := NULL]
rm(ris); gc()

for (s in c("tmean", "tmin", "tmax")) {
  set(temp_prov, j = s, value = round(temp_prov[[s]], 3))
  set(temp_com,  j = s, value = round(temp_com[[s]], 2))
}
setorder(temp_prov, cod_prov, data)
setorder(temp_com, cod_comune, data)
write_parquet(temp_prov, "input/derivati/temperatura_provincia.parquet")
write_parquet(temp_com, "input/derivati/temperatura_comune.parquet")

# ---- 4) Controlli ------------------------------------------------------------

cat("\nProvince:", uniqueN(temp_prov$cod_prov), "| comuni:", uniqueN(temp_com$cod_comune),
    "| NA province:", sum(is.na(temp_prov)), "| NA comuni:", sum(is.na(temp_com)), "\n")
cat("Giorni per anno (province) e mesi coperti:\n")
print(temp_prov[cod_prov == 1, .(giorni = .N, dal = min(data), al = max(data)), by = .(anno = year(data))])
stopifnot(temp_prov[, .N, by = data][, all(N == uniqueN(temp_prov$cod_prov))],
          temp_com[, .N, by = data][, all(N == nrow(comuni))])

# media Italia ricostruita dalle province (pesi = popolazione della provincia)
pop_prov <- pesi_prov[, .(pop = sum(pop)), by = cod_prov]
ita_prov <- temp_prov[pop_prov, on = "cod_prov"][
  , .(tmean = sum(tmean * pop) / sum(pop), tmin = sum(tmin * pop) / sum(pop),
      tmax = sum(tmax * pop) / sum(pop)), by = data]

serie <- fread(SERIE_IT)[, data := as.IDate(data)]
confronta <- function(x, etichetta) {
  z <- serie[x, on = "data", nomatch = NULL]
  z[, periodo := fifelse(year(data) <= 2019, "2011-2019", "2020-2026")]
  cat("\n", etichetta, " vs serie_giornaliera_italia (t_pop_*), giorni in comune: ",
      nrow(z), "\n", sep = "")
  print(z[, .(giorni = .N,
              diff_media_mean = mean(tmean - t_pop_mean), max_abs_mean = max(abs(tmean - t_pop_mean)),
              max_abs_min = max(abs(tmin - t_pop_min)), max_abs_max = max(abs(tmax - t_pop_max)),
              cor_mean = cor(tmean, t_pop_mean)), by = periodo])
  print(z[order(-abs(tmean - t_pop_mean))][1:3, .(data, tmean, t_pop_mean)])
}
confronta(temp_ita_ref, "Replica (pesi delle celle come in 04)")
confronta(ita_prov, "Italia dalle province")

cat("\nGiugno 2026 e anomalia rispetto a giugno 2011-2025:\n")
sel <- c(Torino = 1L, Milano = 15L, Roma = 58L, Napoli = 63L, Palermo = 82L)
giu <- temp_prov[month(data) == 6 & cod_prov %in% sel,
                 .(tmean = mean(tmean), tmin = mean(tmin), tmax = mean(tmax)),
                 by = .(cod_prov, anno = year(data))]
clima <- giu[anno <= 2025, .(tmean_rif = mean(tmean), tmin_rif = mean(tmin),
                             tmax_rif = mean(tmax)), by = cod_prov]
g26 <- giu[anno == 2026][clima, on = "cod_prov"]
g26[, provincia := names(sel)[match(cod_prov, sel)]]
print(g26[, .(provincia, tmean = round(tmean, 1), anom_tmean = round(tmean - tmean_rif, 1),
              tmin = round(tmin, 1), anom_tmin = round(tmin - tmin_rif, 1),
              tmax = round(tmax, 1), anom_tmax = round(tmax - tmax_rif, 1))])

cat("\nDimensione file (MB): provincia",
    round(file.size("input/derivati/temperatura_provincia.parquet") / 1e6, 1),
    "| comune", round(file.size("input/derivati/temperatura_comune.parquet") / 1e6, 1), "\n")
