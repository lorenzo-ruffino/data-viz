# Esporta in "grafici interattivi/" i valori per cella dell'ESTATE 2026
# (giugno+luglio+agosto) per le versioni interattive delle mappe (es. Flourish):
#   - dati_italia_estate2026.csv  (id, lon, lat, regione, t_2026, media_1991_2020, anomalia)
#   - dati_europa_estate2026.csv  (id, lon, lat, paese,   t_2026, media_1991_2020, anomalia)
# Gli id sono gli stessi delle griglie geojson gia' esportate da 14
# (griglia_italia_025.geojson, griglia_europa_025.geojson: "it_775_4525" =
# cella 0,25° centrata su 7,75E 45,25N). I geojson NON vengono rigenerati se
# gia' presenti; se mancano vengono ricreati con la stessa costruzione di 14.
#
# Calcolo della media JJA identico a 23 (Italia) e 25/26 (Europa).
#
# EUROPA, DOPPIA SORGENTE (come 25/26): per ogni anno-mese della baseline e
# degli anni di confronto si usa il file giornaliero
# input/nc/europa/era5land_t2m_mean_AAAA-MM.nc se esiste e copre il mese intero,
# altrimenti la media mensile ERA5-Land da
# input/nc/europa_mensili/era5land_t2m_monthly_mMM_1991-2025.nc (01b). Il 2026
# viene sempre dai giornalieri; finche' agosto 2026 e' parziale e la baseline
# di agosto e' mensile, si confronta col mese intero (avviso a video).

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
library(tidyverse)
library(sf)
library(ncdf4)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")
dir.create("grafici interattivi", showWarnings = FALSE)

PASSO <- 0.25

quadrato <- function(x, y, mezzo = PASSO / 2) {
  st_polygon(list(rbind(c(x - mezzo, y - mezzo), c(x - mezzo, y + mezzo),
                        c(x + mezzo, y + mezzo), c(x + mezzo, y - mezzo),
                        c(x - mezzo, y - mezzo))))
}

griglia_blocchi <- function(bb) {
  expand_grid(
    bx = seq(floor(bb["xmin"] / PASSO) * PASSO, bb["xmax"] + PASSO, by = PASSO),
    by = seq(floor(bb["ymin"] / PASSO) * PASSO, bb["ymax"] + PASSO, by = PASSO))
}

riempi_vicini <- function(dominio, valori) {
  buchi <- which(is.na(dominio$anomalia))
  for (i in buchi) {
    vic <- valori |>
      filter(abs(bx - dominio$bx[i]) <= PASSO + 1e-6,
             abs(by - dominio$by[i]) <= PASSO + 1e-6)
    dominio$anomalia[i] <- mean(vic$anomalia, na.rm = TRUE)
    dominio$t_2026[i]   <- mean(vic$t_2026, na.rm = TRUE)
    dominio$baseline[i] <- mean(vic$baseline, na.rm = TRUE)
  }
  dominio |> filter(!is.na(anomalia))
}

scrivi_geojson <- function(obj, path) {
  if (file.exists(path)) {
    cat("Gia' presente, non rigenerato:", path, "\n")
    return(invisible(NULL))
  }
  st_write(obj, path, driver = "GeoJSON",
           layer_options = "COORDINATE_PRECISION=4", quiet = TRUE)
  cat("Creato:", path, "\n")
}

# ---- ITALIA -----------------------------------------------------------------

message("Italia...")
MESI_JJA <- c(6L, 7L, 8L)
GIORNI_MESE <- c(`6` = 30, `7` = 31, `8` = 31)

griglia <- read_csv("output/griglia_mensile.csv.gz", show_col_types = FALSE)
celle   <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)
serie   <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE)
gg_ago <- as.integer(format(serie$data[format(serie$data, "%Y-%m") == "2026-08"], "%d"))
fin_ago_it <- if (length(gg_ago)) max(gg_ago) else 0L
giorni_2026 <- c(`6` = 30, `7` = 31, `8` = fin_ago_it)

mensile <- griglia |>
  filter(stat == "mean", finestra == "mese_intero", mese %in% MESI_JJA)
copertura <- mensile |> filter(anno %in% 1991:2020) |> distinct(mese, anno) |> count(mese)
mesi_it <- copertura |> filter(n >= 25) |> pull(mese)
mesi_it <- intersect(mesi_it, unique(mensile$mese[mensile$anno == 2026]))
mesi_it <- mesi_it[giorni_2026[as.character(mesi_it)] > 0]
stopifnot(length(mesi_it) > 0)
if (!setequal(mesi_it, MESI_JJA))
  message("ATTENZIONE Italia: baseline incompleta, mesi usati ", paste(mesi_it, collapse = ","),
          " -> export PROVVISORIO")

dati_celle <- mensile |>
  filter(mese %in% mesi_it) |>
  group_by(ilon, ilat, lon, lat, mese) |>
  summarise(base_m = mean(valore[anno %in% 1991:2020]),
            n_base = sum(anno %in% 1991:2020),
            t26_m  = mean(valore[anno == 2026]), .groups = "drop") |>
  filter(!is.na(t26_m), n_base >= 25) |>
  mutate(w_b = GIORNI_MESE[as.character(mese)], w_26 = giorni_2026[as.character(mese)]) |>
  group_by(ilon, ilat, lon, lat) |>
  filter(n() == length(mesi_it)) |>
  summarise(baseline = sum(base_m * w_b) / sum(w_b),
            t_2026   = sum(t26_m * w_26) / sum(w_26), .groups = "drop") |>
  left_join(celle |> select(ilon, ilat, area_kmq, regione), by = c("ilon", "ilat"))

blocchi_it <- dati_celle |>
  mutate(bx = round(lon / PASSO) * PASSO,
         by = round(lat / PASSO) * PASSO) |>
  group_by(bx, by) |>
  summarise(t_2026   = weighted.mean(t_2026, area_kmq),
            baseline = weighted.mean(baseline, area_kmq),
            regione  = regione[which.max(area_kmq)], .groups = "drop") |>
  mutate(anomalia = t_2026 - baseline)

regioni <- read_sf("input/geo/Reg01012025_g_WGS84.json") |> st_make_valid()
italia  <- st_union(regioni)

bb_it <- st_bbox(italia)
tutti_it <- griglia_blocchi(bb_it)
tutti_it_sf <- st_sf(tutti_it,
                     geometry = st_sfc(map2(tutti_it$bx, tutti_it$by, quadrato),
                                       crs = 4326))
suppressWarnings(dominio_it <- st_intersection(tutti_it_sf, italia))
dominio_it <- dominio_it[as.numeric(st_area(dominio_it)) > 0, ] |>
  left_join(blocchi_it, by = c("bx", "by"))

dominio_it <- riempi_vicini(dominio_it, blocchi_it)
manca_reg <- which(is.na(dominio_it$regione))
if (length(manca_reg) > 0) {
  idx <- st_nearest_feature(st_centroid(st_geometry(dominio_it[manca_reg, ])),
                            st_transform(regioni, 4326))
  dominio_it$regione[manca_reg] <- regioni$DEN_REG[idx]
}

dominio_it <- dominio_it |>
  mutate(id = sprintf("it_%d_%d", round(bx * 100), round(by * 100)),
         across(c(t_2026, baseline, anomalia), ~ round(.x, 2)))

scrivi_geojson(dominio_it |> select(id, geometry),
               "grafici interattivi/griglia_italia_025.geojson")
dominio_it |>
  st_drop_geometry() |>
  select(id, lon = bx, lat = by, regione, t_2026,
         media_1991_2020 = baseline, anomalia) |>
  write_csv("grafici interattivi/dati_italia_estate2026.csv")

cat("Italia:", nrow(dominio_it), "celle\n")

# ---- EUROPA -----------------------------------------------------------------

message("Europa...")
MESI_JJA <- c("06", "07", "08")
NOME_MESE <- c(`06` = "giugno", `07` = "luglio", `08` = "agosto")
# ---- Doppia sorgente: medie giornaliere + medie mensili ----------------------
# Per ogni anno-mese (1991-2026, giugno/luglio/agosto):
#   1. input/nc/europa/era5land_t2m_mean_AAAA-MM.nc (medie giornaliere CDS) se
#      il file c'e' e copre il mese intero -> media dei giorni;
#   2. altrimenti input/nc/europa_mensili/era5land_t2m_monthly_mMM_1991-2025.nc
#      (medie mensili ERA5-Land, tutti gli anni in un file).
# Il 2026 viene SEMPRE dai giornalieri. Stessa griglia 0,1°, t2m in Kelvin.

DIR_GG <- "input/nc/europa"
DIR_MM <- "input/nc/europa_mensili"
GIORNI_MESE <- c(`06` = 30L, `07` = 31L, `08` = 31L)
ANNI_BASE <- 1991:2020
ANNI_CONFRONTO <- 1991:2026

leggi_tempo <- function(nc) {
  tm <- as.vector(ncvar_get(nc, "valid_time"))
  un <- ncatt_get(nc, "valid_time", "units")$value
  unita <- sub(" since.*", "", un)
  origine <- as.POSIXct(sub(".*since ", "", un), tz = "UTC")
  mult <- c(seconds = 1, hours = 3600, days = 86400)[[unita]]
  as.Date(origine + tm * mult)
}
leggi_nc <- function(path) {
  nc <- nc_open(path)
  on.exit(nc_close(nc))
  list(lon = as.numeric(ncvar_get(nc, "longitude")),
       lat = as.numeric(ncvar_get(nc, "latitude")),
       date = leggi_tempo(nc), t2m = ncvar_get(nc, "t2m"))
}
date_nc <- function(path) {          # solo l'asse tempo, senza leggere t2m
  nc <- nc_open(path)
  on.exit(nc_close(nc))
  leggi_tempo(nc)
}

# inventario dei giornalieri
files <- list.files(DIR_GG, pattern = "^era5land_t2m_mean_\\d{4}-0[678]\\.nc$",
                    full.names = TRUE)
stopifnot(length(files) > 0)
info <- tibble(file = files,
               anno = as.integer(str_match(basename(files), "_(\\d{4})-")[, 2]),
               mese = str_match(basename(files), "-(\\d{2})\\.nc$")[, 2]) |>
  mutate(giorni = map(file, ~ as.integer(format(date_nc(.x), "%d"))),
         n_giorni = map_int(giorni, length),
         intero = n_giorni == GIORNI_MESE[mese])
cat("File giornalieri trovati:", nrow(info), "\n")

# finestra di giorni disponibile per agosto 2026
fin_ago <- 31L
r26 <- info |> filter(anno == 2026, mese == "08")
if (nrow(r26) == 1) fin_ago <- max(r26$giorni[[1]])
cat("Finestra agosto 2026: 1-", fin_ago, "\n", sep = "")

# medie mensili: per mese, anni contenuti e array t2m[lon, lat, anno]
mensili <- list()
for (mm in MESI_JJA) {
  f <- file.path(DIR_MM, sprintf("era5land_t2m_monthly_m%s_1991-2025.nc", mm))
  if (!file.exists(f)) next
  nc <- nc_open(f)
  mensili[[mm]] <- list(anni = as.integer(format(leggi_tempo(nc), "%Y")),
                        lon = as.numeric(ncvar_get(nc, "longitude")),
                        lat = as.numeric(ncvar_get(nc, "latitude")),
                        t2m = ncvar_get(nc, "t2m"))
  nc_close(nc)
}
cat("File mensili trovati:", length(mensili), "(mesi", paste(names(mensili), collapse = ","), ")\n")

# piano: fonte per ogni anno-mese
piano <- expand_grid(anno = ANNI_CONFRONTO, mese = MESI_JJA) |>
  left_join(info |> select(anno, mese, file, n_giorni, intero), by = c("anno", "mese")) |>
  mutate(ha_mensile = map2_lgl(anno, mese, ~ !is.null(mensili[[.y]]) && .x %in% mensili[[.y]]$anni),
         fonte = case_when(
           anno == 2026 & !is.na(file)      ~ "giornaliero",
           anno == 2026                      ~ NA_character_,
           !is.na(file) & coalesce(intero, FALSE) ~ "giornaliero",
           ha_mensile                        ~ "mensile",
           TRUE                              ~ NA_character_))

# finestra omogenea per agosto: solo se TUTTA la baseline di agosto e' giornaliera
base_ago <- piano |> filter(mese == "08", anno %in% ANNI_BASE, !is.na(fonte))
finestra_omogenea <- fin_ago < 31 && nrow(base_ago) > 0 && all(base_ago$fonte == "giornaliero")
if (fin_ago < 31 && !finestra_omogenea && any(base_ago$fonte == "mensile")) {
  message(sprintf(paste0(
    "AVVISO: agosto 2026 e' parziale (1-%d) ma la baseline di agosto viene dalle medie\n",
    "  mensili (mese intero) per %d anni su %d: il confronto usa il mese intero della\n",
    "  baseline. L'avviso sparisce quando arriva il 31 agosto 2026."),
    fin_ago, sum(base_ago$fonte == "mensile"), nrow(base_ago)))
}

# media per cella (vettore lon x lat, lon piu' veloce) e giorni pesati, per anno-mese
medie <- list(); ngiorni <- list(); lon <- NULL; lat <- NULL
for (i in seq_len(nrow(piano))) {
  if (is.na(piano$fonte[i])) next
  k  <- paste(piano$anno[i], piano$mese[i])
  mm <- piano$mese[i]
  if (piano$fonte[i] == "giornaliero") {
    d <- leggi_nc(piano$file[i])
    gg <- as.integer(format(d$date, "%d"))
    sel <- if (mm == "08" && (piano$anno[i] == 2026 || finestra_omogenea)) gg <= fin_ago else
      rep(TRUE, length(gg))
    m <- matrix(d$t2m[, , sel, drop = FALSE], ncol = sum(sel))
    medie[[k]]   <- rowMeans(m) - 273.15
    ngiorni[[k]] <- sum(sel)
    if (is.null(lon)) { lon <- d$lon; lat <- d$lat }
    rm(d, m)
  } else {
    j <- match(piano$anno[i], mensili[[mm]]$anni)
    medie[[k]]   <- as.vector(mensili[[mm]]$t2m[, , j]) - 273.15
    ngiorni[[k]] <- GIORNI_MESE[[mm]]
  }
}
invisible(gc())
for (mm in names(mensili)) stopifnot(isTRUE(all.equal(mensili[[mm]]$lon, lon)),
                                     isTRUE(all.equal(mensili[[mm]]$lat, lat)))

# riepilogo delle fonti per mese
cat("\nFonti per mese (anni dai giornalieri / dai mensili / mancanti):\n")
for (mm in MESI_JJA) {
  b <- piano |> filter(mese == mm, anno %in% ANNI_BASE)
  c <- piano |> filter(mese == mm, anno %in% 2021:2025)
  f26 <- piano$fonte[piano$anno == 2026 & piano$mese == mm]
  cat(sprintf("  %-7s baseline 1991-2020: %2d giornalieri, %2d mensili, %2d mancanti | 2021-2025: %d giornalieri, %d mensili | 2026: %s\n",
              NOME_MESE[mm],
              sum(b$fonte == "giornaliero", na.rm = TRUE), sum(b$fonte == "mensile", na.rm = TRUE),
              sum(is.na(b$fonte)),
              sum(c$fonte == "giornaliero", na.rm = TRUE), sum(c$fonte == "mensile", na.rm = TRUE),
              if (is.na(f26)) "ASSENTE" else sprintf("giornaliero (%d giorni)", ngiorni[[paste(2026, mm)]])))
}

# verifica di coerenza giornalieri vs mensili (giugno, un anno a caso, alcune celle)
chk <- piano |> filter(mese == "06", fonte == "giornaliero", ha_mensile, anno != 2026)
if (nrow(chk) > 0) {
  a_chk <- sample(chk$anno, 1)
  j <- match(a_chk, mensili[["06"]]$anni)
  v_gg <- medie[[paste(a_chk, "06")]]
  v_mm <- as.vector(mensili[["06"]]$t2m[, , j]) - 273.15
  dif <- v_gg - v_mm
  idx <- sample(which(!is.na(dif)), 6)
  griglia_ll <- expand_grid(lat = lat, lon = lon)
  cat(sprintf("\nVerifica coerenza giugno %d, giornalieri vs mensile (celle a caso):\n", a_chk))
  print(tibble(lon = griglia_ll$lon[idx], lat = griglia_ll$lat[idx],
               giornalieri = round(v_gg[idx], 3), mensile = round(v_mm[idx], 3),
               differenza = round(dif[idx], 3)))
  q99 <- quantile(abs(dif), 0.99, na.rm = TRUE)
  ok_chk <- max(abs(dif[idx])) < 0.05 && q99 < 0.05
  cat(sprintf("  |differenza| celle campionate max %.3f°C | tutte le celle: mediana %.3f, 99° pct %.3f, max %.3f°C -> %s\n",
              max(abs(dif[idx])), median(abs(dif), na.rm = TRUE), q99,
              max(abs(dif), na.rm = TRUE), if (ok_chk) "OK (sotto 0,05)" else "ERRORE"))
  if (!ok_chk)
    stop("Giornalieri e mensili non coincidono (differenza >= 0,05°C): controllare le sorgenti")
  rm(v_gg, v_mm, dif, griglia_ll)
} else {
  message("Verifica coerenza giornalieri/mensili non possibile: nessun anno con entrambe le fonti a giugno")
}

# mesi utilizzabili: almeno 25 anni di baseline (da qualsiasi fonte) e il 2026 presente
n_base <- piano |> filter(anno %in% ANNI_BASE, !is.na(fonte)) |> count(mese)
mesi_ok <- n_base |> filter(n >= 25) |> pull(mese)
mesi_ok <- intersect(mesi_ok, piano$mese[piano$anno == 2026 & !is.na(piano$fonte)])
stopifnot(length(mesi_ok) > 0)
completo <- setequal(mesi_ok, MESI_JJA)
if (!completo) {
  message("ATTENZIONE: baseline incompleta, mesi usati: ",
          paste(NOME_MESE[mesi_ok], collapse = ", "),
          " -> risultato PROVVISORIO, rilanciare a dati completi")
}

# media JJA per anno = media dei mesi pesata per i giorni (30/31/31; agosto 2026
# per i giorni disponibili); solo gli anni con tutti i mesi usati
jja_anno <- function(a) {
  chiavi <- paste(a, mesi_ok)
  if (!all(chiavi %in% names(medie))) return(NULL)
  n <- unlist(ngiorni[chiavi])
  Reduce(`+`, Map(`*`, medie[chiavi], n)) / sum(n)
}
jja <- setNames(lapply(ANNI_CONFRONTO, jja_anno), ANNI_CONFRONTO)
jja <- jja[!vapply(jja, is.null, logical(1))]
anni_base <- intersect(ANNI_BASE, as.integer(names(jja)))
cat("\nAnni con tutti i mesi usati:", length(jja), "| baseline 1991-2020:",
    length(anni_base), "anni\n")
stopifnot("2026" %in% names(jja), length(anni_base) >= 25)

celle_eu <- expand_grid(lat = lat, lon = lon)
celle_eu$baseline <- Reduce(`+`, jja[as.character(anni_base)]) / length(anni_base)
celle_eu$t_2026   <- jja[["2026"]]
celle_eu <- celle_eu |> filter(!is.na(baseline), !is.na(t_2026))

blocchi_eu <- celle_eu |>
  mutate(bx = round(lon / PASSO) * PASSO,
         by = round(lat / PASSO) * PASSO,
         peso = cos(lat * pi / 180)) |>
  group_by(bx, by) |>
  summarise(t_2026   = weighted.mean(t_2026, peso),
            baseline = weighted.mean(baseline, peso), .groups = "drop") |>
  mutate(anomalia = t_2026 - baseline)

geo <- load_geo_europa()
europa_3035 <- st_union(geo)

bb_eu <- st_bbox(st_transform(geo, 4326))
tutti_eu <- griglia_blocchi(bb_eu)
tutti_eu_sf <- st_sf(tutti_eu,
                     geometry = st_sfc(map2(tutti_eu$bx, tutti_eu$by, quadrato),
                                       crs = 4326)) |>
  st_transform(3035)
suppressWarnings(dominio_eu <- st_intersection(tutti_eu_sf, europa_3035))
dominio_eu <- dominio_eu[as.numeric(st_area(dominio_eu)) > 0, ] |>
  left_join(blocchi_eu, by = c("bx", "by"))
dominio_eu <- riempi_vicini(dominio_eu, blocchi_eu)

nomi_paesi <- c(
  AL = "Albania", AT = "Austria", BA = "Bosnia ed Erzegovina", BE = "Belgio",
  BG = "Bulgaria", CH = "Svizzera", CY = "Cipro", CZ = "Cechia",
  DE = "Germania", DK = "Danimarca", EE = "Estonia", EL = "Grecia",
  ES = "Spagna", FI = "Finlandia", FR = "Francia", HR = "Croazia",
  HU = "Ungheria", IE = "Irlanda", IS = "Islanda", IT = "Italia",
  LI = "Liechtenstein", LT = "Lituania", LU = "Lussemburgo", LV = "Lettonia",
  ME = "Montenegro", MK = "Macedonia del Nord", MT = "Malta",
  NL = "Paesi Bassi", NO = "Norvegia", PL = "Polonia", PT = "Portogallo",
  RO = "Romania", RS = "Serbia", SE = "Svezia", SI = "Slovenia",
  SK = "Slovacchia", UK = "Regno Unito", XK = "Kosovo"
)
idx <- st_nearest_feature(st_centroid(st_geometry(dominio_eu)), geo)
dominio_eu$paese <- nomi_paesi[geo$CNTR_ID[idx]]

dominio_eu <- dominio_eu |>
  st_transform(4326) |>
  mutate(id = sprintf("eu_%d_%d", round(bx * 100), round(by * 100)),
         across(c(t_2026, baseline, anomalia), ~ round(.x, 2)))

scrivi_geojson(dominio_eu |> select(id, geometry),
               "grafici interattivi/griglia_europa_025.geojson")
dominio_eu |>
  st_drop_geometry() |>
  select(id, lon = bx, lat = by, paese, t_2026,
         media_1991_2020 = baseline, anomalia) |>
  write_csv("grafici interattivi/dati_europa_estate2026.csv")

cat("Europa:", nrow(dominio_eu), "celle\n")

# controllo: gli id del csv devono coincidere con quelli del geojson esistente
for (z in c("italia", "europa")) {
  gj <- sprintf("grafici interattivi/griglia_%s_025.geojson", z)
  cs <- sprintf("grafici interattivi/dati_%s_estate2026.csv", z)
  if (file.exists(gj)) {
    id_gj <- read_sf(gj, quiet = TRUE)$id
    id_cs <- read_csv(cs, show_col_types = FALSE)$id
    cat(sprintf("%s: %d id nel geojson, %d nel csv, %d in comune, %d solo csv, %d solo geojson\n",
                z, length(id_gj), length(id_cs), length(intersect(id_gj, id_cs)),
                length(setdiff(id_cs, id_gj)), length(setdiff(id_gj, id_cs))))
  }
}
cat("Fatto. Tutto in 'grafici interattivi/'\n")
