# Anomalia dell'ESTATE 2026 (giugno+luglio+agosto) rispetto alle estati
# 1991-2020 per ogni paese europeo. Gemello di 11 per JJA, con in piu' le
# anomalie dei singoli mesi (giugno, luglio, agosto) come colonne aggiuntive.
# Media pesata per l'area della cella (coseno della latitudine), celle
# assegnate al paese del proprio centro.
#
# DOPPIA SORGENTE (il download dei giornalieri di luglio/agosto 1991-2025 dal
# CDS e' troppo lento): per ogni anno-mese della baseline e degli anni di
# confronto si usa
#   1. il file giornaliero input/nc/europa/era5land_t2m_mean_AAAA-MM.nc se
#      esiste e copre il mese intero (media dei giorni), altrimenti
#   2. la media mensile ERA5-Land da
#      input/nc/europa_mensili/era5land_t2m_monthly_mMM_1991-2025.nc
#      (scaricata con 01b_scarica_mensili_europa.py).
# Il 2026 viene SEMPRE dai giornalieri. Le due fonti differiscono di pochi
# centesimi di grado (verifica di coerenza a video su giugno, soglia 0,05°C).
#
# Media JJA per cella e anno = media dei tre mesi pesata per i giorni (30/31/31;
# agosto 2026 per i giorni disponibili). Finche' agosto 2026 e' incompleto, la
# finestra omogenea sulla baseline si applica SOLO se tutta la baseline di
# agosto e' giornaliera; se viene dai mensili (mese intero) si confronta
# l'agosto parziale col mese intero e si stampa un avviso.
# Baseline: almeno 25 anni con tutti e tre i mesi; se non ci sono ancora,
# usa i mesi disponibili (risultato PROVVISORIO, segnalato a video).
#
# Output: output/analisi_europa_paesi_estate.csv + classifica a video.
# Verifica finale: l'anomalia Italia deve coincidere (+-0,1: le due pipeline
# hanno fonti leggermente diverse per agosto) con quella nazionale calcolata
# dalla griglia Italia (output/griglia_mensile.csv.gz).

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
library(tidyverse)
library(ncdf4)
library(sf)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

MESI_JJA <- c("06", "07", "08")
NOME_MESE <- c(`06` = "giugno", `07` = "luglio", `08` = "agosto")
SUFFISSO  <- c(`06` = "giu", `07` = "lug", `08` = "ago")

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

celle <- expand_grid(lat = lat, lon = lon)          # lon varia più veloce
celle$baseline <- Reduce(`+`, jja[as.character(anni_base)]) / length(anni_base)
celle$t2026    <- jja[["2026"]]

# ---- Singoli mesi: anomalia 2026 per cella (dove baseline >= 25 anni) --------

for (m in MESI_JJA) {
  ab <- piano |> filter(mese == m, anno %in% ANNI_BASE, !is.na(fonte)) |> pull(anno)
  col <- paste0("anom_", SUFFISSO[m])
  if (length(ab) >= 25 && paste(2026, m) %in% names(medie)) {
    base_m <- Reduce(`+`, medie[paste(ab, m)]) / length(ab)
    celle[[col]] <- medie[[paste(2026, m)]] - base_m
    cat(sprintf("%s: baseline %d anni (%d giornalieri, %d mensili), 2026 presente\n",
                NOME_MESE[m], length(ab),
                sum(piano$fonte[piano$mese == m & piano$anno %in% ab] == "giornaliero"),
                sum(piano$fonte[piano$mese == m & piano$anno %in% ab] == "mensile")))
  } else {
    celle[[col]] <- NA_real_
    cat(sprintf("%s: baseline %d anni, 2026 %s -> anomalia mensile NA\n",
                NOME_MESE[m], length(ab),
                if (paste(2026, m) %in% names(medie)) "presente" else "assente"))
  }
}

celle <- celle |> filter(!is.na(baseline), !is.na(t2026))

# assegnazione cella -> paese (centro cella dentro il poligono)
geo <- load_geo_europa()
punti <- st_as_sf(celle, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(st_crs(geo))
dentro <- st_within(punti, geo)
idx <- vapply(dentro, function(i) if (length(i)) i[1] else NA_integer_, integer(1))
celle$CNTR_ID <- geo$CNTR_ID[idx]
celle <- celle |> filter(!is.na(CNTR_ID))

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

periodo <- if (completo) sprintf("1 giu-%d ago", fin_ago) else
  paste0(NOME_MESE[mesi_ok], collapse = "+")

paesi <- celle |>
  mutate(peso = cos(lat * pi / 180)) |>
  group_by(CNTR_ID) |>
  summarise(celle = n(),
            media_1991_2020 = weighted.mean(baseline, peso),
            t_2026 = weighted.mean(t2026, peso),
            anomalia_giu = weighted.mean(anom_giu, peso),
            anomalia_lug = weighted.mean(anom_lug, peso),
            anomalia_ago = weighted.mean(anom_ago, peso),
            .groups = "drop") |>
  mutate(anomalia = t_2026 - media_1991_2020,
         paese = nomi_paesi[CNTR_ID],
         periodo = periodo) |>
  select(paese, CNTR_ID, periodo, celle, media_1991_2020, t_2026, anomalia,
         anomalia_giu, anomalia_lug, anomalia_ago) |>
  arrange(desc(anomalia))

write_csv(paesi, "output/analisi_europa_paesi_estate.csv")

fmt <- function(x, d = 2) ifelse(is.na(x), "  n.d.",
                                 formatC(x, format = "f", digits = d, decimal.mark = ","))
cat("\n=== Estate 2026 vs media estati 1991-2020, per paese (°C)",
    if (!completo) "[PROVVISORIO]", "===\n")
cat(sprintf("%-22s %7s  %6s %6s %6s   (2026 | 91-20)\n", "", "JJA", "giu", "lug", "ago"))
for (i in seq_len(nrow(paesi))) {
  r <- paesi[i, ]
  cat(sprintf("%-22s %+6s°C  %6s %6s %6s   (%s | %s)\n",
              r$paese, fmt(r$anomalia), fmt(r$anomalia_giu), fmt(r$anomalia_lug),
              fmt(r$anomalia_ago), fmt(r$t_2026, 1), fmt(r$media_1991_2020, 1)))
}

# ---- Verifica: Italia vs griglia Italia (04) ---------------------------------
# Stessa regola di 23: media dei mesi pesata per i giorni (30/31/31; agosto 2026
# per i giorni disponibili), celle con baseline >= 25 anni, pesi = area della cella.
# Tolleranza +-0,1°C: la griglia Italia viene dagli orari (04), quella Europa
# dai giornalieri/mensili CDS, e per agosto le fonti sono leggermente diverse.

griglia <- read_csv("output/griglia_mensile.csv.gz", show_col_types = FALSE)
celle_it <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)
serie <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE)
gg_ago <- as.integer(format(serie$data[format(serie$data, "%Y-%m") == "2026-08"], "%d"))
fin_ago_it <- if (length(gg_ago)) max(gg_ago) else 0L
giorni_base <- c(`6` = 30, `7` = 31, `8` = 31)
giorni_2026 <- c(`6` = 30, `7` = 31, `8` = fin_ago_it)
mesi_it <- as.integer(mesi_ok)

per_mese <- griglia |>
  filter(stat == "mean", finestra == "mese_intero", mese %in% mesi_it) |>
  group_by(ilon, ilat, mese) |>
  summarise(base_m = mean(valore[anno %in% 1991:2020]),
            n_base = sum(anno %in% 1991:2020),
            t26_m  = mean(valore[anno == 2026]), .groups = "drop") |>
  filter(!is.na(t26_m), n_base >= 25)
copertura_it <- per_mese |> distinct(mese, n_base) |> group_by(mese) |>
  summarise(n_base = max(n_base))
if (!setequal(copertura_it$mese, mesi_it)) {
  message("Verifica Italia: la griglia Italia non copre gli stessi mesi (",
          paste(copertura_it$mese, collapse = ","), " vs ", paste(mesi_it, collapse = ","),
          "): confronto non omogeneo")
}
nazionale_it <- per_mese |>
  mutate(w_b = giorni_base[as.character(mese)], w_26 = giorni_2026[as.character(mese)]) |>
  group_by(ilon, ilat) |>
  filter(n() == length(unique(per_mese$mese))) |>
  summarise(baseline = sum(base_m * w_b) / sum(w_b),
            t_2026 = sum(t26_m * w_26) / sum(w_26), .groups = "drop") |>
  inner_join(celle_it |> select(ilon, ilat, area_kmq), by = c("ilon", "ilat")) |>
  summarise(anomalia = weighted.mean(t_2026 - baseline, area_kmq)) |>
  pull(anomalia)

anom_it_eu <- paesi$anomalia[paesi$CNTR_ID == "IT"]
diff <- anom_it_eu - nazionale_it
cat(sprintf("\nVerifica Italia: Europa %+.3f°C | griglia Italia %+.3f°C | differenza %+.3f°C -> %s\n",
            anom_it_eu, nazionale_it, diff,
            if (abs(diff) <= 0.1) "OK" else "ATTENZIONE: oltre 0,1"))
if (abs(diff) > 0.1) warning("Anomalia Italia non coincide con la griglia Italia (differenza ",
                              round(diff, 3), " °C)")
