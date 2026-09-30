# Mappa dell'Europa (griglia ERA5-Land 0,1° riproiettata in EPSG:3035) con
# l'anomalia della temperatura media dell'ESTATE 2026 (giugno+luglio+agosto)
# rispetto alle estati 1991-2020. Gemella della mappa di giugno (08).
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
# finestra omogenea sulla baseline (come fa 08 per giugno) si applica SOLO se
# tutta la baseline di agosto e' giornaliera; se viene dai mensili (mese
# intero) si confronta l'agosto parziale col mese intero e si stampa un avviso.
# La baseline richiede almeno 25 anni con tutti e tre i mesi; se non ci sono
# ancora, usa i mesi disponibili e segnala che il risultato e' provvisorio.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
library(tidyverse)
library(ncdf4)
library(sf)
library(terra)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

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

baseline <- Reduce(`+`, jja[as.character(anni_base)]) / length(anni_base)
anomalia <- jja[["2026"]] - baseline

# raster 4326 -> proiezione EPSG:3035 -> maschera sui paesi delle mappe europee
df <- expand_grid(lat = lat, lon = lon)   # lon varia più veloce
df$valore <- as.vector(anomalia)          # t2m[lon, lat]: lon più veloce
r <- rast(df |> select(lon, lat, valore) |> as.data.frame(),
          type = "xyz", crs = "EPSG:4326")
geo <- load_geo_europa()
r3035 <- project(r, "EPSG:3035", res = 9000)
r3035 <- mask(r3035, vect(geo))
tiles <- as.data.frame(r3035, xy = TRUE, na.rm = TRUE) |> rename(valore = 3)

cat("Celle in mappa:", nrow(tiles), "\n")
print(round(quantile(tiles$valore, c(0, 0.02, 0.1, 0.5, 0.9, 0.98, 1)), 2))

# ---- Bin discreti (0,5 °C, aperti agli estremi) -----------------------------

bin_levels <- c("sotto 0", "da 0 a 0,5", "da 0,5 a 1", "da 1 a 1,5", "da 1,5 a 2",
                "da 2 a 2,5", "da 2,5 a 3", "da 3 a 3,5", "da 3,5 a 4",
                "da 4 a 4,5", "da 4,5 a 5", "5 e oltre")
bin_colours <- c(
  "sotto 0"    = "#A1C6EE",
  "da 0 a 0,5" = "#FDF1F3",
  "da 0,5 a 1" = "#FCE4E7",
  "da 1 a 1,5" = "#F8C0C7",
  "da 1,5 a 2" = "#F49BA5",
  "da 2 a 2,5" = "#F2707D",
  "da 2,5 a 3" = "#F12938",
  "da 3 a 3,5" = "#D42430",
  "da 3,5 a 4" = "#B01D28",
  "da 4 a 4,5" = "#8E1622",
  "da 4,5 a 5" = "#6E1019",
  "5 e oltre"  = "#4A0A10"
)

tiles <- tiles |>
  mutate(bin = factor(case_when(
    valore < 0   ~ "sotto 0",
    valore < 0.5 ~ "da 0 a 0,5",
    valore < 1   ~ "da 0,5 a 1",
    valore < 1.5 ~ "da 1 a 1,5",
    valore < 2   ~ "da 1,5 a 2",
    valore < 2.5 ~ "da 2 a 2,5",
    valore < 3   ~ "da 2,5 a 3",
    valore < 3.5 ~ "da 3 a 3,5",
    valore < 4   ~ "da 3,5 a 4",
    valore < 4.5 ~ "da 4 a 4,5",
    valore < 5   ~ "da 4,5 a 5",
    TRUE         ~ "5 e oltre"
  ), levels = bin_levels))

print(table(tiles$bin))

# confini: interni (tra paesi) in bianco, perimetro esterno (coste) in nero
contorno <- st_union(geo)

# i bin estremi senza celle non vanno in legenda
bin_presenti <- bin_levels[bin_levels %in% unique(as.character(tiles$bin))]

periodo_2026 <- if (completo && fin_ago >= 31) {
  "dell'estate 2026 (giugno-agosto)"
} else if (completo) {
  sprintf("dell'estate 2026 (1 giugno-%d agosto)", fin_ago)
} else {
  paste0("di ", paste(NOME_MESE[mesi_ok], collapse = "-"), " 2026 (dati provvisori)")
}
periodo_base <- if (completo) "delle estati" else "degli stessi mesi nel"
titolo <- if (completo) "Dove l'estate 2026 è stata più calda del normale" else
  "Dove l'estate 2026 è stata più calda del normale (provvisorio)"

p <- ggplot() +
  geom_tile(data = tiles, aes(x, y, fill = bin), width = 9000, height = 9000) +
  geom_sf(data = geo, fill = NA, color = "white", linewidth = 0.3) +
  geom_sf(data = contorno, fill = NA, color = "#1C1C1C", linewidth = 0.22) +
  scale_fill_manual(values = bin_colours, drop = FALSE, name = NULL,
                    breaks = bin_presenti) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.5, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 11,
                               color = "#1C1C1C", hjust = 0))) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")],
           ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.98, 0.80),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 11, color = "#1C1C1C", hjust = 0),
        plot.title = element_text(size = 17.5, color = "#1C1C1C", hjust = 0,
                                  margin = margin(b = 0.1, unit = "cm")),
        plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0,
                                     lineheight = 1.0,
                                     margin = margin(b = 0.25, t = 0.1, unit = "cm")),
        plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                                    margin = margin(t = 0.4, unit = "cm"))) +
  labs(
    title = titolo,
    subtitle = paste0("Differenza in gradi tra la temperatura media ", periodo_2026,
                      " e la media\n", periodo_base, " 1991-2020, celle di circa 9 km, Europa"),
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land"
  )

ggsave("output/mappa_estate_2026_europa.png", p,
       width = 9, height = 9, units = "in", dpi = 300, bg = "white")
cat("Salvata output/mappa_estate_2026_europa.png\n")
