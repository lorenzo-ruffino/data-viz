# Mappa dell'Italia a blocchi di 0,25° con l'anomalia della temperatura media
# dell'ESTATE 2026 (giugno+luglio+agosto, JJA) rispetto alla media delle estati
# 1991-2020. Stessa impostazione della mappa di luglio (16): blocchi ritagliati
# sulla sagoma, buchi riempiti dai vicini, confini regionali marcati, province
# sottili, capoluoghi, bin automatici di larghezza 0,5°.
#
# Media JJA per cella = media dei tre mesi pesata per i giorni (30/31/31);
# per il 2026 agosto parziale pesa per i giorni effettivamente disponibili.
# Se esiste output/griglia_estate.csv.gz (cella x anno, media JJA, prodotto da
# 20_analisi_estate.R) viene usato; altrimenti la media JJA si calcola qui da
# output/griglia_mensile.csv.gz. Con baseline incompleta (agosto 1961-2019 in
# download) si usano solo i mesi con almeno 25 anni di baseline e lo si
# segnala; a dati completi il risultato e' quello definitivo.
#
# A video: classifica delle regioni per anomalia e blocchi oltre le soglie
# dei bin piu' alti.

library(tidyverse)
library(sf)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

PASSO <- 0.25
MESI_JJA <- c(6L, 7L, 8L)
GIORNI_MESE <- c(`6` = 30, `7` = 31, `8` = 31)
NOME_MESE <- c(`6` = "giugno", `7` = "luglio", `8` = "agosto")

griglia <- read_csv("output/griglia_mensile.csv.gz", show_col_types = FALSE)
celle   <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)
serie   <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE)

# giorni disponibili di agosto 2026 (la serie giornaliera segue gli stessi file)
gg_ago <- as.integer(format(serie$data[format(serie$data, "%Y-%m") == "2026-08"], "%d"))
fin_ago <- if (length(gg_ago)) max(gg_ago) else 0L
giorni_2026 <- c(`6` = 30, `7` = 31, `8` = fin_ago)
cat("Agosto 2026: giorni 1-", fin_ago, "\n", sep = "")

# ---- Media JJA per cella e anno ----------------------------------------------

mensile <- griglia |>
  filter(stat == "mean", finestra == "mese_intero", mese %in% MESI_JJA)

# anni di baseline per mese (per capire quali mesi sono utilizzabili)
copertura <- mensile |>
  filter(anno %in% 1991:2020) |>
  distinct(mese, anno) |>
  count(mese, name = "anni_base")
print(copertura)
mesi_usati <- copertura |> filter(anni_base >= 25) |> pull(mese) |> sort()
mesi_usati <- intersect(mesi_usati, mensile |> filter(anno == 2026) |> pull(mese) |> unique())
mesi_usati <- mesi_usati[giorni_2026[as.character(mesi_usati)] > 0]
stopifnot(length(mesi_usati) > 0)
completo <- setequal(mesi_usati, MESI_JJA)
if (!completo) {
  message("ATTENZIONE: baseline incompleta, mesi usati: ",
          paste(NOME_MESE[as.character(mesi_usati)], collapse = ", "),
          " -> risultato PROVVISORIO, rilanciare a dati completi")
}

f_estate <- "output/griglia_estate.csv.gz"
usa_estate <- FALSE
if (file.exists(f_estate) && completo) {
  estate <- read_csv(f_estate, show_col_types = FALSE)
  if ("stat" %in% names(estate)) estate <- estate |> filter(stat == "mean")
  col_val <- intersect(c("valore", "t_jja", "media_jja", "t_mean", "media", "mean"),
                       names(estate))[1]
  if (all(c("ilon", "ilat", "anno") %in% names(estate)) && !is.na(col_val)) {
    usa_estate <- TRUE
    cat("Uso", f_estate, "(colonna", col_val, ")\n")
    dati_celle <- estate |>
      rename(valore = all_of(col_val)) |>
      group_by(ilon, ilat) |>
      summarise(
        baseline = mean(valore[anno %in% 1991:2020]),
        n_base   = sum(anno %in% 1991:2020),
        t_2026   = mean(valore[anno == 2026]),
        .groups = "drop") |>
      filter(!is.na(t_2026), n_base >= 25) |>
      inner_join(celle |> select(ilon, ilat, lon, lat, regione, area_kmq),
                 by = c("ilon", "ilat"))
    if (nrow(dati_celle) == 0) {
      message("griglia_estate senza celle con baseline >= 25 anni: ricalcolo da griglia_mensile")
      usa_estate <- FALSE
    }
  }
}

if (!usa_estate) {
  cat("Calcolo la media JJA da griglia_mensile (mesi:",
      paste(mesi_usati, collapse = ","), ")\n")
  per_mese <- mensile |>
    filter(mese %in% mesi_usati) |>
    group_by(ilon, ilat, lon, lat, mese) |>
    summarise(
      base_m = mean(valore[anno %in% 1991:2020]),
      n_base = sum(anno %in% 1991:2020),
      t26_m  = mean(valore[anno == 2026]),
      .groups = "drop") |>
    filter(!is.na(t26_m), n_base >= 25)

  dati_celle <- per_mese |>
    mutate(w_base = GIORNI_MESE[as.character(mese)],
           w_2026 = giorni_2026[as.character(mese)]) |>
    group_by(ilon, ilat, lon, lat) |>
    filter(n() == length(mesi_usati)) |>          # tutti i mesi usati presenti
    summarise(baseline = sum(base_m * w_base) / sum(w_base),
              t_2026   = sum(t26_m * w_2026) / sum(w_2026),
              n_base   = min(n_base),
              .groups = "drop") |>
    left_join(celle |> select(ilon, ilat, regione, area_kmq), by = c("ilon", "ilat"))
}

dati_celle <- dati_celle |> mutate(anomalia = t_2026 - baseline)
cat("Celle 0,1° con dato:", nrow(dati_celle), "\n")

# ---- Posto dell'estate 2026 nella serie nazionale (per il titolo) -----------

jja_anni <- serie |>
  mutate(anno = as.integer(format(data, "%Y")), mese = as.integer(format(data, "%m"))) |>
  filter(mese %in% mesi_usati) |>
  group_by(anno) |>
  summarise(t = mean(t_area_mean), n = n(), .groups = "drop") |>
  filter(n >= sum(giorni_2026[as.character(mesi_usati)]) - 2) |>
  arrange(desc(t))
posto_2026 <- which(jja_anni$anno == 2026)
cat("Estate 2026 nella serie nazionale", min(jja_anni$anno), "-", max(jja_anni$anno),
    ": posto", posto_2026, "su", nrow(jja_anni), "\n")
print(head(jja_anni, 5))

# ---- Classifica delle regioni ----------------------------------------------

classifica <- dati_celle |>
  group_by(regione) |>
  summarise(anomalia = weighted.mean(anomalia, area_kmq),
            t_2026   = weighted.mean(t_2026, area_kmq),
            baseline = weighted.mean(baseline, area_kmq),
            celle = n(), .groups = "drop") |>
  arrange(desc(anomalia))
nazionale <- with(dati_celle, c(anomalia = weighted.mean(anomalia, area_kmq),
                                t_2026 = weighted.mean(t_2026, area_kmq),
                                baseline = weighted.mean(baseline, area_kmq)))
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")
cat("\n=== Estate 2026 vs media 1991-2020, per regione (°C) ===\n")
for (i in seq_len(nrow(classifica))) {
  r <- classifica[i, ]
  cat(sprintf("%2d. %-22s %s°C (2026: %s | 91-20: %s)\n", i, r$regione,
              sprintf("%+s", fmt(r$anomalia)), fmt(r$t_2026, 1), fmt(r$baseline, 1)))
}
cat(sprintf("    %-22s %s°C (2026: %s | 91-20: %s)\n\n", "ITALIA",
            sprintf("%+s", fmt(nazionale["anomalia"])), fmt(nazionale["t_2026"], 1),
            fmt(nazionale["baseline"], 1)))

# ---- Aggregazione a 0,25° ---------------------------------------------------

blocchi <- dati_celle |>
  mutate(bx = round(lon / PASSO) * PASSO,
         by = round(lat / PASSO) * PASSO) |>
  group_by(bx, by) |>
  summarise(anomalia = weighted.mean(anomalia, area_kmq),
            regione  = regione[which.max(area_kmq)], .groups = "drop")

regioni  <- read_sf("input/geo/Reg01012025_g_WGS84.json") |> st_make_valid()
province <- readRDS("input/geo/geo_province_2025.rds") |> st_transform(4326)
italia   <- st_union(regioni)

capoluoghi <- tribble(
  ~citta,       ~lon,   ~lat,
  "Torino",      7.686, 45.070,
  "Aosta",       7.315, 45.737,
  "Milano",      9.190, 45.464,
  "Trento",     11.121, 46.067,
  "Venezia",    12.316, 45.440,
  "Trieste",    13.776, 45.649,
  "Genova",      8.934, 44.407,
  "Bologna",    11.343, 44.494,
  "Firenze",    11.256, 43.770,
  "Perugia",    12.389, 43.111,
  "Ancona",     13.518, 43.617,
  "Roma",       12.496, 41.903,
  "L'Aquila",   13.399, 42.351,
  "Campobasso", 14.667, 41.561,
  "Napoli",     14.268, 40.852,
  "Bari",       16.871, 41.117,
  "Potenza",    15.805, 40.640,
  "Catanzaro",  16.594, 38.910,
  "Palermo",    13.361, 38.116,
  "Cagliari",    9.110, 39.223
)

quadrato <- function(x, y, mezzo = PASSO / 2) {
  st_polygon(list(rbind(c(x - mezzo, y - mezzo), c(x - mezzo, y + mezzo),
                        c(x + mezzo, y + mezzo), c(x + mezzo, y - mezzo),
                        c(x - mezzo, y - mezzo))))
}

bb <- st_bbox(italia)
tutti_blocchi <- expand_grid(
  bx = seq(round(bb["xmin"] / PASSO) * PASSO - PASSO, bb["xmax"] + PASSO, by = PASSO),
  by = seq(round(bb["ymin"] / PASSO) * PASSO - PASSO, bb["ymax"] + PASSO, by = PASSO))
tutti_sf <- st_sf(tutti_blocchi,
                  geometry = st_sfc(map2(tutti_blocchi$bx, tutti_blocchi$by, quadrato),
                                    crs = 4326))

suppressWarnings(mappa_dati <- st_intersection(tutti_sf, italia))
mappa_dati <- mappa_dati[as.numeric(st_area(mappa_dati)) > 0, ] |>
  left_join(blocchi, by = c("bx", "by"))

buchi <- which(is.na(mappa_dati$anomalia))
if (length(buchi) > 0) {
  cat("Blocchi senza dato riempiti dai vicini:", length(buchi), "\n")
  for (i in buchi) {
    vic <- blocchi |>
      filter(abs(bx - mappa_dati$bx[i]) <= PASSO + 1e-6,
             abs(by - mappa_dati$by[i]) <= PASSO + 1e-6)
    mappa_dati$anomalia[i] <- mean(vic$anomalia)
    if (nrow(vic)) mappa_dati$regione[i] <- vic$regione[1]
  }
  mappa_dati <- mappa_dati |> filter(!is.na(anomalia))
}

cat("Blocchi in mappa:", nrow(mappa_dati), "\n")
print(round(quantile(mappa_dati$anomalia, c(0, 0.02, 0.1, 0.5, 0.9, 0.98, 1)), 2))

# ---- Bin a scala automatica (larghezza 0,5°, estremi aperti) ----------------

# sempre due decimali, così in legenda i numeri restano allineati (2,00 / 2,25 / 2,50...)
itlab <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")
PASSO_BIN <- 0.25

negativi <- min(mappa_dati$anomalia) < 0
lo <- if (negativi) 0 else floor(quantile(mappa_dati$anomalia, 0.03) / PASSO_BIN) * PASSO_BIN
hi <- min(5, ceiling(max(mappa_dati$anomalia) / PASSO_BIN) * PASSO_BIN)   # fino a "5 e oltre" se ci sono celle
soglie <- seq(lo, hi, by = PASSO_BIN)

bin_levels <- c(
  if (negativi) "sotto 0" else paste0("meno di ", itlab(lo)),
  paste0("da ", itlab(head(soglie, -1)), " a ", itlab(tail(soglie, -1))),
  paste0(itlab(hi), " e oltre")
)
n_rossi <- length(bin_levels) - if (negativi) 1 else 0
# scala calda a molti gradini: giallo chiaro -> arancio -> rosso -> bordeaux -> quasi nero
rossi <- colorRampPalette(c("#FFF1B8", "#FDC46A", "#F98A3C", "#F12938", "#A8172B", "#5C0A1A", "#2B040C"))(n_rossi)
bin_colours <- setNames(c(if (negativi) "#A1C6EE", rossi), bin_levels)

mappa_dati$bin <- cut(mappa_dati$anomalia,
                      breaks = c(-Inf, soglie, Inf),
                      labels = bin_levels, right = FALSE)
print(table(mappa_dati$bin))
# i bin vuoti (es. "meno di 2" o "5 e oltre" senza celle) non vanno in legenda
mappa_dati$bin <- droplevels(mappa_dati$bin)
bin_colours <- bin_colours[levels(mappa_dati$bin)]

# blocchi oltre le soglie dei due bin piu' alti
cat("\n=== Blocchi oltre", itlab(hi), "°C (bin piu' alto) ===\n")
top <- mappa_dati |> st_drop_geometry() |>
  filter(anomalia >= hi) |> arrange(desc(anomalia))
if (nrow(top) == 0) cat("nessuno\n")
for (i in seq_len(nrow(top))) {
  cat(sprintf("  %5.2f°E %5.2f°N  %-22s %s°C\n", top$bx[i], top$by[i],
              coalesce(top$regione[i], "-"), sprintf("%+s", fmt(top$anomalia[i]))))
}
sogl2 <- hi - 0.5
cat("\n=== Blocchi oltre", itlab(sogl2), "°C (ultimi due bin):",
    sum(mappa_dati$anomalia >= sogl2), "su", nrow(mappa_dati), "===\n")
print(mappa_dati |> st_drop_geometry() |> filter(anomalia >= sogl2) |>
        count(regione, sort = TRUE, name = "blocchi"), n = 30)

# ---- Mappa ------------------------------------------------------------------

theme_mappa <- theme_minimal() +
  theme(
    text = element_text(family = "Source Sans Pro"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.99, 0.90),
    legend.justification = c(1, 1),
    legend.text = element_text(size = 11, color = "#1C1C1C", hjust = 0),
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 17.5, color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0,
                                 lineheight = 1.0,
                                 margin = margin(b = 0.25, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                                margin = margin(t = 0.4, unit = "cm"))
  )

# titolo in base ai dati: record nazionale, altrimenti anomalia diffusa
titolo <- if (length(posto_2026) && posto_2026 == 1 && completo) {
  "L'estate più calda da quando abbiamo i dati"
} else if (!negativi) {
  "L'estate 2026 è stata ovunque più calda della media"
} else {
  "L'estate 2026 è stata quasi ovunque più calda della media"
}

periodo_2026 <- if (completo && fin_ago >= 31) {
  "dell'estate 2026 (giugno-agosto)"
} else if (completo) {
  sprintf("dell'estate 2026 (1 giugno-%d agosto)", fin_ago)
} else {
  paste0("di ", paste(NOME_MESE[as.character(mesi_usati)], collapse = "-"),
         " 2026 (dati provvisori)")
}
periodo_base <- if (completo) "delle estati" else "degli stessi mesi nel"

p <- ggplot() +
  geom_sf(data = mappa_dati, aes(fill = bin), color = NA) +
  geom_sf(data = province, fill = NA, color = "#C9C9C9", linewidth = 0.14) +
  geom_sf(data = regioni,  fill = NA, color = "#2B2B2B", linewidth = 0.5) +
  geom_point(data = capoluoghi, aes(lon, lat),
             color = "#1C1C1C", fill = "white", shape = 21,
             size = 1.7, stroke = 0.7) +
  scale_fill_manual(values = bin_colours, drop = FALSE, name = NULL) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.62, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 11,
                               color = "#1C1C1C", hjust = 0))) +
  coord_sf(expand = FALSE) +
  theme_mappa +
  labs(
    title = titolo,
    subtitle = paste0("Differenza in gradi tra la temperatura media ", periodo_2026,
                      " e la media\n", periodo_base, " 1991-2020, blocchi di 0,25°, Italia; i cerchi indicano i capoluoghi di regione"),
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land"
  )

ggsave("output/mappa_estate_2026_italia.png", p,
       width = 8, height = 9.3, units = "in", dpi = 300, bg = "white")
cat("Salvata output/mappa_estate_2026_italia.png\n")
