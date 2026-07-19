# Mappa dell'Italia a quadrati con l'aumento della temperatura media di
# giugno 2026 rispetto alla media di giugno 1991-2020.
#
# I dati ERA5-Land (0,1°) vengono aggregati su blocchi di 0,25° (media pesata
# per la superficie italiana delle celle) e i quadrati vengono RITAGLIATI sul
# perimetro dell'Italia, così nessun blocco esce dai confini. I blocchi senza
# dato (specchi d'acqua per la maschera ERA5-Land: laguna di Venezia, Garda,
# valli di Comacchio...) vengono riempiti con la media dei blocchi confinanti.
# Confini regionali marcati, confini provinciali sottili e chiari.
# Finché giugno 2026 è incompleto, il confronto usa la stessa finestra di
# giorni (es. 1-26) anche per la baseline.

library(tidyverse)
library(sf)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

PASSO <- 0.25

griglia <- read_csv("output/griglia_mensile.csv.gz", show_col_types = FALSE)
celle   <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)

# con giugno 2026 completo la finestra coincide col mese intero
serie <- read_csv("output/serie_giornaliera_italia.csv", show_col_types = FALSE)
fin <- max(as.integer(format(serie$data[format(serie$data, "%Y-%m") == "2026-06"], "%d")))
sotto_periodo <- if (fin >= 30) {
  "la temperatura media di giugno 2026 e la media di giugno"
} else {
  sprintf("la temperatura media dei primi %d giorni di giugno 2026 e la media\ndegli stessi giorni nel", fin)
}

# ---- Anomalia per cella 0,1° e aggregazione a 0,25° -------------------------

dati_celle <- griglia |>
  filter(stat == "mean", mese == 6, finestra == "finestra_giu2026") |>
  group_by(ilon, ilat, lon, lat) |>
  summarise(
    baseline = mean(valore[anno %in% 1991:2020]),
    n_base   = sum(anno %in% 1991:2020),
    t_2026   = mean(valore[anno == 2026]),
    .groups = "drop") |>
  filter(!is.na(t_2026), n_base >= 25) |>
  mutate(anomalia = t_2026 - baseline) |>
  left_join(celle |> select(ilon, ilat, area_kmq), by = c("ilon", "ilat"))

blocchi <- dati_celle |>
  mutate(bx = round(lon / PASSO) * PASSO,
         by = round(lat / PASSO) * PASSO) |>
  group_by(bx, by) |>
  summarise(anomalia = weighted.mean(anomalia, area_kmq), .groups = "drop")

# ---- Geometrie: blocchi ritagliati sull'Italia ------------------------------

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

suppressWarnings({
  dominio <- st_intersection(tutti_sf, italia)
})
dominio <- dominio[as.numeric(st_area(dominio)) > 0, ]

mappa_dati <- dominio |> left_join(blocchi, by = c("bx", "by"))

# blocchi dentro l'Italia ma senza dato (acqua per ERA5-Land): media dei vicini
buchi <- which(is.na(mappa_dati$anomalia))
if (length(buchi) > 0) {
  cat("Blocchi senza dato riempiti dai vicini:", length(buchi), "\n")
  valori <- blocchi
  for (i in buchi) {
    vic <- valori |>
      filter(abs(bx - mappa_dati$bx[i]) <= PASSO + 1e-6,
             abs(by - mappa_dati$by[i]) <= PASSO + 1e-6)
    mappa_dati$anomalia[i] <- mean(vic$anomalia)
  }
  mappa_dati <- mappa_dati |> filter(!is.na(anomalia))
}

cat("Blocchi in mappa:", nrow(mappa_dati), "\n")
print(round(quantile(mappa_dati$anomalia, c(0, 0.02, 0.1, 0.5, 0.9, 0.98, 1)), 2))

# ---- Bin discreti (larghezza costante 0,5 °C) -------------------------------

bin_levels <- c("meno di 2", "da 2 a 2,5", "da 2,5 a 3", "da 3 a 3,5",
                "da 3,5 a 4", "da 4 a 4,5", "4,5 e oltre")
bin_colours <- c(
  "meno di 2"   = "#FCE4E7",
  "da 2 a 2,5"  = "#F8C0C7",
  "da 2,5 a 3"  = "#F49BA5",
  "da 3 a 3,5"  = "#F12938",
  "da 3,5 a 4"  = "#C21E2B",
  "da 4 a 4,5"  = "#8E1622",
  "4,5 e oltre" = "#5A1018"
)

mappa_dati <- mappa_dati |>
  mutate(bin = factor(case_when(
    anomalia < 2   ~ "meno di 2",
    anomalia < 2.5 ~ "da 2 a 2,5",
    anomalia < 3   ~ "da 2,5 a 3",
    anomalia < 3.5 ~ "da 3 a 3,5",
    anomalia < 4   ~ "da 3,5 a 4",
    anomalia < 4.5 ~ "da 4 a 4,5",
    TRUE           ~ "4,5 e oltre"
  ), levels = bin_levels))

print(table(mappa_dati$bin))

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
    legend.text = element_text(size = 9, color = "#1C1C1C", hjust = 0),
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 14, color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                 lineheight = 1.35,
                                 margin = margin(b = 0.25, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                margin = margin(t = 0.4, unit = "cm"))
  )

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
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0))) +
  coord_sf(expand = FALSE) +
  theme_mappa +
  labs(
    title = "Giugno 2026 è stato ovunque più caldo della media",
    subtitle = paste0("Differenza in gradi tra ", sotto_periodo,
                      " 1991-2020, blocchi di 0,25°, Italia;\ni cerchi indicano i capoluoghi di regione"),
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land"
  )

ggsave("output/mappa_giugno_2026_italia.png", p,
       width = 8, height = 9.3, units = "in", dpi = 300, bg = "white")
cat("Salvata output/mappa_giugno_2026_italia.png\n")
