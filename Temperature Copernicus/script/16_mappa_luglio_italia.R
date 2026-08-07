# Mappa dell'Italia a blocchi di 0,25° con l'anomalia della temperatura media
# di LUGLIO 2026 rispetto alla media di luglio 1991-2020. Stessa impostazione
# della mappa di giugno (06): blocchi ritagliati sulla sagoma, buchi riempiti
# dai vicini, confini regionali marcati, province sottili, capoluoghi.
# I bin (larghezza costante 0,5°) si adattano alla distribuzione di luglio.

library(tidyverse)
library(sf)
library(showtext)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Temperature Copernicus")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

PASSO <- 0.25
MESE  <- 7

griglia <- read_csv("output/griglia_mensile.csv.gz", show_col_types = FALSE)
celle   <- read_csv("input/geo/celle_griglia.csv", show_col_types = FALSE)

dati_celle <- griglia |>
  filter(stat == "mean", mese == MESE, finestra == "mese_intero") |>
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
  for (i in buchi) {
    vic <- blocchi |>
      filter(abs(bx - mappa_dati$bx[i]) <= PASSO + 1e-6,
             abs(by - mappa_dati$by[i]) <= PASSO + 1e-6)
    mappa_dati$anomalia[i] <- mean(vic$anomalia)
  }
  mappa_dati <- mappa_dati |> filter(!is.na(anomalia))
}

cat("Blocchi in mappa:", nrow(mappa_dati), "\n")
print(round(quantile(mappa_dati$anomalia, c(0, 0.02, 0.1, 0.5, 0.9, 0.98, 1)), 2))

# ---- Bin a scala automatica (larghezza 0,5°, estremi aperti) ----------------

itlab <- function(x) ifelse(x %% 1 == 0, as.character(as.integer(x)),
                            formatC(x, format = "f", digits = 1, decimal.mark = ","))

negativi <- min(mappa_dati$anomalia) < 0
lo <- if (negativi) 0 else floor(quantile(mappa_dati$anomalia, 0.03) * 2) / 2
hi <- ceiling(quantile(mappa_dati$anomalia, 0.97) * 2) / 2
soglie <- seq(lo, hi, by = 0.5)

bin_levels <- c(
  if (negativi) "sotto 0" else paste0("meno di ", itlab(lo)),
  paste0("da ", itlab(head(soglie, -1)), " a ", itlab(tail(soglie, -1))),
  paste0(itlab(hi), " e oltre")
)
n_rossi <- length(bin_levels) - if (negativi) 1 else 0
rossi <- colorRampPalette(c("#FCE4E7", "#F49BA5", "#F12938", "#A02530", "#4A0A10"))(n_rossi)
bin_colours <- setNames(c(if (negativi) "#A1C6EE", rossi), bin_levels)

mappa_dati$bin <- cut(mappa_dati$anomalia,
                      breaks = c(-Inf, soglie, Inf),
                      labels = bin_levels, right = FALSE)
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
    legend.text = element_text(size = 11, color = "#1C1C1C", hjust = 0),
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 17.5, color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0,
                                 lineheight = 1.35,
                                 margin = margin(b = 0.25, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                                margin = margin(t = 0.4, unit = "cm"))
  )

titolo <- "Il luglio più caldo da quando abbiamo i dati"

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
    subtitle = "Differenza in gradi tra la temperatura media di luglio 2026 e la media di luglio 1991-2020,\nblocchi di 0,25°, Italia; i cerchi indicano i capoluoghi di regione",
    caption = "Elaborazione di Lorenzo Ruffino su dati Copernicus ERA5-Land"
  )

ggsave("output/mappa_luglio_2026_italia.png", p,
       width = 8, height = 9.3, units = "in", dpi = 300, bg = "white")
cat("Salvata output/mappa_luglio_2026_italia.png\n")
