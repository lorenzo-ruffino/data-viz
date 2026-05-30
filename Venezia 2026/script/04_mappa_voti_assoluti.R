# Mappa sezioni elettorali Venezia 2026 — cerchi gialli proporzionali ai voti totali
#
# Variante di 03_mappa_sezioni.R: cerchi tutti dello stesso colore (giallo),
# dimensione proporzionale al totale di voti validi per sezione.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(sf)
  library(packcircles)
  library(showtext)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
sf_use_s2(FALSE)

PROJ_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Venezia 2026 - Proiezione sezioni"

theme_map <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      axis.text = element_blank(), axis.title = element_blank(),
      axis.line = element_blank(),  axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(size = 11, color = "#1C1C1C"),
      legend.text = element_text(size = 11, color = "#1C1C1C"),
      plot.margin = unit(c(0.65, 0.1, 0.4, 0.1), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(size = 19, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0, unit = "cm")),
      plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.1,
                                   margin = margin(b = 0.2, t = 0.12, unit = "cm")),
      plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                                  lineheight = 1.2,
                                  margin = margin(t = -0.3, unit = "cm"))
    )
}

# ----------------------------------------------------------------------------
# 1. Dati
# ----------------------------------------------------------------------------
sez_geo <- st_read(file.path(PROJ_DIR, "input", "sezioni_geo.json"), quiet = TRUE) %>%
  select(SEZ, geometry) %>%
  st_make_valid()

dati <- read.csv(file.path(PROJ_DIR, "output", "risultati_2026_per_sezione.csv"))

gdf <- sez_geo %>%
  left_join(dati %>% select(sezione, totale), by = c("SEZ" = "sezione")) %>%
  filter(!is.na(totale), totale > 0)

cat(sprintf("Sezioni con voti: %d (totale voti: %s)\n",
            nrow(gdf), formatC(sum(gdf$totale), big.mark = ".", format = "d")))

gdf_utm <- st_transform(gdf, 32632)

# Centroide del poligono più grande (per le sezioni multi-isola, es. Sant'Erasmo)
main_centroid <- function(geom) {
  parts <- suppressWarnings(st_cast(st_sfc(geom, crs = 32632), "POLYGON"))
  if (length(parts) == 1) return(st_centroid(parts))
  areas <- as.numeric(st_area(parts))
  st_centroid(parts[which.max(areas)])
}
centroidi_geom <- do.call(c, lapply(st_geometry(gdf_utm), main_centroid))
st_crs(centroidi_geom) <- 32632
coords <- st_coordinates(centroidi_geom)
df_circ <- gdf_utm %>%
  st_drop_geometry() %>%
  mutate(x = coords[, 1], y = coords[, 2])

confini <- gdf_utm %>%
  summarise(geometry = st_union(geometry)) %>%
  st_make_valid()

# ----------------------------------------------------------------------------
# 2. Raggio + repulsione
# ----------------------------------------------------------------------------
DIAM_MIN_MM <- 0.4
DIAM_MAX_MM <- 8.0
MIN_REPULSE_MM <- 1.4
SIZE_POWER <- 1.4   # i voti per sezione hanno meno range del margine, esponente più basso

bbox <- st_bbox(confini)
panel_height_m  <- bbox["ymax"] - bbox["ymin"]
panel_height_mm <- 8 * 25.4 * 0.87
mm_per_m <- panel_height_mm / as.numeric(panel_height_m)
m_per_mm <- 1 / mm_per_m

voti_max <- max(df_circ$totale)
radius_max_m <- (DIAM_MAX_MM / 2) * m_per_mm
min_repulse_m <- (MIN_REPULSE_MM / 2) * m_per_mm

df_circ$voti_norm <- (df_circ$totale / voti_max)^SIZE_POWER
df_circ$radius_visual <- (DIAM_MIN_MM + (DIAM_MAX_MM - DIAM_MIN_MM) *
                           df_circ$voti_norm) / 2 * m_per_mm
df_circ$safety_g <- 1.0 + 0.40 * sqrt(df_circ$totale / voti_max)
df_circ$radius <- pmax(df_circ$radius_visual * df_circ$safety_g, min_repulse_m)
df_circ$weight <- exp(-df_circ$radius_visual / (radius_max_m / 6))

cat(sprintf("Repulsione su %d cerchi...\n", nrow(df_circ)))
t0 <- Sys.time()
layout <- circleRepelLayout(
  df_circ[, c("x", "y", "radius")],
  xysizecols = c("x", "y", "radius"),
  sizetype   = "radius",
  maxiter    = 4000,
  weights    = df_circ$weight
)
cat(sprintf("Repulsione in %.1f sec\n", as.numeric(Sys.time() - t0, units = "secs")))
df_circ$x_new <- layout$layout$x
df_circ$y_new <- layout$layout$y

# ----------------------------------------------------------------------------
# 3. Plot
# ----------------------------------------------------------------------------
png(file.path(PROJ_DIR, "output", "mappa_voti_assoluti_venezia_2026.png"),
    width = 7.5, height = 8, units = "in", res = 300)
ggplot() +
  geom_sf(data = confini, fill = "#F0F0F0", color = "#BBBBBB", linewidth = 0.35) +
  geom_point(
    data = df_circ %>% arrange(totale),
    aes(x = x_new, y = y_new, size = voti_norm),
    alpha = 0.85, shape = 21, fill = "#F4B400", color = "white", stroke = 0.2
  ) +
  scale_size_continuous(
    name = "Voti per sezione",
    range = c(DIAM_MIN_MM, DIAM_MAX_MM),
    breaks = (c(200, 400, 600) / voti_max)^SIZE_POWER,
    labels = c("200", "400", "600")
  ) +
  guides(
    size = guide_legend(override.aes = list(fill = "#F4B400", color = "white", stroke = 0.3))
  ) +
  coord_sf(crs = 32632, expand = FALSE) +
  theme_map() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.55),
    legend.justification = c(1, 1),
    legend.box = "vertical",
    legend.key.size = unit(0.45, "cm")
  ) +
  labs(
    title = "Voti per sezione a Venezia",
    subtitle = "Voti validi totali raccolti in ogni sezione elettorale\nComunali 24-25 maggio 2026",
    caption = "Elaborazione di Lorenzo Ruffino\nFonte: Comune di Venezia"
  )
dev.off()
cat("Salvata: output/mappa_voti_assoluti_venezia_2026.png\n")
