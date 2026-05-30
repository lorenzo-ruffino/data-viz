# Mappa sezioni elettorali Venezia 2026 — cerchi colorati per vincitore
#
# Stile ispirato a Referendum_Giustizia_2026/mappa_risultati_repel.r:
#   - cerchi colorati posizionati sul centroide di ogni sezione
#   - colore: blu Venturini, rosso Martella
#   - dimensione: proporzionale al margine in voti del vincitore
#   - repulsione opzionale per evitare sovrapposizioni
#
# Input:
#   input/sezioni_geo.json                         (geometrie sezioni dal sito comune.venezia.it)
#   output/risultati_2026_per_sezione.csv          (voti per candidato per sezione)
#
# Output:
#   output/mappa_sezioni_venezia_2026.png

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

# ----------------------------------------------------------------------------
# Theme
# ----------------------------------------------------------------------------
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

# Calcolo vincitore, secondo e margine per ogni sezione
cand_cols <- c("venturini", "martella", "del_zotto", "coro", "boldrin",
               "agirmo", "vernier", "martini_2026")
dati <- dati %>%
  rowwise() %>%
  mutate(
    primo_voti   = max(c_across(all_of(cand_cols))),
    primo_nome   = cand_cols[which.max(c_across(all_of(cand_cols)))],
    secondo_voti = sort(c_across(all_of(cand_cols)), decreasing = TRUE)[2],
    margine      = primo_voti - secondo_voti,
    margine_pct  = ifelse(totale > 0, 100 * margine / totale, 0)
  ) %>%
  ungroup()

# Etichetta colore: blu Venturini, rosso Martella, grigio altri
dati <- dati %>%
  mutate(vincitore = case_when(
    primo_nome == "venturini" ~ "Venturini",
    primo_nome == "martella"  ~ "Martella",
    TRUE                       ~ "Altri"
  ))

cat("Vincitori per sezione:\n"); print(table(dati$vincitore))

# Join geometrie + dati
gdf <- sez_geo %>%
  left_join(dati, by = c("SEZ" = "sezione")) %>%
  filter(!is.na(margine), margine > 0)

# Centroidi in UTM 32N. Per le sezioni MultiPolygon (es. Sant'Erasmo, sez 87,
# che è frammentata in 7 isolotti) il centroide dell'intera geometria cade
# in mezzo al mare. Prendo il centroide del poligono più GRANDE per area.
gdf_utm <- st_transform(gdf, 32632)

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

# Confini comune (unione delle sezioni)
confini <- gdf_utm %>%
  summarise(geometry = st_union(geometry)) %>%
  st_make_valid()

# ----------------------------------------------------------------------------
# 2. Calcolo raggio + repulsione (stessa logica di mappa_risultati_repel.r)
# ----------------------------------------------------------------------------
DIAM_MIN_MM <- 0.25
DIAM_MAX_MM <- 8.0
# Repulsione minima alzata per spaziare meglio i cerchi addensati nel centro storico
MIN_REPULSE_MM <- 1.4
# Esponente per accentuare la differenza tra cerchi piccoli e grandi
# (>1 spinge i grandi verso il max, schiaccia i piccoli verso il min)
SIZE_POWER <- 1.6

bbox <- st_bbox(confini)
panel_height_m  <- bbox["ymax"] - bbox["ymin"]
# plot 8x8 inch, panel utile ~87% dell'altezza
panel_height_mm <- 8 * 25.4 * 0.87
mm_per_m <- panel_height_mm / as.numeric(panel_height_m)
m_per_mm <- 1 / mm_per_m

margine_max <- max(df_circ$margine)
radius_max_m <- (DIAM_MAX_MM / 2) * m_per_mm
min_repulse_m <- (MIN_REPULSE_MM / 2) * m_per_mm

# Raggio visivo con curva power: (margine/margine_max)^SIZE_POWER
# Più alto SIZE_POWER, più i grandi si stagliano rispetto ai piccoli.
df_circ$margine_norm <- (df_circ$margine / margine_max)^SIZE_POWER
df_circ$radius_visual <- (DIAM_MIN_MM + (DIAM_MAX_MM - DIAM_MIN_MM) *
                           df_circ$margine_norm) / 2 * m_per_mm
df_circ$safety_g <- 1.0 + 0.40 * sqrt(df_circ$margine / margine_max)
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
colori <- c("Venturini" = "#0478EA", "Martella" = "#E74C3C", "Altri" = "#888888")

# Levels ordinati per stacking estetico (Martella sopra Venturini per renderlo visibile)
df_circ <- df_circ %>%
  mutate(vincitore = factor(vincitore, levels = c("Venturini", "Martella", "Altri")))

png(file.path(PROJ_DIR, "output", "mappa_sezioni_venezia_2026.png"),
    width = 7.5, height = 8, units = "in", res = 300)
ggplot() +
  geom_sf(data = confini, fill = "#F0F0F0", color = "#BBBBBB", linewidth = 0.35) +
  geom_point(
    data = df_circ %>% arrange(margine),  # piccoli sotto, grandi sopra
    aes(x = x_new, y = y_new, size = margine_norm, fill = vincitore),
    alpha = 0.78, shape = 21, color = "white", stroke = 0.2
  ) +
  scale_fill_manual(
    values = colori,
    name = NULL,
    labels = c("Venturini" = "Venturini in vantaggio",
               "Martella"  = "Martella in vantaggio",
               "Altri"     = "Altro")
  ) +
  scale_size_continuous(
    name = "Vantaggio (voti)",
    range = c(DIAM_MIN_MM, DIAM_MAX_MM),
    breaks = (c(50, 100, 200, 300) / margine_max)^SIZE_POWER,
    labels = c("50", "100", "200", "300")
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(size = 5, color = "white", stroke = 0.3)),
    size = guide_legend(order = 2, override.aes = list(fill = "#888888", color = "white", stroke = 0.3))
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
    title = "La mappa delle elezioni a Venezia",
    subtitle = "Vantaggio in voti del candidato vincente in ogni sezione elettorale\nComunali 24-25 maggio 2026",
    caption = "Elaborazione di Lorenzo Ruffino\nFonte: Comune di Venezia"
  )
dev.off()
cat("Salvata: output/mappa_sezioni_venezia_2026.png\n")
