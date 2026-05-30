# Mappa choropleth — risultato di Michele Boldrin per sezione a Venezia
# Comunali 24-25 maggio 2026
# Scala sequenziale dal bianco (poco Boldrin) al giallo scuro (molto Boldrin).

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(sf)
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
      legend.position = "inside",
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

dati <- read.csv(file.path(PROJ_DIR, "output", "risultati_2026_per_sezione.csv")) %>%
  mutate(pct_boldrin = ifelse(totale > 0, 100 * boldrin / totale, NA))

gdf <- sez_geo %>%
  left_join(dati %>% select(sezione, pct_boldrin, totale), by = c("SEZ" = "sezione")) %>%
  filter(!is.na(pct_boldrin), totale > 0)

# ----------------------------------------------------------------------------
# 2. Bin a larghezza costante (1 pt)
# ----------------------------------------------------------------------------
breaks <- c(-0.001, 1, 2, 3, 4, 5, 6, Inf)
labels <- c("meno dell'1%", "1–2%", "2–3%", "3–4%", "4–5%", "5–6%", "più del 6%")

gdf <- gdf %>%
  mutate(bin = cut(pct_boldrin, breaks = breaks, labels = labels, include.lowest = TRUE)) %>%
  mutate(bin = factor(bin, levels = rev(labels)))   # legenda invertita: alti in cima

# Palette sequenziale bianco → giallo scuro (RColorBrewer YlOrBr accorciato)
palette_bin <- c(
  "più del 6%"   = "#CC4C02",
  "5–6%"         = "#EC7014",
  "4–5%"         = "#FE9929",
  "3–4%"         = "#FEC44F",
  "2–3%"         = "#FEE391",
  "1–2%"         = "#FFF7BC",
  "meno dell'1%" = "#FFFFE5"
)

cat("Distribuzione sezioni per bin:\n")
print(table(gdf$bin))

# ----------------------------------------------------------------------------
# 3. Plot
# ----------------------------------------------------------------------------
gdf_utm <- st_transform(gdf, 32632)

png(file.path(PROJ_DIR, "output", "mappa_boldrin_venezia_2026.png"),
    width = 7.5, height = 8, units = "in", res = 300)
ggplot() +
  geom_sf(data = gdf_utm, aes(fill = bin), color = "#BBBBBB", linewidth = 0.12) +
  scale_fill_manual(
    name   = "Voti per Boldrin",
    values = palette_bin,
    drop   = FALSE
  ) +
  coord_sf(crs = 32632, expand = FALSE) +
  guides(fill = guide_legend(reverse = FALSE)) +
  theme_map() +
  theme(
    legend.position.inside = c(0.98, 0.55),
    legend.justification = c(1, 1),
    legend.key.size = unit(0.45, "cm")
  ) +
  labs(
    title = "Il voto a Michele Boldrin a Venezia",
    subtitle = "Percentuale di voti raccolti da Boldrin in ogni sezione elettorale\nComunali 24-25 maggio 2026",
    caption = "Elaborazione di Lorenzo Ruffino\nFonte: Comune di Venezia"
  )
dev.off()
cat("Salvata: output/mappa_boldrin_venezia_2026.png\n")
