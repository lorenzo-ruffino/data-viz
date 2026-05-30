# Mappa choropleth — Venezia, sindaco 2026
# Ogni sezione è riempita in base al vantaggio (in pt%) del candidato vincente.
# Blu = Venturini avanti, Rosso = Martella avanti. Bin a larghezza costante (10 pt).
# Legenda invertita (valori alti in cima), etichette intere.

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

dati <- read.csv(file.path(PROJ_DIR, "output", "risultati_2026_per_sezione.csv"))

# Margine signed in pt% = pct_venturini - pct_martella
# >0 Venturini avanti, <0 Martella avanti
dati <- dati %>%
  mutate(
    pct_venturini = ifelse(totale > 0, 100 * venturini / totale, NA),
    pct_martella  = ifelse(totale > 0, 100 * martella  / totale, NA),
    margine_pp    = pct_venturini - pct_martella
  )

gdf <- sez_geo %>%
  left_join(dati %>% select(sezione, margine_pp, totale), by = c("SEZ" = "sezione")) %>%
  filter(!is.na(margine_pp), totale > 0)

# ----------------------------------------------------------------------------
# 2. Bin a larghezza costante (10 pt%)
# ----------------------------------------------------------------------------
# Limito gli estremi: chi vince con margine >40 pt è quasi ovunque a Mestre nord
# e a centro storico, accorpo nel bin >40 / <-40.
breaks <- c(-Inf, -30, -20, -10, 0, 10, 20, 30, Inf)
labels <- c("più di 30 pt Martella", "20–30 pt Martella", "10–20 pt Martella",
            "0–10 pt Martella", "0–10 pt Venturini", "10–20 pt Venturini",
            "20–30 pt Venturini", "più di 30 pt Venturini")

gdf <- gdf %>%
  mutate(bin = cut(margine_pp, breaks = breaks, labels = labels, include.lowest = TRUE)) %>%
  mutate(bin = factor(bin, levels = rev(labels)))   # invertita: alti in cima

# Palette diverging RdBu (rosso scuro Martella → blu scuro Venturini)
palette_bin <- c(
  "più di 30 pt Venturini" = "#08519c",
  "20–30 pt Venturini"     = "#3182bd",
  "10–20 pt Venturini"     = "#6baed6",
  "0–10 pt Venturini"      = "#bdd7e7",
  "0–10 pt Martella"       = "#fcae91",
  "10–20 pt Martella"      = "#fb6a4a",
  "20–30 pt Martella"      = "#cb181d",
  "più di 30 pt Martella"  = "#67000d"
)

cat("Distribuzione sezioni per bin:\n")
print(table(gdf$bin))

# ----------------------------------------------------------------------------
# 3. Plot
# ----------------------------------------------------------------------------
gdf_utm <- st_transform(gdf, 32632)

png(file.path(PROJ_DIR, "output", "mappa_choropleth_venezia_2026.png"),
    width = 7.5, height = 8, units = "in", res = 300)
ggplot() +
  geom_sf(data = gdf_utm, aes(fill = bin), color = "white", linewidth = 0.12) +
  scale_fill_manual(
    name   = "Vantaggio del vincitore",
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
    title = "La mappa delle elezioni a Venezia",
    subtitle = "Vantaggio del candidato vincente in punti percentuali, per sezione\nComunali 24-25 maggio 2026",
    caption = "Elaborazione di Lorenzo Ruffino\nFonte: Comune di Venezia"
  )
dev.off()
cat("Salvata: output/mappa_choropleth_venezia_2026.png\n")
