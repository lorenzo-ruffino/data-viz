#' Genera il PNG della mappa repel a partire dal layout calcolato da
#' 04a_layout_iterativo.R (cache RDS). Rapido (~30 secondi), permette di
#' iterare sullo stile senza rifare la repulsione.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(showtext)
})
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
sf_use_s2(FALSE)

PROJ_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum 2 giugno 1946"

# Parametri visuali (modificabili senza rifare la repulsione)
DIAM_MIN_MM <- 0.50
DIAM_MAX_MM <- 7.0
STROKE      <- 0.10
ALPHA_SMALL <- 0.92
ALPHA_TOP   <- 0.92    # ora i top non oscurano l'hinterland perche' i comuni
                       # sono stati pull-back vicini ai loro centroidi veri
N_TOP       <- 50

theme_map <- function() {
  theme_minimal() +
    theme(
      text             = element_text(family = "Source Sans Pro"),
      axis.text        = element_blank(),
      axis.title       = element_blank(),
      axis.line        = element_blank(),
      axis.ticks       = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background  = element_blank(),
      panel.border     = element_blank(),
      legend.position  = "bottom",
      legend.box       = "vertical",
      legend.background     = element_blank(),
      legend.box.background = element_blank(),
      legend.key            = element_blank(),
      legend.title          = element_text(size = 11, color = "#1C1C1C"),
      legend.text           = element_text(size = 11, color = "#1C1C1C"),
      plot.margin           = unit(c(b = 0.4, l = 0.4, t = 0.4, r = 0.4), "cm"),
      plot.title.position   = "plot",
      plot.title    = element_text(size = 19, color = "#1C1C1C", hjust = 0,
                                   margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
      plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.1,
                                   margin = margin(b = 0.2, l = 0, t = 0.35, r = 0, unit = "cm")),
      plot.caption  = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                                   margin = margin(b = 0, l = 0, t = -0.5, r = 0, unit = "cm"))
    )
}

df <- readRDS(file.path(PROJ_DIR, "output", "repel_layout_final.rds"))
df$vincitore <- factor(df$vincitore, levels = c("SI", "NO"))
df_top   <- df[order(-df$diff_abs), ][1:N_TOP, ]
df_small <- df[!(rownames(df) %in% rownames(df_top)), ]

regioni <- st_read(
  file.path(PROJ_DIR, "Limiti1991_g", "Reg1991_g", "Reg1991_g_WGS84.shp"),
  quiet = TRUE
) %>% st_make_valid() %>% st_transform(32632)

# Provincia di Bolzano (Alto Adige): non votò al referendum 1946 perché era
# sotto Governo militare alleato. La evidenzio con un grigio piu' caldo/scuro.
bolzano <- st_read(
  file.path(PROJ_DIR, "Limiti1991_g", "Prov1991_g", "Prov1991_g_WGS84.shp"),
  quiet = TRUE
) %>%
  filter(COD_PROV == 21) %>%
  st_make_valid() %>% st_transform(32632)

# Punto per l'annotazione "Non votò" (più a sinistra, leggermente sotto)
bolzano_centroid <- st_centroid(bolzano) %>% st_coordinates()
anno_x <- bolzano_centroid[1, "X"] - 260000
anno_y <- bolzano_centroid[1, "Y"] - 5000

colori <- c("SI" = "#0478EA", "NO" = "#E74C3C")

png(file.path(PROJ_DIR, "output", "mappa_referendum_1946_repel.png"),
    width = 8, height = 8, units = "in", res = 300)
ggplot() +
  geom_sf(data = regioni, fill = "#F0F0F0", color = "#BBBBBB", linewidth = 0.35) +
  # Bolzano (non votò): grigio piu' scuro, tratteggio diagonale visivamente
  geom_sf(data = bolzano, fill = "#C8C0B8", color = "#9A8E80", linewidth = 0.35) +
  # Annotation che spiega la diversa colorazione (a sinistra di Bolzano)
  annotate("segment",
           x = bolzano_centroid[1, "X"] - 8000,
           y = bolzano_centroid[1, "Y"] - 2000,
           xend = anno_x + 90000,
           yend = anno_y - 4000,
           color = "#6B5E50", linewidth = 0.3) +
  annotate("text",
           x = anno_x, y = anno_y,
           label = "L'Alto Adige\nnon votò",
           hjust = 0, vjust = 0.5,
           family = "Source Sans Pro",
           size = 3.2, color = "#6B5E50", lineheight = 0.95) +
  # Strato 1 (sotto): piccoli
  geom_point(
    data  = df_small,
    aes(x = x_new, y = y_new, size = diff_abs, fill = vincitore),
    alpha = ALPHA_SMALL, shape = 21, color = "white", stroke = STROKE
  ) +
  # Strato 2 (sopra): top con alpha alto (l'hinterland e' stato pull-back
  # vicino ai centroidi originali, quindi non viene piu' oscurato dal cerchio
  # del top -- i piccoli sono dove devono geograficamente).
  geom_point(
    data  = df_top,
    aes(x = x_new, y = y_new, size = diff_abs, fill = vincitore),
    alpha = ALPHA_TOP, shape = 21, color = "white", stroke = STROKE
  ) +
  scale_fill_manual(
    values = colori,
    labels = c("SI" = "Ha vinto la Repubblica", "NO" = "Ha vinto la Monarchia"),
    name   = NULL
  ) +
  scale_size_continuous(
    name   = "Vantaggio (voti)",
    range  = c(DIAM_MIN_MM, DIAM_MAX_MM),
    breaks = c(500, 2500, 10000, 50000, 250000),
    labels = function(x) formatC(x, format = "d", big.mark = ".")
  ) +
  guides(
    fill = guide_legend(order = 1,
                        override.aes = list(size = 4, color = "white", stroke = 0.25)),
    size = guide_legend(order = 2,
                        override.aes = list(fill = "#888888", color = "white", stroke = 0.25))
  ) +
  coord_sf(crs = 32632, expand = FALSE) +
  theme_map() +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.87, 0.80),
    legend.box             = "vertical",
    legend.key.size        = unit(0.5, "cm")
  ) +
  labs(
    title    = "Come la Repubblica vinse il 2 giugno 1946",
    subtitle = "Vantaggio in voti assoluti della Repubblica o della Monarchia a livello comunale",
    caption  = "Elaborazione di Lorenzo Ruffino | Fonte: Ministero dell'Interno, Istat",
    x = NULL, y = NULL
  )
dev.off()
cat("Salvata: output/mappa_referendum_1946_repel.png\n")
