# Età mediana della popolazione in Europa, ultimo anno disponibile.
# Eseguito da `cd script && Rscript eta_mediana_europa.R`.
#
# Conversione di YouTrend/2022_02_27_EtàMediaUE: la mappa originale era
# continua (viridis "rocket"); qui rifatta binned con scale_fill_stepsn,
# fonte aggiornata a Eurostat live.

library(eurostat)
library(tidyverse)
library(showtext)
library(sf)
library(scales)
library(viridis)
# Niente giscoR: l'endpoint GISCO va in 503 ricorrente. Usiamo la geometria
# NUTS_RG_20M_2024 scaricata in input/.

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO <- "#1C1C1C"

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      legend.position = c(1, 0.95),
      legend.justification = c(1, 1),
      legend.key.height = unit(1.2, "cm"),
      legend.key.width = unit(0.45, "cm"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.text = element_text(size = 10, color = COL_NERO, hjust = 0),
      plot.title = element_text(size = 14, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

# --- Geometria paesi UE + EFTA ----------------------------------------------

paesi_eu_efta <- c(
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR",
  "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO",
  "SE", "SI", "SK", "CH", "NO", "IS"
)

geo <- read_sf("../input/NUTS_RG_20M_2024_4326.geojson") %>%
  filter(LEVL_CODE == 0, CNTR_CODE %in% paesi_eu_efta) %>%
  st_transform(crs = 3035)

# Box stretto sul continente (no territori d'oltremare, no spazio est inutile)
bbox <- st_bbox(c(xmin = 2400000, ymin = 1380000,
                  xmax = 6200000, ymax = 5500000), crs = 3035)

# --- Dati: età mediana popolazione, ultimo anno ----------------------------

eta <- get_eurostat("demo_pjanind", time_format = "num",
                    filters = list(indic_de = "MEDAGEPOP"),
                    update_cache = TRUE)

# Per ogni paese UE/EFTA prendi l'ultimo anno disponibile
eta_recent <- eta %>%
  filter(geo %in% paesi_eu_efta) %>%
  group_by(geo) %>%
  slice_max(time, n = 1) %>%
  ungroup() %>%
  select(geo, anno = time, eta_mediana = values)

ultimo_anno <- max(eta_recent$anno, na.rm = TRUE)
cat("Ultimo anno disponibile:", ultimo_anno, "\n")
cat("Range età mediana:", range(eta_recent$eta_mediana, na.rm = TRUE), "\n")
cat("Top 5 paesi più anziani:\n")
eta_recent %>% arrange(desc(eta_mediana)) %>% head(5) %>% print()
cat("Paesi senza valore:\n")
print(setdiff(paesi_eu_efta, eta_recent$geo))

# Join con la geometria
geo_eta <- geo %>%
  left_join(eta_recent, by = c("CNTR_CODE" = "geo"))

# Esporta dato pulito
write_csv(st_drop_geometry(geo_eta) %>%
            select(CNTR_CODE, NUTS_NAME, anno, eta_mediana),
          "../output/eta_mediana_europa.csv")

# --- Mappa ------------------------------------------------------------------

p <- ggplot(geo_eta) +
  geom_sf(aes(fill = eta_mediana), color = "white", linewidth = 0.2) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  scale_fill_stepsn(
    breaks = seq(38, 49, 1),
    limits = c(38, 49),
    colours = viridis::magma(11, direction = -1),
    na.value = "#E5E5E5",
    oob = scales::squish,
    labels = function(x) format(x, decimal.mark = ","),
    guide = guide_colorsteps(
      barheight = unit(8, "cm"),
      barwidth = unit(0.45, "cm"),
      frame.colour = NA,
      ticks.colour = NA
    )
  ) +
  labs(
    title = "L'Italia è il paese più anziano d'Europa",
    subtitle = paste0("Età mediana della popolazione, in anni, ", ultimo_anno),
    caption = CAP_EUROSTAT
  ) +
  theme_map()

ggsave("../output/eta_mediana_europa.png",
       plot = p,
       width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/eta_mediana_europa.png\n")
