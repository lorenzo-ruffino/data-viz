# Ore di lavoro lunghe (>= 49 ore settimanali) in Europa, ultimo anno disponibile.
# Eseguito da `cd script && Rscript ore_di_lavoro_europa.R`.
#
# Mappa binned con scale_fill_stepsn. Fonte: Eurostat live 2025.

library(eurostat)
library(tidyverse)
library(showtext)
library(sf)
library(scales)
library(viridis)

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

bbox <- st_bbox(c(xmin = 2400000, ymin = 1380000,
                  xmax = 6200000, ymax = 5500000), crs = 3035)

# --- Dati: occupati 15-64 con 49+ ore settimanali, ultimo anno -------------

ore <- get_eurostat("lfsa_qoe_3a2", time_format = "num",
                    update_cache = TRUE)

ore_recent <- ore %>%
  filter(unit == "PC", sex == "T", age == "Y15-64",
         wstatus == "EMP", isco08 == "TOTAL",
         geo %in% paesi_eu_efta) %>%
  group_by(geo) %>%
  slice_max(TIME_PERIOD, n = 1) %>%
  ungroup() %>%
  select(geo, anno = TIME_PERIOD, quota_long = values)

ultimo_anno <- max(ore_recent$anno, na.rm = TRUE)
cat("Ultimo anno disponibile:", ultimo_anno, "\n")
cat("Range quota long-hours:", range(ore_recent$quota_long, na.rm = TRUE), "\n")
cat("Top 5 paesi:\n")
ore_recent %>% arrange(desc(quota_long)) %>% head(5) %>% print()
cat("Italia:\n")
print(ore_recent %>% filter(geo == "IT"))
cat("Aggregato EU27_2020:\n")
print(ore %>% filter(unit=="PC", sex=="T", age=="Y15-64",
                     wstatus=="EMP", isco08=="TOTAL",
                     geo == "EU27_2020", TIME_PERIOD == ultimo_anno))
cat("Paesi senza valore:\n")
print(setdiff(paesi_eu_efta, ore_recent$geo))

# Join con la geometria
geo_ore <- geo %>%
  left_join(ore_recent, by = c("CNTR_CODE" = "geo"))

# Esporta dato pulito
write_csv(st_drop_geometry(geo_ore) %>%
            select(CNTR_CODE, NUTS_NAME, anno, quota_long),
          "../output/ore_di_lavoro_europa.csv")

# --- Mappa ------------------------------------------------------------------

p <- ggplot(geo_ore) +
  geom_sf(aes(fill = quota_long), color = "white", linewidth = 0.2) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  scale_fill_stepsn(
    breaks = seq(0, 12, 1),
    limits = c(0, 12),
    colours = viridis::magma(12, direction = -1),
    na.value = "#E5E5E5",
    oob = scales::squish,
    labels = function(x) paste0(format(x, decimal.mark = ","), "%"),
    guide = guide_colorsteps(
      barheight = unit(8, "cm"),
      barwidth = unit(0.45, "cm"),
      frame.colour = NA,
      ticks.colour = NA
    )
  ) +
  labs(
    title = "In Grecia oltre uno su dieci lavora 49 ore o più",
    subtitle = paste0(
      "Quota di occupati 15-64 anni che lavora 49 ore o più alla settimana, ", ultimo_anno
    ),
    caption = CAP_EUROSTAT
  ) +
  theme_map()

ggsave("../output/ore_di_lavoro_europa.png",
       plot = p,
       width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/ore_di_lavoro_europa.png\n")
