library(eurostat)
library(giscoR)
library(tidyverse)
library(showtext)
library(sf)
library(scales)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

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

# --- Geometria paesi europei ----------------------------------------------

paesi_eu_efta <- c(
  "AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
  "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI",
  "SK","CH","NO","IS"
)

geo <- gisco_get_countries(year = "2020", region = "Europe", resolution = "20") %>%
  filter(CNTR_ID %in% paesi_eu_efta) %>%
  st_transform(crs = 3035)

# Box per inquadrare l'Europa continentale (esclude territori d'oltremare e
# riduce lo spazio a est dato che non c'è Ucraina)
bbox <- st_bbox(c(xmin = 2400000, ymin = 1380000,
                  xmax = 6200000, ymax = 5500000), crs = 3035)

# --- Mappa 1: salario minimo in PPS ---------------------------------------

mw <- get_eurostat("earn_mw_cur", time_format = "num", update_cache = TRUE)

mw_pps <- mw %>%
  filter(currency == "PPS", TIME_PERIOD == 2026) %>%
  select(geo, pps = values)

cat("Paesi senza valore PPS 2026:\n")
print(setdiff(paesi_eu_efta, mw_pps$geo))

geo_pps <- geo %>%
  left_join(mw_pps, by = c("CNTR_ID" = "geo"))

write_csv(st_drop_geometry(geo_pps) %>% select(CNTR_ID, NAME_ENGL, pps),
          "../output/mappa_salario_minimo_pps.csv")

p1 <- ggplot(geo_pps) +
  geom_sf(aes(fill = pps), color = "white", linewidth = 0.2) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  scale_fill_stepsn(
    breaks = seq(800, 2000, 200),
    limits = c(800, 2000),
    colours = c("#FCE4E7", "#F6A2AA", "#F12938", "#A02530", "#5A1018"),
    na.value = "#E5E5E5",
    oob = scales::squish,
    labels = function(x) format(x, big.mark = ".", decimal.mark = ","),
    guide = guide_colorsteps(
      barheight = unit(8, "cm"),
      barwidth = unit(0.45, "cm"),
      frame.colour = NA,
      ticks.colour = NA
    )
  ) +
  labs(
    title = "Il salario minimo in Europa, a parità di potere d'acquisto",
    subtitle = "Salario minimo mensile lordo espresso in PPS (parità di potere d'acquisto), 2026.\nIn grigio i paesi senza salario minimo legale o privi di dato",
    caption = CAP_EUROSTAT
  ) +
  theme_map()

ggsave("../output/mappa_salario_minimo_pps.png",
       plot = p1,
       width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa PPS salvata.\n")

# --- Mappa 2: salario minimo come % della media ----------------------------

mw_med <- get_eurostat("earn_mw_avgr2", time_format = "num", update_cache = TRUE)

# Most-recent per country, B-S nace, indicatore media (no UK)
mw_ratio <- mw_med %>%
  filter(indic_se == "MMW_MEAN_ME_PP",
         nace_r2 == "B-S",
         geo != "UK") %>%
  group_by(geo) %>%
  slice_max(TIME_PERIOD, n = 1) %>%
  ungroup() %>%
  select(geo, ratio = values, anno = TIME_PERIOD)

cat("Paesi con valore media e anno:\n")
print(mw_ratio %>% arrange(desc(ratio)), n = 30)
cat("Paesi senza valore media:\n")
print(setdiff(paesi_eu_efta, mw_ratio$geo))

geo_ratio <- geo %>%
  left_join(mw_ratio, by = c("CNTR_ID" = "geo"))

write_csv(st_drop_geometry(geo_ratio) %>% select(CNTR_ID, NAME_ENGL, ratio, anno),
          "../output/mappa_salario_minimo_pct_media.csv")

# Range dinamico in funzione dei dati
range_ratio <- range(mw_ratio$ratio, na.rm = TRUE)
brk_min <- floor(range_ratio[1] / 5) * 5
brk_max <- ceiling(range_ratio[2] / 5) * 5
cat("Range ratio:", range_ratio, " => breaks", brk_min, "-", brk_max, "\n")

p2 <- ggplot(geo_ratio) +
  geom_sf(aes(fill = ratio), color = "white", linewidth = 0.2) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  scale_fill_stepsn(
    breaks = seq(brk_min, brk_max, 5),
    limits = c(brk_min, brk_max),
    colours = c("#E0EEFB", "#9BC9F5", "#0478EA", "#0454A0", "#022E59"),
    na.value = "#E5E5E5",
    oob = scales::squish,
    labels = function(x) paste0(round(x), "%"),
    guide = guide_colorsteps(
      barheight = unit(8, "cm"),
      barwidth = unit(0.45, "cm"),
      frame.colour = NA,
      ticks.colour = NA
    )
  ) +
  labs(
    title = "Il salario minimo rispetto al salario medio",
    subtitle = "Salario minimo lordo come percentuale della retribuzione media, ultimo anno disponibile per ciascun paese.\nIn grigio i paesi senza salario minimo legale o privi di dato",
    caption = CAP_EUROSTAT
  ) +
  theme_map()

ggsave("../output/mappa_salario_minimo_pct_media.png",
       plot = p2,
       width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa % media salvata.\n")
