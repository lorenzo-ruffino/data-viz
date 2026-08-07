# donne_stem_europa.R — quota di donne tra gli occupati in professioni
# scientifiche e tecnologiche (STEM), mappa Europa, anno 2025
# Esecuzione: cd script && Rscript donne_stem_europa.R

suppressPackageStartupMessages({
  library(eurostat)
  library(tidyverse)
  library(sf)
  library(showtext)
})

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

# 1) DATI ---------------------------------------------------------------------
# HRSTO = persone occupate in professioni scientifiche e tecnologiche.
# Quota donne = F / (F + M) sui valori in migliaia di persone.

dati_raw <- get_eurostat("hrst_st_rsex", time_format = "num",
                         cache = FALSE, update_cache = TRUE)

ultimo_anno <- dati_raw %>%
  filter(category == "HRSTO", unit == "THS_PER") %>%
  pull(TIME_PERIOD) %>% max(na.rm = TRUE)
cat("Ultimo anno disponibile:", ultimo_anno, "\n")

quote <- dati_raw %>%
  filter(category == "HRSTO", unit == "THS_PER",
         TIME_PERIOD == ultimo_anno, nchar(geo) == 2,
         sex %in% c("F", "M")) %>%
  select(geo, sex, values) %>%
  pivot_wider(names_from = sex, values_from = values) %>%
  mutate(quota = F / (F + M) * 100)

# Media UE27 ponderata (somma delle donne occupate STEM sul totale occupati STEM)
eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU",
          "IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")
media_ue <- quote %>%
  filter(geo %in% eu27) %>%
  summarise(q = sum(F) / sum(F + M) * 100) %>% pull(q)
cat("Media UE27 ponderata:", round(media_ue, 1), "\n")
quote %>% filter(geo %in% eu27) %>% arrange(quota) %>% print(n = 27)

# 2) CSV PULITO ---------------------------------------------------------------

quote %>%
  transmute(paese = geo, occupate_f = F, occupati_m = M,
            quota_donne = round(quota, 1)) %>%
  arrange(quota_donne) %>%
  write_csv(file.path(output_dir, "donne_stem_europa.csv"))

# 3) GEOMETRIE + JOIN ---------------------------------------------------------

geo <- load_geo_europa()
geo_dati <- geo %>%
  left_join(quote %>% transmute(CNTR_ID = geo, quota), by = "CNTR_ID")

# 4) BINNING DISCRETO ---------------------------------------------------------
# Classi da 2 punti; primo e ultimo bin aperti. Soglia di parità (50%)
# al confine tra il secondo e il terzo bin: sotto → rossi, sopra → blu.

bin_levels <- c("Meno di 48%", "Da 48 a 50%", "Da 50 a 52%", "Da 52 a 54%",
                "Da 54 a 56%", "Da 56 a 58%", "58% e oltre")

bin_colours <- c(
  "Meno di 48%"  = "#E8505E",
  "Da 48 a 50%"  = "#F6A2AA",
  "Da 50 a 52%"  = "#D6E6F7",
  "Da 52 a 54%"  = "#A1C6EE",
  "Da 54 a 56%"  = "#5C9CDE",
  "Da 56 a 58%"  = "#0E5BAD",
  "58% e oltre"  = "#06366A"
)

bin_scuri <- c("Meno di 48%", "Da 54 a 56%", "Da 56 a 58%", "58% e oltre")

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(quota) ~ NA_character_,
    quota < 48   ~ "Meno di 48%",
    quota < 50   ~ "Da 48 a 50%",
    quota < 52   ~ "Da 50 a 52%",
    quota < 54   ~ "Da 52 a 54%",
    quota < 56   ~ "Da 54 a 56%",
    quota < 58   ~ "Da 56 a 58%",
    TRUE         ~ "58% e oltre"
  ), levels = bin_levels))

# 5) LABEL PER PAESE ----------------------------------------------------------

fmt_pct <- function(v) paste0(round(v), "%")

geo_labels <- geo_dati %>%
  filter(!is.na(quota)) %>%
  mutate(
    label_value = fmt_pct(quota),
    label_color = case_when(
      CNTR_ID == "MT"    ~ "#1C1C1C",
      bin %in% bin_scuri ~ "white",
      TRUE               ~ "#1C1C1C"
    )
  )

centroidi <- mainland_centroids(geo_labels)
coords    <- sf::st_coordinates(centroidi)
geo_labels$label_x <- coords[, "X"]
geo_labels$label_y <- coords[, "Y"]

adjust_label_xy <- function(cntr, x, y) {
  dx <- case_when(
    cntr == "NO" ~ -150000,
    cntr == "SE" ~ -100000,
    cntr == "FI" ~ -120000,
    cntr == "EL" ~  -60000,
    cntr == "IT" ~  -20000,
    cntr == "LV" ~  100000,
    cntr == "NL" ~   10000,
    cntr == "HR" ~  -55000,
    cntr == "ME" ~   12000,
    cntr == "RS" ~   30000,
    cntr == "MK" ~   20000,
    cntr == "CY" ~  -30000,
    TRUE         ~  0
  )
  dy <- case_when(
    cntr == "NO" ~ -300000,
    cntr == "FI" ~ -250000,
    cntr == "SE" ~ -120000,
    cntr == "IE" ~  -80000,
    cntr == "BE" ~   25000,
    cntr == "HR" ~   70000,
    cntr == "EL" ~   30000,
    cntr == "MT" ~   25000,
    cntr == "SI" ~   35000,
    cntr == "ME" ~  -30000,
    cntr == "AL" ~  -25000,
    cntr == "RS" ~   20000,
    cntr == "MK" ~  -20000,
    cntr == "BA" ~   10000,
    TRUE         ~  0
  )
  list(x = x + dx, y = y + dy)
}
adj <- adjust_label_xy(geo_labels$CNTR_ID, geo_labels$label_x, geo_labels$label_y)
geo_labels$label_x <- adj$x
geo_labels$label_y <- adj$y

# 6) MAPPA --------------------------------------------------------------------

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
  geom_text(data = sf::st_drop_geometry(geo_labels),
            aes(x = label_x, y = label_y,
                label = label_value, color = label_color),
            family = "Source Sans Pro", size = 2.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(
    values = bin_colours,
    drop = FALSE,
    na.value = COL_NA_MAPPA,
    name = NULL,
    breaks = bin_levels
  ) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.75, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")],
           ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.98, 0.72),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "In Italia meno della metà degli occupati STEM è donna",
    subtitle = "Quota di donne tra gli occupati in professioni scientifiche e tecnologiche (STEM), 2025.\nIn rosso i paesi sotto la soglia di parità di genere del 50%.",
    caption = CAP_EUROSTAT
  )

ggsave(file.path(output_dir, "donne_stem_europa.png"),
       plot = p, width = 9, height = 9, dpi = 220, bg = "white")
