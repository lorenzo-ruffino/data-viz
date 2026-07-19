# =============================================================================
# MAPPA — Spesa pubblica in percentuale del PIL nei paesi europei, 2024
# Fonte: Eurostat, gov_10a_main (na_item = TE, unit = PC_GDP, sector = S13)
# Dato gia' scaricato in ../input/eurostat_gov_10a_main.csv
# =============================================================================

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
  library(sf)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

# --- 1) DATI -----------------------------------------------------------------
mappa <- load_geo_europa()                       # 38 paesi, EPSG:3035

dati <- read_csv("../input/eurostat_gov_10a_main.csv", show_col_types = FALSE) %>%
  rename(cntr = geo) %>%
  mutate(anno = as.integer(substr(as.character(TIME_PERIOD), 1, 4))) %>%
  filter(anno == 2024, !cntr %in% c("EA19", "EA20", "EA21", "EU27_2020")) %>%
  transmute(CNTR_ID = cntr, valore = OBS_VALUE)

geo_dati <- mappa %>% left_join(dati, by = "CNTR_ID")

# Dato pulito esportato
dati %>%
  arrange(desc(valore)) %>%
  write_csv("../output/mappa_spesa_pubblica_pil_europa.csv")

# --- 2) BINNING DISCRETO (larghezza costante 5 punti) ------------------------
bin_levels <- c("meno del 35%", "35-40%", "40-45%", "45-50%", "50-55%", "55% e oltre")
# Palette magma discreta (binned): valori alti = scuri/intensi. Una spesa pubblica
# piu' alta non e' di per se' un "bene", quindi niente scala blu corporate.
mag <- scales::viridis_pal(begin = 0.15, end = 0.92, direction = 1, option = "magma")(6)
bin_colours <- c(
  "meno del 35%" = mag[6],   # piu' chiaro (crema/giallo)
  "35-40%"       = mag[5],
  "40-45%"       = mag[4],
  "45-50%"       = mag[3],
  "50-55%"       = mag[2],
  "55% e oltre"  = mag[1]    # piu' scuro (viola/quasi nero)
)
bin_scuri <- c("45-50%", "50-55%", "55% e oltre")   # bin scuri -> etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore < 35   ~ "meno del 35%",
    valore < 40   ~ "35-40%",
    valore < 45   ~ "40-45%",
    valore < 50   ~ "45-50%",
    valore < 55   ~ "50-55%",
    TRUE          ~ "55% e oltre"
  ), levels = bin_levels))

# --- 3) LABEL VALORE PER PAESE -----------------------------------------------
fmt_pct <- function(v) paste0(floor(v + 0.5), "%")   # arrotondato all'intero, senza decimali

# Offset manuali del centroide (metri EPSG:3035), calibrati per width=9 height=9
adjust_label_xy <- function(cntr, x, y) {
  dx <- case_when(
    cntr == "NO" ~ -150000,
    cntr == "SE" ~ -100000,
    cntr == "FI" ~  -20000,   # spostata a destra (prima -120000, troppo a sinistra)
    cntr == "EL" ~  -60000,
    cntr == "IT" ~  -20000,
    cntr == "LV" ~  100000,
    cntr == "NL" ~   10000,
    cntr == "HR" ~  -55000,
    cntr == "CY" ~  -30000,
    TRUE         ~       0
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
    TRUE         ~       0
  )
  list(x = x + dx, y = y + dy)
}

geo_labels <- geo_dati %>%
  filter(!is.na(valore)) %>%
  mutate(
    label_value = fmt_pct(valore),
    label_color = case_when(
      CNTR_ID == "MT"    ~ "#1C1C1C",   # micro-paese: poligono invisibile, label su mare
      bin %in% bin_scuri ~ "white",
      TRUE               ~ "#1C1C1C"
    )
  )

centroidi <- mainland_centroids(geo_labels)
coords    <- sf::st_coordinates(centroidi)
geo_labels$label_x <- coords[, "X"]
geo_labels$label_y <- coords[, "Y"]
adj <- adjust_label_xy(geo_labels$CNTR_ID, geo_labels$label_x, geo_labels$label_y)
geo_labels$label_x <- adj$x
geo_labels$label_y <- adj$y

# --- 4) GGPLOT ---------------------------------------------------------------
p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
  geom_text(data = sf::st_drop_geometry(geo_labels),
            aes(x = label_x, y = label_y, label = label_value, color = label_color),
            family = "Source Sans Pro", size = 2.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(
    values   = bin_colours,
    drop     = FALSE,
    na.value = COL_NA_MAPPA,
    name     = NULL,
    breaks   = bin_levels                # nasconde NA dalla legenda, lo tiene nel plot
  ) +
  guides(fill = guide_legend(
    reverse     = TRUE,
    keyheight   = unit(0.75, "cm"),
    keywidth    = unit(0.45, "cm"),
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
    title    = "Lo Stato pesa quasi la metà del PIL in Europa",
    subtitle = "Spesa delle amministrazioni pubbliche in percentuale del PIL, paesi europei, 2024",
    caption  = "Elaborazione di Lorenzo Ruffino su dati Eurostat"
  )

ggsave("../output/mappa_spesa_pubblica_pil_europa.png", p,
       width = 9, height = 9, dpi = 220, bg = "white")

cat("Mappa creata. Paesi con dato:", nrow(geo_labels),
    "| range:", min(geo_labels$valore), "-", max(geo_labels$valore), "\n")
