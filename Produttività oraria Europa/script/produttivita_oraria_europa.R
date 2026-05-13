# script/produttivita_oraria_europa.R
# Mappa: variazione % della produttività reale per ora lavorata, 2024-2025.
# Fonte: Eurostat nama_10_lp_ulc, na_item = RLPR_HW, unit = PCH_PRE.

suppressPackageStartupMessages({
  library(eurostat)
  library(tidyverse)
  library(showtext)
  library(sf)
  library(scales)
})

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

source_dir <- ".."
output_dir <- file.path(source_dir, "output")

# --- 1) Dati --------------------------------------------------------------

df <- get_eurostat("nama_10_lp_ulc",
                   time_format = "num",
                   cache = FALSE, update_cache = TRUE)

dati <- df |>
  filter(na_item == "RLPR_HW",
         unit == "PCH_PRE",
         TIME_PERIOD == 2025) |>
  transmute(CNTR_ID = geo, valore = values) |>
  filter(!CNTR_ID %in% c("EA", "EA12", "EA19", "EA20", "EA21", "EU27_2020"))

write_csv(dati, file.path(output_dir, "produttivita_oraria_europa.csv"))

# --- 2) Mappa -------------------------------------------------------------

geo <- load_geo_europa()
geo_dati <- geo |> left_join(dati, by = "CNTR_ID")

bin_levels <- c("da 0 a -1%",
                "0%",
                "da 0 a 0,5%",
                "da 0,5 a 1%",
                "da 1 a 2%",
                "da 2 a 4%",
                "da 4 a 6%",
                "≥ 6%")

bin_colours <- c(
  "da 0 a -1%"   = "#F12938",
  "0%"           = "#FFFFFF",
  "da 0 a 0,5%"  = "#EAF1FA",
  "da 0,5 a 1%"  = "#D6E6F7",
  "da 1 a 2%"    = "#A1C6EE",
  "da 2 a 4%"    = "#5C9CDE",
  "da 4 a 6%"    = "#0E5BAD",
  "≥ 6%"         = "#06366A"
)

geo_dati <- geo_dati |>
  mutate(bin_chr = case_when(
    is.na(valore)  ~ NA_character_,
    valore <  0    ~ "da 0 a -1%",
    valore == 0    ~ "0%",
    valore <  0.5  ~ "da 0 a 0,5%",
    valore <  1    ~ "da 0,5 a 1%",
    valore <  2    ~ "da 1 a 2%",
    valore <  4    ~ "da 2 a 4%",
    valore <  6    ~ "da 4 a 6%",
    TRUE           ~ "≥ 6%"
  ),
  bin = factor(bin_chr, levels = bin_levels))

# Label di valore per ogni paese che ha il dato. Formato italiano: 1 decimale,
# virgola, segno %. Colore bianco sui bin scuri (rosso e blu medio/scuri),
# nero sugli altri per garantire il contrasto.
bin_scuri <- c("da 0 a -1%", "da 2 a 4%", "da 4 a 6%", "≥ 6%")

# Posizione: centroide del paese, con aggiustamenti manuali per paesi grandi
# o con forma allungata in cui il centroide cade in una zona poco leggibile.
# Coordinate in EPSG:3035, metri.
adjust_label_xy <- function(cntr, x, y) {
  dx <- case_when(
    cntr == "NO" ~ -150000,   # Norvegia: 150 km più a ovest
    cntr == "SE" ~ -100000,   # Svezia: 100 km più a ovest
    cntr == "EL" ~ -100000,   # Grecia: 100 km più a ovest
    cntr == "IT" ~  -20000,   # Italia: 20 km più a ovest
    cntr == "LV" ~  100000,   # Lettonia: 100 km più a est
    cntr == "NL" ~   10000,   # Paesi Bassi: 10 km più a est
    TRUE         ~  0
  )
  dy <- case_when(
    cntr == "NO" ~ -250000,   # Norvegia: 250 km più a sud
    cntr == "FI" ~ -250000,   # Finlandia: 250 km più a sud
    cntr == "IE" ~  -80000,   # Irlanda: 80 km più a sud (verso Dublino)
    cntr == "BE" ~   30000,   # Belgio: 30 km più a nord
    cntr == "HR" ~   60000,   # Croazia: 60 km più a nord
    cntr == "EL" ~   20000,   # Grecia: 20 km più a nord
    TRUE         ~  0
  )
  list(x = x + dx, y = y + dy)
}

# Formato label: 1 decimale se non zero, altrimenti solo l'intero
# (es. 2,0% → 2%; 0,0% → 0%; -0,6% e 10,5% restano invariati).
fmt_pct <- function(v) {
  ifelse(v %% 1 == 0,
         paste0(as.integer(v), "%"),
         paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%"))
}

geo_labels <- geo_dati |>
  filter(!is.na(valore)) |>
  mutate(
    label_value = fmt_pct(valore),
    # Default: bianco sui bin scuri, nero sugli altri. Malta è un'eccezione:
    # alla scala europea il poligono è praticamente invisibile, quindi una
    # label bianca finirebbe sul mare bianco. Forziamo il colore nero
    # (come gli altri paesi su sfondo chiaro) per renderla visibile.
    label_color = case_when(
      CNTR_ID == "MT"     ~ "#1C1C1C",
      bin %in% bin_scuri  ~ "white",
      TRUE                ~ "#1C1C1C"
    )
  )

# Centroide del "mainland" europeo (helper in utilities/R/mappe.R, gestisce
# Francia con Guyana, Spagna con Canarie, ecc.) + aggiustamenti manuali per
# paesi grandi/allungati in cui il centroide cade in una zona poco leggibile.
centroidi <- mainland_centroids(geo_labels)
coords    <- sf::st_coordinates(centroidi)
geo_labels$label_x <- coords[, "X"]
geo_labels$label_y <- coords[, "Y"]
adj <- adjust_label_xy(geo_labels$CNTR_ID, geo_labels$label_x, geo_labels$label_y)
geo_labels$label_x <- adj$x
geo_labels$label_y <- adj$y

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
  geom_text(data = sf::st_drop_geometry(geo_labels),
            aes(x = label_x, y = label_y,
                label = label_value, color = label_color),
            family = "Source Sans Pro", size = 2.5,
            fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(
    values = bin_colours,
    drop = FALSE,
    na.value = COL_NA_MAPPA,
    name = NULL,
    breaks = bin_levels   # nasconde NA dalla legenda senza rimuoverlo dal plot
  ) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.75, "cm"),
    keywidth  = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")],
           ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.98, 0.72),
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title    = "La produttività italiana è calata nel 2025",
    subtitle = "Variazione percentuale della produttività reale per ora lavorata nel 2025 rispetto al 2024",
    caption  = "Elaborazione di Lorenzo Ruffino su dati Eurostat"
  )

ggsave(file.path(output_dir, "produttivita_oraria_europa.png"),
       plot = p, width = 9, height = 9, dpi = 220, bg = "white")

cat("Fatto.\n")
