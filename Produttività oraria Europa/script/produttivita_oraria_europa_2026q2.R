# script/produttivita_oraria_europa_2026q2.R
# Mappa: variazione % della produttività reale per ora lavorata,
# secondo trimestre 2026 rispetto al secondo trimestre 2025.
# Fonte: Eurostat namq_10_lp_ulc, na_item = RLPR_HW, unit = PCH_SM, s_adj = SCA.
# Dati scaricati via API SDMX 3.0 in ../input/namq_10_lp_ulc_2026q2.csv.

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
  library(sf)
  library(scales)
})

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

# --- 1) Dati --------------------------------------------------------------

raw <- read_csv(file.path(input_dir, "namq_10_lp_ulc_2026q2.csv"),
                col_types = cols(.default = col_character()))

dati <- raw |>
  filter(na_item == "RLPR_HW", TIME_PERIOD == "2026-Q2") |>
  transmute(CNTR_ID = geo, valore = as.numeric(OBS_VALUE)) |>
  filter(!CNTR_ID %in% c("EA", "EA12", "EA19", "EA20", "EA21", "EU27_2020"))

write_csv(dati, file.path(output_dir, "produttivita_oraria_europa_2026q2.csv"))

# --- 2) Mappa -------------------------------------------------------------

geo <- load_geo_europa()
geo_dati <- geo |> left_join(dati, by = "CNTR_ID")

bin_levels <- c("meno di 0%",
                "0%",
                "da 0 a 1%",
                "da 1 a 2%",
                "da 2 a 3%",
                "da 3 a 4%",
                "4% e oltre")

bin_colours <- c(
  "meno di 0%" = "#F12938",
  "0%"         = "#FFFFFF",
  "da 0 a 1%"  = "#D6E6F7",
  "da 1 a 2%"  = "#A1C6EE",
  "da 2 a 3%"  = "#5C9CDE",
  "da 3 a 4%"  = "#0E5BAD",
  "4% e oltre" = "#06366A"
)

geo_dati <- geo_dati |>
  mutate(bin_chr = case_when(
    is.na(valore)  ~ NA_character_,
    valore <  0    ~ "meno di 0%",
    valore == 0    ~ "0%",
    valore <  1    ~ "da 0 a 1%",
    valore <  2    ~ "da 1 a 2%",
    valore <  3    ~ "da 2 a 3%",
    valore <  4    ~ "da 3 a 4%",
    TRUE           ~ "4% e oltre"
  ),
  bin = factor(bin_chr, levels = bin_levels))

bin_scuri <- c("meno di 0%", "da 2 a 3%", "da 3 a 4%", "4% e oltre")

adjust_label_xy <- function(cntr, x, y) {
  dx <- case_when(
    cntr == "NO" ~ -150000,
    cntr == "SE" ~ -100000,
    cntr == "FI" ~ -120000,
    cntr == "EL" ~  -60000,
    cntr == "IT" ~  -20000,
    cntr == "LV" ~  100000,
    cntr == "NL" ~   10000,
    cntr == "HR" ~  140000,
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
    cntr == "HR" ~   40000,
    cntr == "EL" ~   30000,
    cntr == "MT" ~   25000,
    cntr == "SI" ~   25000,
    cntr == "ME" ~  -30000,
    cntr == "AL" ~  -25000,
    cntr == "RS" ~   20000,
    cntr == "MK" ~  -20000,
    cntr == "BA" ~   10000,
    TRUE         ~  0
  )
  list(x = x + dx, y = y + dy)
}

fmt_pct <- function(v) {
  ifelse(v %% 1 == 0,
         paste0(as.integer(v), "%"),
         paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%"))
}

geo_labels <- geo_dati |>
  filter(!is.na(valore)) |>
  mutate(
    label_value = fmt_pct(valore),
    label_color = case_when(
      CNTR_ID == "MT"     ~ "#1C1C1C",
      bin %in% bin_scuri  ~ "white",
      TRUE                ~ "#1C1C1C"
    )
  )

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
    breaks = bin_levels
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
    title    = "La produttività italiana cala anche nel 2026",
    subtitle = "Variazione della produttività reale per ora lavorata nel secondo trimestre del 2026\nrispetto allo stesso trimestre del 2025, dati destagionalizzati",
    caption  = "Elaborazione di Lorenzo Ruffino su dati Eurostat"
  )

ggsave(file.path(output_dir, "produttivita_oraria_europa_2026q2.png"),
       plot = p, width = 9, height = 9, dpi = 220, bg = "white")

cat("Paesi con dato:", nrow(dati), "\n")
cat("Italia:", dati$valore[dati$CNTR_ID == "IT"],
    "| min:", dati$CNTR_ID[which.min(dati$valore)], min(dati$valore),
    "| max:", dati$CNTR_ID[which.max(dati$valore)], max(dati$valore), "\n")
