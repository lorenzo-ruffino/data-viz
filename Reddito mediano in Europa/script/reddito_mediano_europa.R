# Reddito netto mediano equivalente in Europa, ultimo anno disponibile.
# Eseguito da `cd script && Rscript reddito_mediano_europa.R`.
#
# Mappa binned con classi costanti di 4.000 (in PPS) e scale_fill_manual, etichetta
# del valore (in migliaia, suffisso k) su ogni paese. Blu monocromatico.
# Fonte: Eurostat ilc_di03 (statinfo MED_EI, age TOTAL, sex T, unit PPS = standard
# di potere d'acquisto, che neutralizza le differenze dei livelli di prezzo).

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(eurostat)
  library(tidyverse)
  library(showtext)
  library(sf)
})

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

# --- Dati: reddito netto mediano equivalente, ultimo anno ------------------

geo <- load_geo_europa()                       # 38 paesi, EPSG:3035

raw <- get_eurostat("ilc_di03", time_format = "num",
                    cache = FALSE, update_cache = TRUE)

med <- raw %>%
  filter(statinfo == "MED_EI", age == "TOTAL", sex == "T", unit == "PPS")

ultimo_anno <- max(med$TIME_PERIOD, na.rm = TRUE)

paesi <- med %>%
  filter(TIME_PERIOD == ultimo_anno, geo %in% paesi_europa_mappa) %>%
  select(CNTR_ID = geo, anno = TIME_PERIOD, valore = values)

# Mediana UE27 pubblicata da Eurostat (aggregato sulla popolazione UE).
media_ue <- med %>% filter(TIME_PERIOD == ultimo_anno, geo == "EU27_2020") %>%
  pull(values)

cat("Ultimo anno:", ultimo_anno, "\n")
cat("Mediana UE27 (EU27_2020):", media_ue, "\n")
cat("Italia:", paesi$valore[paesi$CNTR_ID == "IT"], "\n")
cat("Range:", paste(range(paesi$valore), collapse = " - "), "\n")
cat("Paesi senza dato:",
    paste(setdiff(paesi_europa_mappa, paesi$CNTR_ID), collapse = ", "), "\n")

geo_dati <- geo %>% left_join(paesi, by = "CNTR_ID")

write_csv(
  st_drop_geometry(geo_dati) %>%
    select(CNTR_ID, paese = NAME_ENGL, anno, reddito_mediano_pps = valore) %>%
    filter(!is.na(reddito_mediano_pps)) %>%
    arrange(desc(reddito_mediano_pps)),
  "../output/reddito_mediano_europa.csv"
)

# --- Binning discreto (classi costanti di 5.000 euro) ----------------------

bin_levels <- c("meno di 12k", "da 12k a 16k", "da 16k a 20k", "da 20k a 24k",
                "da 24k a 28k", "da 28k a 32k", "32k e oltre")
bin_colours <- setNames(
  colorRampPalette(c("#EAF1FA", "#A1C6EE", "#5C9CDE", "#0E5BAD", "#06366A"))(7),
  bin_levels
)
bin_scuri <- c("da 24k a 28k", "da 28k a 32k", "32k e oltre")  # -> etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore)   ~ NA_character_,
    valore < 12000  ~ "meno di 12k",
    valore < 16000  ~ "da 12k a 16k",
    valore < 20000  ~ "da 16k a 20k",
    valore < 24000  ~ "da 20k a 24k",
    valore < 28000  ~ "da 24k a 28k",
    valore < 32000  ~ "da 28k a 32k",
    TRUE            ~ "32k e oltre"
  ), levels = bin_levels))

# --- Etichetta valore per paese (migliaia di euro, suffisso k) -------------

fmt_val <- function(v) paste0(round(v / 1000), "k")

skip_label <- c("LI")

adjust_label_xy <- function(cntr, x, y) {
  dx <- case_when(
    cntr == "NO" ~ -150000, cntr == "SE" ~ -100000, cntr == "FI" ~ -120000,
    cntr == "EL" ~  -60000, cntr == "IT" ~  -20000, cntr == "LV" ~  100000,
    cntr == "NL" ~   10000, cntr == "HR" ~  -55000, cntr == "ME" ~   12000,
    cntr == "RS" ~   30000, cntr == "CY" ~  -30000, TRUE ~ 0
  )
  dy <- case_when(
    cntr == "NO" ~ -300000, cntr == "FI" ~ -250000, cntr == "SE" ~ -120000,
    cntr == "IE" ~  -80000, cntr == "BE" ~   25000, cntr == "HR" ~   70000,
    cntr == "EL" ~   30000, cntr == "MT" ~   25000, cntr == "SI" ~   35000,
    cntr == "NL" ~   45000, cntr == "ME" ~  -30000, cntr == "AL" ~  -25000,
    cntr == "RS" ~   20000, TRUE ~ 0
  )
  list(x = x + dx, y = y + dy)
}

geo_labels <- geo_dati %>%
  filter(!is.na(valore), !CNTR_ID %in% skip_label) %>%
  mutate(
    label_value = fmt_val(valore),
    label_color = case_when(
      CNTR_ID == "MT"    ~ "#1C1C1C",
      bin %in% bin_scuri ~ "white",
      TRUE               ~ "#1C1C1C"
    )
  )

centroidi <- mainland_centroids(geo_labels)
coords    <- st_coordinates(centroidi)
geo_labels$label_x <- coords[, "X"]
geo_labels$label_y <- coords[, "Y"]
adj <- adjust_label_xy(geo_labels$CNTR_ID, geo_labels$label_x, geo_labels$label_y)
geo_labels$label_x <- adj$x
geo_labels$label_y <- adj$y

# --- Mappa ------------------------------------------------------------------

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
  geom_text(data = st_drop_geometry(geo_labels),
            aes(x = label_x, y = label_y,
                label = label_value, color = label_color),
            family = "Source Sans Pro", size = 2.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(values = bin_colours, drop = FALSE,
                    na.value = COL_NA_MAPPA, name = NULL, breaks = bin_levels) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.7, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")],
           ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.86),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "In Italia il reddito mediano è in linea con la media europea",
    subtitle = paste0(
      "Reddito netto mediano equivalente, in standard di potere d'acquisto (SPA), ",
      ultimo_anno),
    caption = CAP_EUROSTAT
  )

ggsave("../output/reddito_mediano_europa.png",
       plot = p, width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/reddito_mediano_europa.png\n")
