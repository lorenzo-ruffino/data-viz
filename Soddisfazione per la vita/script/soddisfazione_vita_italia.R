# Quota di persone di 14 anni e più soddisfatte della propria vita (voto da 8
# a 10), per regione italiana, ultimo anno disponibile.
# Eseguito da `cd script && Rscript soddisfazione_vita_italia.R`.
#
# Mappa binned con 7 classi discrete a larghezza costante e scale_fill_manual,
# etichetta del valore su ogni regione.
# Fonte: Istat, indagine Aspetti della vita quotidiana, dataflow
# 83_63_DF_DCCV_AVQ_PERSONE_141 (grado di soddisfazione per la vita nel
# complesso, regioni). I dati grezzi sono in input/istat_*.tsv (scaricati via
# API SDMX): qui si sommano le quote dei punteggi 8, 9 e 10.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
  library(sf)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

# --- Dati: somma quote voti 8 + 9 + 10 per regione ------------------------

raw <- read_tsv("../input/istat_83_63_DF_DCCV_AVQ_PERSONE_141.tsv",
                show_col_types = FALSE)

anno_label <- max(raw$TIME_PERIOD, na.rm = TRUE)

soddisf <- raw %>%
  group_by(REF_AREA) %>%
  summarise(valore = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop")

italia_val <- soddisf$valore[soddisf$REF_AREA == "IT"]

# Crosswalk codici Istat ITTER107 → codici NUTS 2021 delle geometrie
# (Nord-est e Centro hanno cambiato prefisso ITD*→ITH*, ITE*→ITI*).
nuts_da_istat <- c(
  ITC1 = "ITC1", ITC2 = "ITC2", ITC3 = "ITC3", ITC4 = "ITC4",
  ITD1 = "ITH1", ITD2 = "ITH2", ITD3 = "ITH3", ITD4 = "ITH4", ITD5 = "ITH5",
  ITE1 = "ITI1", ITE2 = "ITI2", ITE3 = "ITI3", ITE4 = "ITI4",
  ITF1 = "ITF1", ITF2 = "ITF2", ITF3 = "ITF3", ITF4 = "ITF4",
  ITF5 = "ITF5", ITF6 = "ITF6", ITG1 = "ITG1", ITG2 = "ITG2"
)

dati_reg <- soddisf %>%
  filter(REF_AREA %in% names(nuts_da_istat)) %>%
  mutate(NUTS_ID = unname(nuts_da_istat[REF_AREA])) %>%
  select(NUTS_ID, valore)

stopifnot(nrow(dati_reg) == 21)

cat("Anno:", anno_label, "\n")
cat("Italia:", round(italia_val, 1), "\n")
cat("Range regioni:", paste(round(range(dati_reg$valore), 1), collapse = " - "), "\n")

geo <- load_geo_italia_regioni()
geo_dati <- geo %>% left_join(dati_reg, by = "NUTS_ID")
stopifnot(!any(is.na(geo_dati$valore)))

write_csv(
  st_drop_geometry(geo_dati) %>%
    select(NUTS_ID, regione = NUTS_NAME, soddisfatti_8_10 = valore) %>%
    arrange(desc(soddisfatti_8_10)),
  "../output/soddisfazione_vita_italia.csv"
)

# --- Binning divergente (classi di 3 punti, bianco sulla media italiana) ---
#
# Scala blu-rossa centrata sulla media nazionale (~46%): la classe bianca
# "da 45 a 48%" contiene il valore Italia; sopra sfuma nel blu (più soddisfatti
# della media), sotto nel rosso (meno soddisfatti).
bin_levels <- c("< 42%", "da 42 a 45%", "da 45 a 48%", "da 48 a 51%",
                "da 51 a 54%", "da 54 a 57%", "≥ 57%")
# Palette divergente RdBu: rosso (basso) → bianco (media) → blu (alto).
bin_colours <- setNames(
  c("< 42%"       = "#B2182B",   # rosso scuro
    "da 42 a 45%" = "#F4A582",   # rosso chiaro
    "da 45 a 48%" = "#F7F7F7",   # bianco (media italiana)
    "da 48 a 51%" = "#D1E5F0",   # blu chiaro
    "da 51 a 54%" = "#92C5DE",
    "da 54 a 57%" = "#4393C3",
    "≥ 57%"       = "#2166AC"),  # blu scuro
  bin_levels
)
# Etichetta bianca sui bin scuri, nera sui chiari (in base alla luminanza).
.lum <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  as.numeric(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}
bin_scuri <- bin_levels[.lum(bin_colours) < 0.5]   # → etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    valore <  42 ~ "< 42%",
    valore <  45 ~ "da 42 a 45%",
    valore <  48 ~ "da 45 a 48%",
    valore <  51 ~ "da 48 a 51%",
    valore <  54 ~ "da 51 a 54%",
    valore <  57 ~ "da 54 a 57%",
    TRUE         ~ "≥ 57%"
  ), levels = bin_levels))

# --- Etichetta valore per regione ------------------------------------------

fmt_pct <- function(v) paste0(formatC(v, format = "f", digits = 1,
                                       decimal.mark = ","), "%")

# Aggiustamenti manuali del centroide (metri EPSG:3035) per le regioni
# allungate o piccole. dx > 0 = est, dy > 0 = nord.
adjust_label_xy <- function(nuts, x, y) {
  dx <- case_when(
    nuts == "ITF2" ~  30000,   # Molise: piccola, label a est
    TRUE           ~     0
  )
  dy <- case_when(
    nuts == "ITF5" ~ -10000,   # Basilicata
    nuts == "ITG2" ~ -20000,   # Sardegna
    nuts == "ITG1" ~ -20000,   # Sicilia
    TRUE           ~     0
  )
  list(x = x + dx, y = y + dy)
}

centroidi <- mainland_centroids(geo_dati)
coords    <- st_coordinates(centroidi)
geo_dati$label_x <- coords[, "X"]
geo_dati$label_y <- coords[, "Y"]
adj <- adjust_label_xy(geo_dati$NUTS_ID, geo_dati$label_x, geo_dati$label_y)
geo_dati$label_x <- adj$x
geo_dati$label_y <- adj$y

geo_dati <- geo_dati %>%
  mutate(
    label_value = fmt_pct(valore),
    label_color = if_else(bin %in% bin_scuri, "white", "#1C1C1C")
  )

# --- Mappa ------------------------------------------------------------------

italia_lab <- fmt_pct(italia_val)

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.3) +
  geom_text(data = st_drop_geometry(geo_dati),
            aes(x = label_x, y = label_y,
                label = label_value, color = label_color),
            family = "Source Sans Pro", size = 2.6, fontface = "bold") +
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
    keyheight = unit(0.7, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.95),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "In Italia meno della metà è soddisfatta della propria vita",
    subtitle = paste0(
      "Quota di persone di 14 anni e più che danno un voto da 8 a 10 alla\n",
      "soddisfazione per la propria vita, regioni italiane, ", anno_label,
      ". Italia ", italia_lab, "\n",
      "In blu le regioni sopra la media italiana, in rosso quelle sotto"
    ),
    caption = CAP_ISTAT
  )

ggsave("../output/soddisfazione_vita_italia.png",
       plot = p, width = 8.5, height = 9.5, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/soddisfazione_vita_italia.png\n")
