# Posti autorizzati negli asili nido e servizi educativi per la prima infanzia
# ogni 100 bambini tra 0 e 2 anni, per regione italiana.
# Eseguito da `cd script && Rscript asili_nido_italia.R`.
#
# Mappa binned con 7 classi discrete a larghezza costante e scale_fill_manual,
# etichetta del valore su ogni regione.
# Fonte: Istat, report "Asili nido e altri servizi socio-educativi per la
# prima infanzia", tavola 1.9 (totale servizi educativi e posti disponibili
# al 31.12.2023, colonna "posti per 100 bambini 0-2 anni", totale pubblico +
# privato), file tavole_nidi_2023.xlsx. Dato anno educativo 2023/2024.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(showtext)
  library(sf)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

# --- Dati -------------------------------------------------------------------

# Il dato si riferisce all'anno educativo 2023/2024: in mappa, sottotitolo
# e tweet si mostra "2024".
anno_label <- 2024

# Tav 1.9: colonna A (1) = regione, colonna L (12) = numero di posti per 100
# bambini 0-2 anni, totale (titolarità pubblica + privata).
raw <- read_excel(
  "../input/tavole_nidi_2023.xlsx",
  sheet = "Tav1.9", col_names = FALSE
)

dati <- raw %>%
  select(regione = 1, valore = 12) %>%
  filter(!is.na(regione), !is.na(suppressWarnings(as.numeric(valore)))) %>%
  mutate(regione = str_squish(as.character(regione)),
         valore = as.numeric(valore))

# Mappa regione (stringa Istat) → codice NUTS 2024 delle geometrie.
nuts_da_regione <- function(r) {
  rl <- str_to_lower(r)
  case_when(
    str_detect(rl, "piemonte")    ~ "ITC1",
    str_detect(rl, "valle")       ~ "ITC2",
    str_detect(rl, "liguria")     ~ "ITC3",
    str_detect(rl, "lombardia")   ~ "ITC4",
    str_detect(rl, "bolzano")     ~ "ITH1",
    str_detect(rl, "trento")      ~ "ITH2",
    str_detect(rl, "veneto")      ~ "ITH3",
    str_detect(rl, "friuli")      ~ "ITH4",
    str_detect(rl, "emilia")      ~ "ITH5",
    str_detect(rl, "toscana")     ~ "ITI1",
    str_detect(rl, "umbria")      ~ "ITI2",
    str_detect(rl, "marche")      ~ "ITI3",
    str_detect(rl, "lazio")       ~ "ITI4",
    str_detect(rl, "abruzzo")     ~ "ITF1",
    str_detect(rl, "molise")      ~ "ITF2",
    str_detect(rl, "campania")    ~ "ITF3",
    str_detect(rl, "puglia")      ~ "ITF4",
    str_detect(rl, "basilicata")  ~ "ITF5",
    str_detect(rl, "calabria")    ~ "ITF6",
    str_detect(rl, "sicilia")     ~ "ITG1",
    str_detect(rl, "sardegna")    ~ "ITG2",
    TRUE                          ~ NA_character_
  )
}

italia_val <- dati %>% filter(regione == "ITALIA") %>% pull(valore)

dati_reg <- dati %>%
  mutate(NUTS_ID = nuts_da_regione(regione)) %>%
  filter(!is.na(NUTS_ID)) %>%        # esclude Trentino-A.A. aggregato, ripartizioni, ITALIA
  select(NUTS_ID, valore)

stopifnot(nrow(dati_reg) == 21)

cat("Anno educativo 2023/2024 (label", anno_label, ")\n")
cat("Italia:", italia_val, "\n")
cat("Range regioni:", paste(range(dati_reg$valore), collapse = " - "), "\n")

geo <- load_geo_italia_regioni()
geo_dati <- geo %>% left_join(dati_reg, by = "NUTS_ID")

stopifnot(!any(is.na(geo_dati$valore)))

write_csv(
  st_drop_geometry(geo_dati) %>%
    select(NUTS_ID, regione = NUTS_NAME, posti_per_100_bambini = valore) %>%
    arrange(desc(posti_per_100_bambini)),
  "../output/asili_nido_italia.csv"
)

# --- Binning discreto (7 classi da 5 punti) --------------------------------

bin_levels <- c("< 15", "da 15 a 20", "da 20 a 25", "da 25 a 30",
                "da 30 a 35", "da 35 a 40", "≥ 40")
# Rampa blu monocromatica a 7 passi, dal chiaro al scuro.
bin_colours <- setNames(
  colorRampPalette(c("#EAF1FA", "#A1C6EE", "#5C9CDE", "#0E5BAD", "#06366A"))(7),
  bin_levels
)
bin_scuri <- c("da 30 a 35", "da 35 a 40", "≥ 40")  # → etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    valore <  15 ~ "< 15",
    valore <  20 ~ "da 15 a 20",
    valore <  25 ~ "da 20 a 25",
    valore <  30 ~ "da 25 a 30",
    valore <  35 ~ "da 30 a 35",
    valore <  40 ~ "da 35 a 40",
    TRUE         ~ "≥ 40"
  ), levels = bin_levels))

# --- Etichetta valore per regione ------------------------------------------

fmt_val <- function(v) formatC(v, format = "f", digits = 1, decimal.mark = ",")

# Aggiustamenti manuali del centroide (metri EPSG:3035) per le regioni
# allungate o piccole. dx > 0 = est, dy > 0 = nord.
adjust_label_xy <- function(nuts, x, y) {
  dx <- case_when(
    nuts == "ITF2" ~   30000,  # Molise: piccola, etichetta spostata a est
    TRUE           ~      0
  )
  dy <- case_when(
    nuts == "ITF5" ~  -10000,  # Basilicata
    nuts == "ITG2" ~  -20000,  # Sardegna
    nuts == "ITG1" ~  -20000,  # Sicilia
    TRUE           ~      0
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
    label_value = fmt_val(valore),
    label_color = if_else(bin %in% bin_scuri, "white", "#1C1C1C")
  )

# --- Mappa ------------------------------------------------------------------

italia_lab <- formatC(italia_val, format = "f", digits = 1, decimal.mark = ",")

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
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
    title = "Al Sud i posti negli asili nido sono ancora pochi",
    subtitle = paste0(
      "Posti autorizzati negli asili nido e servizi educativi per la prima infanzia\n",
      "ogni 100 bambini tra 0 e 2 anni, regioni italiane, ", anno_label,
      ". Italia ", italia_lab, " posti"
    ),
    caption = CAP_ISTAT
  )

ggsave("../output/asili_nido_italia.png",
       plot = p, width = 8.5, height = 9.5, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/asili_nido_italia.png\n")
