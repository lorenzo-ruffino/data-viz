# Famiglie dotate di un impianto per il condizionamento dell'aria in casa,
# per regione italiana (valori percentuali sul totale delle famiglie residenti).
# Eseguito da `cd script && Rscript aria_condizionata_italia.R`.
#
# Mappa binned con 7 classi discrete a larghezza costante (10 punti) e
# scale_fill_manual, etichetta del valore su ogni regione.
# Fonte: Istat, Statistiche Report "Consumi energetici delle famiglie",
# Anno 2024, Tavola 3 - "Famiglie dotate di sistemi per il condizionamento
# dell'abitazione, per regione" (colonna "Famiglie dotate di condizionamento").
# File: input/dotazioni_energetiche_raw.xlsx, foglio "3".

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

anno_label <- 2024

# Tavola 3, foglio "3": colonna A (1) = regione, colonna B (2) = "Famiglie
# dotate di condizionamento" (% sul totale delle famiglie residenti).
raw <- read_excel(
  "../input/dotazioni_energetiche_raw.xlsx",
  sheet = "3", col_names = FALSE
)

dati <- raw %>%
  select(regione = 1, valore = 2) %>%
  filter(!is.na(regione), !is.na(suppressWarnings(as.numeric(valore)))) %>%
  mutate(regione = str_squish(as.character(regione)),
         valore = as.numeric(valore))

# Mappa regione (stringa Istat) -> codice NUTS 2024 delle geometrie.
# Bolzano e Trento separati (NUTS-2); "Trentino-Alto Adige" aggregato escluso.
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

italia_val <- dati %>% filter(str_to_upper(regione) == "ITALIA") %>% pull(valore)

dati_reg <- dati %>%
  mutate(NUTS_ID = nuts_da_regione(regione)) %>%
  filter(!is.na(NUTS_ID)) %>%      # esclude Trentino-A.A. aggregato, ripartizioni, ITALIA
  distinct(NUTS_ID, .keep_all = TRUE) %>%
  select(NUTS_ID, valore)

stopifnot(nrow(dati_reg) == 21)

cat("Anno indagine:", anno_label, "\n")
cat("Italia:", italia_val, "\n")
cat("Range regioni:", paste(range(dati_reg$valore), collapse = " - "), "\n")

geo <- load_geo_italia_regioni()
geo_dati <- geo %>% left_join(dati_reg, by = "NUTS_ID")

stopifnot(!any(is.na(geo_dati$valore)))

write_csv(
  st_drop_geometry(geo_dati) %>%
    select(NUTS_ID, regione = NUTS_NAME, quota_condizionamento = valore) %>%
    arrange(desc(quota_condizionamento)),
  "../output/aria_condizionata_italia.csv"
)

# --- Binning discreto (7 classi da 10 punti) -------------------------------

bin_levels <- c("meno di 20%", "da 20% a 30%", "da 30% a 40%", "da 40% a 50%",
                "da 50% a 60%", "da 60% a 70%", "70% e oltre")
# Rampa rossa monocromatica a 7 passi (caldo -> condizionamento), dal chiaro
# allo scuro. Ancorata al rosso di casa #F12938.
bin_colours <- setNames(
  colorRampPalette(c("#FCE4E7", "#F6A2AA", "#F12938", "#A02530", "#5A1018"))(7),
  bin_levels
)
bin_scuri <- c("da 40% a 50%", "da 50% a 60%", "da 60% a 70%", "70% e oltre")  # -> etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    valore <  20 ~ "meno di 20%",
    valore <  30 ~ "da 20% a 30%",
    valore <  40 ~ "da 30% a 40%",
    valore <  50 ~ "da 40% a 50%",
    valore <  60 ~ "da 50% a 60%",
    valore <  70 ~ "da 60% a 70%",
    TRUE         ~ "70% e oltre"
  ), levels = bin_levels))

# --- Etichetta valore per regione ------------------------------------------

fmt_val <- function(v) paste0(round(v), "%")

# Aggiustamenti manuali del centroide (metri EPSG:3035) per le regioni
# allungate o piccole. dx > 0 = est, dy > 0 = nord.
adjust_label_xy <- function(nuts, x, y) {
  dx <- case_when(
    nuts == "ITF2" ~   25000,  # Molise: piccola, etichetta a est
    nuts == "ITC3" ~  -10000,  # Liguria: stretta, etichetta a ovest
    nuts == "ITH4" ~   15000,  # Friuli-Venezia Giulia
    nuts == "ITI4" ~   14000,  # Lazio: a est, lontano dalle isole Ponziane
    TRUE           ~       0
  )
  dy <- case_when(
    nuts == "ITF5" ~  -10000,  # Basilicata
    nuts == "ITG2" ~  -20000,  # Sardegna
    nuts == "ITG1" ~  -15000,  # Sicilia
    nuts == "ITC3" ~  -12000,  # Liguria verso il mare
    nuts == "ITI4" ~   12000,  # Lazio verso l'interno
    TRUE           ~       0
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

italia_lab <- round(italia_val)

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
    title = "Più di metà delle case italiane ha l'aria condizionata",
    subtitle = paste0(
      "Quota di famiglie con un impianto per il condizionamento dell'aria\n",
      "in casa, regioni italiane, ", anno_label, ". In Italia ", italia_lab, "%"
    ),
    caption = CAP_ISTAT
  )

ggsave("../output/aria_condizionata_italia.png",
       plot = p, width = 8.5, height = 9.5, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/aria_condizionata_italia.png\n")
