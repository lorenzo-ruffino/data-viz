# biblioteche_italia.R — biblioteche di pubblica lettura per 10 mila abitanti,
# mappa per regione (anno 2022)
# Esecuzione: cd script && Rscript biblioteche_italia.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
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

CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

# 1) CONTEGGIO BIBLIOTECHE ----------------------------------------------------
# Tav. 4.1 — biblioteche di pubblica lettura per titolarità e regione, anno 2022

raw <- read_excel(file.path(input_dir, "Biblioteche_2023.xlsx"),
                  sheet = "Tav. 4.1", skip = 8, col_names = FALSE)

regioni_keep <- c("Piemonte", "Valle d'Aosta - Vallée d'Aoste", "Lombardia",
                  "Bolzano/Bozen", "Trento", "Veneto", "Friuli-Venezia Giulia",
                  "Liguria", "Emilia-Romagna", "Toscana", "Umbria", "Marche",
                  "Lazio", "Abruzzo", "Molise", "Campania", "Puglia",
                  "Basilicata", "Calabria", "Sicilia", "Sardegna")

bib <- raw %>%
  select(regione = 1, totale = 4) %>%
  filter(regione %in% regioni_keep) %>%
  mutate(totale = as.numeric(totale))

nuts_map <- c(
  "Piemonte"                        = "ITC1",
  "Valle d'Aosta - Vallée d'Aoste"  = "ITC2",
  "Lombardia"                       = "ITC4",
  "Bolzano/Bozen"                   = "ITH1",
  "Trento"                          = "ITH2",
  "Veneto"                          = "ITH3",
  "Friuli-Venezia Giulia"           = "ITH4",
  "Liguria"                         = "ITC3",
  "Emilia-Romagna"                  = "ITH5",
  "Toscana"                         = "ITI1",
  "Umbria"                          = "ITI2",
  "Marche"                          = "ITI3",
  "Lazio"                           = "ITI4",
  "Abruzzo"                         = "ITF1",
  "Molise"                          = "ITF2",
  "Campania"                        = "ITF3",
  "Puglia"                          = "ITF4",
  "Basilicata"                      = "ITF5",
  "Calabria"                        = "ITF6",
  "Sicilia"                         = "ITG1",
  "Sardegna"                        = "ITG2"
)

nome_breve <- c(
  "Piemonte"                        = "Piemonte",
  "Valle d'Aosta - Vallée d'Aoste"  = "Valle d'Aosta",
  "Lombardia"                       = "Lombardia",
  "Bolzano/Bozen"                   = "Bolzano",
  "Trento"                          = "Trento",
  "Veneto"                          = "Veneto",
  "Friuli-Venezia Giulia"           = "Friuli-Venezia Giulia",
  "Liguria"                         = "Liguria",
  "Emilia-Romagna"                  = "Emilia-Romagna",
  "Toscana"                         = "Toscana",
  "Umbria"                          = "Umbria",
  "Marche"                          = "Marche",
  "Lazio"                           = "Lazio",
  "Abruzzo"                         = "Abruzzo",
  "Molise"                          = "Molise",
  "Campania"                        = "Campania",
  "Puglia"                          = "Puglia",
  "Basilicata"                      = "Basilicata",
  "Calabria"                        = "Calabria",
  "Sicilia"                         = "Sicilia",
  "Sardegna"                        = "Sardegna"
)

bib <- bib %>% mutate(NUTS_ID = nuts_map[regione],
                      nome = nome_breve[regione])

# 2) POPOLAZIONE RESIDENTE AL 1° GENNAIO 2022 ---------------------------------
# File SDMX Istat (popolazione al 1° gennaio), codici territorio in
# classificazione NUTS 2010: ITD1-ITD5 e ITE1-ITE4 vanno ricodificati
# nella classificazione 2024 (ITH*, ITI*) usata dalle geometrie.

pop <- read_csv(file.path(input_dir, "istat_22_289.csv"),
                show_col_types = FALSE) %>%
  filter(TIME_PERIOD == "2022-01-01",
         nchar(REF_AREA) == 4,
         !REF_AREA %in% c("ITCD", "ITDA", "ITFG")) %>%
  mutate(NUTS_ID = recode(REF_AREA,
                          "ITD1" = "ITH1", "ITD2" = "ITH2", "ITD3" = "ITH3",
                          "ITD4" = "ITH4", "ITD5" = "ITH5",
                          "ITE1" = "ITI1", "ITE2" = "ITI2", "ITE3" = "ITI3",
                          "ITE4" = "ITI4",
                          .default = REF_AREA)) %>%
  select(NUTS_ID, pop = OBS_VALUE)

dati <- bib %>%
  left_join(pop, by = "NUTS_ID") %>%
  mutate(tasso = totale / pop * 10000)

stopifnot(nrow(dati) == 21, !any(is.na(dati$pop)))

# Controllo nazionale
italia <- dati %>% summarise(tot = sum(totale), pop = sum(pop)) %>%
  mutate(tasso = tot / pop * 10000)
cat("Italia:", italia$tot, "biblioteche,",
    round(italia$tasso, 2), "per 10 mila abitanti\n")
dati %>% arrange(desc(tasso)) %>%
  mutate(tasso = round(tasso, 2)) %>% print(n = 21)

# 3) CSV PULITO ---------------------------------------------------------------

dati %>%
  transmute(regione = nome, nuts_id = NUTS_ID,
            biblioteche = totale, popolazione = pop,
            per_10mila_abitanti = round(tasso, 2)) %>%
  arrange(desc(per_10mila_abitanti)) %>%
  write_csv(file.path(output_dir, "biblioteche_italia.csv"))

# 4) BINNING DISCRETO ---------------------------------------------------------
# Classi da 0,5 punti; solo l'ultimo bin è aperto.

bin_levels <- c("Meno di 0,5", "Da 0,5 a 1", "Da 1 a 1,5",
                "Da 1,5 a 2", "Da 2 a 2,5", "2,5 e oltre")

bin_colours <- c(
  "Meno di 0,5" = "#EAF1FA",
  "Da 0,5 a 1"  = "#CFE2F6",
  "Da 1 a 1,5"  = "#A1C6EE",
  "Da 1,5 a 2"  = "#5C9CDE",
  "Da 2 a 2,5"  = "#0E5BAD",
  "2,5 e oltre" = "#06366A"
)

bin_scuri <- c("Da 1,5 a 2", "Da 2 a 2,5", "2,5 e oltre")

# 5) GEOMETRIE + LABEL --------------------------------------------------------

# Geometrie regionali GISCO ad alta risoluzione (1:1M) per confini più netti
geo <- giscoR::gisco_get_nuts(year = "2024", nuts_level = 2,
                              resolution = "01", country = "IT") %>%
  select(NUTS_ID) %>%
  st_transform(3035)
geo_dati <- geo %>%
  left_join(dati %>% select(NUTS_ID, nome, totale, pop, tasso), by = "NUTS_ID") %>%
  mutate(bin = factor(case_when(
    is.na(tasso) ~ NA_character_,
    tasso < 0.5  ~ "Meno di 0,5",
    tasso < 1    ~ "Da 0,5 a 1",
    tasso < 1.5  ~ "Da 1 a 1,5",
    tasso < 2    ~ "Da 1,5 a 2",
    tasso < 2.5  ~ "Da 2 a 2,5",
    TRUE         ~ "2,5 e oltre"
  ), levels = bin_levels))

fmt_num <- function(v) formatC(v, format = "f", digits = 1, decimal.mark = ",")

geo_labels <- geo_dati %>%
  filter(!is.na(tasso)) %>%
  mutate(
    label_value = fmt_num(tasso),
    label_color = if_else(bin %in% bin_scuri, "white", "#1C1C1C")
  )

centroidi <- mainland_centroids(geo_labels)
coords    <- sf::st_coordinates(centroidi)
geo_labels$label_x <- coords[, "X"]
geo_labels$label_y <- coords[, "Y"]

# Aggiustamenti manuali (metri, EPSG:3035): Liguria e Valle d'Aosta hanno il
# centroide fuori posto o la poligonale troppo stretta per la label.
adjust_label_xy <- function(id, x, y) {
  dx <- dplyr::case_when(
    id == "ITC3" ~  35000,   # Liguria: centroide nel golfo, sposto sulla costa
    TRUE         ~      0
  )
  dy <- dplyr::case_when(
    id == "ITC3" ~  40000,
    id == "ITC2" ~   8000,   # Valle d'Aosta: leggermente più in alto nella valle
    TRUE         ~      0
  )
  list(x = x + dx, y = y + dy)
}
adj <- adjust_label_xy(geo_labels$NUTS_ID, geo_labels$label_x, geo_labels$label_y)
geo_labels$label_x <- adj$x
geo_labels$label_y <- adj$y

# 6) MAPPA --------------------------------------------------------------------

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "white", linewidth = 0.3) +
  geom_text(data = sf::st_drop_geometry(geo_labels),
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
    keyheight = unit(0.5, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.93, 0.84),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "Più biblioteche a Bolzano e in Valle d'Aosta",
    subtitle = "Biblioteche di pubblica lettura ogni 10 mila abitanti per regione, Italia, 2022.\nSono comprese le biblioteche statali e non statali aperte al pubblico.",
    caption = CAP_ISTAT
  )

ggsave(file.path(output_dir, "biblioteche_italia.png"),
       plot = p, width = 8.5, height = 9, dpi = 220, bg = "white")
