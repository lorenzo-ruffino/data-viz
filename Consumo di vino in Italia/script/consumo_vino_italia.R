# Quota di persone (11+) che bevono vino tutti i giorni, per regione, 2025.
# Eseguito da `cd script && Rscript consumo_vino_italia.R`.
#
# Consumo giornaliero = 1-2 bicchieri/giorno (11_BIC_VINO) + oltre mezzo litro
# (11_PIU_VINO), misura HSC (per 100 persone 11+). Mappa regionale binnata.
# Fonte: Istat, Aspetti della vita quotidiana, dataflow 83_85_DF_DCCV_AVQ_PERSONE1_218
# (scaricato via server MCP istat SDMX in input/istat_vino.csv).

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
  library(sf)
})

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

# --- Dati: consumo giornaliero di vino per regione --------------------------

grezzi <- read_csv("../input/istat_vino.csv", show_col_types = FALSE) %>%
  filter(MEASURE == "HSC", DATA_TYPE %in% c("11_BIC_VINO", "11_PIU_VINO"))

ultimo_anno <- max(grezzi$TIME_PERIOD, na.rm = TRUE)

giorno <- grezzi %>%
  filter(TIME_PERIOD == ultimo_anno) %>%
  group_by(REF_AREA) %>%
  summarise(valore = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop")

valore_italia <- giorno$valore[giorno$REF_AREA == "IT"]

# Crosswalk codici Istat (CL_ITTER107, NUTS 2010) -> NUTS_ID GISCO (NUTS 2024).
# Cambiano solo Nord-est (ITD* -> ITH*) e Centro (ITE* -> ITI*).
cw <- c(
  ITC1 = "ITC1", ITC2 = "ITC2", ITC3 = "ITC3", ITC4 = "ITC4",
  ITD1 = "ITH1", ITD2 = "ITH2", ITD3 = "ITH3", ITD4 = "ITH4", ITD5 = "ITH5",
  ITE1 = "ITI1", ITE2 = "ITI2", ITE3 = "ITI3", ITE4 = "ITI4",
  ITF1 = "ITF1", ITF2 = "ITF2", ITF3 = "ITF3", ITF4 = "ITF4",
  ITF5 = "ITF5", ITF6 = "ITF6", ITG1 = "ITG1", ITG2 = "ITG2"
)

regioni <- giorno %>%
  filter(REF_AREA %in% names(cw)) %>%
  mutate(NUTS_ID = cw[REF_AREA])

geo <- load_geo_italia_regioni() %>% st_transform(3035)
geo_dati <- geo %>% left_join(regioni, by = "NUTS_ID")

cat("Ultimo anno:", ultimo_anno, "\n")
cat("Italia:", round(valore_italia, 1), "\n")
cat("Regioni senza dato:",
    paste(geo_dati$NUTS_ID[is.na(geo_dati$valore)], collapse = ", "), "\n")
cat("Classifica:\n")
print(geo_dati %>% st_drop_geometry() %>%
        select(NAME_LATN, valore) %>% arrange(desc(valore)), n = 21)

# Esporta dato pulito
write_csv(
  geo_dati %>% st_drop_geometry() %>%
    transmute(nuts_id = NUTS_ID, regione = NAME_LATN, anno = ultimo_anno,
              quota_vino_giornaliero = round(valore, 1)) %>%
    arrange(desc(quota_vino_giornaliero)),
  "../output/consumo_vino_italia.csv"
)

# --- Binning discreto (classi costanti di 2 punti) --------------------------

bin_levels <- c("meno di 10", "da 10 a 12", "da 12 a 14",
                "da 14 a 16", "da 16 a 18", "18 e oltre")
# Rampa bordeaux monocromatica a 6 passi, dal chiaro (basso) allo scuro (alto).
bin_colours <- setNames(
  colorRampPalette(c("#F6E9ED", "#DC97A9", "#C2607E", "#9A3055", "#5E132F"))(6),
  bin_levels
)
bin_scuri <- c("da 14 a 16", "da 16 a 18", "18 e oltre")  # -> etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <  10  ~ "meno di 10",
    valore <  12  ~ "da 10 a 12",
    valore <  14  ~ "da 12 a 14",
    valore <  16  ~ "da 14 a 16",
    valore <  18  ~ "da 16 a 18",
    TRUE          ~ "18 e oltre"
  ), levels = bin_levels))

# --- Etichetta valore per regione (percentuale intera) ----------------------

fmt_val <- function(v) formatC(v, format = "f", digits = 0, decimal.mark = ",")

pts <- suppressWarnings(st_point_on_surface(st_geometry(geo_dati)))
coords <- st_coordinates(pts)
geo_dati$lx <- coords[, "X"]
geo_dati$ly <- coords[, "Y"]

# Aggiustamenti per regioni piccole/sottili (metri EPSG:3035).
nudge <- function(id, x, y) {
  dx <- case_when(id == "ITC2" ~ -15000, id == "ITC3" ~ -28000,
                  id == "ITH2" ~  -8000, id == "ITF2" ~   8000, TRUE ~ 0)
  dy <- case_when(id == "ITC2" ~  15000, id == "ITH1" ~  10000,
                  id == "ITC3" ~ -52000, id == "ITH2" ~ -12000,
                  id == "ITF5" ~ -10000, TRUE ~ 0)
  list(x = x + dx, y = y + dy)
}
adj <- nudge(geo_dati$NUTS_ID, geo_dati$lx, geo_dati$ly)
geo_dati$lx <- adj$x
geo_dati$ly <- adj$y

geo_dati <- geo_dati %>%
  mutate(label_color = case_when(
    NUTS_ID == "ITC3"  ~ "#1C1C1C",      # Liguria: label nel mare, sempre scura
    bin %in% bin_scuri ~ "white",
    TRUE               ~ "#1C1C1C"
  ))

# --- Mappa ------------------------------------------------------------------

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "white", linewidth = 0.3) +
  geom_text(aes(x = lx, y = ly, label = fmt_val(valore), color = label_color),
            family = "Source Sans Pro", size = 2.7, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(values = bin_colours, drop = FALSE,
                    na.value = COL_NA_MAPPA, name = NULL, breaks = bin_levels) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.7, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(xlim = c(4055456, 5049071), ylim = c(1505000, 2665352),
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm"),
        plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                    margin = margin(t = 0.15, unit = "cm"))) +
  labs(
    title = "In Toscana si beve vino ogni giorno più che nel resto d'Italia",
    subtitle = paste0(
      "Quota di persone di 11 anni e più che bevono vino tutti i giorni, ",
      "per regione, ", ultimo_anno),
    caption = CAP_ISTAT
  )

ggsave("../output/consumo_vino_italia.png",
       plot = p, width = 7.5, height = 8.5, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/consumo_vino_italia.png\n")
