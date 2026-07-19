# Grafici per l'articolo "Com'è messa la sanità italiana"
# 01 - Spesa sanitaria pubblica: valori correnti vs aggiustata per l'inflazione (linea)
# 02 - Quota della sanita sulla spesa pubblica, mappa Europa (binned)
# 03 - Medici vs infermieri per 1.000 abitanti (barre appaiate)

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
library(tidyverse)
library(showtext)
library(sf)

INPUT  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Sistema Sanitario Italiano/input"
OUTPUT <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Sistema Sanitario Italiano/output"

# --- Tema -------------------------------------------------------------------
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = "#1C1C1C", hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0),
      plot.title = element_text(size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

SERIE_REALE <- "Spesa aggiustata per l’inflazione"
SERIE_NOM   <- "Valori nominali"

# ============================================================================
# 01 - SPESA NOMINALE VS REALE
# ============================================================================
spesa <- read_csv(file.path(INPUT, "spesa_ssn_pubblica_reale.csv"), show_col_types = FALSE) %>%
  transmute(anno,
            nom  = spesa_nominale_mln / 1000,
            real = spesa_reale_mln_prezzi2024 / 1000) %>%
  pivot_longer(c(nom, real), names_to = "serie", values_to = "mld") %>%
  mutate(serie = factor(ifelse(serie == "real", SERIE_REALE, SERIE_NOM),
                        levels = c(SERIE_REALE, SERIE_NOM)))

col_serie <- setNames(c(COL_ROSSO, COL_BLU), c(SERIE_REALE, SERIE_NOM))
lt_serie  <- setNames(c("solid", "dashed"), c(SERIE_REALE, SERIE_NOM))

lab_serie <- tibble(
  serie = factor(c(SERIE_REALE, SERIE_NOM), levels = c(SERIE_REALE, SERIE_NOM)),
  anno  = c(1994, 2015),
  mld   = c(143, 110),
  label_text = c("Spesa aggiustata\nper l’inflazione", "Valori nominali")
)
lab_val <- spesa %>% filter(anno %in% c(1995, 2024)) %>%
  mutate(txt = paste0("€ ", format(round(mld), decimal.mark = ","), " mld"))

p1 <- ggplot(spesa, aes(anno, mld, color = serie)) +
  geom_line(aes(linetype = serie), linewidth = 0.9) +
  geom_point(data = lab_val, size = 2.0) +
  geom_text(data = lab_serie, aes(label = label_text), hjust = 0, vjust = 0.5,
            lineheight = 0.92,
            family = "Source Sans Pro", fontface = "bold", size = 3.6,
            show.legend = FALSE) +
  geom_text(data = filter(lab_val, anno == 1995),
            aes(label = txt), hjust = 1.12, vjust = 0.4,
            family = "Source Sans Pro", fontface = "bold", size = 3.2,
            show.legend = FALSE) +
  geom_text(data = filter(lab_val, anno == 2024) %>% distinct(anno, mld, .keep_all = TRUE) %>% slice(1),
            aes(label = txt), hjust = 0, nudge_x = 0.4, vjust = 0.4, color = COL_NERO,
            family = "Source Sans Pro", fontface = "bold", size = 3.2,
            show.legend = FALSE) +
  scale_color_manual(values = col_serie) +
  scale_linetype_manual(values = lt_serie) +
  scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015, 2020, 2024),
                     limits = c(1989, 2032), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(40, 80, 120, 160), limits = c(0, 165),
                     labels = function(x) paste0("€ ", x, " mld"), expand = c(0.01, 0.01)) +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "La spesa per la sanità cresce meno di quanto sembri",
       subtitle = "Spesa sanitaria pubblica, a valori nominali e aggiustata per l’inflazione, Italia, 1995-2024",
       caption = CAP_EUROSTAT)

ggsave(file.path(OUTPUT, "01_spesa_nominale_reale.png"), p1,
       width = 8, height = 6.5, dpi = 220, bg = "white")
message("01 salvato")

# ============================================================================
# 02 - MAPPA: QUOTA DELLA SANITA SULLA SPESA PUBBLICA
# ============================================================================
geo <- load_geo_europa()
dati_map <- read_csv(file.path(INPUT, "spesa_sanita_su_spesa_pubblica_europa.csv"), show_col_types = FALSE) %>%
  filter(!paese %in% c("EU27_2020", "EA", "EA19", "EA20", "CH")) %>%  # CH anomala
  transmute(CNTR_ID = paese, valore = quota_salute_pct_direct)

geo_dati <- geo %>% left_join(dati_map, by = "CNTR_ID")

bin_levels  <- c("meno dell’11%", "11-13%", "13-15%", "15-17%", "17-19%", "19% e oltre")
bin_colours <- c("meno dell’11%" = "#E3EEF9", "11-13%" = "#C2D9F0", "13-15%" = "#93BCE6",
                 "15-17%" = "#5E97D6", "17-19%" = "#2E6DBC", "19% e oltre" = "#0A2E66")
bin_scuri   <- c("15-17%", "17-19%", "19% e oltre")

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <  11  ~ "meno dell’11%",
    valore <  13  ~ "11-13%",
    valore <  15  ~ "13-15%",
    valore <  17  ~ "15-17%",
    valore <  19  ~ "17-19%",
    TRUE          ~ "19% e oltre"
  ), levels = bin_levels))

fmt_pct <- function(v) ifelse(
  v %% 1 == 0,
  paste0(as.integer(v), "%"),
  paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%"))

adjust_label_xy <- function(cntr, x, y) {
  dx <- case_when(cntr == "NO" ~ -150000, cntr == "SE" ~ -100000, cntr == "FI" ~ -120000,
                  cntr == "EL" ~ -60000, cntr == "IT" ~ -20000, cntr == "HR" ~ -40000,
                  cntr == "SI" ~  30000, cntr == "CY" ~ -30000, TRUE ~ 0)
  dy <- case_when(cntr == "NO" ~ -300000, cntr == "FI" ~ -250000, cntr == "SE" ~ -120000,
                  cntr == "IE" ~ -80000, cntr == "HR" ~  90000, cntr == "SI" ~ 20000,
                  cntr == "EL" ~  30000, cntr == "MT" ~  25000, cntr == "DK" ~ 20000,
                  cntr == "AT" ~ -25000, TRUE ~ 0)
  list(x = x + dx, y = y + dy)
}

geo_labels <- geo_dati %>%
  filter(!is.na(valore)) %>%
  mutate(label_value = fmt_pct(valore),
         label_color = case_when(CNTR_ID == "MT" ~ "#1C1C1C",
                                 bin %in% bin_scuri ~ "white",
                                 TRUE ~ "#1C1C1C"))
centroidi <- mainland_centroids(geo_labels)
coords <- sf::st_coordinates(centroidi)
geo_labels$label_x <- coords[, "X"]
geo_labels$label_y <- coords[, "Y"]
adj <- adjust_label_xy(geo_labels$CNTR_ID, geo_labels$label_x, geo_labels$label_y)
geo_labels$label_x <- adj$x
geo_labels$label_y <- adj$y

p2 <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
  geom_text(data = sf::st_drop_geometry(geo_labels),
            aes(x = label_x, y = label_y, label = label_value, color = label_color),
            family = "Source Sans Pro", size = 2.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(values = bin_colours, drop = FALSE,
                    na.value = COL_NA_MAPPA, name = NULL, breaks = bin_levels) +
  guides(fill = guide_legend(reverse = TRUE,
                             keyheight = unit(0.6, "cm"), keywidth = unit(0.45, "cm"),
                             label.theme = element_text(family = "Source Sans Pro",
                                                        size = 9, color = "#1C1C1C", hjust = 0))) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")], ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.98, 0.74), legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm"),
        text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(size = 14, color = "#1C1C1C", hjust = 0,
                                  margin = margin(b = 0.1, unit = "cm")),
        plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                     margin = margin(b = 0.25, unit = "cm")),
        plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                    margin = margin(t = 0.4, unit = "cm"))) +
  labs(title = "In Italia la sanità pesa poco sul bilancio pubblico",
       subtitle = "Spesa pubblica per la sanità in percentuale della spesa pubblica totale, 2023",
       caption = CAP_EUROSTAT)

ggsave(file.path(OUTPUT, "02_quota_sanita_bilancio_europa.png"), p2,
       width = 9, height = 9, dpi = 220, bg = "white")
message("02 salvato")

# ============================================================================
# 03 - MEDICI VS INFERMIERI PER 1.000 ABITANTI
# ============================================================================
leggi_personale <- function(file, etichetta) {
  read_csv(file.path(INPUT, file), show_col_types = FALSE) %>%
    filter(geo %in% c("IT", "DE", "FR")) %>%
    mutate(anno = as.integer(substr(TIME_PERIOD, 1, 4))) %>%
    group_by(geo) %>% slice_max(anno, n = 1, with_ties = FALSE) %>% ungroup() %>%
    transmute(geo, tipo = etichetta, val = OBS_VALUE / 100)
}

medici     <- leggi_personale("eurostat_hlth_rs_prs1_doctors_per100k.csv", "Medici")
infermieri <- leggi_personale("eurostat_hlth_rs_prsns_nurses_per100k.csv", "Infermieri")

nomi <- c(IT = "Italia", DE = "Germania", FR = "Francia")
pers <- bind_rows(medici, infermieri) %>%
  mutate(paese = factor(nomi[geo], levels = c("Italia", "Germania", "Francia")),
         tipo  = factor(tipo, levels = c("Medici", "Infermieri")))

col_tipo <- c("Medici" = COL_ROSSO, "Infermieri" = COL_BLU)

p3 <- ggplot(pers, aes(paese, val, fill = tipo)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7) +
  geom_text(aes(label = format(round(val, 1), nsmall = 1, decimal.mark = ",")),
            position = position_dodge(width = 0.78), vjust = -0.6,
            family = "Source Sans Pro", fontface = "bold", size = 3.6, color = COL_NERO) +
  scale_fill_manual(values = col_tipo) +
  scale_y_continuous(limits = c(0, 16.5), expand = c(0, 0)) +
  theme_linechart() +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11, color = "#1C1C1C")) +
  labs(title = "In Italia ci sono pochi infermieri",
       subtitle = "Medici e infermieri ogni 1.000 abitanti",
       caption = CAP_EUROSTAT)

ggsave(file.path(OUTPUT, "03_medici_infermieri.png"), p3,
       width = 8, height = 6.5, dpi = 220, bg = "white")
message("03 salvato")
message("Fatto.")
