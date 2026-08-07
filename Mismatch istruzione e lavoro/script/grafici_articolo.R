# ============================================================
# Mismatch istruzione-lavoro - Grafici articolo
# 01_andamento.png - serie storica 2015-2025, doppio asse:
#   barre  = numero di laureati sovraistruiti (migliaia, scala sx)
#   linea  = tasso di sovraistruzione (%, scala dx)
# ============================================================

library(tidyverse)
library(showtext)
library(sf)

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO       <- "#1C1C1C"
COL_BLU        <- "#0478EA"
COL_BLU_CHIARO <- "#A1C6EE"
COL_ROSSO      <- "#F12938"
COL_GRIGIO     <- "#9A9A9A"
COL_GRIGIO_SC  <- "#5A5A5A"
COL_AZZURRINO  <- "#CBD5E1"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = COL_NERO, hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 10, color = COL_NERO, hjust = 0),
      plot.title = element_text(size = 14, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su microdati Istat"
CAP_EURO  <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"
out_dir   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Mismatch istruzione e lavoro/output"

# Percentuale italiana: 1 decimale, virgola, simbolo %
fmt_pct1_force <- function(v) paste0(
  formatC(v, format = "f", digits = 1, decimal.mark = ","), "%")

# --- Dati: media annuale da serie trimestrale ---
ann <- read_csv(file.path(out_dir, "v10_serie_trimestrale_2015_2025.csv"),
                show_col_types = FALSE) %>%
  group_by(anno) %>%
  summarise(sovra_mig = mean(sovra) / 1000,
            base_mig  = mean(base) / 1000, .groups = "drop") %>%
  mutate(tasso = sovra_mig / base_mig * 100)

# --- Trasformazione asse secondario (% -> scala conteggi) ---
LEFT_MAX  <- 1400
RIGHT_MAX <- 30
to_left   <- function(p) p / RIGHT_MAX * LEFT_MAX

# Formatter asse sinistro: "600 mila" sotto il milione, "1,2 mln" sopra
fmt_left <- function(x) ifelse(
  x == 0, "0",
  ifelse(x < 1000,
         paste0(format(x, big.mark = ".", decimal.mark = ","), " mila"),
         paste0(format(x / 1000, decimal.mark = ","), " mln")))

# Etichette di valore sulle barre (prima e ultima)
lab_barre <- ann %>% filter(anno %in% c(2015, 2025)) %>%
  mutate(txt = c("893 mila", "1,28 mln"))

# Etichette % del tasso, prima e ultima, sotto la linea
lab_linea <- ann %>% filter(anno %in% c(2015, 2025)) %>%
  mutate(txt = paste0(formatC(tasso, format = "f", digits = 1, decimal.mark = ","), "%"))

# Etichette serie
lab_serie <- tibble(
  anno  = c(2015,         2019.2),
  y     = c(1355,         to_left(19.2) - 95),
  txt   = c("Numero di laureati sovraistruiti", "Tasso di sovraistruzione"),
  col   = c(COL_BLU,      COL_ROSSO),
  h     = c(0,            0)
)

p <- ggplot(ann, aes(anno)) +
  geom_col(aes(y = sovra_mig), fill = COL_BLU_CHIARO, width = 0.66) +
  geom_line(aes(y = to_left(tasso)), color = COL_ROSSO, linewidth = 1.0) +
  geom_point(aes(y = to_left(tasso)), color = COL_ROSSO, size = 1.8) +
  geom_text(data = lab_linea,
            aes(y = to_left(tasso), label = txt),
            vjust = 1.9, family = "Source Sans Pro", fontface = "bold",
            size = 3.2, color = COL_ROSSO) +
  geom_text(data = lab_barre,
            aes(y = sovra_mig, label = txt),
            vjust = -0.8, family = "Source Sans Pro", fontface = "bold",
            size = 3.3, color = COL_BLU) +
  geom_text(data = lab_serie,
            aes(y = y, label = txt, color = col, hjust = h),
            family = "Source Sans Pro", fontface = "bold", size = 3.5) +
  scale_color_identity() +
  scale_y_continuous(
    name   = NULL,
    limits = c(0, LEFT_MAX), breaks = seq(0, 1200, 300),
    labels = fmt_left,
    expand = c(0, 0),
    sec.axis = sec_axis(~ . / LEFT_MAX * RIGHT_MAX,
                        name   = NULL,
                        breaks = seq(0, 30, 5),
                        labels = function(x) paste0(x, "%"))
  ) +
  scale_x_continuous(breaks = seq(2015, 2025, 1),
                     expand = expansion(mult = c(0.03, 0.055))) +
  labs(title = "I laureati sovraistruiti sono sempre di più",
       subtitle = paste0(
         "Numero di laureati che svolgono un lavoro a più bassa qualifica (barre, scala sinistra) e loro quota\n",
         "sul totale dei laureati occupati (linea, scala destra). Italia, 2015-2025."),
       caption = CAP_ISTAT) +
  theme_linechart() +
  theme(
    legend.position = "none",
    axis.text.y        = element_text(color = COL_BLU),
    axis.text.y.right  = element_text(color = COL_ROSSO),
    axis.line.y.right  = element_line(linewidth = 0.3),
    panel.grid.major.y = element_line(color = "#EEEEEE", linewidth = 0.3)
  )

ggsave(file.path(out_dir, "01_andamento.png"), p,
       width = 8, height = 6.5, dpi = 220, bg = "white")
cat("[OK] 01_andamento.png\n")

# ====================================================================
# 02. GRADIENTE PER CAMPO DI STUDIO (2025)
# ====================================================================
campo <- read_csv(file.path(out_dir, "v01_per_campo_studio_2025.csv"),
                  show_col_types = FALSE) %>%
  filter(field != "001") %>%   # esclude "Programmi generici" (base minima, n=56)
  mutate(
    campo = recode(campo,
      "Medico-sanitario-farmaceutico"       = "Medico-sanitario",
      "Architettura/Ing. civile"            = "Architettura e ing. civile",
      "Ingegneria industriale/informazione" = "Ingegneria industriale",
      "Letterario-umanistico-linguistico"   = "Umanistico e linguistico",
      "Informatica/ICT"                     = "Informatica",
      "Agrario-forestale-veterinario"       = "Agrario e veterinario",
      "Servizi"                             = "Turismo, sport e sicurezza"),
    campo = fct_reorder(campo, tasso_sovra))

media_it <- 20.5

p2 <- ggplot(campo, aes(tasso_sovra, campo)) +
  geom_col(fill = COL_BLU, width = 0.72) +
  geom_vline(xintercept = media_it, linetype = "dashed",
             color = COL_GRIGIO, linewidth = 0.4) +
  annotate("text", x = media_it + 0.5, y = 0.7, label = "media Italia, 20%",
           hjust = 0, vjust = 0.5, family = "Source Sans Pro",
           size = 2.9, color = COL_GRIGIO_SC) +
  geom_text(aes(label = fmt_pct1_force(tasso_sovra)),
            hjust = -0.2, family = "Source Sans Pro", fontface = "bold",
            size = 3.2, color = COL_NERO) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)),
                     limits = c(0, max(campo$tasso_sovra) * 1.13)) +
  labs(title = "La sovraistruzione dipende molto dal campo di studio",
       subtitle = "Quota di laureati occupati in un lavoro a più bassa qualifica, per area disciplinare. Italia, 2025.",
       caption = CAP_ISTAT) +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_line(linewidth = 0.3),
        axis.text.y = element_text(size = 10, color = COL_NERO))

ggsave(file.path(out_dir, "02_campo_studio.png"), p2,
       width = 8, height = 6.5, dpi = 220, bg = "white")
cat("[OK] 02_campo_studio.png\n")

# ====================================================================
# 03. CONFRONTO EUROPEO - mappa choropleth (Eurostat, 25-64, 2025)
# ====================================================================
geo <- load_geo_europa()

eu_map <- read_csv(file.path(out_dir, "e01_overqual_UE_2025.csv"),
                   show_col_types = FALSE) %>%
  filter(geo != "EU27_2020") %>%
  transmute(CNTR_ID = geo, valore = tasso)

bin_levels  <- c("meno del 10%", "dal 10 al 15%", "dal 15 al 20%",
                 "dal 20 al 25%", "dal 25 al 30%", "oltre il 30%")
bin_colours <- c(
  "meno del 10%"  = "#EAF1FA",
  "dal 10 al 15%" = "#CCDFF4",
  "dal 15 al 20%" = "#A1C6EE",
  "dal 20 al 25%" = "#5C9CDE",
  "dal 25 al 30%" = "#1F6FC7",
  "oltre il 30%"  = "#0E4F95")
bin_scuri <- c("dal 20 al 25%", "dal 25 al 30%", "oltre il 30%")

geo_dati <- geo %>%
  left_join(eu_map, by = "CNTR_ID") %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore < 10   ~ "meno del 10%",
    valore < 15   ~ "dal 10 al 15%",
    valore < 20   ~ "dal 15 al 20%",
    valore < 25   ~ "dal 20 al 25%",
    valore < 30   ~ "dal 25 al 30%",
    TRUE          ~ "oltre il 30%"
  ), levels = bin_levels))

fmt_pct_int <- function(v) paste0(round(v), "%")

geo_labels <- geo_dati %>%
  filter(!is.na(valore)) %>%
  mutate(label_value = fmt_pct_int(valore),
         label_color = case_when(
           CNTR_ID %in% c("MT", "LU", "CY") ~ "#1C1C1C",
           bin %in% bin_scuri               ~ "white",
           TRUE                             ~ "#1C1C1C"))

centroidi <- mainland_centroids(geo_labels)
coords    <- sf::st_coordinates(centroidi)
geo_labels$label_x <- coords[, "X"]
geo_labels$label_y <- coords[, "Y"]

adj_x <- function(c, x) case_when(
  c == "SE" ~ x - 100000, c == "EL" ~ x - 100000, c == "IT" ~ x - 20000,
  c == "LV" ~ x +  60000, c == "LT" ~ x - 20000,  c == "NL" ~ x + 10000,
  c == "MT" ~ x + 160000, c == "FR" ~ x - 30000,  TRUE ~ x)
adj_y <- function(c, y) case_when(
  c == "FI" ~ y - 250000, c == "IE" ~ y - 80000, c == "BE" ~ y + 30000,
  c == "HR" ~ y +  60000, c == "EL" ~ y + 20000, c == "LV" ~ y + 20000,
  c == "LT" ~ y -  20000, c == "MT" ~ y + 360000, c == "DK" ~ y + 30000,
  TRUE ~ y)
geo_labels <- geo_labels %>%
  mutate(label_x = adj_x(CNTR_ID, label_x),
         label_y = adj_y(CNTR_ID, label_y))

p3 <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
  geom_text(data = sf::st_drop_geometry(geo_labels),
            aes(x = label_x, y = label_y, label = label_value, color = label_color),
            family = "Source Sans Pro", size = 2.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(values = bin_colours, drop = FALSE,
                    na.value = COL_NA_MAPPA, name = NULL, breaks = bin_levels) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.75, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0))) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")],
           ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.98, 0.72),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(title = "Dove i laureati sono più sovraistruiti in Europa",
       subtitle = paste0(
         "Quota di laureati tra 25 e 64 anni che svolgono un lavoro a più bassa qualifica, anno 2025.\n",
         "La media dell’Unione europea è del 21 per cento, come l’Italia."),
       caption = CAP_EURO)

ggsave(file.path(out_dir, "03_europa.png"), p3,
       width = 9, height = 9, dpi = 220, bg = "white")
cat("[OK] 03_europa.png\n")
