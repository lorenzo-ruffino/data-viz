# ============================================================
# Lavoro da remoto - Grafici articolo
# ============================================================
# Produce 4 PNG:
#   01_serie_storica.png       - andamento trimestrale 2015-2025 con
#                                 break questionario nel 2021 evidenziato
#   02_eta_standardizzata.png  - stacked bar per fascia d'eta, dati
#                                 standardizzati (settore * posizione)
#   03_settori.png             - stacked bar per settore ATECO 12, 2025
#   04_mappa_europa.png        - choropleth Eurostat lfsa_ehomp 2025
#
# Negli stacked bar le due tonalita' di blu rappresentano:
#   - blu scuro  = "Per la maggior parte del tempo" (almeno meta' + esclusivo)
#   - blu chiaro = "Qualche volta" (meno della meta' del tempo)
# Totali a destra, asse X dei valori nascosto, solo asse Y categorico.
# ============================================================

library(tidyverse)
library(showtext)
library(sf)

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

# --- Colori ---
COL_BLU_SCURO  <- "#0E5BAD"   # "Per la maggior parte del tempo"
COL_BLU_CHIARO <- "#A1C6EE"   # "Qualche volta"
COL_NERO       <- "#1C1C1C"
COL_GRIGIO     <- "#9A9A9A"
COL_GRIGIO_SC  <- "#5A5A5A"

# Per la serie storica:
#   - grigio = vecchia indagine (C48, 2015-2020)
#   - blu medio = QC52 qualsiasi (anche solo qualche ora)
#   - blu scuro = QC52 frequente (per la maggior parte del tempo)
COL_VECCHIA   <- "#5A5A5A"
COL_QC52_TUTTI <- "#5C9CDE"
COL_QC52_MAGG  <- "#0E5BAD"

# --- Tema ---
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

base_dir <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Lavoro da remoto"
out_dir  <- file.path(base_dir, "output")
dim_dir  <- file.path(out_dir, "2025_dimensioni")

# Formatter italiano per percentuali
fmt_pct1 <- function(v) ifelse(
  v %% 1 == 0,
  paste0(as.integer(v), "%"),
  paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%")
)
# Variante che forza sempre 1 decimale (per coerenza sui bar charts)
fmt_pct1_force <- function(v) paste0(
  formatC(v, format = "f", digits = 1, decimal.mark = ","), "%"
)

# ====================================================================
# 1. SERIE STORICA con break 2021
# ====================================================================

serie <- read_csv(file.path(out_dir, "lavoro_da_remoto_serie_lungo.csv"),
                  show_col_types = FALSE) %>%
  mutate(t = anno + (trim - 1) / 4)

serie_c48  <- serie %>% filter(era == "C48")
serie_qc52 <- serie %>% filter(era == "QC52")

# Tre label posizionate in zone libere o a fine linea
label_end <- tibble(
  t      = c(2017, 2025.75, 2025.75),
  perc_y = c(7,    9.5,     4.2),
  label  = c("Ore di lavoro a casa\nper accordo col datore\n(vecchia indagine)",
             "Almeno\nun giorno",
             "Per la maggior\nparte del tempo"),
  col    = c(COL_VECCHIA, COL_QC52_TUTTI, COL_QC52_MAGG),
  h      = c(0.5, 1, 1)
)

p1 <- ggplot() +
  # Shading del periodo pandemico (Q2 2020 - Q1 2021)
  annotate("rect", xmin = 2020.25, xmax = 2021.25,
           ymin = -Inf, ymax = Inf,
           fill = "#F4F4F4") +
  annotate("text", x = 2020.75, y = 21,
           label = "Picco\npandemico",
           hjust = 0.5, vjust = 1, family = "Source Sans Pro",
           size = 2.8, color = COL_GRIGIO_SC, lineheight = 0.95) +
  # Linea verticale sul break tra le due serie
  geom_vline(xintercept = 2020.875, color = COL_GRIGIO,
             linewidth = 0.3, linetype = "dashed") +
  # C48: una sola linea (qualsiasi)
  geom_line(data = serie_c48,  aes(t, perc_qualsiasi),
            color = COL_VECCHIA, linewidth = 0.9) +
  geom_point(data = serie_c48, aes(t, perc_qualsiasi),
             color = COL_VECCHIA, size = 1.3) +
  # QC52: due linee (qualsiasi + per la maggior parte del tempo)
  geom_line(data = serie_qc52,  aes(t, perc_qualsiasi),
            color = COL_QC52_TUTTI, linewidth = 0.9) +
  geom_point(data = serie_qc52, aes(t, perc_qualsiasi),
             color = COL_QC52_TUTTI, size = 1.3) +
  geom_line(data = serie_qc52,  aes(t, perc_frequente),
            color = COL_QC52_MAGG, linewidth = 0.9) +
  geom_point(data = serie_qc52, aes(t, perc_frequente),
             color = COL_QC52_MAGG, size = 1.3) +
  # Etichette inline
  geom_text(data = label_end,
            aes(t, perc_y, label = label, color = col, hjust = h),
            vjust = 0, family = "Source Sans Pro",
            fontface = "bold", size = 3.2, lineheight = 0.95) +
  scale_color_identity() +
  scale_x_continuous(breaks = seq(2015, 2025, 1),
                     limits = c(2014.7, 2026),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 20, 5),
                     labels = function(x) paste0(x, "%"),
                     limits = c(0, 22),
                     expand = c(0, 0)) +
  labs(title = "Come è cambiato il lavoro da casa in Italia",
       subtitle = paste0(
         "Quota di occupati che ha lavorato da casa nelle 4 settimane prima dell’intervista, Italia, 2015-2025.\n",
         "Nel 2021 Istat ha cambiato la domanda: le due serie non sono direttamente confrontabili."),
       caption = CAP_ISTAT) +
  theme_linechart() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#EAEAEA", linewidth = 0.3))

ggsave(file.path(out_dir, "01_serie_storica.png"), p1,
       width = 9, height = 6.5, dpi = 220, bg = "white")

cat("[OK] 01_serie_storica.png\n")

# ====================================================================
# 2. ETA STANDARDIZZATA - stacked bar non al 100%
# ====================================================================

eta <- read_csv(file.path(dim_dir, "34_eta_grezza_vs_standardizzata_2025.csv"),
                show_col_types = FALSE) %>%
  mutate(
    maggior_parte = perc_frequente_std,
    qualche_volta = perc_qualsiasi_std - perc_frequente_std,
    totale        = perc_qualsiasi_std,
    fascia        = factor(fascia,
                           levels = c("<20", "20-29", "30-39",
                                      "40-49", "50-59", "60+"))
  )

eta_long <- eta %>%
  select(fascia, maggior_parte, qualche_volta, totale) %>%
  pivot_longer(c(maggior_parte, qualche_volta),
               names_to = "intensita", values_to = "perc") %>%
  mutate(intensita = factor(
    intensita,
    levels = c("qualche_volta", "maggior_parte"),
    labels = c("Qualche volta",
               "Per la maggior parte del tempo")))

eta_tot <- eta %>% select(fascia, totale)

p2 <- ggplot(eta_long, aes(perc, fct_rev(fascia), fill = intensita)) +
  geom_col(width = 0.7) +
  geom_text(data = eta_tot,
            aes(x = totale, y = fct_rev(fascia),
                label = fmt_pct1_force(totale)),
            inherit.aes = FALSE,
            hjust = -0.2, family = "Source Sans Pro",
            fontface = "bold", size = 3.4, color = COL_NERO) +
  scale_fill_manual(
    values = c("Qualche volta"                  = COL_BLU_CHIARO,
               "Per la maggior parte del tempo" = COL_BLU_SCURO),
    breaks = c("Per la maggior parte del tempo", "Qualche volta")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                     limits = c(0, max(eta_tot$totale) * 1.22)) +
  labs(title = "Il lavoro da casa è più diffuso tra i trentenni",
       subtitle = "Quota di occupati per fascia d’età che ha lavorato da casa nel 2025, standardizzata per settore e posizione",
       caption = CAP_ISTAT) +
  theme_linechart() +
  theme(legend.position = "top",
        legend.justification = "left",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_line(linewidth = 0.3),
        axis.text.y = element_text(size = 10, color = COL_NERO))

ggsave(file.path(out_dir, "02_eta_standardizzata.png"), p2,
       width = 9, height = 6.5, dpi = 220, bg = "white")

cat("[OK] 02_eta_standardizzata.png\n")

# ====================================================================
# 3. SETTORI ATECO 12 - stacked bar non al 100%
# ====================================================================

sett <- read_csv(file.path(dim_dir, "07_settore_ateco_12_.csv"),
                 show_col_types = FALSE) %>%
  mutate(
    etichetta = case_when(
      etichetta == "Info e comunicazione"                  ~ "Settore informatico",
      etichetta == "Finanza e assicurazioni"               ~ "Finanza e assicurazioni",
      etichetta == "Immobiliari e servizi alle imprese"    ~ "Servizi alle imprese",
      etichetta == "P.A. e difesa"                          ~ "Pubblica Amministrazione",
      etichetta == "Istruzione, sanita', servizi sociali"  ~ "Istruzione e sanità",
      etichetta == "Altri servizi collettivi e personali"  ~ "Altri servizi alla persona",
      etichetta == "Industria in senso stretto"            ~ "Industria",
      etichetta == "Commercio"                              ~ "Commercio",
      etichetta == "Trasporto e magazzinaggio"             ~ "Trasporti e logistica",
      etichetta == "Costruzioni"                            ~ "Costruzioni",
      etichetta == "Agricoltura, silvicoltura, pesca"      ~ "Agricoltura",
      etichetta == "Alberghi e ristoranti"                  ~ "Alberghi e ristoranti",
      TRUE ~ etichetta),
    maggior_parte = perc_frequente,
    qualche_volta = perc_qualsiasi - perc_frequente,
    totale        = perc_qualsiasi
  ) %>%
  arrange(totale) %>%
  mutate(etichetta = factor(etichetta, levels = etichetta))

sett_long <- sett %>%
  select(etichetta, maggior_parte, qualche_volta, totale) %>%
  pivot_longer(c(maggior_parte, qualche_volta),
               names_to = "intensita", values_to = "perc") %>%
  mutate(intensita = factor(
    intensita,
    levels = c("qualche_volta", "maggior_parte"),
    labels = c("Qualche volta",
               "Per la maggior parte del tempo")))

sett_tot <- sett %>% select(etichetta, totale)

p3 <- ggplot(sett_long, aes(perc, etichetta, fill = intensita)) +
  geom_col(width = 0.72) +
  geom_text(data = sett_tot,
            aes(x = totale, y = etichetta,
                label = fmt_pct1_force(totale)),
            inherit.aes = FALSE,
            hjust = -0.2, family = "Source Sans Pro",
            fontface = "bold", size = 3.2, color = COL_NERO) +
  scale_fill_manual(
    values = c("Qualche volta"                  = COL_BLU_CHIARO,
               "Per la maggior parte del tempo" = COL_BLU_SCURO),
    breaks = c("Per la maggior parte del tempo", "Qualche volta")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                     limits = c(0, max(sett_tot$totale) * 1.18)) +
  labs(title = "Il settore informatico è quello dove si lavora di più da casa",
       subtitle = "Quota di occupati per settore ATECO che ha lavorato da casa nel 2025, per intensità del lavoro da remoto",
       caption = CAP_ISTAT) +
  theme_linechart() +
  theme(legend.position = "top",
        legend.justification = "left",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_line(linewidth = 0.3),
        axis.text.y = element_text(size = 9.5, color = COL_NERO))

ggsave(file.path(out_dir, "03_settori.png"), p3,
       width = 9, height = 6.5, dpi = 220, bg = "white")  # stesso formato di 01 e 02

cat("[OK] 03_settori.png\n")

# ====================================================================
# 4. MAPPA EUROPA - choropleth Eurostat lfsa_ehomp 2025
# ====================================================================

geo <- load_geo_europa()

eu_data <- read_csv(file.path(base_dir, "input/eurostat_lfsa_ehomp_2025.csv"),
                    show_col_types = FALSE) %>%
  filter(age_band == "15-64",
         !geo %in% c("EU27_2020", "EA21")) %>%
  transmute(CNTR_ID = geo, usually)

# Bin discreti a larghezza costante di 3 punti percentuali,
# palette monocromatica blu crescente.
# "usually" = occupati che lavorano da casa per la maggior parte del
# tempo (almeno la meta' delle ore). Range osservato 1,3% - 20,5%.
bin_levels <- c("meno del 3%", "dal 3 al 6%", "dal 6 al 9%",
                "dal 9 al 12%", "dal 12 al 15%", "oltre il 15%")
bin_colours <- c(
  "meno del 3%"     = "#EAF1FA",
  "dal 3 al 6%"     = "#CCDFF4",
  "dal 6 al 9%"     = "#A1C6EE",
  "dal 9 al 12%"    = "#5C9CDE",
  "dal 12 al 15%"   = "#1F6FC7",
  "oltre il 15%"    = "#0E4F95"
)
bin_scuri <- c("dal 9 al 12%", "dal 12 al 15%", "oltre il 15%")

geo_dati <- geo %>%
  left_join(eu_data, by = "CNTR_ID") %>%
  mutate(bin = factor(case_when(
    is.na(usually)   ~ NA_character_,
    usually < 3      ~ "meno del 3%",
    usually < 6      ~ "dal 3 al 6%",
    usually < 9      ~ "dal 6 al 9%",
    usually < 12     ~ "dal 9 al 12%",
    usually < 15     ~ "dal 12 al 15%",
    TRUE             ~ "oltre il 15%"
  ), levels = bin_levels))

geo_labels <- geo_dati %>%
  filter(!is.na(usually)) %>%
  mutate(label_value = paste0(round(usually), "%"),
         label_color = case_when(
           CNTR_ID == "MT"    ~ "#1C1C1C",
           bin %in% bin_scuri ~ "white",
           TRUE               ~ "#1C1C1C"))

centroidi <- mainland_centroids(geo_labels)
coords    <- sf::st_coordinates(centroidi)
geo_labels$label_x <- coords[, "X"]
geo_labels$label_y <- coords[, "Y"]

# Aggiustamenti manuali centroidi
adj_x <- function(c, x) {
  case_when(
    c == "NO" ~ x - 150000,
    c == "SE" ~ x - 100000,
    c == "EL" ~ x - 100000,
    c == "IT" ~ x - 20000,
    c == "LV" ~ x +  50000,
    c == "LT" ~ x -  20000,
    c == "NL" ~ x +  10000,
    c == "MT" ~ x +  95000,
    TRUE      ~ x
  )
}
adj_y <- function(c, y) {
  case_when(
    c == "NO" ~ y - 250000,
    c == "FI" ~ y - 250000,
    c == "IE" ~ y -  80000,
    c == "BE" ~ y +  30000,
    c == "HR" ~ y +  60000,
    c == "EL" ~ y +  20000,
    c == "LV" ~ y +  20000,
    c == "LT" ~ y -  20000,
    c == "MT" ~ y +  25000,
    TRUE      ~ y
  )
}
geo_labels <- geo_labels %>%
  mutate(label_x = adj_x(CNTR_ID, label_x),
         label_y = adj_y(CNTR_ID, label_y))

p4 <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
  geom_text(data = sf::st_drop_geometry(geo_labels),
            aes(x = label_x, y = label_y,
                label = label_value, color = label_color),
            family = "Source Sans Pro", size = 2.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(
    values  = bin_colours,
    drop    = FALSE,
    na.value = COL_NA_MAPPA,
    name    = NULL,
    breaks  = bin_levels
  ) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.75, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = COL_NERO, hjust = 0)
  )) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")],
           ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.98, 0.72),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(title = "L’Italia è quart’ultima in Europa per lavoro abituale da casa",
       subtitle = "Quota di occupati tra 15 e 64 anni che ha lavorato da casa per la maggior parte del tempo\n(almeno la metà delle ore) nelle 4 settimane precedenti l’intervista, anno 2025",
       caption = CAP_EURO)

ggsave(file.path(out_dir, "04_mappa_europa.png"), p4,
       width = 9, height = 9, dpi = 220, bg = "white")

cat("[OK] 04_mappa_europa.png\n")
cat("\nTutti e 4 i grafici salvati in:\n", out_dir, "\n", sep = "")
