# =============================================================================
# Espatri, rimpatri e saldo migratorio dei cittadini italiani, 1872-2024
#
# Fonti (Istat):
#   - Tavola_2.9.xls          -> serie storica espatriati/rimpatriati 1869-2014
#   - tavole-appendice_2024.xlsx -> foglio "tavola_sintetica", 2014-2024
#     (espatri = Emigrazioni Italiani, rimpatri = Immigrazioni Italiani:
#      il 2014 combacia esattamente fra i due file, la serie e' continua)
#
# Barre = dato annuale grezzo; linee = medie mobili centrate a 3 anni.
# Eseguito da: cd script && Rscript espatri_rimpatri_italia.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(showtext)
  library(ggrepel)
})

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

# --- Tema --------------------------------------------------------------------
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO         <- "#1C1C1C"
COL_BLU          <- "#0478EA"
COL_ROSSO        <- "#F12938"
COL_ROSSO_CHIARO <- "#F6A2AA"
COL_BLU_CHIARO   <- "#A1C6EE"
COL_GRIGIO       <- "#9A9A9A"
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
      legend.box.background = element_blank(),
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

CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

# --- 1) Serie storica 1869-2014 (Tavola 2.9, tre fogli) ----------------------
file_hist <- file.path(input_dir, "Tavola_2.9.xls")

leggi_foglio <- function(idx) {
  raw <- read_excel(file_hist, sheet = idx, col_names = FALSE, col_types = "text")
  tibble(anno_raw = raw[[1]], espatri = raw[[2]], rimpatri = raw[[3]]) %>%
    mutate(anno = as.integer(str_extract(anno_raw, "^\\d{4}"))) %>%
    filter(!is.na(anno)) %>%
    transmute(
      anno,
      espatri  = suppressWarnings(as.numeric(espatri)),
      rimpatri = suppressWarnings(as.numeric(rimpatri))
    )
}

storico <- map_dfr(1:3, leggi_foglio) %>%
  filter(anno <= 2014) %>%
  distinct(anno, .keep_all = TRUE)

# --- 2) Serie recente 2015-2024 (appendice, solo cittadini italiani) ---------
file_app <- file.path(input_dir, "tavole-appendice_2024.xlsx")
raw_app  <- read_excel(file_app, sheet = "tavola_sintetica",
                       col_names = FALSE, col_types = "text")
# col 1 = anno | col 5 = Immigrazioni Italiani | col 8 = Emigrazioni Italiani
recente <- tibble(anno_raw = raw_app[[1]],
                  rimpatri = raw_app[[5]],
                  espatri  = raw_app[[8]]) %>%
  mutate(anno = as.integer(str_extract(anno_raw, "^\\d{4}"))) %>%
  filter(!is.na(anno), anno >= 2015) %>%
  transmute(
    anno,
    espatri  = suppressWarnings(as.numeric(espatri)),
    rimpatri = suppressWarnings(as.numeric(rimpatri))
  )

# --- 3) Serie unica 1872-2024 su griglia completa ----------------------------
# La griglia completa tiene gli anni senza dato (1943-45) e il buco dei
# rimpatri (1877-1904) come NA: cosi' le barre spariscono e le linee si spezzano.
dati <- tibble(anno = 1872:2024) %>%
  left_join(bind_rows(storico, recente), by = "anno") %>%
  arrange(anno) %>%
  mutate(saldo = rimpatri - espatri)

# --- 4) Medie mobili centrate a 3 anni ---------------------------------------
# Calcolate per blocco di anni consecutivi, cosi' non scavalcano i buchi.
# Finestra parziale ai bordi di ogni blocco (media dei 2 anni disponibili).
calc_ma <- function(df, col) {
  df %>%
    transmute(anno, valore = .data[[col]]) %>%
    filter(!is.na(valore)) %>%
    arrange(anno) %>%
    mutate(blocco = cumsum(c(TRUE, diff(anno) > 1))) %>%
    group_by(blocco) %>%
    mutate(ma = rowMeans(cbind(lag(valore), valore, lead(valore)), na.rm = TRUE)) %>%
    ungroup()
}

espatri_ma  <- calc_ma(dati, "espatri")
rimpatri_ma <- calc_ma(dati, "rimpatri")
saldo_ma    <- calc_ma(dati, "saldo")

dati <- dati %>%
  left_join(select(espatri_ma,  anno, espatri_ma  = ma), by = "anno") %>%
  left_join(select(rimpatri_ma, anno, rimpatri_ma = ma), by = "anno") %>%
  left_join(select(saldo_ma,    anno, saldo_ma    = ma), by = "anno")

write_csv(dati, file.path(output_dir, "espatri_rimpatri_italia.csv"))

# --- 5) Grafico --------------------------------------------------------------

# Etichette inline a destra, agganciate alla fine delle medie mobili (2024).
fine_espatri  <- espatri_ma  %>% slice_max(anno, n = 1)
fine_rimpatri <- rimpatri_ma %>% slice_max(anno, n = 1)
fine_saldo    <- saldo_ma    %>% slice_max(anno, n = 1)
label_serie <- tibble(
  anno  = 2024,
  y     = c(-fine_espatri$ma, fine_rimpatri$ma, fine_saldo$ma),
  testo = c("Espatri", "Rimpatri", "Saldo"),
  col   = c(COL_ROSSO, COL_BLU, COL_NERO)
)

fmt_k <- function(x) ifelse(x == 0, "0", paste0(abs(x) / 1000, "k"))

p <- ggplot(dati, aes(x = anno)) +
  geom_col(aes(y = -espatri), fill = COL_ROSSO_CHIARO, width = 0.9) +
  geom_col(aes(y = rimpatri), fill = COL_BLU_CHIARO,   width = 0.9) +
  geom_hline(yintercept = 0, color = COL_NERO, linewidth = 0.35) +
  geom_line(data = espatri_ma,  aes(y = -ma, group = blocco),
            color = COL_ROSSO, linewidth = 1) +
  geom_line(data = rimpatri_ma, aes(y = ma, group = blocco),
            color = COL_BLU,   linewidth = 1) +
  geom_line(data = saldo_ma,    aes(y = ma, group = blocco),
            color = COL_NERO,  linewidth = 1) +
  annotate("text", x = 1890.5, y = 90000,
           label = "Rimpatri\nnon rilevati\n1877-1904",
           family = "Source Sans Pro", size = 4.3, color = COL_GRIGIO_SCURO,
           lineheight = 0.95) +
  geom_text_repel(data = label_serie,
                  aes(x = anno, y = y, label = testo, color = col),
                  hjust = 0, nudge_x = 2, direction = "y",
                  family = "Source Sans Pro", fontface = "bold", size = 4.8,
                  segment.color = COL_GRIGIO, segment.size = 0.3,
                  min.segment.length = 0, box.padding = 0.35, seed = 1) +
  scale_color_identity() +
  scale_x_continuous(breaks = c(1872, 1900, 1925, 1950, 1975, 2000, 2024),
                     limits = c(1870, 2046),
                     expand = expansion(mult = c(0.005, 0.005))) +
  scale_y_continuous(breaks = seq(-800000, 200000, 200000),
                     labels = fmt_k,
                     expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title = "Tornano molti meno italiani di quelli che emigrano",
    subtitle = "Espatri e rimpatri annuali di cittadini italiani in barre.\nIn linea le medie mobili a 3 anni di espatri, rimpatri e saldo, Italia, 1872-2024",
    caption = CAP_ISTAT
  ) +
  theme_linechart() +
  # Testi ingranditi: il grafico e' piu' largo del riferimento da 8 pollici
  # della skill, quindi i font vanno scalati per restare proporzionati.
  theme(legend.position = "none",
        plot.title    = element_text(size = 18),
        plot.subtitle = element_text(size = 12),
        plot.caption  = element_text(size = 12),
        axis.text     = element_text(size = 12))

ggsave(file.path(output_dir, "espatri_rimpatri_italia.png"),
       plot = p, width = 10.5, height = 7.5, units = "in", dpi = 220, bg = "white")

message("Fatto: output/espatri_rimpatri_italia.png")
