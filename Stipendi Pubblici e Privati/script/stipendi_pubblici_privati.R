# script/stipendi_pubblici_privati.R — eseguito da `cd script && Rscript stipendi_pubblici_privati.R`

library(tidyverse)
library(showtext)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_BLU   <- "#0478EA"
COL_ROSSO <- "#F12938"

CAP_MISTO <- "Elaborazione di Lorenzo Ruffino su dati Inps e Istat"

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
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

# 1) INPS: redditi e lavoratori per posizione prevalente -----------------------
# Il CSV dell'osservatorio ha 36 righe di intestazione/markup: i dati partono
# dalla riga "2014". Coppie di colonne (redditi, lavoratori); dipendenti
# privati = colonne 9-10, pubblici = 11-12.
righe <- read_lines(file.path(input_dir, "inps_redditi_lavoratori_raw.csv"))
righe_dati <- righe[str_detect(righe, '^"20\\d\\d"')]

parse_num <- function(x) as.numeric(str_remove_all(x, '[."\\s]'))

inps <- map_dfr(righe_dati, function(r) {
  campi <- str_split(r, ";")[[1]]
  tibble(
    anno            = parse_num(campi[1]),
    redditi_priv    = parse_num(campi[9]),
    lavoratori_priv = parse_num(campi[10]),
    redditi_pub     = parse_num(campi[11]),
    lavoratori_pub  = parse_num(campi[12])
  )
})

# 2) FOI: concatenazione base 2010 (2013-2015) e base 2015 (2016-) ------------
# Il TSV contiene numeri indici (MEASURE 4) nelle due basi e le variazioni
# percentuali medie annue (MEASURE 8). Il link 2015->2016 usa la variazione
# del 2016 (base-independent), poi la serie base 2015 viene riscalata.
foi_raw <- read_tsv(file.path(input_dir, "istat_169_750_DF_DCSP_FOI2B2025_1.tsv"),
                    col_types = cols(DATA_TYPE = col_character(),
                                     MEASURE = col_character()))

idx_b2010 <- foi_raw %>% filter(DATA_TYPE == "12", MEASURE == "4") %>%
  select(anno = TIME_PERIOD, foi = OBS_VALUE)
idx_b2015 <- foi_raw %>% filter(DATA_TYPE == "56", MEASURE == "4") %>%
  select(anno = TIME_PERIOD, idx = OBS_VALUE)
var_2016 <- foi_raw %>%
  filter(DATA_TYPE == "56", MEASURE == "8", TIME_PERIOD == 2016) %>%
  pull(OBS_VALUE)

foi_2015 <- idx_b2010 %>% filter(anno == 2015) %>% pull(foi)
foi_2016 <- foi_2015 * (1 + var_2016 / 100)
fattore  <- foi_2016 / (idx_b2015 %>% filter(anno == 2016) %>% pull(idx))

foi <- bind_rows(
  idx_b2010,
  idx_b2015 %>% transmute(anno, foi = idx * fattore)
) %>% arrange(anno)

# 3) Redditi medi reali (euro 2024) -------------------------------------------
foi_2024 <- foi %>% filter(anno == 2024) %>% pull(foi)

dati <- inps %>%
  left_join(foi, by = "anno") %>%
  transmute(
    anno,
    `Dipendenti pubblici` = redditi_pub / lavoratori_pub * foi_2024 / foi,
    `Dipendenti privati`  = redditi_priv / lavoratori_priv * foi_2024 / foi
  ) %>%
  pivot_longer(-anno, names_to = "serie", values_to = "valore")

write_csv(dati %>% pivot_wider(names_from = serie, values_from = valore),
          file.path(output_dir, "stipendi_pubblici_privati.csv"))

# 4) Grafico -------------------------------------------------------------------
colori <- c("Dipendenti pubblici" = COL_ROSSO,
            "Dipendenti privati"  = COL_BLU)

label_serie <- dati %>% filter(anno == max(anno)) %>%
  mutate(label = str_replace(serie, " ", "\n"))

label_valori <- dati %>% filter(anno %in% c(2014, 2024))

fmt_eur <- function(x) paste0("€ ", format(round(x), big.mark = ".", decimal.mark = ","))

p <- ggplot(dati, aes(anno, valore, color = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  geom_text(data = label_serie,
            aes(label = label), hjust = 0, nudge_x = 0.3,
            size = 3.6, fontface = "bold", family = "Source Sans Pro",
            lineheight = 0.95) +
  geom_text(data = label_valori,
            aes(label = fmt_eur(valore),
                hjust = ifelse(anno == 2014, 0, 1)),
            vjust = -1.4, family = "Source Sans Pro", fontface = "bold",
            size = 3.4, show.legend = FALSE) +
  scale_color_manual(values = colori) +
  scale_x_continuous(breaks = seq(2014, 2024, 2),
                     limits = c(2014, 2026.5), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 44000),
                     breaks = seq(0, 40000, 10000),
                     labels = function(x) paste0("€ ", format(x / 1000, big.mark = ".", decimal.mark = ","), "k"),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(
    title = "Gli stipendi reali sono più bassi di dieci anni fa",
    subtitle = "Reddito medio annuo lordo per lavoratore dipendente, euro a prezzi 2024\n(deflazionato con l'indice FOI), Italia, 2014-2024",
    caption = CAP_MISTO
  )

ggsave(file.path(output_dir, "stipendi_pubblici_privati.png"),
       plot = p, width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# Sanity check
dati %>% pivot_wider(names_from = serie, values_from = valore) %>%
  filter(anno %in% c(2014, 2020, 2024)) %>% print()
cat("FOI 2014:", foi %>% filter(anno == 2014) %>% pull(foi),
    "FOI 2024:", foi_2024,
    "inflazione cumulata 2014-2024:",
    round((foi_2024 / (foi %>% filter(anno == 2014) %>% pull(foi)) - 1) * 100, 1), "%\n")
var_pub  <- dati %>% filter(serie == "Dipendenti pubblici") %>%
  summarise(v = valore[anno == 2024] / valore[anno == 2014] - 1) %>% pull(v)
var_priv <- dati %>% filter(serie == "Dipendenti privati") %>%
  summarise(v = valore[anno == 2024] / valore[anno == 2014] - 1) %>% pull(v)
cat("Variazione reale 2014-2024 — pubblici:", round(var_pub * 100, 1),
    "% privati:", round(var_priv * 100, 1), "%\n")
