# =============================================================================
# Spesa pubblica italiana a prezzi correnti e a prezzi costanti, 2000-2025
# Fonti: Eurostat gov_10a_main (na_item = TE, sector = S13, unit = MIO_EUR)
#        Eurostat prc_hicp_aind (coicop = CP00, unit = INX_A_AVG) per deflazionare
#        Eurostat nama_10_gdp (unit = PD15_EUR) solo come controllo di robustezza
# Dati gia' scaricati in ../input/
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_BLU   <- "#0478EA"
COL_ROSSO <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

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

ANNO_MIN  <- 2000
ANNO_BASE <- 2025   # anno a cui riportare i prezzi

# --- Dati --------------------------------------------------------------------

spesa <- read_csv("../input/eurostat_gov_10a_main_it.csv", show_col_types = FALSE) %>%
  filter(unit == "MIO_EUR", na_item == "TE", sector == "S13") %>%
  transmute(anno = as.integer(time), spesa_mio = values) %>%
  filter(!is.na(spesa_mio))

hicp <- read_csv("../input/eurostat_prc_hicp_aind_it.csv", show_col_types = FALSE) %>%
  transmute(anno = as.integer(time), ipca = values) %>%
  filter(!is.na(ipca))

# Controllo di robustezza: stessa cosa con il deflatore del PIL
defl_pil <- read_csv("../input/eurostat_nama_10_gdp_it.csv", show_col_types = FALSE) %>%
  filter(unit == "PD15_EUR") %>%
  transmute(anno = as.integer(time), defl = values) %>%
  filter(!is.na(defl))

ipca_base <- hicp$ipca[hicp$anno == ANNO_BASE]
defl_base <- defl_pil$defl[defl_pil$anno == ANNO_BASE]
stopifnot(length(ipca_base) == 1, length(defl_base) == 1)

dati <- spesa %>%
  inner_join(hicp, by = "anno") %>%
  left_join(defl_pil, by = "anno") %>%
  filter(anno >= ANNO_MIN, anno <= ANNO_BASE) %>%
  mutate(
    corrente  = spesa_mio / 1000,                        # miliardi correnti
    reale     = spesa_mio / 1000 * ipca_base / ipca,     # miliardi a prezzi ANNO_BASE
    reale_pil = spesa_mio / 1000 * defl_base / defl      # variante deflatore PIL
  )

# Dato pulito esportato
dati %>%
  transmute(
    anno,
    spesa_miliardi_correnti = round(corrente, 1),
    spesa_miliardi_prezzi_2025 = round(reale, 1),
    indice_prezzi_consumo_2015_100 = ipca,
    spesa_miliardi_prezzi_2025_deflatore_pil = round(reale_pil, 1)
  ) %>%
  write_csv("../output/spesa_pubblica_reale_nominale.csv")

col_serie <- c("A prezzi 2025" = COL_ROSSO, "A prezzi correnti" = COL_BLU)

plot_data <- dati %>%
  select(anno, corrente, reale) %>%
  pivot_longer(-anno, names_to = "serie", values_to = "valore") %>%
  mutate(serie  = factor(serie, levels = c("reale", "corrente"),
                         labels = c("A prezzi 2025", "A prezzi correnti")),
         colore = unname(col_serie[as.character(serie)]))

# --- Etichette ---------------------------------------------------------------

label_serie <- tibble(
  anno   = c(2009.6, 2012.8),
  valore = c(1150, 700),
  colore = c(COL_ROSSO, COL_BLU),
  testo  = c("Spesa al netto dell'inflazione", "Spesa a prezzi correnti")
)

fmt_eur <- function(v) paste0("€ ", formatC(round(v), format = "d",
                                           big.mark = ".", decimal.mark = ","),
                              " mld")

# Nell'anno base le due serie coincidono per costruzione: una sola etichetta,
# in nero, perche' vale per entrambe le linee.
label_estremi <- plot_data %>%
  filter(anno == ANNO_MIN | (anno == ANNO_BASE & serie == "A prezzi 2025")) %>%
  mutate(
    testo = fmt_eur(valore),
    hjust = if_else(anno == ANNO_MIN, 0, 1),
    # Nel 2000 entrambe le etichette vanno sotto il punto: sopra la linea rossa
    # sale subito e ci passerebbe sopra
    vjust = if_else(anno == ANNO_MIN, 2.0, -1.2),
    colore = if_else(anno == ANNO_BASE, COL_NERO,
                     unname(col_serie[as.character(serie)]))
  )

# --- Grafico -----------------------------------------------------------------

p <- ggplot(plot_data, aes(x = anno, y = valore, color = colore, group = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = filter(plot_data, anno %in% c(ANNO_MIN, ANNO_BASE)),
             size = 2.2) +
  geom_text(data = label_estremi,
            aes(label = testo, hjust = hjust, vjust = vjust, color = colore),
            family = "Source Sans Pro", fontface = "bold", size = 3.4,
            show.legend = FALSE) +
  geom_text(data = label_serie,
            aes(label = testo, group = 1), family = "Source Sans Pro",
            fontface = "bold", size = 3.6, hjust = 0, lineheight = 1.1,
            show.legend = FALSE) +
  scale_color_identity(guide = "none",
                       aesthetics = "color") +
  scale_x_continuous(breaks = seq(2000, 2025, 5),
                     limits = c(ANNO_MIN - 0.4, ANNO_BASE + 0.6),
                     expand = c(0, 0)) +
  # Lo zero resta come base dell'asse ma senza etichetta: non aggiunge nulla
  scale_y_continuous(limits = c(0, 1300), breaks = seq(200, 1200, 200),
                     labels = function(x) paste0("€ ", formatC(x, format = "d",
                                                              big.mark = ".",
                                                              decimal.mark = ","),
                                                 " mld"),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Come è cambiata la spesa pubblica in 25 anni",
    subtitle = "Spesa totale delle amministrazioni pubbliche a prezzi correnti e a prezzi 2025, Italia, 2000-2025",
    caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat"
  ) +
  theme_linechart() +
  theme(legend.position = "none")

ggsave("../output/spesa_pubblica_reale_nominale.png", p,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Sanity check ------------------------------------------------------------

r <- function(a) dati %>% filter(anno == a)
cat("\nAnni:", min(dati$anno), "-", max(dati$anno), "| n =", nrow(dati), "\n")
cat(sprintf("Correnti  %d: %.1f mld -> %d: %.1f mld  (%+.1f%%)\n",
            ANNO_MIN, r(ANNO_MIN)$corrente, ANNO_BASE, r(ANNO_BASE)$corrente,
            (r(ANNO_BASE)$corrente / r(ANNO_MIN)$corrente - 1) * 100))
cat(sprintf("Reali     %d: %.1f mld -> %d: %.1f mld  (%+.1f%%)\n",
            ANNO_MIN, r(ANNO_MIN)$reale, ANNO_BASE, r(ANNO_BASE)$reale,
            (r(ANNO_BASE)$reale / r(ANNO_MIN)$reale - 1) * 100))
cat(sprintf("Reali (deflatore PIL) %d -> %d: %+.1f%%\n", ANNO_MIN, ANNO_BASE,
            (r(ANNO_BASE)$reale_pil / r(ANNO_MIN)$reale_pil - 1) * 100))
cat(sprintf("Picco reale: %d (%.1f mld) | reale 2019: %.1f | reale 2024: %.1f\n",
            dati$anno[which.max(dati$reale)], max(dati$reale),
            r(2019)$reale, r(2024)$reale))
cat(sprintf("Reale 2009: %.1f | 2015: %.1f | var. 2009-2019: %+.1f%%\n",
            r(2009)$reale, r(2015)$reale,
            (r(2019)$reale / r(2009)$reale - 1) * 100))
