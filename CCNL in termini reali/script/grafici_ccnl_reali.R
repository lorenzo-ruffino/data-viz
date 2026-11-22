# =============================================================================
# Retribuzioni contrattuali per CCNL al netto dell'inflazione, 2005-2025
# Fonti: Istat 155_318_DF_DCSC_RETRCONTR1C_4 (indice retribuzione contrattuale
#        per dipendente, base dic. 2021 = 100, totale dipendenti esclusi
#        dirigenti, dati annuali)
#        Eurostat prc_hicp_aind (IPCA Italia, CP00, INX_A_AVG) per deflazionare
# Dati gia' scaricati in ../input/
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
  library(ggrepel)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"
COL_VERDE  <- "#1B9E77"
COL_ARANCIO <- "#E07700"
COL_BLU2   <- "#3686D6"
COL_GRIGIO_SCURO <- "#5A5A5A"
COL_MARRONE <- "#8C6D46"
COL_MAGENTA <- "#C2185B"

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

CAP <- "Elaborazione di Lorenzo Ruffino su dati Istat ed Eurostat"

ANNO_BASE <- 2005
ANNO_MAX  <- 2025
# Fiammata dell'inflazione: IPCA +8,7% nel 2022 e +5,9% nel 2023
BANDA <- c(2021, 2023)

# --- Dati --------------------------------------------------------------------

ccnl <- read_csv("../input/istat_155_318_4_annuale.csv",
                 col_types = cols(.default = col_character())) %>%
  transmute(codice = MAIN_AGREEMENT_GROUP,
            anno   = as.integer(substr(TIME_PERIOD, 1, 4)),
            indice = as.numeric(OBS_VALUE))

hicp <- read_csv("../input/eurostat_hicp_italia.csv", show_col_types = FALSE) %>%
  transmute(anno = as.integer(anno), ipca = hicp)

nomi <- c(
  # Grandi contratti
  Z0720 = "Metalmeccanici",
  Z2120 = "Credito",
  Z0200 = "Alimentari",
  Z1480 = "Logistica",
  Z3060 = "Sanità pubblica",
  Z1180 = "Edilizia",
  Z1260 = "Commercio",
  Z1840 = "Turismo e ristorazione",
  Z2340 = "Pulizie",
  Z3180 = "Scuola",
  # I piu' colpiti
  Z1520 = "Trasporto aereo",
  Z1960 = "Giornalisti",
  Z2420 = "Cliniche private",
  Z1680 = "Corrieri espressi",
  Z2320 = "Vigilanza privata",
  # Riferimento
  Z3620 = "Totale economia"
)

reale <- ccnl %>%
  inner_join(hicp, by = "anno") %>%
  filter(anno >= ANNO_BASE, anno <= ANNO_MAX) %>%
  group_by(codice) %>%
  filter(any(anno == ANNO_BASE)) %>%
  mutate(nominale = indice / indice[anno == ANNO_BASE] * 100,
         reale    = (indice / ipca) / (indice[anno == ANNO_BASE] /
                                         ipca[anno == ANNO_BASE]) * 100) %>%
  ungroup()

# Dato pulito esportato: tutti i contratti con serie dal 2005
reale %>%
  mutate(contratto = unname(nomi[codice])) %>%
  select(codice, contratto, anno, indice_istat_dic2021 = indice,
         nominale_2005_100 = nominale, reale_2005_100 = reale) %>%
  mutate(across(c(nominale_2005_100, reale_2005_100), ~ round(.x, 2))) %>%
  write_csv("../output/ccnl_reali_2005_2025.csv")

# --- Funzione grafico --------------------------------------------------------

grafico <- function(codici, colori, titolo, sottotitolo, y_lim, y_breaks) {
  dati <- reale %>%
    filter(codice %in% codici) %>%
    mutate(serie = factor(nomi[codice], levels = nomi[codici]))

  col_serie <- setNames(colori, nomi[codici])

  etichette <- dati %>%
    filter(anno == ANNO_MAX) %>%
    mutate(testo = paste0(serie, "  ",
                          formatC(round(reale), format = "d")))

  ggplot(dati, aes(anno, reale, colour = serie, group = serie)) +
    annotate("rect", xmin = BANDA[1], xmax = BANDA[2],
             ymin = -Inf, ymax = Inf, fill = COL_GRIGIO, alpha = 0.15) +
    geom_vline(xintercept = BANDA, colour = COL_GRIGIO, linewidth = 0.3) +
    annotate("text", x = BANDA[2] + 0.25, y = y_lim[2],
             label = "Fiammata\ndell'inflazione", vjust = 1, hjust = 0,
             family = "Source Sans Pro", size = 3.0, colour = "#5A5A5A",
             lineheight = 0.95) +
    geom_hline(yintercept = 100, colour = COL_GRIGIO, linewidth = 0.4,
               linetype = "dashed") +
    geom_line(aes(linewidth = serie == "Totale economia")) +
    geom_point(data = filter(dati, anno == ANNO_MAX), size = 1.6) +
    geom_text_repel(data = etichette, aes(label = testo),
                    hjust = 0, nudge_x = 0.4, direction = "y",
                    family = "Source Sans Pro", fontface = "bold", size = 3.3,
                    segment.colour = COL_GRIGIO, segment.size = 0.3,
                    min.segment.length = 0, box.padding = 0.2,
                    xlim = c(ANNO_MAX + 0.4, NA), seed = 1) +
    scale_colour_manual(values = col_serie) +
    scale_linewidth_manual(values = c(`FALSE` = 0.8, `TRUE` = 1.1)) +
    scale_x_continuous(limits = c(ANNO_BASE, ANNO_MAX + 7.5),
                       breaks = seq(ANNO_BASE, ANNO_MAX, 5),
                       expand = c(0.01, 0)) +
    scale_y_continuous(limits = y_lim, breaks = y_breaks,
                       expand = c(0.01, 0)) +
    coord_cartesian(clip = "off") +
    labs(title = titolo, subtitle = sottotitolo, caption = CAP) +
    theme_linechart() + theme(legend.position = "none")
}

# --- 1. I grandi contratti ---------------------------------------------------

p1 <- grafico(
  codici = c("Z0720", "Z0200", "Z1480", "Z2120", "Z1180", "Z3620", "Z1260", "Z3060",
             "Z1840", "Z2340", "Z3180"),
  colori = c(COL_BLU, COL_VERDE, COL_MARRONE, COL_MAGENTA, COL_GIALLO, COL_NERO, COL_ROSSO,
             COL_BLU2, COL_VIOLA, COL_GRIGIO_SCURO, COL_ARANCIO),
  titolo = "Come è cambiato il valore dei contratti collettivi",
  sottotitolo = paste0(
    "Stipendio contrattuale per dipendente al netto dell'inflazione, ",
    "principali contratti, 2005 = 100, Italia, 2005-2025"),
  y_lim = c(85, 115), y_breaks = seq(85, 115, 5)
)
ggsave("../output/01_grandi_ccnl_reali.png", p1,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 2. I contratti piu' colpiti ---------------------------------------------

p2 <- grafico(
  codici = c("Z3620", "Z2320", "Z1680", "Z2420", "Z1960", "Z1520"),
  colori = c(COL_NERO, COL_GIALLO, COL_VERDE, COL_VIOLA, COL_BLU, COL_ROSSO),
  titolo = "I contratti che hanno perso di più contro l'inflazione",
  sottotitolo = paste0(
    "Stipendio contrattuale per dipendente al netto dell'inflazione, ",
    "contratti con il calo maggiore,\nindice 2005 = 100, Italia, 2005-2025"),
  y_lim = c(60, 110), y_breaks = seq(60, 110, 10)
)
ggsave("../output/02_ccnl_piu_colpiti_reali.png", p2,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Controlli ---------------------------------------------------------------

ipca_var <- hicp$ipca[hicp$anno == ANNO_MAX] / hicp$ipca[hicp$anno == ANNO_BASE]
cat(sprintf("Prezzi al consumo 2005-2025: %+.1f%%\n", (ipca_var - 1) * 100))
reale %>%
  filter(codice %in% names(nomi), anno %in% c(2019, 2021, ANNO_MAX)) %>%
  mutate(contratto = nomi[codice]) %>%
  select(contratto, anno, reale) %>%
  pivot_wider(names_from = anno, values_from = reale, names_prefix = "r") %>%
  arrange(desc(r2025)) %>%
  mutate(across(starts_with("r"), ~ round(.x, 1))) %>%
  as.data.frame() %>%
  print()
