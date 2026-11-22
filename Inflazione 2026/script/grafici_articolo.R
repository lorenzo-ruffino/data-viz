# Grafici per l'articolo "Perché l'inflazione è tornata a salire"
# 01 - inflazione NIC mensile, 1997-settembre 2026
# 02 - variazione annua dei prezzi per voce, settembre 2026
# 03 - inflazione per quinti di spesa delle famiglie, 2019-2025
# 04 - salari reali per dipendente nei paesi Ue, 2025 rispetto al 2019

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

CAP_ISTAT    <- "Elaborazione di Lorenzo Ruffino su dati Istat"
CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Inflazione 2026"
IN   <- file.path(BASE, "input")
OUT  <- file.path(BASE, "output")

fmt_it <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
fmt_segno <- function(x, d = 1) paste0(ifelse(x > 0, "+", ifelse(x < 0, "−", "")), fmt_it(abs(x), d), "%")

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

# ============================================================================
# GRAFICO 01 - Inflazione NIC mensile 1997-2026
# ============================================================================

nic <- read_csv(file.path(OUT, "nic_tendenziale_mensile_1997_2026.csv"),
                col_types = cols(mese = col_character(), obs_status = col_character())) %>%
  mutate(data = as.Date(paste0(mese, "-01")))
write_csv(nic, file.path(OUT, "01_inflazione_mensile_1997_2026.csv"))

picco <- nic %>% filter(nic_tend == max(nic_tend)) %>% slice_max(data, n = 1)
ultimo <- nic %>% slice_max(data, n = 1)
etichette1 <- bind_rows(picco, ultimo) %>%
  mutate(lab = paste0(fmt_it(nic_tend), "%"),
         hj = c(0.5, 1))

g1 <- ggplot(nic, aes(data, nic_tend)) +
  geom_hline(yintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_hline(yintercept = 2, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = as.Date("2014-01-01"), y = 2, label = "Obiettivo della BCE: 2%",
           hjust = 0, vjust = -0.6, size = 3.1, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro") +
  geom_line(colour = COL_ROSSO, linewidth = 0.8) +
  geom_point(data = etichette1, colour = COL_ROSSO, size = 2.2) +
  geom_text(data = etichette1, aes(label = lab, hjust = hj), vjust = -1.1,
            fontface = "bold", family = "Source Sans Pro", size = 3.6, colour = COL_ROSSO) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y",
               limits = c(as.Date("1997-01-01"), as.Date("2026-12-31")),
               expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(-2, 12, 2), labels = function(x) paste0(x, "%"),
                     limits = c(-1, 13.5), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Sta tornando l'inflazione",
       subtitle = "Variazione dei prezzi al consumo rispetto allo stesso mese dell'anno prima, indice NIC, Italia, 1997-2026",
       caption = CAP_ISTAT) +
  theme_linechart()

ggsave(file.path(OUT, "01_inflazione_mensile_1997_2026.png"), g1,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# ============================================================================
# GRAFICO 02 - Variazione annua per voce, settembre 2026
# ============================================================================

voci <- tribble(
  ~codice, ~voce,
  "00",    "Indice generale",
  "0722",  "Carburanti",
  "0452",  "Gas",
  "0451",  "Elettricità",
  "0117",  "Ortaggi",
  "1120",  "Alberghi e alloggi",
  "0112",  "Carne",
  "0411",  "Affitti",
  "0723",  "Manutenzione dell'auto",
  "1111",  "Ristoranti e bar",
  "0312",  "Abbigliamento",
  "0111",  "Pane, pasta e cereali",
  "0114",  "Latte, formaggi e uova",
  "0611",  "Medicinali",
  "0733",  "Biglietti aerei",
  "0812",  "Smartphone"
)
energia <- c("0722", "0452", "0451")

voci_dati <- read_csv(file.path(OUT, "nic_voci_variazioni_cumulate.csv"),
                      col_types = cols(codice = col_character()), show_col_types = FALSE) %>%
  select(codice, tend = tend_set2026) %>%
  inner_join(voci, by = "codice") %>%
  mutate(gruppo = case_when(codice == "00" ~ "generale",
                            codice %in% energia ~ "energia",
                            TRUE ~ "altro")) %>%
  arrange(tend) %>%
  mutate(voce = factor(voce, levels = voce),
         lab = fmt_segno(tend),
         hj = ifelse(tend >= 0, -0.15, 1.15))
write_csv(voci_dati, file.path(OUT, "02_variazione_per_voce_set2026.csv"))

g2 <- ggplot(voci_dati, aes(tend, voce, fill = gruppo)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_text(aes(label = lab, hjust = hj, colour = gruppo), fontface = "bold",
            family = "Source Sans Pro", size = 3.2) +
  scale_fill_manual(values = c(energia = COL_ROSSO, generale = COL_NERO, altro = COL_BLU),
                    guide = "none") +
  scale_colour_manual(values = c(energia = COL_ROSSO, generale = COL_NERO, altro = COL_NERO),
                      guide = "none") +
  scale_x_continuous(limits = c(-10, 31), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "I prezzi salgono soprattutto per l'energia",
       subtitle = "Variazione dei prezzi rispetto a un anno prima per alcune voci del paniere, Italia, settembre 2026",
       caption = CAP_ISTAT) +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 10),
        axis.text.x = element_blank(),
        axis.line = element_blank())

ggsave(file.path(OUT, "02_variazione_per_voce_set2026.png"), g2,
       width = 8, height = 7, dpi = 220, bg = "white")

# ============================================================================
# GRAFICO 03 - Inflazione per quinti di spesa, 2019-2025
# ============================================================================
# IPCA per quinto di spesa equivalente delle famiglie (base 2015),
# DATA_TYPE 44 = primo quinto (spende meno), 48 = quinto quinto (spende di più),
# MEASURE 7 = variazione tendenziale.

quinti <- read_csv(file.path(IN, "istat_168_760_3_ipca_classi_spesa_mensile_b2015.csv"),
                   col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
  filter(E_COICOP_REV_ISTAT == "00", MEASURE == "7", DATA_TYPE %in% c("44", "48")) %>%
  transmute(data = as.Date(TIME_PERIOD), valore = as.numeric(OBS_VALUE),
            gruppo = ifelse(DATA_TYPE == "44", "Il 20% delle famiglie che spende meno",
                            "Il 20% delle famiglie che spende di più")) %>%
  filter(data >= as.Date("2019-01-01"))
write_csv(quinti, file.path(OUT, "03_inflazione_quinti_2019_2025.csv"))

col_quinti <- c("Il 20% delle famiglie che spende meno" = COL_ROSSO,
                "Il 20% delle famiglie che spende di più" = COL_BLU)

picchi3 <- quinti %>% group_by(gruppo) %>% slice_max(valore, n = 1, with_ties = FALSE) %>% ungroup()
fine3 <- quinti %>% filter(data == max(data))

g3 <- ggplot(quinti, aes(data, valore, colour = gruppo)) +
  geom_hline(yintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_line(linewidth = 0.9) +
  geom_point(data = picchi3, size = 2) +
  geom_text(data = picchi3, aes(label = paste0(fmt_it(valore), "%")),
            vjust = -1, fontface = "bold", family = "Source Sans Pro", size = 3.4,
            show.legend = FALSE) +
  annotate("text", x = as.Date("2023-04-01"), y = 15.2,
           label = "Il 20% delle famiglie\nche spende meno", hjust = 0, lineheight = 0.95,
           colour = COL_ROSSO, fontface = "bold", family = "Source Sans Pro", size = 3.6) +
  annotate("text", x = as.Date("2023-06-01"), y = 9.6,
           label = "Il 20% delle famiglie\nche spende di più", hjust = 0, lineheight = 0.95,
           colour = COL_BLU, fontface = "bold", family = "Source Sans Pro", size = 3.6) +
  scale_colour_manual(values = col_quinti, guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2019-01-01"), as.Date("2025-12-31")),
               expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 20, 4), labels = function(x) paste0(x, "%"),
                     limits = c(-3, 21), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "L'inflazione ha colpito di più le famiglie povere",
       subtitle = "Variazione annua dei prezzi per le famiglie ordinate in base a quanto spendono, indice IPCA, Italia, 2019-2025",
       caption = CAP_ISTAT) +
  theme_linechart()

ggsave(file.path(OUT, "03_inflazione_quinti_2019_2025.png"), g3,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# ============================================================================
# GRAFICO 04 - Salari reali per dipendente, 2025 rispetto al 2019
# ============================================================================

nomi <- c(AT="Austria", BE="Belgio", BG="Bulgaria", HR="Croazia", CY="Cipro",
          CZ="Cechia", DK="Danimarca", EE="Estonia", FI="Finlandia", FR="Francia",
          DE="Germania", EL="Grecia", HU="Ungheria", IE="Irlanda", IT="Italia",
          LV="Lettonia", LT="Lituania", LU="Lussemburgo", MT="Malta", NL="Paesi Bassi",
          PL="Polonia", PT="Portogallo", RO="Romania", SK="Slovacchia", SI="Slovenia",
          ES="Spagna", SE="Svezia", EA20="Area euro")

salari <- read_csv(file.path(IN, "eurostat_salari_reali_per_dipendente.csv"),
                   show_col_types = FALSE) %>%
  filter(geo %in% names(nomi)) %>%
  transmute(geo, paese = nomi[geo], var = reale_vs2019, anno = ultimo_anno) %>%
  arrange(var) %>%
  mutate(paese = factor(paese, levels = paese),
         gruppo = case_when(geo == "IT" ~ "italia", geo == "EA20" ~ "euro", TRUE ~ "altro"),
         lab = fmt_segno(var),
         hj = ifelse(var >= 0, -0.15, 1.15))
write_csv(salari, file.path(OUT, "04_salari_reali_ue_2019_2025.csv"))

g4 <- ggplot(salari, aes(var, paese, fill = gruppo)) +
  geom_col(width = 0.74) +
  geom_vline(xintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_text(aes(label = lab, hjust = hj, colour = gruppo,
                fontface = ifelse(gruppo == "altro", "plain", "bold")),
            family = "Source Sans Pro", size = 2.9) +
  scale_fill_manual(values = c(italia = COL_ROSSO, euro = COL_NERO, altro = COL_BLU),
                    guide = "none") +
  scale_colour_manual(values = c(italia = COL_ROSSO, euro = COL_NERO, altro = COL_NERO),
                      guide = "none") +
  scale_x_continuous(limits = c(-9, 51), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "In Italia i salari reali sono calati più che altrove",
       subtitle = "Variazione della retribuzione media per dipendente al netto dell'inflazione, paesi Ue, 2025 rispetto al 2019",
       caption = CAP_EUROSTAT) +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 8.5),
        axis.text.x = element_blank(),
        axis.line = element_blank())

ggsave(file.path(OUT, "04_salari_reali_ue_2019_2025.png"), g4,
       width = 8, height = 7.5, dpi = 220, bg = "white")

# --- Controlli ---------------------------------------------------------------
cat("Picco NIC:", format(picco$data), picco$nic_tend, "| ultimo:", format(ultimo$data), ultimo$nic_tend, "\n")
cat("Voci set 2026:", paste(voci_dati$voce, voci_dati$tend, sep = "=", collapse = "; "), "\n")
cat("Picchi quinti:", paste(picchi3$gruppo, format(picchi3$data), picchi3$valore, collapse = " | "), "\n")
cat("Fine quinti:", paste(fine3$gruppo, fine3$valore, collapse = " | "), "\n")
cat("Salari: Italia", round(salari$var[salari$geo == "IT"], 1), "area euro",
    round(salari$var[salari$geo == "EA20"], 1), "| ultimo posto:", as.character(salari$paese[1]), "\n")
