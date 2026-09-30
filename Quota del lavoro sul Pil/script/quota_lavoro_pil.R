# script/quota_lavoro_pil.R — eseguito da `cd script && Rscript quota_lavoro_pil.R`
#
# Quota del lavoro sul Pil in Italia, replica della serie "Share of Labour
# Compensation in GDP at Current National Prices" (Penn World Table / FRED)
# con i conti nazionali Istat.
#
# Dati in input scaricati via SDMX (opensdmx, provider istat):
#   - 92_506_DF_DCCN_PILN_1  Pil e principali componenti, prezzi correnti
#   - 92_507_DF_DCCN_OCCNSEC2010_1  unità di lavoro per posizione professionale

library(tidyverse)
library(showtext)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_BLU   <- "#0478EA"
COL_ROSSO <- "#F12938"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = COL_NERO, hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

# --- 1) Dati ----------------------------------------------------------------

conti <- read_csv(file.path(input_dir, "istat_pil_conto_reddito.csv"),
                  col_types = cols(.default = col_character(),
                                   OBS_VALUE = col_double())) %>%
  transmute(anno = as.integer(str_sub(TIME_PERIOD, 1, 4)),
            voce = DATA_TYPE_AGGR,
            valore = OBS_VALUE) %>%
  filter(voce %in% c("B1GQ_B_W2_S1",   # Pil ai prezzi di mercato
                     "D1_D_W2_S1",     # redditi interni da lavoro dipendente
                     "B2A3G_B_W2_S1"), # risultato lordo di gestione e reddito misto
         !is.na(valore)) %>%
  pivot_wider(names_from = voce, values_from = valore) %>%
  rename(pil = B1GQ_B_W2_S1, redditi_dip = D1_D_W2_S1, rlg_misto = B2A3G_B_W2_S1)

# Unità di lavoro (equivalenti a tempo pieno) per posizione professionale:
# 1 = dipendenti, 2 = indipendenti, 9 = totale.
ula <- read_csv(file.path(input_dir, "istat_unita_lavoro.csv"),
                col_types = cols(.default = col_character(),
                                 OBS_VALUE = col_double())) %>%
  transmute(anno = as.integer(str_sub(TIME_PERIOD, 1, 4)),
            posizione = EMPLOYMENT_STATUS,
            ula = OBS_VALUE) %>%
  pivot_wider(names_from = posizione, values_from = ula,
              names_prefix = "ula_") %>%
  rename(ula_dip = ula_1, ula_indip = ula_2, ula_tot = ula_9)

# La quota "grezza" rapporta al Pil i soli redditi da lavoro dipendente e
# quindi ignora il lavoro degli autonomi, il cui reddito finisce nel reddito
# misto. La quota corretta imputa a ogni unità di lavoro indipendente lo stesso
# reddito medio di una dipendente: è la correzione usata da Penn World Table
# e da Ameco per rendere confrontabili paesi con quote di autonomi diverse.
dati <- conti %>%
  inner_join(ula, by = "anno") %>%
  mutate(
    quota_grezza   = redditi_dip / pil * 100,
    quota_corretta = redditi_dip * (ula_tot / ula_dip) / pil * 100,
    quota_indip    = ula_indip / ula_tot * 100
  ) %>%
  arrange(anno)

write_csv(dati, file.path(output_dir, "quota_lavoro_pil.csv"))

ANNO_MIN <- min(dati$anno)
ANNO_MAX <- max(dati$anno)

plot_data <- dati %>%
  select(anno, quota_corretta, quota_grezza) %>%
  pivot_longer(-anno, names_to = "serie", values_to = "quota") %>%
  mutate(serie = factor(serie, levels = c("quota_corretta", "quota_grezza")))

col_serie <- c(quota_corretta = COL_ROSSO, quota_grezza = COL_BLU)

# --- 2) Grafico -------------------------------------------------------------

# Etichette inline nella fascia vuota fra le due linee, ciascuna accostata
# alla propria serie.
label_serie <- tibble(
  serie = factor(c("quota_corretta", "quota_grezza"),
                 levels = levels(plot_data$serie)),
  anno  = c(2003, 2003),
  quota = c(52.4, 41.6),
  testo = c("Compreso il lavoro autonomo", "Solo il lavoro dipendente")
)

anno_min_quota <- dati$anno[which.min(dati$quota_corretta)]

label_valori <- plot_data %>%
  filter(anno %in% c(ANNO_MIN, ANNO_MAX) |
           (serie == "quota_corretta" & anno == anno_min_quota)) %>%
  mutate(
    testo = paste0(formatC(quota, format = "f", digits = 1, decimal.mark = ","), "%"),
    hjust = case_when(anno == ANNO_MIN ~ 0, anno == ANNO_MAX ~ 1, TRUE ~ 0.5),
    vjust = if_else(serie == "quota_corretta" & anno == anno_min_quota, 2.2, -1.3)
  )

p <- ggplot(plot_data, aes(x = anno, y = quota, colour = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = label_valori, size = 1.8) +
  geom_text(data = label_serie, aes(label = testo),
            family = "Source Sans Pro", fontface = "bold",
            size = 3.6, hjust = 0) +
  geom_text(data = label_valori,
            aes(label = testo, hjust = hjust, vjust = vjust),
            family = "Source Sans Pro", fontface = "bold", size = 3.4) +
  scale_colour_manual(values = col_serie) +
  scale_x_continuous(breaks = seq(1995, 2025, 5),
                     limits = c(ANNO_MIN - 0.5, ANNO_MAX + 0.5),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(35, 60, 5), limits = c(34, 61),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  labs(
    title = "La quota di Pil che va al lavoro resta sotto il 1995",
    subtitle = paste0("Redditi da lavoro in percentuale del Pil ai prezzi correnti, Italia, ",
                      ANNO_MIN, "-", ANNO_MAX),
    caption = CAP_ISTAT
  )

ggsave(file.path(output_dir, "quota_lavoro_pil.png"), plot = p,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 3) Sanity check --------------------------------------------------------

controllo <- dati %>%
  filter(anno %in% c(ANNO_MIN, 2009, 2019, anno_min_quota, ANNO_MAX))

cat("\nQuota del lavoro sul Pil, anni chiave:\n")
print(controllo %>% select(anno, quota_grezza, quota_corretta, quota_indip) %>%
        mutate(across(-anno, ~ round(.x, 1))))

cat("\nMinimo della serie corretta:", round(min(dati$quota_corretta), 1),
    "% nel", anno_min_quota, "\n")
cat("Massimo della serie corretta:", round(max(dati$quota_corretta), 1),
    "% nel", dati$anno[which.max(dati$quota_corretta)], "\n")

# Il conto del reddito deve chiudere: Pil = redditi da lavoro dipendente +
# risultato lordo di gestione e reddito misto + imposte nette sulla produzione.
# Qui verifico solo che le due componenti non superino il Pil.
stopifnot(all(dati$redditi_dip + dati$rlg_misto < dati$pil))
stopifnot(all(dati$quota_corretta > dati$quota_grezza))
