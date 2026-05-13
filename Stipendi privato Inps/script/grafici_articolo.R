# =============================================================================
# Grafici dell'articolo "Perche' in Italia i salari non crescono"
#
# 1. Stipendio reale a parita' di tempo lavorato, 2014-2024
# 2. Stipendio per fascia di eta' decennale, base 100 nel 2014
# 3. Stipendio per macro-settore, base 100 nel 2014
# 4. Occupati per macro-settore, base 100 nel 2014
# 5. Valore aggiunto per occupato e crescita dell'occupazione, per branca
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
  library(ggrepel)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

PROJ <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Stipendi privato Inps"
OUT  <- file.path(PROJ, "output")

CAP_INPS  <- "Elaborazione di Lorenzo Ruffino su dati Inps"
CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"
CAP_MISTO <- "Elaborazione di Lorenzo Ruffino su dati Inps e Istat"

# -----------------------------------------------------------------------------
# Tema (testi piu' piccoli rispetto al default)
# -----------------------------------------------------------------------------
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

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"
COL_VERDE  <- "#1B9E77"   # extra per categorie addizionali
COL_BLU2   <- "#3686D6"   # extra per categorie addizionali

# Arrotondamento a multipli di 50
round_50 <- function(x) round(x / 50) * 50

# =============================================================================
# 1. Stipendio reale 2014-2024
# =============================================================================
d1 <- read_csv(file.path(OUT, "headline.csv"), show_col_types = FALSE) %>%
  filter(vista == "complessivo") %>%
  arrange(anno)

label_anni <- d1 %>%
  filter(anno %in% c(2014, 2024)) %>%
  mutate(stip_round = round_50(stipendio_annuo_fte_reale))

g1 <- ggplot(d1, aes(anno, stipendio_annuo_fte_reale)) +
  geom_line(linewidth = 1, color = COL_ROSSO) +
  geom_point(size = 2.2, color = COL_ROSSO) +
  geom_text(data = label_anni,
            aes(label = paste0("€ ", format(stip_round, big.mark = ".", decimal.mark = ",")),
                hjust = ifelse(anno == 2014, 0, 1)),
            vjust = -1.4, family = "Source Sans Pro", fontface = "bold",
            size = 3.6, color = COL_NERO) +
  scale_x_continuous(breaks = seq(2014, 2024, 2),
                     expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(5000, 40000, 5000),
    labels = function(x) paste0("€ ", format(x / 1000, big.mark = ".", decimal.mark = ","), "k"),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Lo stipendio del privato è sceso del 5 per cento",
    subtitle = "Stipendio annuo equivalente a tempo pieno per 52 settimane, corretto per l'inflazione.\nLavoratori dipendenti del settore privato, Italia, 2014-2024",
    caption = CAP_INPS
  ) +
  theme_linechart()

ggsave(file.path(OUT, "01_stipendio_reale_2014_2024.png"),
       plot = g1, width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# =============================================================================
# 2. Stipendio per fascia di eta' decennale, base 100 nel 2014
# =============================================================================
d2 <- read_csv(file.path(OUT, "eta_decennale.csv"), show_col_types = FALSE) %>%
  filter(vista == "complessivo") %>%
  filter(eta_decennale != "<20") %>%
  group_by(eta_decennale) %>%
  arrange(anno) %>%
  mutate(idx = stipendio_annuo_fte_reale / stipendio_annuo_fte_reale[anno == 2014] * 100) %>%
  ungroup()

ord_eta <- c("20-29", "30-39", "40-49", "50-59", "60+")
d2$eta_decennale <- factor(d2$eta_decennale, levels = ord_eta)

pal_eta <- c(
  "20-29" = COL_BLU,
  "30-39" = COL_VIOLA,
  "40-49" = COL_NERO,
  "50-59" = COL_GIALLO,
  "60+"   = COL_ROSSO
)

g2 <- ggplot(d2, aes(anno, idx, color = eta_decennale)) +
  geom_hline(yintercept = 100, color = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.3) +
  scale_color_manual(values = pal_eta) +
  scale_x_continuous(breaks = seq(2014, 2024, 2),
                     limits = c(2014, 2025.5), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  geom_text_repel(
    data = d2 %>% filter(anno == 2024),
    aes(label = eta_decennale, color = eta_decennale),
    hjust = 0, nudge_x = 0.3, direction = "y",
    family = "Source Sans Pro", fontface = "bold", size = 3.4,
    segment.colour = COL_GRIGIO, min.segment.length = 0,
    seed = 1
  ) +
  labs(
    title = "Il calo maggiore di stipendio è tra gli over 40",
    subtitle = "Stipendio annuo equivalente a tempo pieno corretto per l'inflazione, base 100 nel 2014, per fascia di età.\nLavoratori dipendenti del settore privato, Italia, 2014-2024",
    caption = CAP_INPS
  ) +
  theme_linechart() +
  theme(legend.position = "none")

ggsave(file.path(OUT, "02_stipendio_per_eta.png"),
       plot = g2, width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# =============================================================================
# 3 e 4. Settori: stipendio e occupati base 100 nel 2014
# =============================================================================
d_set <- read_csv(file.path(OUT, "settore_macro.csv"), show_col_types = FALSE) %>%
  filter(vista == "complessivo") %>%
  mutate(settore = case_when(
    macro_settore == "Manifattura"                              ~ "Manifattura",
    macro_settore == "Turismo e ristorazione"                   ~ "Turismo",
    macro_settore == "Servizi alle imprese"                     ~ "Servizi alle imprese",
    macro_settore == "Sanita' e assistenza sociale (privata)"   ~ "Sanità privata",
    macro_settore == "Finanza e assicurazioni"                  ~ "Finanza",
    macro_settore == "Istruzione (privata)"                     ~ "Istruzione privata",
    macro_settore == "Commercio"                                ~ "Commercio",
    macro_settore == "Costruzioni"                              ~ "Costruzioni",
    macro_settore == "ICT"                                      ~ "Settore informatico",
    macro_settore == "Trasporti e magazzinaggio"                ~ "Trasporti",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(settore))

ord_set <- c("Settore informatico", "Finanza", "Manifattura", "Trasporti",
             "Commercio", "Costruzioni", "Servizi alle imprese",
             "Sanità privata", "Istruzione privata", "Turismo")
d_set$settore <- factor(d_set$settore, levels = ord_set)

pal_set <- c(
  "Manifattura"          = COL_NERO,
  "Finanza"              = COL_BLU,
  "Settore informatico"  = COL_BLU2,
  "Servizi alle imprese" = COL_VIOLA,
  "Sanità privata"       = COL_GIALLO,
  "Istruzione privata"   = COL_VERDE,
  "Turismo"              = COL_ROSSO,
  "Commercio"            = COL_GRIGIO_SCURO,
  "Costruzioni"          = COL_GRIGIO,
  "Trasporti"            = "#E07700"
)

# -- 3. stipendio per settore -------------------------------------------------
d3 <- d_set %>%
  group_by(settore) %>%
  arrange(anno) %>%
  mutate(idx = stipendio_annuo_fte_reale / stipendio_annuo_fte_reale[anno == 2014] * 100) %>%
  ungroup()

g3 <- ggplot(d3, aes(anno, idx, color = settore)) +
  geom_hline(yintercept = 100, color = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.1) +
  scale_color_manual(values = pal_set) +
  scale_x_continuous(breaks = seq(2014, 2024, 2),
                     limits = c(2014, 2027), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  geom_text_repel(
    data = d3 %>% filter(anno == 2024),
    aes(label = settore, color = settore),
    hjust = 0, nudge_x = 0.3, direction = "y",
    family = "Source Sans Pro", fontface = "bold", size = 3.0,
    segment.colour = COL_GRIGIO, min.segment.length = 0,
    box.padding = 0.15, seed = 1
  ) +
  labs(
    title = "Il turismo è dove calano di più gli stipendi",
    subtitle = "Stipendio annuo equivalente a tempo pieno corretto per l'inflazione, base 100 nel 2014, per settore.\nLavoratori dipendenti del settore privato, Italia, 2014-2024",
    caption = CAP_INPS
  ) +
  theme_linechart() +
  theme(legend.position = "none")

ggsave(file.path(OUT, "03_stipendio_per_settore.png"),
       plot = g3, width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# -- 4. occupati per settore --------------------------------------------------
d4 <- d_set %>%
  group_by(settore) %>%
  arrange(anno) %>%
  mutate(idx = lavoratori / lavoratori[anno == 2014] * 100) %>%
  ungroup()

g4 <- ggplot(d4, aes(anno, idx, color = settore)) +
  geom_hline(yintercept = 100, color = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.1) +
  scale_color_manual(values = pal_set) +
  scale_x_continuous(breaks = seq(2014, 2024, 2),
                     limits = c(2014, 2027), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(80, 200, 10),
                     expand = expansion(mult = c(0.05, 0.05))) +
  geom_text_repel(
    data = d4 %>% filter(anno == 2024),
    aes(label = settore, color = settore),
    hjust = 0, nudge_x = 0.3, direction = "y",
    family = "Source Sans Pro", fontface = "bold", size = 3.0,
    segment.colour = COL_GRIGIO, min.segment.length = 0,
    box.padding = 0.15, seed = 1
  ) +
  labs(
    title = "Crescono i settori che pagano meno",
    subtitle = "Numero di lavoratori per settore, base 100 nel 2014.\nLavoratori dipendenti del settore privato, Italia, 2014-2024",
    caption = CAP_INPS
  ) +
  theme_linechart() +
  theme(legend.position = "none")

ggsave(file.path(OUT, "04_occupati_per_settore.png"),
       plot = g4, width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# =============================================================================
# 5. Valore aggiunto per occupato per branca, scatter vs crescita occupati
# =============================================================================
d5 <- read_csv(file.path(OUT, "produttivita_branche_istat.csv"),
               show_col_types = FALSE) %>%
  # Escludo solo: L (immobiliari, anomalia VA), O (PA, non privato),
  # __TOT__ (totale economia, riga di servizio).
  filter(!codice %in% c("L", "O", "__TOT__")) %>%
  mutate(label = case_when(
    codice == "A" ~ "Agricoltura",
    codice == "B" ~ "Industria estrattiva",
    codice == "C" ~ "Manifattura",
    codice == "D" ~ "Energia",
    codice == "E" ~ "Acqua e rifiuti",
    codice == "F" ~ "Costruzioni",
    codice == "G" ~ "Commercio",
    codice == "H" ~ "Trasporti",
    codice == "I" ~ "Turismo",
    codice == "J" ~ "Settore informatico",
    codice == "K" ~ "Finanza",
    codice == "M" ~ "Servizi professionali",
    codice == "N" ~ "Servizi alle imprese",
    codice == "P" ~ "Istruzione",
    codice == "Q" ~ "Sanità",
    codice == "R" ~ "Cultura",
    codice == "S" ~ "Altri servizi",
    codice == "T" ~ "Lavoro domestico",
    TRUE ~ branca
  )) %>%
  filter(!is.na(produttivita_kEUR_per_occupato_2024),
         !is.na(occupati_var_pct))

g5 <- ggplot(d5, aes(occupati_var_pct, produttivita_kEUR_per_occupato_2024)) +
  geom_vline(xintercept = 0, color = COL_GRIGIO, linewidth = 0.4,
             linetype = "dashed") +
  # Linea di trend (regressione lineare pesata per gli occupati 2024).
  # Mostra che a maggiore crescita dell'occupazione corrisponde un valore
  # aggiunto per occupato piu' basso.
  geom_smooth(aes(weight = occupati_migliaia_2024),
              method = "lm", se = FALSE,
              color = COL_ROSSO, linewidth = 0.7, linetype = "dashed") +
  geom_point(size = 2.6, color = COL_BLU) +
  geom_text_repel(aes(label = label),
                  family = "Source Sans Pro", size = 3.1, fontface = "bold",
                  color = COL_BLU,
                  segment.colour = COL_GRIGIO, min.segment.length = 0,
                  box.padding = 0.45, point.padding = 0.35,
                  max.overlaps = 30, seed = 1) +
  scale_x_continuous(labels = function(x) paste0(round(x), "%"),
                     expand = expansion(mult = c(0.07, 0.1))) +
  scale_y_continuous(labels = function(x) paste0("€ ", round(x), "k"),
                     expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    title = "Cresce di più il lavoro a basso valore aggiunto",
    subtitle = "Valore aggiunto per occupato 2024 e variazione del numero di occupati nel decennio.\nLa retta tratteggiata è la regressione pesata per gli occupati. Branche economiche, Italia, 2014-2024",
    caption = CAP_MISTO
  ) +
  theme_linechart() +
  theme(
    axis.title.x = element_text(size = 10, color = COL_NERO, hjust = 0.5,
                                margin = margin(t = 0.3, unit = "cm")),
    axis.title.y = element_text(size = 10, color = COL_NERO, hjust = 0.5,
                                angle = 90, margin = margin(r = 0.3, unit = "cm"))
  ) +
  xlab("Variazione % degli occupati 2014-2024") +
  ylab("Valore aggiunto per occupato 2024")

ggsave(file.path(OUT, "05_valore_aggiunto_settori.png"),
       plot = g5, width = 8, height = 7.5, units = "in", dpi = 220, bg = "white")

cat("Generati 5 PNG in", OUT, "\n")
