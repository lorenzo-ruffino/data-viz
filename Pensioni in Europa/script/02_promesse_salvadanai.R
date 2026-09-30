# Grafico 2 — "Promesse contro salvadanai"
# Barre specchiate per 7 paesi:
#  - a sinistra: patrimonio dei fondi pensione in % del Pil, fine 2024
#    (fonte: OCSE, Pensions at a Glance 2025 — valori verificati e cablati)
#  - a destra: diritti pensionistici già maturati nei sistemi pubblici a
#    ripartizione non coperti da attivi, in % del Pil, 2021
#    (fonte: Eurostat, conti patrimoniali T29 / nasa_10_pens1 — letti dal
#    file input/diritti_maturati_t29.csv, righe unit = PC_GDP)

library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
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

# --- Dati -------------------------------------------------------------------

# Patrimonio fondi pensione, % Pil, fine 2024.
# Fonte: OCSE, Pensions at a Glance 2025. Valori verificati, cablati.
fondi <- tribble(
  ~geo, ~paese,          ~fondi_pct,
  "DK", "Danimarca",     206,
  "NL", "Paesi Bassi",   151,
  "SE", "Svezia",        116,
  "FR", "Francia",       12.9,
  "IT", "Italia",        11.7,
  "ES", "Spagna",        10.8,
  "DE", "Germania",      6.4
)

# Diritti pensionistici maturati nei sistemi pubblici a ripartizione non
# coperti da attivi, % Pil, 2021. Fonte: Eurostat, conti patrimoniali T29
# (nasa_10_pens1): il CSV in input contiene direttamente le righe in % del
# Pil (unit = PC_GDP, na_item = F63_LE, anno 2021).
t29 <- read_csv("../input/diritti_maturati_t29.csv", show_col_types = FALSE,
                col_types = cols(freq = col_character(), unit = col_character(),
                                 penscheme = col_character(), na_item = col_character(),
                                 geo = col_character(), anno = col_integer(),
                                 valore = col_double()))

promesse <- t29 %>%
  filter(unit == "PC_GDP", penscheme == "S13PU", na_item == "F63_LE",
         anno == 2021, geo %in% fondi$geo) %>%
  select(geo, promesse_pct = valore)

# Conferma contro i valori verificati a mano (ES 496, IT 429, FR 397,
# DE 320, NL 210, SE 192, DK ~20): se il CSV non coprisse tutti i paesi,
# fallback sui valori cablati.
promesse_cablate <- tribble(
  ~geo, ~promesse_pct,
  "ES", 496, "IT", 429, "FR", 397, "DE", 320,
  "NL", 210, "SE", 192, "DK", 24
)
if (nrow(promesse) < 7 || any(is.na(promesse$promesse_pct))) {
  promesse <- promesse_cablate
}

dati <- fondi %>%
  left_join(promesse, by = "geo") %>%
  arrange(desc(promesse_pct)) %>%           # promesse più alte in cima
  mutate(ypos = rev(row_number()))          # ES = 7 (in alto) ... DK = 1

write_csv(dati %>% select(paese, fondi_pct, promesse_pct),
          "../output/02_promesse_salvadanai.csv")

# --- Grafico ----------------------------------------------------------------

GAP  <- 48        # mezza larghezza della fascia centrale coi nomi dei paesi
BARH <- 0.62      # altezza delle barre

plot_data <- dati %>%
  mutate(
    xmin_sx = -GAP - fondi_pct, xmax_sx = -GAP,
    xmin_dx =  GAP,             xmax_dx =  GAP + promesse_pct,
    lab_sx  = format(round(fondi_pct), big.mark = ".", decimal.mark = ","),
    lab_dx  = format(round(promesse_pct), big.mark = ".", decimal.mark = ",")
  )

p <- ggplot(plot_data) +
  # barre
  geom_rect(aes(xmin = xmin_sx, xmax = xmax_sx,
                ymin = ypos - BARH / 2, ymax = ypos + BARH / 2),
            fill = COL_BLU) +
  geom_rect(aes(xmin = xmin_dx, xmax = xmax_dx,
                ymin = ypos - BARH / 2, ymax = ypos + BARH / 2),
            fill = COL_ROSSO) +
  # basi verticali delle due metà
  annotate("segment", x = -GAP, xend = -GAP, y = 0.55, yend = 7.45,
           colour = COL_NERO, linewidth = 0.3) +
  annotate("segment", x = GAP, xend = GAP, y = 0.55, yend = 7.45,
           colour = COL_NERO, linewidth = 0.3) +
  # nomi dei paesi nella fascia centrale
  geom_text(aes(x = 0, y = ypos, label = paese),
            family = "Source Sans Pro", fontface = "bold",
            size = 3.4, color = COL_NERO) +
  # etichette di valore (intere) all'estremità delle barre
  geom_text(aes(x = xmin_sx, y = ypos, label = lab_sx),
            hjust = 1, nudge_x = -8, family = "Source Sans Pro",
            fontface = "bold", size = 3.4, color = COL_BLU) +
  geom_text(aes(x = xmax_dx, y = ypos, label = lab_dx),
            hjust = 0, nudge_x = 8, family = "Source Sans Pro",
            fontface = "bold", size = 3.4, color = COL_ROSSO) +
  # testate delle due metà
  annotate("text", x = -GAP, y = 8.25, hjust = 1, vjust = 1,
           label = "Risparmio accumulato\nnei fondi pensione",
           family = "Source Sans Pro", fontface = "bold",
           size = 3.6, color = COL_BLU, lineheight = 1.05) +
  annotate("text", x = GAP, y = 8.25, hjust = 0, vjust = 1,
           label = "Promesse da pagare\nsenza risparmio dietro",
           family = "Source Sans Pro", fontface = "bold",
           size = 3.6, color = COL_ROSSO, lineheight = 1.05) +
  scale_x_continuous(limits = c(-315, 570), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.4, 8.35), clip = "off") +
  theme_linechart() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        legend.position = "none") +
  labs(
    title = "Promesse contro salvadanai",
    subtitle = "Patrimonio dei fondi pensione e diritti pensionistici già maturati nei sistemi a ripartizione non coperti da attivi, in % del Pil",
    caption = "Fonte: OCSE (patrimonio, 2024) ed Eurostat (diritti maturati, 2021)"
  )

ggsave("../output/02_promesse_salvadanai.png", p,
       width = 9.5, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- Sanity check -----------------------------------------------------------

cat("Valori usati (ordinati per promesse):\n")
print(dati %>% select(paese, fondi_pct, promesse_pct))
cat("\nConfronto con i valori cablati (diff = CSV - cablato):\n")
print(dati %>% left_join(promesse_cablate, by = "geo", suffix = c("", "_cablato")) %>%
        mutate(diff = promesse_pct - promesse_pct_cablato) %>%
        select(paese, promesse_pct, promesse_pct_cablato, diff))
