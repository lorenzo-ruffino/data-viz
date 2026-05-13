library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

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

CAP_INPS <- "Elaborazione di Lorenzo Ruffino su dati Inps"

# --- Dati FYFT 2024 ---------------------------------------------------------
# Distribuzione del salario orario lordo dei dipendenti privati a tempo pieno
# per tutto l'anno (Full-Year Full-Time), stimata applicando una distribuzione
# lognormale (sigma = 0,15) interna a ciascuna classe Inps di importo annuo.
# Bin di 1 euro fino a 49-50 + bin "50+" residuale.

distrib <- tribble(
  ~bin_label, ~bin_ord, ~pct,
  "5-6",    1,  0.42,
  "6-7",    2,  1.68,
  "7-8",    3,  4.30,
  "8-9",    4,  7.45,
  "9-10",   5,  9.68,
  "10-11",  6, 10.37,
  "11-12",  7,  9.80,
  "12-13",  8,  8.54,
  "13-14",  9,  7.11,
  "14-15", 10,  5.82,
  "15-16", 11,  4.76,
  "16-17", 12,  3.91,
  "17-18", 13,  3.25,
  "18-19", 14,  2.74,
  "19-20", 15,  2.33,
  "20-21", 16,  1.99,
  "21-22", 17,  1.72,
  "22-23", 18,  1.50,
  "23-24", 19,  1.31,
  "24-25", 20,  1.15,
  "25-26", 21,  1.01,
  "26-27", 22,  0.87,
  "27-28", 23,  0.75,
  "28-29", 24,  0.63,
  "29-30", 25,  0.52,
  "30-31", 26,  0.41,
  "31-32", 27,  0.32,
  "32-33", 28,  0.24,
  "33-34", 29,  0.18,
  "34-35", 30,  0.14,
  "35-36", 31,  0.10,
  "36-37", 32,  0.08,
  "37-38", 33,  0.08,
  "38-39", 34,  0.08,
  "39-40", 35,  0.08,
  "40-41", 36,  0.10,
  "41-42", 37,  0.12,
  "42-43", 38,  0.14,
  "43-44", 39,  0.16,
  "44-45", 40,  0.18,
  "45-46", 41,  0.20,
  "46-47", 42,  0.22,
  "47-48", 43,  0.23,
  "48-49", 44,  0.25,
  "49-50", 45,  0.25,
  "50+",   46,  2.83
)

# Salvo il dato pulito
write_csv(distrib, "../output/distribuzione_salario_orario.csv")

# Etichette di percentuale sopra le barre. Mostro solo se >= 1% per evitare
# clutter sulle barre molto piccole. Format italiano con virgola decimale.
distrib <- distrib %>%
  mutate(label_val = if_else(
    pct >= 1,
    paste0(format(round(pct, 1), nsmall = 1, decimal.mark = ","), "%"),
    ""
  ))

# --- Grafico ---------------------------------------------------------------

p <- ggplot(distrib, aes(x = factor(bin_label, levels = bin_label), y = pct)) +
  geom_col(width = 0.78, fill = COL_BLU) +
  geom_text(aes(label = label_val),
            vjust = -0.6,
            family = "Source Sans Pro",
            size = 2.6,
            color = COL_NERO) +
  scale_y_continuous(
    labels = function(x) paste0(format(x, decimal.mark = ","), "%"),
    limits = c(0, max(distrib$pct) * 1.12),
    expand = c(0, 0)
  ) +
  labs(
    title = "1 dipendente full-time su 7 guadagna meno di 9 euro l'ora",
    subtitle = "Distribuzione stimata dei lavoratori dipendenti privati a tempo pieno per tutto l'anno per fascia\ndi salario orario lordo, Italia, 2024",
    caption = CAP_INPS,
    x = NULL, y = NULL
  ) +
  theme_linechart() +
  theme(
    axis.text.x = element_text(size = 7, color = COL_NERO, angle = 0),
    axis.line.y = element_blank(),
    axis.text.y = element_blank()
  )

ggsave("../output/distribuzione_salario_orario.png",
       plot = p,
       width = 11, height = 6.5, units = "in", dpi = 220, bg = "white")

cat("Grafico salvato.\n")
cat("Somma percentuali:", sum(distrib$pct), "\n")
