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

CAP_MEF <- "Elaborazione di Lorenzo Ruffino su dati Ministero dell'Economia e delle Finanze"

OUT <- "../output"
ANNO_0 <- 2004
ANNO_1 <- 2024

aliquote <- read_csv(file.path(OUT, "aliquote_medie_2002_2024.csv"), show_col_types = FALSE)
ventili  <- read_csv(file.path(OUT, "ventili_2011_2024.csv"), show_col_types = FALSE)
classi   <- read_csv(file.path(OUT, "classi_reddito_2002_2024.csv"), show_col_types = FALSE)

# --- Soglie di reddito dell'ultimo anno, per le etichette --------------------

soglie <- ventili |> filter(anno == ANNO_1) |> select(ventile, soglia) |> deframe()

# Soglia dell'1% più ricco: interpolazione lineare dentro la classe
soglia_top1 <- classi |>
  filter(anno == ANNO_1) |>
  arrange(desc(lo)) |>
  mutate(cum = cumsum(n), target = 0.01 * sum(n)) |>
  filter(cum >= target) |>
  slice(1) |>
  mutate(s = hi - (target - (cum - n)) / n * (hi - lo)) |>
  pull(s)

euro <- function(x, acc = 100) paste0("€ ", format(round(x / acc) * acc, big.mark = ".",
                                                   decimal.mark = ",", trim = TRUE))

righe <- tribble(
  ~gruppo,            ~etichetta,       ~dettaglio,                                        ~y,
  "Primo quintile",   "Primo quinto",   paste("fino a", euro(soglie[4])),                  7.0,
  "Secondo quintile", "Secondo quinto", paste(euro(soglie[4]), "-", euro(soglie[8])),      6.0,
  "Terzo quintile",   "Terzo quinto",   paste(euro(soglie[8]), "-", euro(soglie[12])),     5.0,
  "Quarto quintile",  "Quarto quinto",  paste(euro(soglie[12]), "-", euro(soglie[16])),    4.0,
  "Quinto quintile",  "Quinto quinto",  paste("oltre", euro(soglie[16])),                  3.0,
  "5% più ricco",     "5% più ricco",   paste("oltre", euro(soglie[19])),                  1.7,
  "1% più ricco",     "1% più ricco",   paste("oltre", euro(soglia_top1, 1000)),           0.7
)

fmt_pct <- function(v) paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%")
fmt_delta <- function(v) {
  segno <- ifelse(round(v, 1) > 0, "+", ifelse(round(v, 1) < 0, "−", ""))
  paste0(segno, formatC(abs(v), format = "f", digits = 1, decimal.mark = ","), " punti")
}

dati <- aliquote |>
  filter(anno %in% c(ANNO_0, ANNO_1)) |>
  select(anno, gruppo, aliquota) |>
  pivot_wider(names_from = anno, values_from = aliquota, names_prefix = "a") |>
  rename(prima = paste0("a", ANNO_0), dopo = paste0("a", ANNO_1)) |>
  # variazione dai valori arrotondati, così coincide con la colonna a destra
  mutate(delta = round(dopo, 1) - round(prima, 1),
         colore = if_else(delta < 0, COL_BLU, COL_ROSSO)) |>
  inner_join(righe, by = "gruppo")

# --- Grafico -----------------------------------------------------------------

X_ETIC  <- -4.3   # nomi dei gruppi (allineati a destra)
X_COL   <-  2.75  # colonna prima -> dopo
X_MIN   <- -6.3
X_MAX   <-  4.5
H_BAR   <-  0.62

p <- ggplot(dati) +
  # separatore tra i quintili e le code del quinto quinto
  annotate("segment", x = X_MIN + 0.1, xend = X_MAX, y = 2.35, yend = 2.35,
           colour = "#E3E3E3", linewidth = 0.4) +
  geom_rect(aes(xmin = pmin(0, delta), xmax = pmax(0, delta),
                ymin = y - H_BAR / 2, ymax = y + H_BAR / 2, fill = colore)) +
  geom_segment(aes(x = 0, xend = 0, y = 0.2, yend = 7.5),
               data = tibble(), colour = COL_NERO, linewidth = 0.35) +
  # valore della variazione in fondo alla barra
  geom_text(aes(x = delta + if_else(delta < 0, -0.08, 0.08), y = y,
                label = fmt_delta(delta), colour = colore,
                hjust = if_else(delta < 0, 1, 0)),
            family = "Source Sans Pro", fontface = "bold", size = 3.6) +
  # nomi dei gruppi e soglie di reddito
  geom_text(aes(x = X_ETIC, y = y + 0.13, label = etichetta),
            hjust = 1, family = "Source Sans Pro", fontface = "bold",
            size = 3.6, colour = COL_NERO) +
  geom_text(aes(x = X_ETIC, y = y - 0.2, label = dettaglio),
            hjust = 1, family = "Source Sans Pro", size = 3.0,
            colour = COL_GRIGIO_SCURO) +
  # colonna con le aliquote dei due anni
  geom_text(aes(x = X_COL, y = y,
                label = paste0(fmt_pct(prima), "  →  ", fmt_pct(dopo))),
            hjust = 0, family = "Source Sans Pro", size = 3.4, colour = COL_NERO) +
  annotate("text", x = X_COL, y = 7.75, hjust = 0,
           label = paste0(ANNO_0, "  →  ", ANNO_1),
           family = "Source Sans Pro", fontface = "bold", size = 3.2,
           colour = COL_GRIGIO_SCURO) +
  annotate("text", x = X_ETIC, y = 7.75, hjust = 1,
           label = paste0("Reddito ", ANNO_1),
           family = "Source Sans Pro", fontface = "bold", size = 3.2,
           colour = COL_GRIGIO_SCURO) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(limits = c(X_MIN, X_MAX), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.2, 8.0), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(axis.text = element_blank(), axis.line = element_blank()) +
  labs(
    title = "Dal 2004 l'Irpef pesa di più sul ceto medio",
    subtitle = paste0(
      "Variazione dell'aliquota media su Irpef, addizionali e cedolare secca, contribuenti ordinati\n",
      "per reddito complessivo, Italia, anni d'imposta ", ANNO_0, "-", ANNO_1),
    caption = CAP_MEF
  )

ggsave(file.path(OUT, paste0("aliquote_medie_", ANNO_0, "_", ANNO_1, ".png")), p,
       width = 9, height = 6.5, dpi = 220, bg = "white")

cat("Soglia 1% più ricco", ANNO_1, ":", round(soglia_top1), "\n")
dati |> select(gruppo, prima, dopo, delta) |> mutate(across(where(is.numeric), ~ round(.x, 2))) |> print()
