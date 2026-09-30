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

# --- Soglie di reddito dell'ultimo anno, per i titoli dei pannelli -----------

soglie <- ventili |> filter(anno == ANNO_1) |> select(ventile, soglia) |> deframe()
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

pannelli <- tribble(
  ~gruppo,            ~titolo,
  "Primo quintile",   paste0("Primo quinto\nfino a ", euro(soglie[4])),
  "Secondo quintile", paste0("Secondo quinto\n", euro(soglie[4]), " - ", euro(soglie[8])),
  "Terzo quintile",   paste0("Terzo quinto\n", euro(soglie[8]), " - ", euro(soglie[12])),
  "Quarto quintile",  paste0("Quarto quinto\n", euro(soglie[12]), " - ", euro(soglie[16])),
  "Quinto quintile",  paste0("Quinto quinto\noltre ", euro(soglie[16])),
  "5% più ricco",     paste0("5% più ricco\noltre ", euro(soglie[19])),
  "1% più ricco",     paste0("1% più ricco\noltre ", euro(soglia_top1, 1000))
)

dati <- aliquote |>
  filter(between(anno, ANNO_0, ANNO_1)) |>
  select(anno, gruppo, aliquota) |>
  inner_join(pannelli, by = "gruppo") |>
  mutate(titolo = factor(titolo, levels = pannelli$titolo)) |>
  group_by(gruppo) |>
  mutate(colore = if_else(round(aliquota[anno == ANNO_1], 1) < round(aliquota[anno == ANNO_0], 1),
                          COL_BLU, COL_ROSSO)) |>
  ungroup()

# Ogni pannello ha la sua scala ma la stessa ampiezza in punti, così le
# pendenze si confrontano da un pannello all'altro
AMPIEZZA <- dati |>
  group_by(gruppo) |>
  summarise(r = diff(range(aliquota))) |>
  pull(r) |> max() * 1.35

limiti <- dati |>
  group_by(titolo) |>
  summarise(centro = (min(aliquota) + max(aliquota)) / 2) |>
  mutate(lo = centro - AMPIEZZA / 2, hi = centro + AMPIEZZA / 2) |>
  pivot_longer(c(lo, hi), values_to = "aliquota") |>
  mutate(anno = ANNO_0)

fmt_pct <- function(v) paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%")

# Etichette dei valori agli estremi: sopra o sotto il punto, dal lato in cui
# non incrociano la linea. L'etichetta del primo anno si estende verso destra,
# quella dell'ultimo verso sinistra; ingombro stimato in anni e in punti.
LARG_ANNI <- 4.8
ALT_PUNTI <- AMPIEZZA * 0.075
GAP       <- AMPIEZZA * 0.03

sovrapposizione <- function(serie, x0, x1, y0, y1) {
  xs <- seq(x0, x1, length.out = 40)
  ys <- approx(serie$anno, serie$aliquota, xs, rule = 2)$y
  sum(ys > y0 & ys < y1)
}

serie_di <- function(g) filter(dati, gruppo == g)

estremi <- dati |>
  filter(anno %in% c(ANNO_0, ANNO_1)) |>
  rowwise() |>
  mutate(
    serie = list(serie_di(gruppo)),
    x0 = if_else(anno == ANNO_0, anno, anno - LARG_ANNI),
    x1 = if_else(anno == ANNO_0, anno + LARG_ANNI, anno),
    sopra = sovrapposizione(serie, x0, x1, aliquota + GAP, aliquota + GAP + ALT_PUNTI),
    sotto = sovrapposizione(serie, x0, x1, aliquota - GAP - ALT_PUNTI, aliquota - GAP),
    y_lab = if_else(sopra <= sotto, aliquota + GAP, aliquota - GAP),
    vjust = if_else(sopra <= sotto, 0, 1),
    # nel primo anno la serie oscilla da entrambi i lati: l'etichetta va a
    # sinistra del punto, nello spazio lasciato libero sull'asse x
    x_lab = if_else(anno == ANNO_0, anno - 0.7, anno),
    y_lab = if_else(anno == ANNO_0, aliquota, y_lab),
    vjust = if_else(anno == ANNO_0, 0.5, vjust)
  ) |>
  ungroup() |>
  select(-serie)

p <- ggplot(dati, aes(anno, aliquota)) +
  geom_blank(data = limiti) +
  geom_line(aes(colour = colore), linewidth = 0.9) +
  geom_point(data = estremi, aes(colour = colore), size = 1.8) +
  geom_text(data = estremi,
            aes(x = x_lab, y = y_lab, label = fmt_pct(aliquota), colour = colore,
                hjust = 1, vjust = vjust),
            family = "Source Sans Pro", fontface = "bold", size = 3.2) +
  facet_wrap(~ titolo, ncol = 4, scales = "free_y") +
  scale_colour_identity() +
  scale_x_continuous(breaks = c(ANNO_0, (ANNO_0 + ANNO_1) / 2, ANNO_1),
                     limits = c(ANNO_0 - 5.6, ANNO_1 + 0.8), expand = c(0, 0)) +
  # asse y a destra: a sinistra c'è l'etichetta del primo anno
  scale_y_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), 1),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0),
                     position = "right") +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(
    strip.text = element_text(family = "Source Sans Pro", face = "bold", size = 9.5,
                              colour = COL_NERO, hjust = 0, lineheight = 1.15,
                              margin = margin(b = 0.25, t = 0.2, unit = "cm")),
    panel.grid.major.y = element_line(colour = "#EDEDED", linewidth = 0.3),
    axis.line = element_blank(),
    axis.text = element_text(size = 8.5),
    panel.spacing.x = unit(0.7, "cm"),
    panel.spacing.y = unit(0.5, "cm")
  ) +
  labs(
    title = "Dal 2021 l'Irpef è risalita per i redditi medi",
    subtitle = paste0(
      "Aliquota media su Irpef, addizionali e cedolare secca per gruppi di contribuenti ordinati per reddito\n",
      "complessivo (soglie del ", ANNO_1, "), Italia, anni d'imposta ", ANNO_0, "-", ANNO_1),
    caption = CAP_MEF
  )

ggsave(file.path(OUT, paste0("andamento_aliquote_", ANNO_0, "_", ANNO_1, ".png")), p,
       width = 10, height = 7, dpi = 220, bg = "white")

cat("Ampiezza comune dei pannelli:", round(AMPIEZZA, 2), "punti\n")
dati |>
  filter(anno %in% c(ANNO_0, 2013, 2021, ANNO_1)) |>
  select(anno, gruppo, aliquota) |>
  mutate(aliquota = round(aliquota, 1)) |>
  pivot_wider(names_from = anno, values_from = aliquota) |>
  print()
