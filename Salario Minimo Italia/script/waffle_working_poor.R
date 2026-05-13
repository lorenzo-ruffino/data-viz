library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO          <- "#1C1C1C"
COL_ARANCIONE     <- "#F2A900"
COL_BLU           <- "#0478EA"
COL_VERDE         <- "#1B9E77"
COL_VIOLA         <- "#A82DE3"
COL_GRIGIO        <- "#9A9A9A"
COL_ROSSO         <- "#F12938"

theme_waffle <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      legend.position = "top",
      legend.justification = "left",
      legend.box.just = "left",
      legend.text = element_text(size = 10, color = COL_NERO, hjust = 0,
                                 margin = margin(l = 0.1, r = 0.4, unit = "cm")),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(size = 14, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.4, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP_INPS <- "Elaborazione di Lorenzo Ruffino su dati Inps"

# --- Dati -------------------------------------------------------------------
# 871.800 dipendenti privati italiani con retribuzione mensile inferiore al
# 60% della mediana di gruppo. Fonte: Tabelle 1.31-1.34 del XII Rapporto
# Annuale INPS, ottobre 2022.

categorie <- tribble(
  ~ordine, ~label_short,                          ~conteggio, ~colore,
  1,       "Part-time mese intero",                    359700, COL_ARANCIONE,
  2,       "Part-time con assenze",                    157500, COL_BLU,
  3,       "Full-time apprendisti",                     77500, COL_VERDE,
  4,       "Full-time intermittenti",                  113200, COL_VIOLA,
  5,       "Full-time con CIG, malattia o transitorio", 143700, COL_GRIGIO,
  6,       "Veri lavoratori poveri",                    20300, COL_ROSSO
)

totale <- sum(categorie$conteggio)

# Allocazione dei puntini, somma forzata
N_DOTS <- 1024

categorie <- categorie %>%
  mutate(pct_esatta = conteggio / totale * 100,
         pct = round(pct_esatta),
         dots_raw = conteggio / totale * N_DOTS,
         dots = round(dots_raw))

diff_dots <- N_DOTS - sum(categorie$dots)
categorie$dots[1] <- categorie$dots[1] + diff_dots
stopifnot(sum(categorie$dots) == N_DOTS)

diff_pct <- 100 - sum(categorie$pct)
categorie$pct[5] <- categorie$pct[5] + diff_pct
stopifnot(sum(categorie$pct) == 100)

write_csv(categorie %>% select(ordine, label_short, conteggio, pct, dots),
          "../output/waffle_working_poor.csv")

# --- Costruzione griglia 32x32 = 1024 puntini (forma più quadrata) -------

NCOL <- 32
NROW <- 32

# Riallocazione su 1024 puntini (ogni puntino ≈ 851 lavoratori)
N_DOTS_NEW <- NCOL * NROW

grid <- expand.grid(col = 1:NCOL, row = 1:NROW) %>%
  arrange(row, col) %>%
  mutate(idx = row_number())

assignments <- categorie %>%
  arrange(ordine) %>%
  mutate(end = cumsum(dots),
         start = lag(end, default = 0) + 1) %>%
  rowwise() %>%
  mutate(idxs = list(start:end)) %>%
  ungroup() %>%
  unnest(idxs) %>%
  rename(idx = idxs) %>%
  select(idx, ordine, label_short)

grid <- grid %>%
  left_join(assignments, by = "idx") %>%
  mutate(label_factor = factor(label_short, levels = categorie$label_short))

palette_cat <- categorie$colore
names(palette_cat) <- categorie$label_short

# Posizione delle etichette % FUORI dal grafico, sulla destra,
# allineate verticalmente alla riga centrale di ogni categoria
label_pos <- grid %>%
  group_by(ordine, label_factor) %>%
  summarise(cy = mean(row), .groups = "drop") %>%
  left_join(categorie %>% select(ordine, pct, label_short), by = "ordine") %>%
  mutate(label_x = NCOL + 2,
         pct_label = sprintf("%d%%", pct),
         label_factor = factor(label_short, levels = categorie$label_short))

# --- Grafico ---------------------------------------------------------------

p <- ggplot(grid, aes(x = col, y = -row, color = label_factor)) +
  geom_point(shape = 16, size = 6.5) +
  geom_text(data = label_pos,
            aes(x = label_x, y = -cy, label = pct_label, color = label_factor),
            hjust = 0,
            family = "Source Sans Pro",
            fontface = "bold",
            size = 8,
            show.legend = FALSE) +
  coord_equal(clip = "off") +
  scale_color_manual(values = palette_cat) +
  scale_x_continuous(limits = c(0.5, NCOL + 6), expand = c(0, 0)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE,
                              override.aes = list(size = 4))) +
  labs(
    title = "Chi sono i veri lavoratori poveri",
    subtitle = "Composizione dei dipendenti privati con retribuzione mensile sotto il 60% della mediana di gruppo, Italia.\nOgni puntino rappresenta circa 850 lavoratori",
    caption = CAP_INPS
  ) +
  theme_waffle()

ggsave("../output/waffle_working_poor.png",
       plot = p,
       width = 10, height = 9.5, units = "in", dpi = 220, bg = "white")

cat("Waffle salvato.\n")
print(categorie %>% select(label_short, conteggio, pct, dots))
