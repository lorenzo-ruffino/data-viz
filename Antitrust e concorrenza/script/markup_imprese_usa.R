# script/markup_imprese_usa.R — eseguito da `cd script && Rscript markup_imprese_usa.R`
#
# Margine tra prezzo e costo di produzione delle imprese quotate americane,
# 1955-2016: media ponderata per le vendite, impresa mediana e decimo con i
# margini più alti.
#
# Fonte: De Loecker, Eeckhout, Unger, "The Rise of Market Power and the
# Macroeconomic Implications", Quarterly Journal of Economics 2020.
# La serie è ricostruita da prep_markup.R sui dati e sul codice di replica
# degli autori (input/markup_dle_us_serie.csv).

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
})

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#8A8A8A"

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

CAP_DLEU <- "Elaborazione di Lorenzo Ruffino su dati De Loecker, Eeckhout e Unger (2020)"

# Vedi prezzi_telefonia.R: una riga di sottotitolo tiene circa 124 caratteri e
# l'a capo va messo solo a riga piena, su una virgola o un punto.

# --- 1) Dati ----------------------------------------------------------------

# Il markup è un ricarico sul costo: 1,21 significa un prezzo del 21 per cento
# sopra il costo di produzione. Qui si rappresenta direttamente il margine.
serie <- read_csv(file.path(input_dir, "markup_dle_us_serie.csv"),
                  col_types = cols()) %>%
  transmute(anno = year,
            media = (markup_aggregato - 1) * 100,
            alti  = (p90_vendite - 1) * 100,
            meta  = (p50_vendite - 1) * 100)

plot_data <- serie %>%
  pivot_longer(-anno, names_to = "serie", values_to = "margine") %>%
  mutate(serie = factor(serie, levels = c("alti", "media", "meta")))

ANNO_MIN <- min(serie$anno)
ANNO_MAX <- max(serie$anno)

col_serie <- c(alti = COL_ROSSO, media = COL_NERO, meta = COL_BLU)

# --- 2) Grafico -------------------------------------------------------------

label_serie <- tibble(
  serie   = factor(c("alti", "media", "meta"), levels = levels(plot_data$serie)),
  anno    = c(1956, 1990, 1957),
  margine = c(110, 57, 7),
  testo   = c("Il decimo di mercato con i margini più alti",
              "Media di tutte le imprese",
              "L’impresa a metà del mercato")
)

label_valori <- plot_data %>%
  filter(anno == ANNO_MAX) %>%
  mutate(testo = paste0(round(margine), "%"))

anno_min <- serie$anno[which.min(serie$media)]

p <- ggplot(plot_data, aes(x = anno, y = margine, colour = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = label_valori, size = 1.8) +
  geom_text(data = label_serie, aes(label = testo),
            family = "Source Sans Pro", fontface = "bold",
            size = 3.5, hjust = 0) +
  geom_text(data = label_valori, aes(label = testo),
            family = "Source Sans Pro", fontface = "bold", size = 3.4,
            hjust = -0.3) +
  scale_colour_manual(values = col_serie) +
  scale_x_continuous(breaks = seq(1960, 2010, 10),
                     limits = c(ANNO_MIN, ANNO_MAX + 9),
                     expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 150, 25), limits = c(0, 155),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.01, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  labs(
    title = "Negli Stati Uniti i margini sono cresciuti solo in cima al mercato",
    subtitle = paste0(
      "Margine fra prezzo e costo di produzione delle imprese quotate ",
      "americane, in percentuale del costo, ", ANNO_MIN, "-", ANNO_MAX,
      ".\nImprese ordinate per margine e pesate per le vendite"
    ),
    caption = CAP_DLEU
  )

ggsave(file.path(output_dir, "markup_imprese_usa.png"), plot = p,
       width = 9, height = 5.6, dpi = 220, bg = "white")

write_csv(serie %>% mutate(across(-anno, ~ round(.x, 1))),
          file.path(output_dir, "markup_imprese_usa.csv"))

# --- 3) Sanity check --------------------------------------------------------

cat("\nMargini in percentuale del costo, anni chiave:\n")
print(as.data.frame(serie %>%
  filter(anno %in% c(1960, 1980, 2000, ANNO_MAX)) %>%
  mutate(across(-anno, ~ round(.x, 1)))))

cat("\nMinimo della media:", round(min(serie$media), 1), "% nel", anno_min, "\n")
cat("Media", ANNO_MAX, ":", round(serie$media[serie$anno == ANNO_MAX], 1), "%\n")

# I due valori citati nell'articolo: 21 per cento nel 1980, 61 nel 2016.
stopifnot(round(serie$media[serie$anno == 1980]) == 21)
stopifnot(round(serie$media[serie$anno == 2016]) == 61)
stopifnot(all(serie$alti > serie$media), all(serie$media > serie$meta))
