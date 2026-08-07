# script/evoluzione_popolazione_italia.R — eseguito da `cd script && Rscript evoluzione_popolazione_italia.R`
#
# Popolazione residente in Italia al 1° gennaio, 1862-2026, e previsioni Istat
# (base 2024, scenario mediano) fino al 2080.
# Fonti (tutte Istat):
#  - input/popolazione_storica_1862_1951.csv  — serie storica, confini attuali
#  - input/istat_164_346_ricpop_1952_1971.csv — ricostruzione intercensuaria 1952-1971
#  - input/istat_164_347_ricpop_1972_1981.csv — ricostruzione intercensuaria 1972-1981
#  - input/istat_164_279_ricpop_1982_1991.csv — ricostruzione intercensuaria 1982-1991
#  - input/istat_164_305_ricpop_1991_2001.csv — ricostruzione intercensuaria 1991-2001
#  - input/popolazione_2002_2018.csv          — ricostruzione post-censimento 2002-2018
#  - input/istat_22_289_popres_2019_2026.tsv  — popolazione residente al 1° gennaio (dataflow 22_289)
#  - input/istat_165_889_previsioni_2024_2080.tsv — previsioni demografiche, mediana (dataflow 165_889)

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 220)   # allineato al dpi di ggsave: font alle dimensioni nominali del tema

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"
CAP_ISTAT  <- "Elaborazione di Lorenzo Ruffino su dati Istat"

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

input_dir  <- "../input"
output_dir <- "../output"

# 1) Serie osservata -----------------------------------------------------------

storico_1862_1951 <- read_csv(file.path(input_dir, "popolazione_storica_1862_1951.csv"),
                              show_col_types = FALSE)

leggi_ricostruzione <- function(file, anni) {
  read_csv(file.path(input_dir, file), show_col_types = FALSE) %>%
    filter(DATA_TYPE == "JAN", TIME_PERIOD %in% anni) %>%
    transmute(anno = TIME_PERIOD, popolazione = OBS_VALUE)
}

ric_52_71 <- leggi_ricostruzione("istat_164_346_ricpop_1952_1971.csv", 1952:1971)
ric_72_81 <- leggi_ricostruzione("istat_164_347_ricpop_1972_1981.csv", 1972:1981)
ric_82_90 <- leggi_ricostruzione("istat_164_279_ricpop_1982_1991.csv", 1982:1990)
ric_91_01 <- leggi_ricostruzione("istat_164_305_ricpop_1991_2001.csv", 1991:2001)

pop_02_18 <- read_csv(file.path(input_dir, "popolazione_2002_2018.csv"),
                      show_col_types = FALSE)

pop_19_26 <- read_tsv(file.path(input_dir, "istat_22_289_popres_2019_2026.tsv"),
                      show_col_types = FALSE) %>%
  transmute(anno = TIME_PERIOD, popolazione = OBS_VALUE)

osservato <- bind_rows(storico_1862_1951, ric_52_71, ric_72_81, ric_82_90,
                       ric_91_01, pop_02_18, pop_19_26) %>%
  arrange(anno) %>%
  mutate(tipo = "osservato")

stopifnot(!any(duplicated(osservato$anno)),
          all(diff(osservato$anno) == 1),
          min(osservato$anno) == 1862, max(osservato$anno) == 2026)

# 2) Previsioni (mediana, base 2024) ------------------------------------------

previsione <- read_tsv(file.path(input_dir, "istat_165_889_previsioni_2024_2080.tsv"),
                       show_col_types = FALSE) %>%
  filter(TIME_PERIOD >= 2027) %>%
  transmute(anno = TIME_PERIOD, popolazione = OBS_VALUE, tipo = "previsione")

serie <- bind_rows(osservato, previsione)
write_csv(serie, file.path(output_dir, "evoluzione_popolazione_italia.csv"))

# 3) Grafico -------------------------------------------------------------------

# punto di raccordo al 2026: duplica l'ultimo osservato per la continuità delle aree
raccordo <- osservato %>% filter(anno == 2026) %>% mutate(tipo = "previsione")

plot_data <- bind_rows(serie, raccordo) %>%
  mutate(pop_mln = popolazione / 1e6)

picco   <- osservato  %>% slice_max(popolazione, n = 1)
ultimo  <- osservato  %>% filter(anno == max(anno))
inizio  <- osservato  %>% filter(anno == min(anno))
fine    <- previsione %>% filter(anno == max(anno))

fmt_mln <- function(x) paste0(formatC(x / 1e6, format = "f", digits = 1,
                                      decimal.mark = ","), " mln")

cat("Inizio serie:", inizio$anno, "=", fmt_mln(inizio$popolazione), "\n")
cat("Picco:", picco$anno, "=", fmt_mln(picco$popolazione), "\n")
cat("Ultimo osservato:", ultimo$anno, "=", fmt_mln(ultimo$popolazione), "\n")
cat("Fine previsioni:", fine$anno, "=", fmt_mln(fine$popolazione), "\n")

punti_chiave <- tibble(
  anno    = c(inizio$anno, picco$anno, fine$anno),
  pop_mln = c(inizio$popolazione, picco$popolazione, fine$popolazione) / 1e6,
  colore  = c(COL_BLU, COL_BLU, COL_ROSSO)
)

p <- ggplot(plot_data, aes(x = anno, y = pop_mln, fill = tipo, colour = tipo)) +
  geom_area(alpha = 0.25, position = "identity", linewidth = 0) +
  geom_line(linewidth = 0.9, show.legend = FALSE) +
  geom_vline(xintercept = 2026, colour = COL_GRIGIO,
             linewidth = 0.4, linetype = "dashed") +
  geom_point(data = punti_chiave, aes(x = anno, y = pop_mln),
             colour = punti_chiave$colore, size = 1.8, inherit.aes = FALSE) +
  annotate("text", x = inizio$anno + 2, y = inizio$popolazione / 1e6 - 2.2,
           label = fmt_mln(inizio$popolazione), hjust = 0, vjust = 1,
           family = "Source Sans Pro", fontface = "bold", size = 3.4, color = COL_BLU) +
  annotate("text", x = picco$anno, y = picco$popolazione / 1e6 + 1.8,
           label = paste0(fmt_mln(picco$popolazione), " nel ", picco$anno),
           hjust = 1, vjust = 0,
           family = "Source Sans Pro", fontface = "bold", size = 3.4, color = COL_BLU) +
  annotate("text", x = fine$anno - 2, y = fine$popolazione / 1e6 - 2.2,
           label = fmt_mln(fine$popolazione), hjust = 1, vjust = 1,
           family = "Source Sans Pro", fontface = "bold", size = 3.4, color = COL_ROSSO) +
  annotate("text", x = 1942, y = 20, label = "Dato reale",
           family = "Source Sans Pro", fontface = "bold", size = 3.8, color = COL_BLU) +
  annotate("text", x = 2054, y = 20, label = "Previsioni",
           family = "Source Sans Pro", fontface = "bold", size = 3.8, color = COL_ROSSO) +
  scale_colour_manual(values = c("osservato" = COL_BLU, "previsione" = COL_ROSSO)) +
  scale_fill_manual(values = c("osservato" = COL_BLU, "previsione" = COL_ROSSO)) +
  scale_x_continuous(limits = c(1862, 2080), breaks = seq(1880, 2080, 40),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 60, 10),
                     labels = function(x) ifelse(x == 0, "0", paste0(x, " mln")),
                     expand = c(0, 0)) +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(
    title = "L'Italia perderà 13 milioni di abitanti entro il 2080",
    subtitle = "Popolazione residente al 1° gennaio, in milioni; dal 2027 previsione mediana Istat, Italia, 1862-2080.",
    caption = CAP_ISTAT
  )

ggsave(file.path(output_dir, "evoluzione_popolazione_italia.png"),
       plot = p, width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

cat("Grafico salvato in ../output/evoluzione_popolazione_italia.png\n")
