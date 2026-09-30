# script/prezzi_telefonia.R — eseguito da `cd script && Rscript prezzi_telefonia.R`
#
# Prezzi dei servizi di telefonia in Italia contro l'indice generale dei prezzi
# al consumo, numeri indice mensili con base gennaio 2012 = 100.
#
# Dati in input scaricati dall'API SDMX di Eurostat (prc_hicp_midx, base 2015):
#   M.I15.CP0830.IT  servizi di telefonia e telefax
#   M.I15.CP00.IT    indice generale (tutte le voci)
#
# Le due date annotate sono la condizione imposta da Bruxelles alla fusione
# Wind-Tre (caso M.7758, 1 settembre 2016: cessione di frequenze a un quarto
# operatore) e il lancio commerciale di Iliad (29 maggio 2018).

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

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

# Su un canvas da 9 pollici una riga di sottotitolo tiene circa 124 caratteri:
# gli a capo vanno messi solo quando la riga è piena, e su una virgola o un
# punto, altrimenti sembrano casuali.

# --- 1) Dati ----------------------------------------------------------------

leggi_hicp <- function(file, etichetta) {
  read_csv(file.path(input_dir, file),
           col_types = cols(.default = col_character(),
                            OBS_VALUE = col_double())) %>%
    transmute(anno = as.integer(str_sub(TIME_PERIOD, 1, 4)),
              mese = as.integer(str_sub(TIME_PERIOD, 6, 7)),
              serie = etichetta,
              indice = OBS_VALUE) %>%
    filter(!is.na(indice))
}

BASE <- 2012

grezzi <- bind_rows(
  leggi_hicp("eurostat_hicp_telefonia_mensile.csv", "telefonia"),
  leggi_hicp("eurostat_hicp_indice_generale_mensile.csv", "generale")
)

# Medie annue, come le pubblica Eurostat: sono i numeri citati nell'articolo.
completi <- grezzi %>%
  count(anno, serie) %>%
  filter(n == 12) %>%
  distinct(anno)

dati <- grezzi %>%
  semi_join(completi, by = "anno") %>%
  filter(anno >= BASE) %>%
  group_by(serie, anno) %>%
  summarise(indice = mean(indice), .groups = "drop") %>%
  group_by(serie) %>%
  mutate(valore = indice / indice[anno == BASE] * 100) %>%
  ungroup() %>%
  mutate(serie = factor(serie, levels = c("generale", "telefonia")))

write_csv(
  dati %>%
    select(anno, serie, indice_2015 = indice, indice_2012 = valore) %>%
    mutate(across(starts_with("indice"), ~ round(.x, 1))) %>%
    arrange(serie, anno),
  file.path(output_dir, "prezzi_telefonia.csv")
)

ANNO_MIN <- min(dati$anno)
ANNO_MAX <- max(dati$anno)

# --- 2) Grafico -------------------------------------------------------------

col_serie <- c(generale = COL_BLU, telefonia = COL_ROSSO)

# Etichette delle serie accostate alla propria linea, senza legenda.
label_serie <- tibble(
  serie  = factor(c("generale", "telefonia"), levels = levels(dati$serie)),
  anno   = c(2022.5, 2021.4),
  valore = c(112, 90),
  testo  = c("Inflazione", "Servizi di telefonia")
)

# Il grosso del calo precede sia la condizione di Bruxelles sia Iliad.
nota_calo <- tibble(
  anno   = 2012.1,
  valore = 83.5,
  testo  = "Fra il 2012 e il 2016 i prezzi\nerano già scesi dell’11 per cento"
)

label_valori <- dati %>%
  filter(anno == ANNO_MAX) %>%
  mutate(testo = formatC(valore, format = "f", digits = 0, decimal.mark = ","))

# Eventi: la condizione antitrust e l'ingresso del quarto operatore.
eventi <- tibble(
  anno  = c(2016.67, 2018.4),
  testo = c("Bruxelles impone\nun quarto operatore\nnella fusione Wind-Tre",
            "Iliad entra\nnel mercato"),
  y     = c(131, 118),
  hjust = c(1.04, -0.04)
)

p <- ggplot(dati, aes(x = anno, y = valore, colour = serie)) +
  geom_hline(yintercept = 100, linewidth = 0.3, colour = COL_GRIGIO,
             linetype = "dotted") +
  geom_segment(data = eventi, inherit.aes = FALSE,
               aes(x = anno, xend = anno, y = 86, yend = y - 3),
               linewidth = 0.3, colour = COL_GRIGIO) +
  geom_text(data = eventi, inherit.aes = FALSE,
            aes(x = anno, y = y, label = testo, hjust = hjust),
            family = "Source Sans Pro", size = 3, colour = COL_NERO,
            lineheight = 1.05, vjust = 1) +
  geom_text(data = nota_calo, inherit.aes = FALSE,
            aes(x = anno, y = valore, label = testo),
            family = "Source Sans Pro", size = 3, colour = COL_GRIGIO,
            lineheight = 1.05, hjust = 0, vjust = 1) +
  geom_line(linewidth = 0.9) +
  geom_point(data = label_valori, size = 1.8) +
  geom_text(data = label_serie, aes(label = testo),
            family = "Source Sans Pro", fontface = "bold",
            size = 3.6, hjust = 0) +
  geom_text(data = label_valori, aes(label = testo),
            family = "Source Sans Pro", fontface = "bold", size = 3.4,
            hjust = -0.35) +
  scale_colour_manual(values = col_serie) +
  scale_x_continuous(breaks = seq(2012, 2024, 2),
                     limits = c(ANNO_MIN, ANNO_MAX + 1.1),
                     expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(80, 130, 10), limits = c(78, 133),
                     expand = c(0.01, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  labs(
    title = "Telefonare costa meno che in passato",
    subtitle = paste0(
      "Prezzi al consumo dei servizi di telefonia e inflazione complessiva in ",
      "Italia, medie annue con base ", BASE, " = 100"
    ),
    caption = CAP_EUROSTAT
  )

ggsave(file.path(output_dir, "prezzi_telefonia.png"), plot = p,
       width = 9, height = 6, dpi = 220, bg = "white")

# --- 3) Sanity check --------------------------------------------------------

chiave <- dati %>%
  filter(anno %in% c(2012, 2017, 2018, 2019, ANNO_MAX)) %>%
  select(anno, serie, valore) %>%
  pivot_wider(names_from = serie, values_from = valore) %>%
  mutate(across(-anno, ~ round(.x, 1)))

cat("\nNumeri indice (media", BASE, "= 100):\n")
print(as.data.frame(chiave))

var_tot <- dati %>% filter(anno == ANNO_MAX) %>%
  mutate(var = round(valore - 100, 1)) %>% select(serie, var)
cat("\nVariazione cumulata dal", BASE, "(punti indice):\n")
print(as.data.frame(var_tot))

tel <- dati %>% filter(serie == "telefonia")
var_2019 <- tel$valore[tel$anno == 2019] / tel$valore[tel$anno == 2018] * 100 - 100
cat("\nTelefonia nel 2019 sul 2018:", paste0(round(var_2019, 1), "%\n"))

# I due valori citati nell'articolo: telefonia -17 per cento, indice +26.
stopifnot(nrow(dati) == 2 * length(unique(dati$anno)))
stopifnot(round(tel$valore[tel$anno == ANNO_MAX] - 100) == -17)
stopifnot(round(dati$valore[dati$serie == "generale" &
                              dati$anno == ANNO_MAX] - 100) == 26)
