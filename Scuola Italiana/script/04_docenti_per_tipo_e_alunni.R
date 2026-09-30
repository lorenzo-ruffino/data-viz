# Scuola Italiana — 04
# Docenti e alunni della scuola statale, indice 2015/16 = 100, dall'a.s. 2015/16
# al 2024/25. Tre linee: docenti totali, docenti di sostegno (di ruolo più
# supplenti) e alunni della scuola statale.
#
# Fonti:
# - `mim_personale_scuola_statale_serie_storica.csv`.
# - `mim_alunni_classi_per_anno_scolastico_ordine_provincia.csv`, filtrato
#   `gestione == "statale"` (escluse le paritarie) e aggregato per anno scolastico.
#   Il dataset MIM non comprende la scuola dell'infanzia.

suppressMessages({
  library(tidyverse)
  library(showtext)
  library(ggrepel)
})

BASE   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Scuola Italiana"
INPUT  <- file.path(BASE, "input")
OUTPUT <- file.path(BASE, "output")

FIG_W <- 10
FIG_H <- 6.8
wrap_titolo <- function(x) str_wrap(x, width = floor(FIG_W * 9.2))
wrap_sub    <- function(x) str_wrap(x, width = floor(FIG_W * 12.3))

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_VERDE  <- "#1B9E77"
COL_GRIGIO <- "#9A9A9A"

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
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = COL_NERO, hjust = 0,
                                lineheight = 1.2,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP <- "Elaborazione di Lorenzo Ruffino su dati del Ministero dell'istruzione"

# --- Dati -------------------------------------------------------------------

serie_nomi <- c("Docenti di sostegno", "Docenti totali",
                "Alunni della scuola statale")

colori <- c("Docenti di sostegno"         = COL_VIOLA,
            "Docenti totali"              = COL_NERO,
            "Alunni della scuola statale" = COL_VERDE)

personale <- read_csv(file.path(INPUT, "mim_personale_scuola_statale_serie_storica.csv"),
                      show_col_types = FALSE) %>%
  mutate(anno = as.integer(str_sub(anno_scolastico, 1, 4)))

alunni <- read_csv(file.path(INPUT, "mim_alunni_classi_per_anno_scolastico_ordine_provincia.csv"),
                   show_col_types = FALSE) %>%
  filter(gestione == "statale") %>%
  group_by(anno_scolastico) %>%
  summarise(alunni = sum(alunni_totale), .groups = "drop") %>%
  mutate(anno = as.integer(str_sub(as.character(anno_scolastico), 1, 4))) %>%
  select(anno, alunni)

livelli <- personale %>%
  transmute(anno, anno_scolastico,
            `Docenti totali` = docenti_totali,
            `Docenti di sostegno` = docenti_titolari_sostegno + docenti_supplenti_sostegno) %>%
  left_join(alunni, by = "anno") %>%
  rename(`Alunni della scuola statale` = alunni)

ANNO_MIN <- min(livelli$anno)
ANNO_MAX <- max(livelli$anno)

etichette_x <- livelli %>% select(anno, anno_scolastico) %>% deframe()

dati <- livelli %>%
  pivot_longer(all_of(serie_nomi), names_to = "serie", values_to = "valore") %>%
  group_by(serie) %>%
  arrange(anno, .by_group = TRUE) %>%
  mutate(indice = valore / first(valore) * 100) %>%
  ungroup() %>%
  mutate(serie = factor(serie, levels = serie_nomi))

etichette <- dati %>%
  filter(anno == ANNO_MAX) %>%
  mutate(testo = paste0(serie, "  ", round(indice)))

p <- ggplot(dati, aes(x = anno, y = indice, colour = serie)) +
  geom_hline(yintercept = 100, colour = COL_GRIGIO,
             linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  geom_text_repel(data = etichette, aes(label = testo),
                  hjust = 0, nudge_x = 0.25, direction = "y",
                  size = 3.2, fontface = "bold", family = "Source Sans Pro",
                  segment.colour = COL_GRIGIO, min.segment.length = 0,
                  box.padding = 0.2, seed = 1) +
  scale_colour_manual(values = colori) +
  scale_x_continuous(limits = c(ANNO_MIN, ANNO_MAX + 4.6),
                     breaks = ANNO_MIN:ANNO_MAX,
                     labels = function(x) etichette_x[as.character(x)],
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(88, 205), breaks = seq(100, 200, 20),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8.5)) +
  labs(title = wrap_titolo("In Italia diminuiscono gli studenti ma aumentano gli insegnanti"),
       subtitle = wrap_sub(paste0(
         "Docenti e alunni della scuola statale esclusa l'infanzia, ",
         "indice 2015/16 = 100, anni scolastici dal 2015/16 al 2024/25")),
       caption = CAP)

ggsave(file.path(OUTPUT, "04_docenti_per_tipo_e_alunni.png"), p,
       width = FIG_W, height = FIG_H, dpi = 220, bg = "white")

write_csv(dati %>% select(anno_scolastico, serie, valore, indice),
          file.path(OUTPUT, "04_docenti_per_tipo_e_alunni.csv"))

# --- Sanity check -----------------------------------------------------------

cat("--- Indice 2024/25 (2015/16 = 100) ---\n")
print(etichette %>% select(serie, valore, indice) %>%
        mutate(indice = round(indice, 1)) %>% arrange(desc(indice)) %>% as.data.frame())
cat("--- Livelli 2015/16 ---\n")
print(dati %>% filter(anno == ANNO_MIN) %>% select(serie, valore) %>% as.data.frame())
