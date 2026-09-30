# Modello Svedese — Grafico 04
# Dove la Svezia è avanti e dove no: griglia di sei pannelli a barre con
# Svezia, Italia e media UE27, ultimo anno disponibile. Fonte: Eurostat.

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

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

DIR_DATI <- "/Users/lorenzoruffino/Downloads/SVEZIA/analisi/dati"
DIR_OUT  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Modello Svedese/output"

# --- Dati -------------------------------------------------------------------

# ordine: prima gli indicatori dove la Svezia è avanti, in fondo
# disoccupazione e disuguaglianza
indicatori <- tribble(
  ~file,                       ~etichetta,                                  ~unita,   ~decimali, ~ordine,
  "occupazione_20_64_F.csv",   "Occupazione femminile 20-64 anni",           "pct",    1,         1,
  "laureati_25_34.csv",        "Laureati tra i 25 e i 34 anni",              "pct",    1,         2,
  "rs_pc_pil.csv",             "Spesa in ricerca e sviluppo, % del PIL",     "pct",    1,         3,
  "eta_mediana.csv",           "Età mediana della popolazione, in anni",     "anni",   1,         4,
  "disoccupazione_15_74.csv",  "Tasso di disoccupazione 15-74 anni",         "pct",    1,         5,
  "gini.csv",                  "Indice di Gini del reddito disponibile",     "indice", 1,         6
)

leggi_indicatore <- function(file, etichetta, unita, decimali, ordine) {
  read_csv(file.path(DIR_DATI, file),
           col_types = cols(time = col_integer(), .default = col_double())) %>%
    # ultimo anno con il dato disponibile per tutti e tre
    filter(!is.na(SE), !is.na(IT), !is.na(EU27_2020)) %>%
    slice_max(time, n = 1) %>%
    pivot_longer(c(SE, IT, EU27_2020), names_to = "codice", values_to = "valore") %>%
    transmute(
      ordine, etichetta, unita, decimali,
      anno = time,
      area = recode(codice, SE = "Svezia", IT = "Italia", EU27_2020 = "Media UE27"),
      valore
    )
}

dati <- pmap_dfr(indicatori, leggi_indicatore)

stopifnot(nrow(dati) == 18)

# etichetta del pannello con l'anno del dato: le diciture sono tenute corte
# perché a 2 colonne stanno su una riga sola e i sei pannelli restano allineati
strip_lab <- dati %>%
  distinct(ordine, etichetta, anno) %>%
  mutate(strip = str_wrap(paste0(etichetta, " (", anno, ")"), width = 46)) %>%
  arrange(ordine)

stopifnot(!any(str_detect(strip_lab$strip, "\n")))

dati <- dati %>%
  left_join(strip_lab %>% select(ordine, strip), by = "ordine") %>%
  mutate(
    strip = factor(strip, levels = strip_lab$strip),
    area  = factor(area, levels = c("Italia", "Svezia", "Media UE27"))
  )

# --- Etichette di valore ----------------------------------------------------

fmt_valore <- function(v, unita, decimali) {
  num <- mapply(function(x, d) formatC(x, format = "f", digits = d,
                                       decimal.mark = ","),
                v, decimali, USE.NAMES = FALSE)
  ifelse(unita == "pct", paste0(num, "%"), num)
}

dati <- dati %>% mutate(label = fmt_valore(valore, unita, decimali))

# spazio sopra la barra più alta di ogni pannello per l'etichetta di valore
cornici <- dati %>%
  group_by(strip) %>%
  summarise(valore = max(valore) * 1.18, .groups = "drop") %>%
  mutate(area = factor("Italia", levels = levels(dati$area)))

colori <- c("Italia" = COL_ROSSO, "Svezia" = COL_BLU, "Media UE27" = COL_GRIGIO)

# --- Grafico ----------------------------------------------------------------

p <- ggplot(dati, aes(x = area, y = valore, fill = area)) +
  geom_col(width = 0.62) +
  # linea di base in ogni pannello: con scales = "free_y" ggplot disegnerebbe
  # l'asse x solo sotto la riga inferiore della griglia
  geom_hline(yintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_blank(data = cornici, aes(x = area, y = valore)) +
  geom_text(aes(label = label, colour = area), vjust = -0.55,
            family = "Source Sans Pro", fontface = "bold", size = 3.4,
            show.legend = FALSE) +
  scale_fill_manual(values = colori) +
  scale_colour_manual(values = colori) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.02))) +
  # axes = "all_x" ripete le etichette dei paesi sotto ogni pannello, non solo
  # sotto la riga inferiore della griglia
  facet_wrap(~ strip, ncol = 2, scales = "free_y", axes = "all_x") +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 9.5, colour = COL_NERO, hjust = 0.5,
                               margin = margin(t = 0.15, unit = "cm")),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    strip.text = element_text(family = "Source Sans Pro SemiBold", size = 10,
                              colour = COL_NERO, hjust = 0, lineheight = 1.2,
                              margin = margin(b = 0.2, t = 0.1, unit = "cm")),
    panel.spacing.x = unit(1.0, "cm"),
    panel.spacing.y = unit(0.9, "cm"),
    # senza legenda il sottotitolo finirebbe attaccato al primo pannello
    plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                 lineheight = 1.35,
                                 margin = margin(b = 0.7, t = 0.1, unit = "cm"))
  ) +
  labs(
    title = "Dove la Svezia è avanti e dove no",
    subtitle = "Svezia, Italia e media UE27, ultimo anno disponibile",
    caption = CAP_EUROSTAT
  )

ggsave(file.path(DIR_OUT, "04_confronto.png"), p,
       width = 9, height = 9, dpi = 220, bg = "white")

# --- Sanity check -----------------------------------------------------------

cat("\nValori usati:\n")
dati %>%
  mutate(indicatore = str_replace_all(as.character(strip), "\n", " ")) %>%
  select(indicatore, anno, area, valore) %>%
  pivot_wider(names_from = area, values_from = valore) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
