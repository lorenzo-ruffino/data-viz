# Grafici per l'articolo "Chi proteggono davvero gli ordini professionali"
# 01: quota di lavoratori con licenza in 44 paesi (Hartley & Kleiner 2026, Fig. 2;
#     valori UE da Koumenta & Pagliero 2019, Tab. 5; Nigeria digitalizzata dalla figura)
# 02: esperimento naturale del Colorado (Pizzola & Tabarrok 2017, Fig. 1, valori digitalizzati)
# 03: indice di restrittività Italia per componente (Banca d'Italia, QEF 900/2024, Fig. 2, digitalizzata)

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

fmt_pct <- function(v) ifelse(
  v %% 1 == 0,
  paste0(as.integer(v), "%"),
  paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%")
)
fmt_num <- function(v) formatC(v, format = "f", digits = 1, decimal.mark = ",")

base_dir <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Licenze professionali"

# --- 01: 44 paesi ------------------------------------------------------------

paesi <- read_csv(file.path(base_dir, "input/licenze_44_paesi.csv"),
                  show_col_types = FALSE) |>
  mutate(paese = factor(paese, levels = paese),   # già ordinati per quota crescente
         evidenza = if_else(paese == "Italia", "italia", "altri"))

p1 <- ggplot(paesi, aes(x = quota, y = paese, fill = evidenza)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = fmt_pct(quota),
                color = evidenza, fontface = if_else(evidenza == "italia", "bold", "plain")),
            hjust = -0.15, size = 2.5, family = "Source Sans Pro", show.legend = FALSE) +
  scale_fill_manual(values = c(italia = COL_ROSSO, altri = COL_BLU)) +
  scale_color_manual(values = c(italia = COL_ROSSO, altri = COL_NERO)) +
  scale_x_continuous(limits = c(0, 47), expand = c(0, 0)) +
  theme_linechart() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_line(linewidth = 0.3),
        axis.text.y = element_text(size = 7.6,
                                   face = if_else(levels(paesi$paese) == "Italia", "bold", "plain"),
                                   color = if_else(levels(paesi$paese) == "Italia", COL_ROSSO, COL_NERO))) +
  labs(title = "Dove serve più spesso una licenza per lavorare",
       subtitle = "Quota di occupati la cui professione richiede per legge una licenza pubblica, 44 paesi, 2013-2026",
       caption = "Elaborazione di Lorenzo Ruffino su dati Hartley e Kleiner (2026)")

ggsave(file.path(base_dir, "output/01_licenze_44_paesi.png"), p1,
       width = 8, height = 10, units = "in", dpi = 220, bg = "white")

# --- 02: esperimento Colorado ------------------------------------------------

colorado <- read_csv(file.path(base_dir, "input/colorado_salari.csv"),
                     show_col_types = FALSE) |>
  pivot_longer(-anno, names_to = "serie", values_to = "salario") |>
  mutate(serie = recode(serie,
                        colorado = "Colorado",
                        stati_uniti = "Stati Uniti\n(escluso\nil Colorado)"))

lab2 <- colorado |> filter(anno == max(anno))
val2 <- colorado |> filter(anno %in% c(1975, 1994))

p2 <- ggplot(colorado, aes(x = anno, y = salario, color = serie)) +
  geom_vline(xintercept = 1983, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 1982.6, y = 470, label = "1983: il Colorado\nabolisce la licenza",
           hjust = 1, size = 3.1, family = "Source Sans Pro", color = "#5A5A5A", lineheight = 1.1) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  geom_text(data = lab2, aes(label = serie),
            hjust = 0, nudge_x = 0.4, size = 3.4, fontface = "bold",
            family = "Source Sans Pro", lineheight = 1, show.legend = FALSE) +
  geom_text(data = val2,
            aes(label = paste0("$ ", salario),
                hjust = if_else(anno == 1975, 0.15, 1.05)),
            vjust = if_else(val2$serie == "Colorado", 2, -1.2),
            size = 3.3, fontface = "bold", family = "Source Sans Pro", show.legend = FALSE) +
  scale_color_manual(values = c("Colorado" = COL_BLU,
                                "Stati Uniti\n(escluso\nil Colorado)" = COL_ROSSO)) +
  scale_x_continuous(breaks = seq(1975, 1993, 2), limits = c(1975, 2000.5), expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 500, 100), limits = c(0, 500), expand = c(0, 0),
                     labels = function(x) paste0("$ ", x)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "Cosa succede quando una licenza viene abolita",
       subtitle = "Salario settimanale medio dei dipendenti del settore dei servizi funebri, dollari correnti, 1975-1994",
       caption = "Elaborazione di Lorenzo Ruffino su dati Pizzola e Tabarrok (2017)")

ggsave(file.path(base_dir, "output/02_colorado_esperimento.png"), p2,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 03: indice di restrittività Italia --------------------------------------

indice <- read_csv(file.path(base_dir, "input/italia_indice_restrittivita.csv"),
                   show_col_types = FALSE) |>
  mutate(componente = factor(componente,
                             levels = c("Totale", "Barriere all'ingresso", "Regole di condotta")))

lab3 <- indice |> filter(anno == max(anno))
val3 <- indice |> filter(anno %in% c(2003, 2023))

p3 <- ggplot(indice, aes(x = anno, y = valore, color = componente)) +
  geom_vline(xintercept = c(2006, 2012), colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2006.3, y = 4.45, label = "riforme\nBersani", hjust = 0, vjust = 1,
           size = 3, family = "Source Sans Pro", color = "#5A5A5A", lineheight = 1.05) +
  annotate("text", x = 2012.3, y = 4.45, label = "riforme\nMonti", hjust = 0, vjust = 1,
           size = 3, family = "Source Sans Pro", color = "#5A5A5A", lineheight = 1.05) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  geom_text(data = lab3, aes(label = componente),
            hjust = 0, nudge_x = 0.5, size = 3.4, fontface = "bold",
            family = "Source Sans Pro", lineheight = 1, show.legend = FALSE) +
  geom_text(data = val3,
            aes(label = fmt_num(valore)),
            vjust = if_else(val3$componente == "Barriere all'ingresso", 1.9, -1.1),
            hjust = if_else(val3$anno == 2003, 0.3, 0.7),
            size = 3.3, fontface = "bold", family = "Source Sans Pro", show.legend = FALSE) +
  scale_color_manual(values = c("Totale" = COL_NERO,
                                "Barriere all'ingresso" = COL_ROSSO,
                                "Regole di condotta" = COL_BLU)) +
  scale_x_continuous(breaks = c(2003, 2008, 2013, 2018, 2023),
                     limits = c(2003, 2031), expand = c(0.01, 0)) +
  scale_y_continuous(breaks = 0:4, limits = c(0, 4.7), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "La liberalizzazione ha inciso soprattutto sulla condotta",
       subtitle = "Indice di restrittività della regolamentazione delle professioni ordinistiche in Italia, per componente,\nda 0 = minima a 6 = massima, 2003-2023",
       caption = "Elaborazione di Lorenzo Ruffino su dati Banca d'Italia")

ggsave(file.path(base_dir, "output/03_italia_indice_restrittivita.png"), p3,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 02b: prezzi dei funerali (Colorado vs resto USA) ------------------------
# Fonte: Pizzola & Tabarrok (2017), Fig. 6 — log dei ricavi reali per decesso
# (Economic Census + CDC), qui convertiti in indice 1982 = 100.

prezzi <- read_csv(file.path(base_dir, "input/colorado_prezzi.csv"),
                   show_col_types = FALSE)

lab2b <- prezzi |> filter(x == 2)

p2b <- ggplot(prezzi, aes(x = x, y = indice, color = serie)) +
  geom_vline(xintercept = 1.5, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 1.53, y = 121, label = "1983: il Colorado\nabolisce la licenza",
           hjust = 0, size = 3.1, family = "Source Sans Pro", color = "#5A5A5A", lineheight = 1.1) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  annotate("text", x = 0.97, y = 100, label = "100", hjust = 1,
           size = 3.3, fontface = "bold", family = "Source Sans Pro", color = COL_NERO) +
  geom_text(data = lab2b,
            aes(label = if_else(serie == "Colorado", "100", "117")),
            hjust = 0.5, vjust = if_else(lab2b$serie == "Colorado", 2, -1.1),
            size = 3.3, fontface = "bold", family = "Source Sans Pro", show.legend = FALSE) +
  geom_text(data = lab2b,
            aes(label = if_else(serie == "Colorado", "Colorado", "Stati Uniti\n(escluso il Colorado)")),
            hjust = 0, nudge_x = 0.06, size = 3.4, fontface = "bold",
            family = "Source Sans Pro", lineheight = 1, show.legend = FALSE) +
  scale_color_manual(values = c("Colorado" = COL_BLU,
                                "Stati Uniti (escluso il Colorado)" = COL_ROSSO)) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("1982\nprima dell'abolizione", "1987-1992\nmedia, dopo l'abolizione"),
                     limits = c(0.85, 2.75), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(100, 120, 10), limits = c(96, 124), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none",
        axis.text.x = element_text(lineheight = 1.15)) +
  labs(title = "Dove la licenza resta, i prezzi dei funerali salgono",
       subtitle = "Ricavi reali delle imprese funerarie per decesso, una misura del prezzo medio di un funerale,\nindice 1982 = 100, Colorado e resto degli Stati Uniti",
       caption = "Elaborazione di Lorenzo Ruffino su dati Pizzola e Tabarrok (2017)")

ggsave(file.path(base_dir, "output/02b_colorado_prezzi.png"), p2b,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- export dati puliti ------------------------------------------------------
write_csv(prezzi, file.path(base_dir, "output/colorado_prezzi.csv"))
write_csv(paesi |> select(paese, quota), file.path(base_dir, "output/licenze_44_paesi.csv"))
write_csv(colorado, file.path(base_dir, "output/colorado_salari.csv"))
write_csv(indice, file.path(base_dir, "output/italia_indice_restrittivita.csv"))

# --- sanity check ------------------------------------------------------------
cat("Paesi:", nrow(paesi), "| Italia:", paesi$quota[paesi$paese == "Italia"],
    "| min:", min(paesi$quota), "| max:", max(paesi$quota), "\n")
cat("Colorado 1994:", colorado$salario[colorado$anno == 1994 & colorado$serie == "Colorado"],
    "| USA 1994:", colorado$salario[colorado$anno == 1994 & colorado$serie != "Colorado"], "\n")
cat("Indice totale 2003 e 2023:",
    indice$valore[indice$anno == 2003 & indice$componente == "Totale"],
    indice$valore[indice$anno == 2023 & indice$componente == "Totale"], "\n")
