# Grafico 1 — Spesa per pensioni in % del Pil, 1995-2023
# Fonte: Eurostat, spr_exp_pens (ESSPROS), dati in input/spesa_pensioni_pil.csv
# Il file contiene sia le funzioni disaggregate sia il TOTAL già in % del Pil:
# si usa il TOTAL (evita artefatti di arrotondamento nella somma delle componenti).

library(tidyverse)
library(showtext)
library(ggrepel)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
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

# --- Dati -------------------------------------------------------------------

ANNO_MIN <- 1995
ANNO_MAX <- 2023

dati <- read_csv("../input/spesa_pensioni_pil.csv",
                 col_types = cols(benefit = col_character(),
                                  geo = col_character(),
                                  anno = col_integer(),
                                  valore = col_double()))

paesi <- c("IT" = "Italia", "FR" = "Francia", "ES" = "Spagna",
           "DE" = "Germania", "EL" = "Grecia", "EU27_2020" = "Media Ue")

plot_data <- dati %>%
  filter(benefit == "TOTAL", geo %in% names(paesi),
         anno >= ANNO_MIN, anno <= ANNO_MAX) %>%
  mutate(paese = paesi[geo])

# --- Validazione obbligatoria (valori 2023 attesi) --------------------------

attesi <- c("Italia" = 15.5, "Francia" = 14.6, "Grecia" = 14.0,
            "Spagna" = 13.2, "Germania" = 11.5, "Media Ue" = 12.3)

check <- plot_data %>%
  filter(anno == 2023) %>%
  mutate(arrotondato = round(valore, 1),
         atteso = attesi[paese],
         ok = arrotondato == atteso)

print(as.data.frame(check[, c("paese", "valore", "arrotondato", "atteso", "ok")]))
if (!all(check$ok) || nrow(check) != 6) {
  stop("VALIDAZIONE FALLITA: i totali 2023 non corrispondono ai valori di controllo.")
}
cat("Validazione 2023 superata.\n")

# Coerenza TOTAL vs somma delle componenti (tolleranza per arrotondamenti)
somma_comp <- dati %>%
  filter(benefit != "TOTAL", geo %in% names(paesi), anno == 2023) %>%
  group_by(geo) %>% summarise(somma = sum(valore), .groups = "drop") %>%
  left_join(filter(plot_data, anno == 2023), by = "geo") %>%
  mutate(diff = abs(somma - valore))
stopifnot(all(somma_comp$diff < 0.05))
cat("Coerenza TOTAL vs somma componenti: ok (diff max",
    max(somma_comp$diff), ")\n")

# --- Grafico ----------------------------------------------------------------

palette_serie <- c(
  "Italia"   = COL_ROSSO,
  "Francia"  = COL_VIOLA,
  "Germania" = COL_NERO,
  "Spagna"   = COL_GIALLO,
  "Grecia"   = COL_BLU,
  "Media Ue" = COL_GRIGIO
)

fmt_it <- function(v) formatC(v, format = "f", digits = 1, decimal.mark = ",")

label_data <- plot_data %>%
  filter(anno == ANNO_MAX) %>%
  mutate(label = paste0(paese, " ", fmt_it(round(valore, 1)), "%"))

secondarie <- plot_data %>% filter(!paese %in% c("Italia", "Media Ue"))
media_ue   <- plot_data %>% filter(paese == "Media Ue")
italia     <- plot_data %>% filter(paese == "Italia")

p <- ggplot(mapping = aes(x = anno, y = valore, colour = paese)) +
  geom_line(data = secondarie, linewidth = 0.7, alpha = 0.55) +
  geom_line(data = media_ue, linewidth = 0.7, linetype = "42") +
  geom_line(data = italia, linewidth = 1.1) +
  geom_text_repel(data = label_data,
                  aes(label = label),
                  direction = "y", nudge_x = 0.4, hjust = 0,
                  size = 3.2, fontface = "bold",
                  family = "Source Sans Pro",
                  segment.colour = "#9A9A9A", min.segment.length = 0,
                  box.padding = 0.15, seed = 1) +
  scale_colour_manual(values = palette_serie) +
  scale_x_continuous(breaks = seq(1995, 2020, 5),
                     limits = c(ANNO_MIN, ANNO_MAX + 4.6),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(8, 18, 2),
                     limits = c(8, 18.6),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(
    title = "L'Italia spende in pensioni più di tutti in Europa",
    subtitle = "Spesa per pensioni in percentuale del Pil (Esspros: vecchiaia, anzianità, superstiti e invalidità), media Ue dal 2008",
    caption = "Fonte: Eurostat"
  )

ggsave("../output/01_spesa_pil.png", p,
       width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

# CSV pulito
plot_data %>%
  select(paese, anno, valore) %>%
  arrange(paese, anno) %>%
  write_csv("../output/01_spesa_pil.csv")

# --- Sanity check finale ----------------------------------------------------
cat("\nValori 2023 usati nel grafico:\n")
plot_data %>% filter(anno == 2023) %>% arrange(desc(valore)) %>%
  with(cat(paste0(paese, ": ", fmt_it(valore)), sep = "\n"))
cat("\nPicco Italia:", max(italia$valore), "nel",
    italia$anno[which.max(italia$valore)], "\n")
cat("Range complessivo:", min(plot_data$valore), "-", max(plot_data$valore), "\n")
cat("Prima osservazione media Ue:", min(media_ue$anno), "\n")
