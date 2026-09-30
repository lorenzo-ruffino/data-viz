# TFR aliquote 2006 e 2026 --------------------------------------------------
# Aliquota media Irpef applicata al Tfr (art. 19 Tuir) in funzione del reddito
# di riferimento, cioe' la base sulla quale si applicano gli scaglioni.
# Confronto tra gli scaglioni in vigore al 31/12/2006 e quelli del 2026.

library(tidyverse)
library(showtext)
library(ggrepel)

source_dir <- ".."
output_dir <- file.path(source_dir, "output")

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"
COL_ROSA   <- "#FCE4E7"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
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

# --- Scaglioni Irpef --------------------------------------------------------

scaglioni_2006 <- tibble(cap = c(26000, 33500, 100000, Inf),
                         aliquota = c(0.23, 0.33, 0.39, 0.43))
scaglioni_2026 <- tibble(cap = c(28000, 50000, Inf),
                         aliquota = c(0.23, 0.33, 0.43))

imposta_erariale <- function(reddito, scaglioni) {
  soglia_prec <- 0
  imposta <- 0
  for (i in seq_len(nrow(scaglioni))) {
    if (reddito > soglia_prec) {
      imposta <- imposta +
        (min(reddito, scaglioni$cap[i]) - soglia_prec) * scaglioni$aliquota[i]
      soglia_prec <- scaglioni$cap[i]
    }
  }
  imposta
}

redditi <- seq(1000, 200000, by = 250)

dati <- tibble(
  reddito = redditi,
  a2006 = map_dbl(redditi, imposta_erariale, scaglioni = scaglioni_2006) / redditi,
  a2026 = map_dbl(redditi, imposta_erariale, scaglioni = scaglioni_2026) / redditi
) |>
  mutate(scarto = a2026 - a2006)

# Soglia di incrocio: da qui il sistema 2026 applica un'aliquota piu' alta
soglia <- uniroot(
  function(x) imposta_erariale(x, scaglioni_2026) - imposta_erariale(x, scaglioni_2006),
  c(50000, 100000)
)$root

write_csv(
  dati |>
    transmute(reddito_di_riferimento = reddito,
              aliquota_2006 = round(a2006, 6),
              aliquota_2026 = round(a2026, 6),
              scarto_punti = round(scarto * 100, 3)),
  file.path(output_dir, "tfr_aliquote_2006_2026.csv")
)

dati_long <- dati |>
  pivot_longer(c(a2006, a2026), names_to = "sistema", values_to = "aliquota") |>
  mutate(sistema = recode(sistema,
                          a2006 = "Con gli scaglioni 2006",
                          a2026 = "Con gli scaglioni 2026"))

etichette <- dati_long |> filter(reddito == max(reddito))

p <- ggplot() +
  geom_rect(aes(xmin = soglia, xmax = 245000), ymin = -Inf, ymax = Inf,
            fill = COL_ROSA, alpha = 0.55) +
  geom_line(data = dati_long,
            aes(x = reddito, y = aliquota, colour = sistema),
            linewidth = 0.9) +
  geom_vline(xintercept = soglia, linetype = "dashed",
             colour = COL_GRIGIO, linewidth = 0.4) +
  geom_text_repel(data = etichette,
                  aes(x = reddito, y = aliquota, label = sistema, colour = sistema),
                  direction = "y", nudge_x = 4000, hjust = 0, size = 3.4,
                  family = "Source Sans Pro", fontface = "bold", seed = 1,
                  segment.colour = COL_GRIGIO, min.segment.length = 0,
                  box.padding = 0.6) +
  annotate("text", x = soglia, y = 0.398, label = "79.750 €",
           hjust = -0.08, size = 3.4, family = "Source Sans Pro",
           fontface = "bold", colour = COL_NERO) +
  scale_colour_manual(values = c("Con gli scaglioni 2006" = COL_BLU,
                                 "Con gli scaglioni 2026" = COL_ROSSO)) +
  scale_x_continuous(breaks = c(50000, 100000, 150000, 200000),
                     labels = function(x) {
                       paste0("€ ", format(x / 1000, big.mark = ".", decimal.mark = ","), "k")
                     },
                     limits = c(0, 245000), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.23, 0.39, 0.04),
                     labels = function(x) paste0(round(x * 100), "%"),
                     limits = c(0.22, 0.41), expand = c(0, 0)) +
  labs(title = "Come cambia la tassazione del TFR",
       subtitle = "Aliquota media Irpef sul TFR per reddito di riferimento, scaglioni 2006 e 2026",
       caption = "Elaborazione di Lorenzo Ruffino su dati normativa tributaria (2006 e 2026)") +
  theme_linechart()

ggsave(file.path(output_dir, "tfr_aliquote_2006_2026.png"),
       plot = p, width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Sanity check -----------------------------------------------------------

stopifnot(abs(soglia - 79750) < 10)
stopifnot(abs(dati$scarto[dati$reddito == 100000] - 0.00810) < 0.0001)

cat("Soglia di incrocio:", round(soglia), "euro\n")
cat("Aliquota 2006 a 50k:", round(100 * dati$a2006[dati$reddito == 50000], 2), "%\n")
cat("Aliquota 2026 a 50k:", round(100 * dati$a2026[dati$reddito == 50000], 2), "%\n")
cat("Aliquota 2006 a 100k:", round(100 * dati$a2006[dati$reddito == 100000], 2), "%\n")
cat("Aliquota 2026 a 100k:", round(100 * dati$a2026[dati$reddito == 100000], 2), "%\n")
cat("Scarto massimo:", round(100 * max(dati$scarto), 2), "punti a",
    format(dati$reddito[which.max(dati$scarto)], big.mark = "."), "euro\n")
