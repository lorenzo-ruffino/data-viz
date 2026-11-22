# script/grafici_thread.R — eseguito da `cd script && Rscript grafici_thread.R`
# Grafici del thread sul ballottaggio (25 ottobre 2026). Il primo e' quello di
# 03_grafico.R, copiato; qui il 2 (voti grezzi dopo il primo turno) e il 3
# (vantaggio di Flavio nello stesso istituto, inizio e fine ottobre).

library(tidyverse)
library(showtext)
library(patchwork)

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

CAP <- "Elaborazione di Lorenzo Ruffino su sondaggi raccolti da Wikipedia"

input_dir  <- file.path("..", "input")
output_dir <- file.path("..", "output")

fmt_pct <- function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")

PRIMO <- as.Date("2026-10-04")
VOTO  <- as.Date("2026-10-25")
colori <- c(lula = COL_ROSSO, flavio = COL_BLU, indecisi = COL_GRIGIO_SCURO)

# ── 1. Media dei sondaggi (da 03_grafico.R) ──────────────────────────────────
file.copy(file.path(output_dir, "sondaggi_brasile_ballottaggio_2026.png"),
          file.path(output_dir, "01_media_sondaggi.png"), overwrite = TRUE)

# ── 2. Voti grezzi dopo il primo turno ───────────────────────────────────────
grezze <- read_csv(file.path(output_dir, "medie_grezze.csv"), show_col_types = FALSE)
punti <- read_csv(file.path(input_dir, "sondaggi_grezzi.csv"), show_col_types = FALSE) %>%
  filter(fase == "dopo") %>%
  select(data = data_media, lula, flavio, indecisi = bianche_indecisi) %>%
  pivot_longer(-data, names_to = "voce", values_to = "valore")

fine <- max(grezze$data)
et2 <- filter(grezze, data == fine) %>%
  mutate(nome = recode(voce, lula = "Lula", flavio = "Bolsonaro", indecisi = ""),
         label = if_else(nome == "", fmt_pct(round(media, 1)),
                         paste0(nome, "\n", fmt_pct(round(media, 1)))))

# I due pannelli coprono lo stesso intervallo verticale (14 punti), cosi' gli
# spostamenti si confrontano a occhio
pannello_grezzi <- function(voci, y_lim, titolo) {
  g <- filter(grezze, voce %in% voci)
  ggplot() +
    geom_ribbon(data = g, aes(x = data, ymin = media - 1.645 * sd,
                              ymax = media + 1.645 * sd, fill = voce), alpha = 0.12) +
    geom_point(data = filter(punti, voce %in% voci),
               aes(x = data, y = valore, colour = voce), size = 1.1, alpha = 0.35) +
    geom_line(data = g, aes(x = data, y = media, colour = voce), linewidth = 1.0) +
    geom_text(data = filter(et2, voce %in% voci),
              aes(x = data, y = media, label = label, colour = voce),
              hjust = 0, nudge_x = 0.4, size = 3.4, fontface = "bold",
              lineheight = 0.9, family = "Source Sans Pro") +
    annotate("text", x = PRIMO + 1.3, y = y_lim[2] - 0.3, label = titolo, hjust = 0,
             vjust = 1, size = 3.4, colour = COL_GRIGIO_SCURO,
             family = "Source Sans Pro SemiBold") +
    scale_colour_manual(values = colori) +
    scale_fill_manual(values = colori) +
    scale_x_date(limits = c(PRIMO + 0.5, VOTO + 7),
                 breaks = as.Date(c("2026-10-05", "2026-10-12", "2026-10-19", "2026-10-25")),
                 labels = c("5 ott", "12 ott", "19 ott", "25 ott"), expand = c(0, 0)) +
    scale_y_continuous(limits = y_lim, breaks = seq(y_lim[1], y_lim[2], 2),
                       labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_linechart()
}

p2 <- pannello_grezzi(c("lula", "flavio"), c(40, 54), "Lula e Bolsonaro") +
  pannello_grezzi("indecisi", c(0, 14), "Bianche, nulle e indecisi") +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = "Lula ha guadagnato più di quanto Bolsonaro abbia perso",
    subtitle = "Media dei sondaggi sul ballottaggio, in percentuale degli intervistati, Brasile, 5-25 ottobre 2026",
    caption = CAP, theme = theme_linechart())

ggsave(file.path(output_dir, "02_voti_grezzi.png"),
       plot = p2, width = 9, height = 6.5, dpi = 220, bg = "white")

# ── 3. Stesso istituto: inizio e fine ottobre ────────────────────────────────
# Solo gli istituti con un sondaggio nella prima settimana dopo il voto (entro
# l'11 ottobre) e almeno un altro dopo: chi ha iniziato il 17 non misura il
# recupero di Lula.
stesso <- read_csv(file.path(output_dir, "stesso_istituto.csv"), show_col_types = FALSE) %>%
  filter(dal <= as.Date("2026-10-11")) %>%
  mutate(da = 100 - 2 * lula_2p_da, a = 100 - 2 * lula_2p_a,
         istituto = fct_reorder(istituto, a),
         col_fine = if_else(a >= 0, "flavio", "lula"))
cat("Istituti nel grafico 3:", nrow(stesso), "| vantaggio ridotto:", sum(stesso$a < stesso$da - 0.3), "\n")
print(select(stesso, istituto, dal, al, da, a))

fmt_punti <- function(x) paste0(ifelse(x > 0, "+", ""), formatC(x, format = "f", digits = 1, decimal.mark = ","))

p3 <- ggplot(stesso) +
  geom_vline(xintercept = 0, colour = COL_GRIGIO, linewidth = 0.3, linetype = "dashed") +
  geom_segment(aes(x = da, xend = a, y = istituto, yend = istituto),
               colour = COL_GRIGIO, linewidth = 0.6,
               arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  geom_point(aes(x = da, y = istituto), colour = COL_GRIGIO, size = 2.2) +
  geom_point(aes(x = a, y = istituto, colour = col_fine), size = 2.6) +
  # Etichette all'esterno del segmento: il primo valore dalla parte opposta
  # all'ultimo. Se non c'e' stato movimento resta solo l'ultimo.
  geom_text(data = filter(stesso, abs(da - a) >= 0.1),
            aes(x = da, y = istituto, label = fmt_punti(da), hjust = if_else(da > a, -0.35, 1.35)),
            colour = COL_GRIGIO_SCURO, size = 3.0, family = "Source Sans Pro") +
  geom_text(aes(x = a, y = istituto, label = fmt_punti(a), colour = col_fine,
                hjust = if_else(da > a, 1.4, -0.4)),
            size = 3.0, fontface = "bold", family = "Source Sans Pro") +
  annotate("text", x = 0.15, y = 10.6, label = "Bolsonaro avanti →", hjust = 0, size = 3.0,
           colour = COL_BLU, family = "Source Sans Pro SemiBold") +
  annotate("text", x = -0.15, y = 10.6, label = "← Lula avanti", hjust = 1, size = 3.0,
           colour = COL_ROSSO, family = "Source Sans Pro SemiBold") +
  scale_colour_manual(values = colori) +
  scale_x_continuous(limits = c(-4, 10), breaks = seq(-2, 10, 2),
                     labels = function(x) paste0(ifelse(x > 0, "+", ""), x)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.2))) +
  coord_cartesian(clip = "off") +
  labs(title = "Nello stesso istituto il distacco si è quasi sempre ridotto",
       subtitle = "Vantaggio di Bolsonaro su Lula nel primo e nell'ultimo sondaggio di ogni istituto dopo il primo turno,\nin punti di voti validi, Brasile, ottobre 2026",
       caption = CAP) +
  theme_linechart(axis.line.y = element_blank(), axis.text.y = element_text(hjust = 1))

ggsave(file.path(output_dir, "03_stesso_istituto.png"),
       plot = p3, width = 9, height = 6.5, dpi = 220, bg = "white")
