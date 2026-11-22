# script/03_grafico.R — eseguito da `cd script && Rscript 03_grafico.R`
# Legge le medie calcolate da 02_medie.R e disegna il grafico: due pannelli,
# prima e dopo il primo turno, sulla stessa scala verticale.

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

output_dir <- file.path("..", "output")

fmt_pct <- function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")

# ── Dati ─────────────────────────────────────────────────────────────────────
medie <- read_csv(file.path(output_dir, "medie_giornaliere.csv"), show_col_types = FALSE)
sond  <- read_csv(file.path(output_dir, "sondaggi_validi.csv"), show_col_types = FALSE)

PRIMO <- as.Date("2026-10-04")
VOTO  <- as.Date("2026-10-25")

colori <- c(lula = COL_ROSSO, flavio = COL_BLU)
Y_LIM <- c(42, 58)
scala_y <- scale_y_continuous(limits = Y_LIM, breaks = seq(44, 56, 2),
                              labels = function(x) paste0(x, "%"), expand = c(0, 0))

# Titolo di pannello dentro l'area del grafico, cosi' i due pannelli restano
# allineati
titolo_pannello <- function(x, testo)
  annotate("text", x = x, y = Y_LIM[2] - 0.3, label = testo, hjust = 0, vjust = 1,
           size = 3.4, colour = COL_GRIGIO_SCURO, family = "Source Sans Pro SemiBold")

base_pannello <- function(m, s) {
  ggplot() +
    geom_hline(yintercept = 50, colour = COL_GRIGIO, linewidth = 0.3, linetype = "dashed") +
    geom_ribbon(data = m, aes(x = data, ymin = media - 1.645 * sd,
                              ymax = media + 1.645 * sd, fill = cand), alpha = 0.12) +
    geom_point(data = s, aes(x = data, y = validi, colour = cand), size = 1.1, alpha = 0.35) +
    geom_line(data = m, aes(x = data, y = media, colour = cand), linewidth = 1.0) +
    scale_colour_manual(values = colori) +
    scale_fill_manual(values = colori) +
    scala_y +
    coord_cartesian(clip = "off") +
    theme_linechart()
}

# ── Pannello 1: prima del primo turno ────────────────────────────────────────
m1 <- filter(medie, fase == "prima")
s1 <- filter(sond, fase == "prima")
fine1 <- max(m1$data)
et1 <- filter(m1, data == fine1) %>% mutate(label = fmt_pct(round(media, 1)))

mesi <- seq(as.Date("2026-05-01"), as.Date("2026-10-01"), by = "month")
p1 <- base_pannello(m1, s1) +
  titolo_pannello(min(m1$data) + 3, "Prima del primo turno") +
  geom_text(data = et1, aes(x = data, y = media, label = label, colour = cand,
                            vjust = ifelse(cand == "flavio", -0.6, 1.6)),
            hjust = 0.7, size = 3.0, fontface = "bold", family = "Source Sans Pro") +
  scale_x_date(limits = c(min(m1$data), PRIMO + 2), breaks = mesi,
               labels = c("Mag", "Giu", "Lug", "Ago", "Set", "Ott"), expand = c(0, 0))

# ── Pannello 2: dopo il primo turno ──────────────────────────────────────────
m2 <- filter(medie, fase == "dopo")
s2 <- filter(sond, fase == "dopo")
fine2 <- max(m2$data)
et2 <- filter(m2, data == fine2) %>%
  mutate(nome = recode(candidato, "Flavio Bolsonaro" = "Bolsonaro"),
         label = paste0(nome, "\n", fmt_pct(round(media, 1))))

p2 <- base_pannello(m2, s2) +
  titolo_pannello(PRIMO + 0.3, "Dopo il primo turno") +
  geom_text(data = et2, aes(x = data, y = media, label = label, colour = cand,
                            vjust = ifelse(cand == "flavio", 0.1, 0.9)),
            hjust = 0, nudge_x = 0.4, size = 3.6, fontface = "bold",
            lineheight = 0.9, family = "Source Sans Pro") +
  scale_x_date(limits = c(PRIMO - 0.5, VOTO + 9),
               breaks = as.Date(c("2026-10-05", "2026-10-12", "2026-10-19", "2026-10-25")),
               labels = c("5 ott", "12 ott", "19 ott", "25 ott"), expand = c(0, 0)) +
  theme(axis.text.y = element_blank())

# ── Composizione ─────────────────────────────────────────────────────────────
p <- p1 + p2 +
  plot_layout(widths = c(1.3, 1)) +
  plot_annotation(
    title = "Bolsonaro rimane avanti nei sondaggi per il ballottaggio",
    subtitle = "Media dei sondaggi sul ballottaggio del 25 ottobre, in percentuale dei voti validi, Brasile, aprile-ottobre 2026",
    caption = CAP,
    theme = theme_linechart())

ggsave(file.path(output_dir, "sondaggi_brasile_ballottaggio_2026.png"),
       plot = p, width = 9, height = 6.5, dpi = 220, bg = "white")

cat("Ultimo giorno:", format(fine2), "\n")
print(select(et2, candidato, media, sd))
