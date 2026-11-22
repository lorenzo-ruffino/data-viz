# script/03_grafico.R — eseguito da `cd script && Rscript 03_grafico.R`
# Legge la media di 02_media.R e gli errori di 04_errori_storici.py e disegna
# il grafico dei candidati sopra il 3 per cento: i singoli sondaggi, la media,
# la fascia di incertezza e il valore finale di ogni candidato.
#
# La fascia somma all'incertezza della media l'errore che i sondaggi hanno
# fatto davvero al primo turno delle presidenziali dal 2007 al 2022 (scarto
# quadratico medio fra risultato e media degli ultimi sondaggi). L'errore
# cresce con il consenso del candidato, quindi si stima un solo coefficiente k
# tale che errore = k * sqrt(p * (100 - p)): con i dati 2007-2022 vale circa
# 2 punti per un candidato al 33 per cento e 1 per uno al 5.

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"
COL_GRIGLIA <- "#E8E8E8"

# Blu scuro Le Pen, rosso Mélenchon, azzurro Philippe, viola Glucksmann, verde
# Lisnard, grigio Retailleau, giallo Attal, rosa Riner, marrone Zemmour
colori <- c(lepen = "#1B2F6B", melenchon = "#F12938", philippe = "#1FA5E0",
            glucksmann = "#A82DE3", lisnard = "#1B9E77", retailleau = "#5A5A5A",
            attal = "#E0A100", riner = "#E6549E", zemmour = "#8B5A2B")
SOGLIA <- 3     # nel grafico i candidati che oggi sono sopra il 3 per cento

theme_sondaggi <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line.x = element_line(linewidth = 0.3, colour = COL_NERO),
      axis.line.y = element_blank(),
      axis.text = element_text(size = 9, color = COL_NERO, hjust = 0.5),
      axis.text.y = element_text(size = 9, color = COL_GRIGIO_SCURO, hjust = 1,
                                 margin = margin(r = 0.15, unit = "cm")),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(0.5, 0.4, 0.4, 0.5), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.45, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP <- "Elaborazione di Lorenzo Ruffino su sondaggi raccolti da Wikipedia"

output_dir <- file.path("..", "output")
fmt_pct <- function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")

# allontana le etichette che si sovrappongono, tenendole in ordine
distanzia <- function(y, gap, minimo = -Inf) {
  o <- order(y); v <- y[o]
  for (giro in 1:500) {
    v[1] <- max(v[1], minimo)
    for (i in seq_along(v)[-1]) {
      d <- v[i] - v[i - 1]
      if (d < gap) {
        s <- (gap - d) / 2
        v[i - 1] <- max(v[i - 1] - s, minimo); v[i] <- v[i] + s
      }
    }
  }
  y[o] <- v
  y
}

INIZIO <- as.Date("2026-07-07")    # la Corte d'appello riapre la strada a Le Pen
Z90 <- qnorm(0.95); Z50 <- qnorm(0.75)

err <- read_csv(file.path(output_dir, "errori_storici.csv"), show_col_types = FALSE) %>%
  filter(media_sondaggi >= 1)
k_storico <- sqrt(mean(err$errore^2 / (err$media_sondaggi * (100 - err$media_sondaggi))))

media <- read_csv(file.path(output_dir, "medie_giornaliere.csv"), show_col_types = FALSE) %>%
  mutate(sd_tot = sqrt(sd^2 + k_storico^2 * media * (100 - media)),
         lo90 = pmax(media - Z90 * sd_tot, 0), hi90 = media + Z90 * sd_tot,
         lo50 = pmax(media - Z50 * sd_tot, 0), hi50 = media + Z50 * sd_tot)
sond  <- read_csv(file.path(output_dir, "sondaggi_validi.csv"), show_col_types = FALSE)
ultimo <- max(media$data)

finale <- media %>% filter(data == ultimo, media >= SOGLIA)
stopifnot(setequal(finale$serie, names(colori)))
media <- filter(media, serie %in% names(colori))
sond  <- filter(sond, serie %in% names(colori))

mesi <- c(INIZIO, seq(as.Date("2026-08-01"), as.Date("2026-11-01"), by = "month"))
mesi_it <- c("7 luglio\n2026", "Agosto", "Settembre", "Ottobre", "Novembre")
x_max <- ultimo + 46
SOTTO_FASCIA <- "La fascia è il margine di incertezza al 90 per cento, che tiene conto degli errori dei sondaggi passati"

# Un grafico per un gruppo di candidati: i punti dei sondaggi, la fascia, la
# media e il valore finale. `gap` e' la distanza minima fra le etichette.
grafico <- function(serie, y_max, tacche, gap, titolo, sottotitolo, eventi = NULL) {
  m <- filter(media, serie %in% !!serie)
  sn <- filter(sond, serie %in% !!serie, pct <= y_max)
  fin <- finale %>% filter(serie %in% !!serie) %>%
    mutate(label = paste(nome, fmt_pct(media)),
           y_lab = distanzia(media, gap = gap, minimo = gap / 2))
  ggplot() +
    annotate("segment", x = INIZIO, xend = ultimo + 2, y = tacche, yend = tacche,
             colour = COL_GRIGLIA, linewidth = 0.3) +
    eventi +
    geom_ribbon(data = m, aes(x = data, ymin = lo90, ymax = pmin(hi90, y_max), fill = serie),
                alpha = 0.08) +
    geom_ribbon(data = m, aes(x = data, ymin = lo50, ymax = hi50, fill = serie),
                alpha = 0.13) +
    geom_point(data = sn, aes(x = data, y = pct, colour = serie),
               size = 1.7, alpha = 0.40, stroke = 0) +
    geom_line(data = m, aes(x = data, y = media, colour = serie), linewidth = 0.95) +
    geom_point(data = fin, aes(x = data, y = media, fill = serie),
               shape = 21, colour = "white", stroke = 0.7, size = 2.6) +
    geom_segment(data = filter(fin, abs(y_lab - media) > gap / 20),
                 aes(x = ultimo + 1.5, xend = ultimo + 3.5, y = media, yend = y_lab,
                     colour = serie), linewidth = 0.25) +
    geom_text(data = fin, aes(x = ultimo + 4.5, y = y_lab, label = label, colour = serie),
              hjust = 0, size = 3.6, fontface = "bold", family = "Source Sans Pro") +
    scale_colour_manual(values = colori) +
    scale_fill_manual(values = colori) +
    scale_x_date(limits = c(INIZIO, x_max), breaks = mesi, labels = mesi_it,
                 expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, y_max), breaks = tacche,
                       labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    labs(title = titolo, subtitle = paste0(sottotitolo, "\n", SOTTO_FASCIA), caption = CAP) +
    theme_sondaggi()
}

# ── Tutti i candidati sopra il 3 per cento ───────────────────────────────────
p1 <- grafico(names(colori), y_max = 41, tacche = c(10, 20, 30), gap = 1.5,
              titolo = "In Francia Le Pen è al 32 per cento, quasi il doppio di Mélenchon",
              sottotitolo = "Sondaggi sul primo turno delle presidenziali 2027, candidati sopra il 3 per cento, Francia, luglio-novembre 2026.")
ggsave(file.path(output_dir, "01_sondaggi_francia_primo_turno.png"),
       plot = p1, width = 9, height = 8, dpi = 220, bg = "white")

# ── Controlli ────────────────────────────────────────────────────────────────
cat("Ultimo giorno:", format(ultimo), "\n")
cat(sprintf("Errore storico: k = %.4f (%.1f punti al 33%%, %.1f al 10%%, %.1f al 5%%)\n", k_storico,
            k_storico * sqrt(33 * 67), k_storico * sqrt(10 * 90), k_storico * sqrt(5 * 95)))
print(select(finale, nome, media, sd, sd_tot, lo90, hi90))
