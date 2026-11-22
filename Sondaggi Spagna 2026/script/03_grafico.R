# script/03_grafico.R — eseguito da `cd script && Rscript 03_grafico.R`
# Legge le medie di 02_media.R e gli errori di 04_errori_storici.py e disegna
# due grafici: le intenzioni di voto per lista e i seggi dei due blocchi.
# In ognuno: i singoli sondaggi, la media, la fascia di incertezza e il valore
# finale.
#
# La fascia somma all'incertezza della media l'errore che i sondaggi hanno
# fatto davvero alle elezioni generali dal 2015 al 2023 (scarto quadratico
# medio fra risultato e media degli ultimi sondaggi):
#   - voti: l'errore cresce con la grandezza della lista, quindi si stima un
#     solo coefficiente k tale che errore = k * sqrt(p * (100 - p)); con i dati
#     2015-2023 vale circa 2,3 punti per una lista al 30 per cento e 1,2 per
#     una al 7;
#   - seggi: circa 11 seggi per ognuno dei due blocchi. L'errore del blocco
#     del PSOE e' quello misurato su PSOE piu' sinistra nazionale: i seggi dei
#     partiti nazionalisti, che qui sono nel blocco, non erano nel conto.

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

# Colori vicini a quelli dei partiti: blu il PP, rosso il PSOE, verde Vox,
# magenta il Frente Amplio (erede di Sumar), viola Podemos, giallo scuro i
# nazionalisti e regionalisti presi insieme
colori <- c(pp = "#0478EA", psoe = "#F12938", vox = "#1B9E77", fa = "#D6278F",
            podemos = "#7B3FB5", salf = "#5A5A5A", naz = "#D99A00")
GRANDI <- c("pp", "psoe", "vox", "fa", "naz")

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

VOTO     <- as.Date("2026-11-29")
CONVOCA  <- as.Date("2026-10-05")   # Sanchez annuncia lo scioglimento delle Cortes
INIZIO   <- as.Date("2026-01-01")
Z90 <- qnorm(0.95); Z50 <- qnorm(0.75)

mesi <- seq(as.Date("2026-01-01"), as.Date("2026-11-01"), by = "month")
mesi_it <- c("Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago", "Set", "Ott", "Nov")
x_max <- VOTO + 100

# righe verticali della convocazione e del voto, con le loro scritte in alto
date_chiave <- function(y_max, passo) list(
  annotate("segment", x = CONVOCA, xend = CONVOCA, y = -Inf, yend = y_max - 1.6 * passo,
           colour = COL_GRIGIO, linewidth = 0.35, linetype = "dashed"),
  annotate("text", x = CONVOCA - 3, y = y_max - 1.6 * passo,
           label = "5 ottobre: Sánchez\nconvoca le elezioni", hjust = 1, vjust = 1,
           size = 3.0, lineheight = 0.95, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro"),
  annotate("segment", x = VOTO, xend = VOTO, y = -Inf, yend = y_max - 1.6 * passo,
           colour = COL_GRIGIO_SCURO, linewidth = 0.4, linetype = "dashed"),
  annotate("text", x = VOTO + 3, y = y_max - 1.6 * passo, label = "Si vota il 29 novembre",
           hjust = 0, vjust = 1, size = 3.0, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro"))

# ── 1. Intenzioni di voto ────────────────────────────────────────────────────
err <- read_csv(file.path(output_dir, "errori_storici.csv"), show_col_types = FALSE)
k_storico <- sqrt(mean(err$errore^2 / (err$media_sondaggi * (100 - err$media_sondaggi))))

media <- read_csv(file.path(output_dir, "medie_giornaliere.csv"), show_col_types = FALSE) %>%
  filter(data >= INIZIO) %>%
  mutate(sd_tot = sqrt(sd^2 + k_storico^2 * media * (100 - media)),
         lo90 = pmax(media - Z90 * sd_tot, 0), hi90 = media + Z90 * sd_tot,
         lo50 = pmax(media - Z50 * sd_tot, 0), hi50 = media + Z50 * sd_tot)
sond  <- read_csv(file.path(output_dir, "sondaggi_validi.csv"), show_col_types = FALSE) %>%
  filter(data >= INIZIO)
ultimo <- max(media$data)

finale <- media %>%
  filter(data == ultimo) %>%
  mutate(label = ifelse(serie == "naz",
                        paste0("Nazionalisti e\nregionalisti ", fmt_pct(media)),
                        paste(nome, fmt_pct(media))),
         y_lab = distanzia(media + ifelse(serie == "naz", 2.6, 0), gap = 2.0, minimo = 0.9))
NAZIONALISTI <- "ERC, Junts, EH Bildu, PNV,\nBNG e Coalición Canaria"
y_lab_naz <- finale$y_lab[finale$serie == "naz"]
y_max <- 40

p1 <- ggplot() +
  annotate("segment", x = INIZIO, xend = VOTO, y = c(10, 20, 30), yend = c(10, 20, 30),
           colour = COL_GRIGLIA, linewidth = 0.3) +
  date_chiave(y_max, passo = 1) +
  geom_ribbon(data = media, aes(x = data, ymin = lo90, ymax = hi90, fill = serie),
              alpha = 0.13) +
  geom_ribbon(data = media, aes(x = data, ymin = lo50, ymax = hi50, fill = serie),
              alpha = 0.16) +
  geom_point(data = sond, aes(x = data, y = pct, colour = serie),
             size = 1.5, alpha = 0.30, stroke = 0) +
  geom_line(data = filter(media, serie %in% GRANDI),
            aes(x = data, y = media, colour = serie), linewidth = 0.95) +
  geom_line(data = filter(media, !serie %in% GRANDI),
            aes(x = data, y = media, colour = serie), linewidth = 0.6) +
  geom_point(data = finale, aes(x = data, y = media, fill = serie),
             shape = 21, colour = "white", stroke = 0.7,
             size = ifelse(finale$serie %in% GRANDI, 2.6, 2.0)) +
  geom_segment(data = filter(finale, abs(y_lab - media) > 0.1),
               aes(x = ultimo + 4, xend = VOTO + 5, y = media, yend = y_lab,
                   colour = serie), linewidth = 0.25) +
  geom_text(data = finale, aes(x = VOTO + 7, y = y_lab, label = label, colour = serie),
            hjust = 0, size = ifelse(finale$serie %in% GRANDI, 3.6, 3.2),
            fontface = "bold", lineheight = 0.9, family = "Source Sans Pro") +
  annotate("text", x = VOTO + 7, y = y_lab_naz + 1.9, label = NAZIONALISTI,
           hjust = 0, vjust = 0, size = 2.9, lineheight = 0.95,
           colour = COL_GRIGIO_SCURO, family = "Source Sans Pro") +
  scale_colour_manual(values = colori) +
  scale_fill_manual(values = colori) +
  scale_x_date(limits = c(INIZIO, x_max), breaks = mesi,
               labels = c("Gen\n2026", mesi_it[-1]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, y_max), breaks = c(10, 20, 30),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "In Spagna il PP è avanti di tre punti sul PSOE",
       subtitle = paste0("Media dei sondaggi per le elezioni generali del 29 novembre e singole rilevazioni, Spagna, gennaio-novembre 2026.\n",
                         "La fascia è il margine di incertezza al 90 per cento, che tiene conto degli errori dei sondaggi alle ultime cinque elezioni"),
       caption = CAP) +
  theme_sondaggi()

ggsave(file.path(output_dir, "01_sondaggi_spagna_voti.png"),
       plot = p1, width = 10, height = 6.5, dpi = 220, bg = "white")

# ── 2. Seggi dei due blocchi ─────────────────────────────────────────────────
MAGGIORANZA <- 176
colori_b <- c(destra = "#0478EA", sinistra = "#F12938")
err_s <- read_csv(file.path(output_dir, "errori_storici_seggi.csv"), show_col_types = FALSE) %>%
  group_by(serie = blocco) %>%
  summarise(sd_storico = sqrt(mean(errore^2)), .groups = "drop")

seggi <- read_csv(file.path(output_dir, "medie_seggi.csv"), show_col_types = FALSE) %>%
  filter(data >= INIZIO) %>%
  left_join(err_s, by = "serie") %>%
  mutate(sd_tot = sqrt(sd^2 + sd_storico^2),
         lo90 = media - Z90 * sd_tot, hi90 = media + Z90 * sd_tot,
         lo50 = media - Z50 * sd_tot, hi50 = media + Z50 * sd_tot)
sond_s <- read_csv(file.path(output_dir, "sondaggi_seggi.csv"), show_col_types = FALSE) %>%
  filter(data >= INIZIO)

finale_s <- seggi %>%
  filter(data == ultimo) %>%
  mutate(label = paste(nome, round(media)))
ALLEATI <- "Frente Amplio, Podemos, ERC,\nJunts, EH Bildu, PNV, BNG\ne Coalición Canaria"
y_min <- 115; y_max_s <- 235

p2 <- ggplot() +
  annotate("segment", x = INIZIO, xend = VOTO, y = c(125, 150, 200), yend = c(125, 150, 200),
           colour = COL_GRIGLIA, linewidth = 0.3) +
  annotate("segment", x = INIZIO, xend = VOTO, y = MAGGIORANZA, yend = MAGGIORANZA,
           colour = COL_GRIGIO_SCURO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = INIZIO + 3, y = MAGGIORANZA, label = "Maggioranza assoluta, 176 seggi",
           hjust = 0, vjust = -0.6, size = 3.0, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro") +
  date_chiave(y_max_s, passo = 3) +
  geom_ribbon(data = seggi, aes(x = data, ymin = lo90, ymax = hi90, fill = serie),
              alpha = 0.13) +
  geom_ribbon(data = seggi, aes(x = data, ymin = lo50, ymax = hi50, fill = serie),
              alpha = 0.16) +
  geom_point(data = sond_s, aes(x = data, y = seggi, colour = serie),
             size = 1.5, alpha = 0.30, stroke = 0) +
  geom_line(data = seggi, aes(x = data, y = media, colour = serie), linewidth = 0.95) +
  geom_point(data = finale_s, aes(x = data, y = media, fill = serie),
             shape = 21, colour = "white", stroke = 0.7, size = 2.6) +
  geom_text(data = finale_s, aes(x = VOTO + 7, y = media, label = label, colour = serie),
            hjust = 0, size = 3.6, fontface = "bold", lineheight = 0.9,
            family = "Source Sans Pro") +
  annotate("text", x = VOTO + 7, y = finale_s$media[finale_s$serie == "sinistra"] - 3.5,
           label = ALLEATI, hjust = 0, vjust = 1, size = 2.9, lineheight = 0.95,
           colour = COL_GRIGIO_SCURO, family = "Source Sans Pro") +
  scale_colour_manual(values = colori_b) +
  scale_fill_manual(values = colori_b) +
  scale_x_date(limits = c(INIZIO, x_max), breaks = mesi,
               labels = c("Gen\n2026", mesi_it[-1]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(y_min, y_max_s), breaks = c(125, 150, 200), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Sánchez recupera, ma resta lontano dalla maggioranza",
       subtitle = paste0("Seggi al Congresso stimati dai sondaggi per i due blocchi, media e singole rilevazioni, Spagna, gennaio-novembre 2026.\n",
                         "La fascia è il margine di incertezza al 90 per cento, che tiene conto degli errori dei sondaggi alle ultime cinque elezioni"),
       caption = CAP) +
  theme_sondaggi()

ggsave(file.path(output_dir, "02_sondaggi_spagna_seggi.png"),
       plot = p2, width = 10, height = 6.5, dpi = 220, bg = "white")

# ── Controlli ────────────────────────────────────────────────────────────────
cat("Ultimo giorno:", format(ultimo), "\n")
cat(sprintf("Errore storico: k = %.4f (%.1f punti al 30%%, %.1f al 7%%)\n", k_storico,
            k_storico * sqrt(30 * 70), k_storico * sqrt(7 * 93)))
print(select(finale, nome, media, sd, sd_tot, lo90, hi90))
print(err_s)
print(select(finale_s, nome, media, sd, sd_tot, lo90, hi90))
cat(sprintf("P(PP + Vox >= %d seggi) = %.0f%%\n", MAGGIORANZA,
            100 * pnorm((finale_s$media[finale_s$serie == "destra"] - MAGGIORANZA) /
                          finale_s$sd_tot[finale_s$serie == "destra"])))
