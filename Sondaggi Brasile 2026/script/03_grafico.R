# script/03_grafico.R — eseguito da `cd script && Rscript 03_grafico.R`
# Legge le medie calcolate da 02_medie.R e disegna il grafico.

library(tidyverse)
library(showtext)
library(ggrepel)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"
COL_GIALLO <- "#F2A900"

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

TESTA <- c("lula", "flavio")
# Colore per schieramento: rosso la sinistra (Lula), blu la destra (Flavio
# Bolsonaro e, in tonalita' piu' chiare o piu' scure, Caiado, Zema e Santos),
# viola il centro (Cury, Avante).
COL_BLU_SCURO  <- "#1F4E8C"
COL_BLU_CHIARO <- "#6FB1F5"
COL_AZZURRO    <- "#A9CFF7"
COL_VIOLA      <- "#A82DE3"
colori <- c(lula = COL_ROSSO, flavio = COL_BLU, cury = COL_VIOLA,
            santos = COL_BLU_SCURO, caiado = COL_BLU_CHIARO, zema = COL_AZZURRO)

ultimo_giorno <- max(medie$data)
etichette <- medie %>%
  filter(data == ultimo_giorno) %>%
  mutate(nome = recode(candidato, "Flávio Bolsonaro" = "Flávio\nBolsonaro",
                       "Renan Santos" = "Renan\nSantos"),
         label = paste0(nome, " ", fmt_pct(round(media, 1))))

inizio <- min(medie$data)
fine_x <- ultimo_giorno + 44

mesi <- seq(as.Date("2026-05-01"), as.Date("2026-10-01"), by = "month")
nomi_mesi <- c("Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre")

# ── Grafico ──────────────────────────────────────────────────────────────────
p <- ggplot() +
  geom_hline(yintercept = 50, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = inizio + 2, y = 50, label = "Soglia per vincere al primo turno",
           hjust = 0, vjust = -0.6, size = 3.0, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro") +
  geom_ribbon(data = filter(medie, cand %in% TESTA),
              aes(x = data, ymin = media - 1.645 * sd, ymax = media + 1.645 * sd,
                  fill = cand), alpha = 0.12) +
  geom_point(data = filter(sond, cand %in% TESTA),
             aes(x = data, y = validi, colour = cand), size = 1.1, alpha = 0.35) +
  geom_line(data = filter(medie, !cand %in% TESTA),
            aes(x = data, y = media, group = cand, colour = cand,
                linewidth = cand == "cury")) +
  scale_linewidth_manual(values = c(`TRUE` = 0.8, `FALSE` = 0.5)) +
  geom_line(data = filter(medie, cand %in% TESTA),
            aes(x = data, y = media, colour = cand), linewidth = 1.0) +
  geom_text(data = filter(etichette, cand %in% TESTA),
            aes(x = data, y = media, label = label, colour = cand,
                vjust = ifelse(cand == "flavio", 0.75, 0.5)),
            hjust = 0, nudge_x = 2, size = 3.6, fontface = "bold",
            lineheight = 0.9, family = "Source Sans Pro") +
  geom_text_repel(data = filter(etichette, !cand %in% TESTA),
                  aes(x = data, y = media, label = label, colour = cand),
                  hjust = 0, nudge_x = 3, direction = "y", size = 3.1,
                  fontface = "bold", family = "Source Sans Pro",
                  segment.colour = COL_GRIGIO, segment.size = 0.3,
                  min.segment.length = 0, box.padding = 0.2, seed = 1, lineheight = 0.9,
                  xlim = c(ultimo_giorno + 3, NA)) +
  scale_colour_manual(values = colori) +
  scale_fill_manual(values = colori) +
  scale_x_date(limits = c(inizio, fine_x), breaks = mesi, labels = nomi_mesi,
               expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(10, 50, 10),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Flávio Bolsonaro si è avvicinato a Lula",
       subtitle = "Media dei sondaggi sul primo turno delle presidenziali, in percentuale dei voti validi, Brasile, aprile-ottobre 2026",
       caption = CAP) +
  theme_linechart()

ggsave(file.path(output_dir, "sondaggi_brasile_2026.png"),
       plot = p, width = 9, height = 6.5, dpi = 220, bg = "white")

cat("Ultimo giorno:", format(ultimo_giorno), "\n")
print(select(etichette, candidato, media, sd))
