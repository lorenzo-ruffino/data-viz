# Grafici per l'articolo "Quanto credere alle previsioni di crescita del governo"
# 01 - previsione d'autunno per l'anno dopo: governo, enti internazionali e crescita effettiva
# 02 - scarto fra governo ed enti internazionali, prima e dopo l'Ufficio parlamentare di bilancio
# 03 - deficit effettivo rispetto all'obiettivo, a uno, due e tre anni
# 04 - crescita aggiunta dalla manovra secondo il governo, secondo gli enti e nei dati
# Anni esclusi: 2008-2009, 2012-2013, 2020-2021

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
COL_GRIGIO_SCURO <- "#5A5A5A"

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

CAP <- "Elaborazione di Lorenzo Ruffino su dati Mef, Commissione europea, Fmi, Ocse e Istat"

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Previsioni crescita Italia"
OUT  <- file.path(BASE, "output")

ANNI <- setdiff(1999:2025, c(2008, 2009, 2012, 2013, 2020, 2021))
ENTI <- c("Commissione europea", "FMI", "OCSE")

# Periodi esclusi dall'analisi, mostrati come aree grigie nei grafici sugli anni
esclusi <- tibble(
  x0 = c(2007.5, 2011.5, 2019.5), x1 = c(2009.5, 2013.5, 2021.5),
  etichetta = c("Crisi\nfinanziaria", "Crisi del\ndebito", "Pandemia")
)
aree_escluse <- function(y_testo) {
  list(
    geom_rect(data = esclusi, aes(xmin = x0, xmax = x1, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = COL_GRIGIO, alpha = 0.15),
    geom_text(data = esclusi, aes(x = (x0 + x1) / 2, y = y_testo,
                                  label = paste0(etichetta, "\n(esclusa)")),
              inherit.aes = FALSE, family = "Source Sans Pro", size = 2.8,
              colour = COL_GRIGIO_SCURO, lineheight = 0.95, vjust = 1)
  )
}

fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
fmt_segno <- function(x, d = 1) paste0(ifelse(round(x, d) > 0, "+", ifelse(round(x, d) < 0, "−", "")), fmt(abs(x), d))

err <- read_csv(file.path(OUT, "errori_previsioni.csv"), show_col_types = FALSE) |>
  filter(anno %in% ANNI, vintage == "prima_stima")

# Previsione d'autunno per l'anno dopo: governo (programmatico) e media dei tre enti
pil <- err |> filter(variabile == "pil_reale", orizzonte == "t+1, autunno")
gov <- pil |> filter(serie == "Governo (programmatico)") |>
  select(anno, governo = previsione, effettivo)
enti <- pil |> filter(serie %in% ENTI) |>
  group_by(anno) |> summarise(enti = mean(previsione), .groups = "drop")
d1 <- gov |> inner_join(enti, by = "anno") |> mutate(scarto = governo - enti)
write_csv(d1, file.path(OUT, "01_previsioni_anno_dopo.csv"))

# --- 01 Previsioni per l'anno dopo e crescita effettiva ------------------------

d1_long <- d1 |>
  pivot_longer(c(governo, enti, effettivo), names_to = "serie", values_to = "valore") |>
  mutate(serie = factor(serie, levels = c("governo", "enti", "effettivo"),
                        labels = c("Previsione del governo",
                                   "Media di Commissione europea, Fmi e Ocse",
                                   "Crescita effettiva")))

p1 <- ggplot(d1, aes(x = anno)) +
  aree_escluse(5) +
  geom_hline(yintercept = 0, colour = COL_GRIGIO, linewidth = 0.3) +
  geom_segment(aes(xend = anno, y = pmin(governo, enti, effettivo),
                   yend = pmax(governo, enti, effettivo)),
               colour = "#D9D9D9", linewidth = 2.2, lineend = "round") +
  geom_point(data = d1_long, aes(y = valore, colour = serie), size = 2.4) +
  scale_colour_manual(values = c(COL_ROSSO, COL_BLU, COL_NERO)) +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2010, 2015, 2019, 2023, 2025),
                     limits = c(1998.4, 2025.6), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-1, 5, 1),
                     labels = function(x) paste0(fmt(x, 0), "%"),
                     limits = c(-1, 5), expand = c(0.01, 0.01)) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  theme_linechart(legend.justification = "left") +
  labs(title = "Il governo prevede più crescita degli enti internazionali e spesso sbaglia",
       subtitle = "Crescita del Pil prevista in autunno per l'anno successivo e crescita registrata nella prima stima Istat,\nItalia, 1999-2025",
       caption = CAP)

ggsave(file.path(OUT, "01_previsioni_anno_dopo.png"), p1,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 02 Scarto prima e dopo l'UPB ----------------------------------------------

d2 <- d1 |> mutate(periodo = if_else(anno <= 2014, "prima", "dopo"))
medie <- d2 |> group_by(periodo) |>
  summarise(m = mean(scarto), x0 = min(anno) - 0.45, x1 = max(anno) + 0.45, .groups = "drop")

p2 <- ggplot(d2, aes(x = anno, y = scarto)) +
  aree_escluse(0.7) +
  geom_hline(yintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_col(aes(fill = periodo), width = 0.7) +
  geom_segment(data = medie, aes(x = x0, xend = x1, y = m, yend = m),
               inherit.aes = FALSE, colour = COL_NERO, linewidth = 0.6, linetype = "dashed") +
  geom_text(data = medie,
            aes(x = (x0 + x1) / 2, y = 0.92,
                label = paste0("Media ", ifelse(periodo == "prima", "1999-2014", "2015-2025"),
                               ": ", fmt_segno(m, 2), " punti")),
            inherit.aes = FALSE, family = "Source Sans Pro", fontface = "bold",
            size = 3.4, colour = COL_NERO) +
  geom_vline(xintercept = 2014.5, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2014.7, y = 0.82,
           label = "Dal 2014 le previsioni sono validate\ndall'Ufficio parlamentare di bilancio",
           hjust = 0, vjust = 1, family = "Source Sans Pro", size = 3.1, colour = COL_GRIGIO_SCURO,
           lineheight = 0.95) +
  scale_fill_manual(values = c(prima = COL_GRIGIO_SCURO, dopo = COL_ROSSO), guide = "none") +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2010, 2015, 2019, 2023, 2025),
                     limits = c(1998.4, 2025.6), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-0.2, 0.8, 0.2),
                     labels = function(x) fmt_segno(x, 1),
                     limits = c(-0.25, 1), expand = c(0.01, 0.01)) +
  theme_linechart() +
  labs(title = "Il controllore indipendente dei conti non ha frenato l'ottimismo",
       subtitle = "Previsione d'autunno del governo per l'anno successivo meno la media di Commissione europea, Fmi e Ocse,\npunti percentuali di crescita, Italia, 1999-2025",
       caption = "Elaborazione di Lorenzo Ruffino su dati Mef, Commissione europea, Fmi e Ocse")

ggsave(file.path(OUT, "02_scarto_upb.png"), p2,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 03 Deficit: obiettivo e risultato -------------------------------------------

d3 <- err |>
  filter(variabile == "indebitamento_netto", serie == "Governo (programmatico)",
         orizzonte %in% c("t+1, autunno", "t+2, autunno", "t+3, autunno")) |>
  group_by(orizzonte) |>
  summarise(n = n(), mancati = sum(errore < 0), scarto = -mean(errore), .groups = "drop") |>
  mutate(etichetta = c("L'anno dopo", "Dopo due anni", "Dopo tre anni"),
         etichetta = factor(etichetta, levels = etichetta))
write_csv(d3, file.path(OUT, "03_deficit_obiettivi.csv"))

p3 <- ggplot(d3, aes(x = etichetta, y = scarto)) +
  geom_col(fill = COL_ROSSO, width = 0.55) +
  geom_text(aes(label = paste0(fmt(scarto), " punti di Pil")), vjust = -2.6,
            family = "Source Sans Pro", fontface = "bold", size = 3.8, colour = COL_NERO) +
  geom_text(aes(label = paste0("obiettivo mancato ", mancati, " volte su ", n)), vjust = -0.9,
            family = "Source Sans Pro", size = 3.3, colour = COL_GRIGIO_SCURO) +
  scale_y_continuous(breaks = seq(0.5, 2.5, 0.5), labels = function(x) fmt(x),
                     limits = c(0, 2.9), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart(axis.line.x = element_line(linewidth = 0.3),
                  axis.text.x = element_text(size = 10)) +
  labs(title = "Gli obiettivi di deficit a tre anni non sono mai stati rispettati",
       subtitle = "Deficit effettivo meno obiettivo fissato dal governo nel documento d'autunno, media in punti di Pil,\nItalia, 1999-2025 (senza gli anni di crisi 2008-2009, 2012-2013 e 2020-2021)",
       caption = "Elaborazione di Lorenzo Ruffino su dati Mef e Istat")

ggsave(file.path(OUT, "03_deficit_obiettivi.png"), p3,
       width = 8, height = 6.5, dpi = 220, bg = "white")

# --- 04 La crescita della manovra --------------------------------------------------

eff <- read_csv(file.path(OUT, "effetto_politiche_anno_dopo.csv"), show_col_types = FALSE)
d4 <- tibble(
  voce = c("Previsione del governo\ncon la manovra",
           "Previsione di Commissione\neuropea, Fmi e Ocse",
           "Crescita effettiva"),
  valore = c(mean(eff$effetto), mean(eff$enti_su_tend, na.rm = TRUE), mean(eff$realizzato_prima))
) |>
  mutate(voce = factor(voce, levels = rev(voce)),
         testo = paste0(fmt_segno(round(valore, 2), 2), " punti"),
         testo = if_else(abs(valore) < 0.01, "0,00 punti", testo))
write_csv(d4 |> select(voce, valore), file.path(OUT, "04_crescita_manovre.csv"))

p4 <- ggplot(d4, aes(x = valore, y = voce)) +
  geom_col(aes(fill = valore > 0), width = 0.55) +
  geom_vline(xintercept = 0, colour = COL_NERO, linewidth = 0.4) +
  geom_text(aes(label = testo, hjust = ifelse(valore >= 0, -0.08, 1.12)),
            family = "Source Sans Pro", fontface = "bold", size = 3.8, colour = COL_NERO,
            lineheight = 0.95) +
  annotate("text", x = 0.02, y = 1.86, vjust = 1, hjust = 0,
           label = "cioè la stessa crescita prevista\ndal governo senza manovra",
           family = "Source Sans Pro", size = 3.3, colour = COL_GRIGIO_SCURO, lineheight = 0.95) +
  scale_fill_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_BLU), guide = "none") +
  scale_x_continuous(limits = c(-0.75, 0.6)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(axis.line = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 1, lineheight = 0.95)) +
  labs(title = "La crescita promessa dalle manovre di bilancio non si vede nei dati",
       subtitle = "Crescita per l'anno successivo rispetto alla previsione del governo senza manovra, media in punti percentuali,\n21 documenti del governo fra il 2001 e il 2024 (senza gli anni di crisi)",
       caption = CAP)

ggsave(file.path(OUT, "04_crescita_manovre.png"), p4,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Controlli -----------------------------------------------------------------------

cat("01: anni", nrow(d1), "| gov sopra enti", sum(d1$scarto > 0),
    "| gov medio", fmt(mean(d1$governo), 2), "| effettivo medio", fmt(mean(d1$effettivo), 2), "\n")
cat("02: medie", paste(medie$periodo, fmt(medie$m, 2), collapse = " / "), "\n")
cat("03:", paste(d3$etichetta, d3$mancati, d3$n, fmt(d3$scarto, 2), collapse = " | "), "\n")
cat("04:", paste(fmt(d4$valore, 2), collapse = " / "), "\n")
