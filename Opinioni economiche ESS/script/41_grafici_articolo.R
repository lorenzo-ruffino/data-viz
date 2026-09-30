# Grafici per l'articolo "Quanto Stato vogliono gli italiani"
# 01 - classifica europea della domanda di redistribuzione
# 02 - responsabilita' attribuita allo Stato (anziani e disoccupati)
# 03 - indice sintetico pro-Stato
# 04 - uguaglianza e merito, scatter a 27 paesi
# 05 - il sospetto sui sussidi, tre affermazioni a confronto

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
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

CAP <- "Elaborazione di Lorenzo Ruffino su dati European Social Survey"

DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS"
IN  <- file.path(DIR, "output", "grafici")
OUT <- file.path(DIR, "output")

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

pct_lab <- function(v) paste0(round(v), "%")
num_lab <- function(v) formatC(v, format = "f", digits = 2, decimal.mark = ",")

# ============================================================================
# 01 - Classifica della domanda di redistribuzione
# ============================================================================
g1d <- read_csv(file.path(IN, "g1_redistribuzione_classifica.csv"), show_col_types = FALSE) %>%
  rename(valore = `D'accordo`) %>%
  arrange(valore) %>%
  mutate(Paese = factor(Paese, levels = Paese), is_it = Paese == "Italia")

media_eu <- read_csv(file.path(DIR, "output/estrazioni/redistribuzione_serie.csv"),
                     show_col_types = FALSE) %>%
  filter(essround == 11, aggregato == "Europa-panel15", tipo_valore == "pct_accordo") %>%
  pull(valore)

g1 <- ggplot(g1d, aes(valore, Paese, fill = is_it)) +
  geom_col(width = 0.74) +
  geom_text(aes(label = pct_lab(valore), colour = is_it,
                fontface = ifelse(g1d$is_it, "bold", "plain")),
            hjust = -0.18, family = "Source Sans Pro", size = 2.9) +
  scale_fill_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_BLU), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_NERO), guide = "none") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Gli italiani vogliono che lo Stato riduca le differenze",
       subtitle = "Quota d'accordo con l'affermazione \"il governo dovrebbe prendere misure per ridurre le differenze\nnei livelli di reddito\", 2023-24",
       caption = CAP) +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 8.5),
        axis.text.x = element_blank(),
        axis.line = element_blank())

ggsave(file.path(OUT, "01_redistribuzione_classifica.png"), g1,
       width = 8, height = 7.5, dpi = 220, bg = "white")

# ============================================================================
# 02 - Responsabilita' attribuita allo Stato
# ============================================================================
g2raw <- read_csv(file.path(IN, "g2_responsabilita_stato.csv"), show_col_types = FALSE)
ord <- g2raw %>% arrange(`Tenore di vita dei disoccupati`) %>% pull(Paese)

LEV2 <- c("Tenore di vita degli anziani",
          "Tenore di vita dei disoccupati")

g2seg <- g2raw %>%
  rename(anziani = `Tenore di vita degli anziani`,
         disocc  = `Tenore di vita dei disoccupati`) %>%
  mutate(Paese = factor(Paese, levels = ord))

g2d <- g2seg %>%
  pivot_longer(c(anziani, disocc), names_to = "serie", values_to = "valore") %>%
  mutate(serie = factor(ifelse(serie == "anziani", LEV2[1], LEV2[2]), levels = LEV2))

g2 <- ggplot(g2seg) +
  geom_segment(aes(x = disocc, xend = anziani, y = Paese, yend = Paese),
               colour = COL_GRIGIO, linewidth = 0.7) +
  geom_point(data = g2d, aes(valore, Paese, colour = serie), size = 4.2) +
  geom_text(data = g2seg, aes(anziani, Paese, label = pct_lab(anziani)),
            hjust = -0.45, colour = COL_BLU, fontface = "bold",
            family = "Source Sans Pro", size = 3.1) +
  geom_text(data = g2seg, aes(disocc, Paese, label = pct_lab(disocc)),
            hjust = 1.45, colour = COL_ROSSO, fontface = "bold",
            family = "Source Sans Pro", size = 3.1) +
  scale_colour_manual(values = setNames(c(COL_BLU, COL_ROSSO), LEV2)) +
  guides(colour = guide_legend(override.aes = list(size = 4.5))) +
  scale_x_continuous(limits = c(28, 100), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.7, 0.7))) +
  coord_cartesian(clip = "off") +
  labs(title = "Gli italiani chiedono allo Stato più di quasi tutti",
       subtitle = "Quota che considera responsabilità del governo garantire un tenore di vita adeguato ad anziani\ne disoccupati, risposte da 7 a 10 su una scala da 0 a 10",
       caption = CAP) +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 10),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(size = 9.5, color = COL_NERO, hjust = 0),
        legend.margin = margin(b = 0.1, unit = "cm"))

ggsave(file.path(OUT, "02_responsabilita_stato.png"), g2,
       width = 8, height = 6.5, dpi = 220, bg = "white")

# ============================================================================
# 03 - Indice sintetico pro-Stato
# ============================================================================
g3d <- read_csv(file.path(IN, "g3_indice_prostato.csv"), show_col_types = FALSE) %>%
  arrange(Indice) %>%
  mutate(Paese = factor(Paese, levels = Paese), is_it = Paese == "Italia")

g3 <- ggplot(g3d, aes(Indice, Paese, fill = is_it)) +
  geom_col(width = 0.74) +
  geom_vline(xintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_text(aes(label = num_lab(Indice), colour = is_it,
                hjust = ifelse(g3d$Indice >= 0, -0.22, 1.22),
                fontface = ifelse(g3d$is_it, "bold", "plain")),
            family = "Source Sans Pro", size = 2.9) +
  annotate("text", x = 0.008, y = 22.1, label = "media europea",
           hjust = 0, size = 2.9, colour = COL_GRIGIO_SCURO, family = "Source Sans Pro") +
  scale_fill_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_BLU), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_NERO), guide = "none") +
  scale_x_continuous(limits = c(-0.30, 0.40), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 2.0))) +
  coord_cartesian(clip = "off") +
  labs(title = "L'Italia è tra i paesi più statalisti d'Europa",
       subtitle = "Indice che combina nove domande su redistribuzione, responsabilità del governo e sussidi:\ndistanza dalla media europea in deviazioni standard",
       caption = CAP) +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 9),
        axis.text.x = element_blank(),
        axis.line = element_blank())

ggsave(file.path(OUT, "03_indice_prostato.png"), g3,
       width = 8, height = 7, dpi = 220, bg = "white")

# ============================================================================
# 04 - Uguaglianza e merito
# ============================================================================
g4d <- read_csv(file.path(IN, "g4_uguaglianza_merito.csv"), show_col_types = FALSE) %>%
  mutate(is_it = Paese == "Italia")

g4 <- ggplot(g4d, aes(Merito, Uguaglianza)) +
  geom_abline(slope = 1, intercept = 0, colour = COL_GRIGIO,
              linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 68.4, y = 83, label = "sopra questa linea i due principi\npesano uguale",
           hjust = 0, size = 2.9, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro", lineheight = 0.95) +
  geom_point(aes(colour = is_it, size = is_it)) +
  geom_text_repel(aes(label = Paese, colour = is_it,
                      fontface = ifelse(g4d$is_it, "bold", "plain")),
                  family = "Source Sans Pro", size = 3, seed = 1,
                  box.padding = 0.3, min.segment.length = 0,
                  segment.colour = COL_GRIGIO) +
  scale_colour_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_BLU), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 3.2, `FALSE` = 2.2), guide = "none") +
  scale_x_continuous(limits = c(68, 94), breaks = seq(70, 90, 5),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(limits = c(18, 88), breaks = seq(20, 80, 10),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "L'Italia vuole uguaglianza e merito allo stesso tempo",
       subtitle = "Quota d'accordo con due diverse idee di società giusta, nei 27 paesi europei rilevati",
       caption = CAP,
       x = "È giusto che chi lavora sodo guadagni di più",
       y = "È giusto che reddito e ricchezza siano distribuiti in modo uguale") +
  theme_linechart() +
  theme(axis.title.x = element_text(size = 9.5, color = COL_NERO, hjust = 0.5,
                                    margin = margin(t = 0.3, unit = "cm")),
        axis.title.y = element_text(size = 9.5, color = COL_NERO, hjust = 0.5,
                                    angle = 90, margin = margin(r = 0.3, unit = "cm")))

ggsave(file.path(OUT, "04_uguaglianza_merito.png"), g4,
       width = 8, height = 7.5, dpi = 220, bg = "white")

# ============================================================================
# 05 - Il sospetto sui sussidi
# ============================================================================
g5raw <- read_csv(file.path(IN, "g5_sospetto_sussidi.csv"), show_col_types = FALSE)
lev <- c("Molti ottengono sussidi\na cui non hanno diritto",
         "Chi ha redditi bassi\nriceve meno del dovuto",
         "I sussidi rendono\nle persone pigre")
ord5 <- g5raw %>% arrange(`Molti ottengono sussidi a cui non hanno diritto`) %>% pull(Paese)

g5d <- g5raw %>%
  pivot_longer(-Paese, names_to = "serie", values_to = "valore") %>%
  mutate(serie = factor(case_when(
           str_detect(serie, "non hanno diritto") ~ lev[1],
           str_detect(serie, "meno del dovuto")   ~ lev[2],
           TRUE                                    ~ lev[3]), levels = lev),
         Paese = factor(Paese, levels = ord5),
         is_it = Paese == "Italia")

g5 <- ggplot(g5d, aes(valore, Paese, fill = is_it)) +
  geom_col(width = 0.74) +
  geom_text(aes(label = pct_lab(valore), colour = is_it,
                fontface = ifelse(g5d$is_it, "bold", "plain")),
            hjust = -0.2, family = "Source Sans Pro", size = 2.9) +
  facet_wrap(~ serie, nrow = 1) +
  scale_fill_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_BLU), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_NERO), guide = "none") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Per gli italiani il problema del welfare sono i furbi",
       subtitle = "Quota d'accordo con tre affermazioni sui sussidi e sui servizi pubblici",
       caption = CAP) +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 9),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(family = "Source Sans Pro", face = "bold",
                                  size = 9.5, colour = COL_NERO, lineheight = 1.05,
                                  margin = margin(b = 0.25, unit = "cm")),
        panel.spacing.x = unit(0.9, "cm"))

ggsave(file.path(OUT, "05_sospetto_sussidi.png"), g5,
       width = 10, height = 6.5, dpi = 220, bg = "white")

cat("\n== Controlli ==\n")
cat("G1 Italia:", g1d$valore[g1d$is_it], "| primo:", as.character(tail(g1d$Paese, 1)),
    tail(g1d$valore, 1), "| media panel15:", media_eu, "\n")
cat("G2 Italia disoccupati:", g2raw$`Tenore di vita dei disoccupati`[g2raw$Paese == "Italia"], "\n")
cat("G3 Italia:", g3d$Indice[g3d$is_it], "| ultimo:", as.character(head(g3d$Paese, 1)), head(g3d$Indice, 1), "\n")
cat("G4 Italia merito/uguaglianza:", g4d$Merito[g4d$is_it], g4d$Uguaglianza[g4d$is_it], "| paesi:", nrow(g4d), "\n")
cat("G5 Italia:", g5d$valore[g5d$is_it], "\n")
