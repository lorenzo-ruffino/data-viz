# script/antisemitismo_francia.R — eseguito da `cd script && Rscript antisemitismo_francia.R`
# Dot plot: adesione ai pregiudizi antisemiti per vicinanza politica.
# Dati: Ifop-AJC (con Fondapol), "Radiographie de l'antisémitisme en 2026",
# slide 63-65, risposta "Total d'accord". Trascritti a mano in input/.

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

COL_NERO   <- "#1C1C1C"
COL_GRIGIO <- "#9A9A9A"

# Tela 9 in: corpi riscalati ×1,125 rispetto ai nominali di grafici-r (8 in)
K <- 9 / 8

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9 * K, color = COL_NERO, hjust = 0.5),
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
      legend.text = element_text(size = 9 * K, color = COL_NERO, hjust = 0),
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14 * K, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9 * K, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9 * K, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP_IFOP <- "Elaborazione di Lorenzo Ruffino su dati Ifop-AJC, sondaggio del 17-24 settembre 2026"

# Colori di partito (quelli usati da Ifop; giallo Renaissance scurito per
# leggibilità su fondo bianco)
partiti <- c("LFI", "Ecologisti", "PS", "Renaissance", "LR", "RN")
nomi_partiti <- c(
  "LFI"         = "LFI (Mélenchon)",
  "Ecologisti"  = "Ecologisti",
  "PS"          = "PS (centrosinistra)",
  "Renaissance" = "Renaissance (Macron)",
  "LR"          = "LR (centrodestra)",
  "RN"          = "RN (Le Pen)"
)
col_partiti <- c(
  "LFI"         = "#D0103A",
  "Ecologisti"  = "#1B9E4B",
  "PS"          = "#F0609E",
  "Renaissance" = "#F2A900",
  "LR"          = "#2E86DE",
  "RN"          = "#0D2A6B"
)

# 1) Dati ----------------------------------------------------------------

# "Gli ebrei sono molto uniti tra loro" escluso: poco distintivo tra partiti
raw <- read_csv(file.path(input_dir, "ifop_ajc_2026_partiti.csv"),
                show_col_types = FALSE) %>%
  filter(id != "unis")

livelli <- rev(raw$affermazione)   # prima affermazione in alto

df <- raw %>%
  pivot_longer(c(Media, all_of(partiti)), names_to = "gruppo", values_to = "pct")

write_csv(df, file.path(output_dir, "antisemitismo_francia.csv"))

media <- df %>%
  filter(gruppo == "Media") %>%
  mutate(y = as.numeric(factor(affermazione, levels = livelli)))

# Punti di partito: quando due partiti sono a meno di SOGLIA punti l'uno
# dall'altro, il secondo si sposta su una corsia sopra o sotto, così nessun
# pallino ne copre un altro.
SOGLIA <- 2.6
OFFSET <- 0.2

assegna_corsie <- function(v) {
  ord <- order(v)
  corsia <- integer(length(v))
  piazzati <- list()
  for (i in ord) {
    for (c in c(0L, 1L, -1L, 2L, -2L)) {
      occupati <- vapply(piazzati, function(p) p$c == c && abs(p$v - v[i]) < SOGLIA,
                         logical(1))
      if (!any(occupati)) {
        corsia[i] <- c
        piazzati[[length(piazzati) + 1]] <- list(v = v[i], c = c)
        break
      }
    }
  }
  corsia
}

punti <- df %>%
  filter(gruppo != "Media") %>%
  mutate(gruppo = factor(gruppo, levels = partiti),
         y0 = as.numeric(factor(affermazione, levels = livelli))) %>%
  # LFI ordinata per prima: a parità di valore resta sulla corsia centrale
  arrange(affermazione, gruppo != "LFI") %>%
  group_by(affermazione) %>%
  mutate(corsia = assegna_corsie(pct)) %>%
  ungroup() %>%
  mutate(y = y0 + corsia * OFFSET)

# Etichetta del valore LFI sempre a destra del pallino (sulla corsia centrale
# a destra di LFI c'è spazio libero in tutte le righe, verificato sul PNG)
lab_lfi <- punti %>%
  filter(gruppo == "LFI") %>%
  mutate(x = pct + 1.6, hjust = 0)

# Etichette delle righe spezzate a mano per evitare parole orfane
etichette <- c(
  unis      = "Gli ebrei sono molto uniti\ntra loro",
  riches    = "Gli ebrei sono più ricchi\ndella media dei francesi",
  victimes  = "Gli ebrei sfruttano a proprio vantaggio\nlo status di vittime della Shoah",
  medias    = "Gli ebrei hanno troppo\npotere nei media",
  finance   = "Gli ebrei hanno troppo potere\nnell'economia e nella finanza",
  politique = "Gli ebrei hanno troppo\npotere in politica",
  crises    = "Gli ebrei sono responsabili\ndi molte crisi economiche"
)
etichette_y <- unname(etichette[rev(raw$id)])

righe <- tibble(y = seq_along(livelli))

# 2) Grafico -------------------------------------------------------------

p <- ggplot() +
  # guida orizzontale leggera per ogni affermazione
  geom_segment(data = righe, aes(x = 0, xend = 60, y = y, yend = y),
               colour = "#E6E6E6", linewidth = 0.4) +
  # media dei francesi: tacca verticale nera + valore sotto
  geom_segment(data = media, aes(x = pct, xend = pct, y = y - 0.3, yend = y + 0.3),
               colour = COL_NERO, linewidth = 0.9) +
  geom_text(data = media, aes(x = pct + 0.4, y = y + 0.33,
                              label = paste0("media ", pct, "%")),
            family = "Source Sans Pro", size = 2.9 * K, colour = COL_GRIGIO,
            hjust = 0, vjust = 0) +
  geom_point(data = arrange(punti, desc(gruppo)),
             aes(x = pct, y = y, fill = gruppo, size = gruppo == "LFI"),
             shape = 21, colour = "white", stroke = 0.5) +
  geom_text(data = lab_lfi, aes(x = x, y = y, label = paste0(pct, "%"), hjust = hjust),
            family = "Source Sans Pro", fontface = "bold", size = 3.2 * K,
            colour = col_partiti[["LFI"]]) +
  scale_fill_manual(values = col_partiti, breaks = partiti, labels = nomi_partiti) +
  scale_size_manual(values = c(`FALSE` = 3.6, `TRUE` = 4.8), guide = "none") +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 20),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(add = c(0.6, 2))) +
  scale_y_continuous(breaks = seq_along(livelli),
                     labels = etichette_y,
                     limits = c(0.6, length(livelli) + 0.62),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE,
                             override.aes = list(size = 3.6, colour = "white"))) +
  labs(
    title = "In Francia l'antisemitismo è più diffuso nell'estrema sinistra",
    subtitle = paste0(
      "Quota di chi è d'accordo con ogni affermazione, per partito a cui si sente più vicino, Francia, 2026"
    ),
    caption = CAP_IFOP
  ) +
  theme_linechart(
    legend.justification = "left",
    legend.location = "plot",
    legend.margin = margin(t = 0, b = 0.1, unit = "cm"),
    legend.key.spacing.x = unit(0.35, "cm"),
    legend.key.spacing.y = unit(0.05, "cm"),
    axis.line.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.3, colour = COL_NERO),
    axis.text.y = element_text(size = 10 * K, colour = COL_NERO, hjust = 1,
                               lineheight = 1.05)
  )

ggsave(file.path(output_dir, "antisemitismo_francia.png"),
       plot = p, width = 9, height = 7.2, dpi = 220, bg = "white")

cat("PNG salvato\n")
