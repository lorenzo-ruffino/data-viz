# =============================================================================
# Spesa pubblica italiana per funzione (COFOG), 1995-2024
# Aree impilate in % del PIL: protezione sociale, sanita', istruzione,
# affari economici, altre funzioni, interessi.
# Fonti: Eurostat gov_10a_exp (COFOG, divisioni GF01-GF10) e gov_10a_main
#        (D41PAY per gli interessi, scorporati dai Servizi generali GF01,
#        dove il dettaglio COFOG GF0107 manca nei primi anni).
# Dati gia' scaricati in ../input/
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO <- "#1C1C1C"

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
      panel.border = element_blank(),
      plot.margin = unit(c(0.55, 0.55, 0.55, 0.55), "cm"),
      plot.title.position = "plot",
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

# --- Dati --------------------------------------------------------------------

cofog <- read_csv("../input/eurostat_gov_10a_exp_it.csv", show_col_types = FALSE) %>%
  filter(!is.na(values)) %>%
  select(cofog99, anno = time, valore = values)

interessi <- read_csv("../input/eurostat_gov_10a_main_it_d41.csv",
                      show_col_types = FALSE) %>%
  filter(!is.na(values)) %>%
  select(anno = time, interessi = values)

larghi <- cofog %>%
  pivot_wider(names_from = cofog99, values_from = valore) %>%
  inner_join(interessi, by = "anno") %>%
  filter(!is.na(TOTAL)) %>%
  transmute(
    anno,
    totale               = TOTAL,
    `Protezione sociale` = GF10,
    `Sanità`             = GF07,
    Istruzione           = GF09,
    `Affari economici`   = GF04,
    Interessi            = interessi,
    `Altre funzioni`     = (GF01 - interessi) + GF02 + GF03 + GF05 + GF06 + GF08
  )

# La somma delle bande deve ricostruire il totale COFOG (tolleranza da
# arrotondamento delle componenti pubblicate)
scarto <- larghi %>%
  mutate(somma = `Protezione sociale` + `Sanità` + Istruzione +
           `Affari economici` + Interessi + `Altre funzioni`) %>%
  summarise(m = max(abs(somma - totale))) %>% pull(m)
stopifnot(scarto < 0.25)

write_csv(larghi, "../output/spesa_funzioni_cofog.csv")

ANNO_MIN <- min(larghi$anno)
ANNO_MAX <- max(larghi$anno)

bande <- c("Protezione sociale", "Sanità", "Istruzione",
           "Affari economici", "Altre funzioni", "Interessi")
col_bande <- c(
  "Protezione sociale" = "#0478EA",  # blu
  "Sanità"             = "#1B9E77",  # verde
  Istruzione           = "#F2A900",  # giallo
  "Affari economici"   = "#A82DE3",  # viola
  "Altre funzioni"     = "#9A9A9A",  # grigio
  Interessi            = "#F12938"   # rosso
)

lunghi <- larghi %>%
  select(-totale) %>%
  pivot_longer(-anno, names_to = "banda", values_to = "valore") %>%
  mutate(banda = factor(banda, levels = bande))

# --- Etichette ---------------------------------------------------------------

fmt_pct <- function(v) {
  s <- formatC(v, format = "f", digits = 1, decimal.mark = ",")
  paste0(sub(",0$", "", s), "%")
}

# posizioni cumulate (dal basso, nell'ordine di `bande`)
cum_anno <- function(a) {
  lunghi %>%
    filter(anno == a) %>%
    arrange(banda) %>%
    mutate(y_mid = cumsum(valore) - valore / 2)
}

label_dx <- cum_anno(ANNO_MAX) %>%
  mutate(testo = paste0(banda, "\n", fmt_pct(valore)),
         colore = ifelse(banda == "Altre funzioni", "#6B6B6B",
                         unname(col_bande[as.character(banda)])))

label_sx <- cum_anno(ANNO_MIN) %>%
  mutate(testo = fmt_pct(valore))

# --- Grafico -----------------------------------------------------------------

p <- ggplot(lunghi, aes(x = anno, y = valore, fill = banda)) +
  geom_area(position = position_stack(reverse = TRUE),
            color = "white", linewidth = 0.25) +
  geom_text(data = label_sx,
            aes(x = ANNO_MIN + 0.35, y = y_mid, label = testo),
            inherit.aes = FALSE, hjust = 0, color = "white",
            family = "Source Sans Pro", fontface = "bold", size = 3.1) +
  geom_text(data = label_dx,
            aes(x = ANNO_MAX + 0.5, y = y_mid, label = testo, color = colore),
            inherit.aes = FALSE, hjust = 0, lineheight = 1.05,
            family = "Source Sans Pro", fontface = "bold", size = 3.2) +
  scale_fill_manual(values = col_bande) +
  scale_color_identity() +
  scale_x_continuous(breaks = seq(1995, 2020, 5),
                     limits = c(ANNO_MIN, ANNO_MAX + 7),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 58), breaks = seq(10, 50, 10),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Dove va la spesa pubblica italiana",
    subtitle = "Spesa delle amministrazioni pubbliche per funzione, in percentuale del Pil, Italia, 1995-2024",
    caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat"
  ) +
  theme_linechart()

ggsave("../output/spesa_funzioni_cofog.png", p,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Sanity check ------------------------------------------------------------

cat("\nAnni:", ANNO_MIN, "-", ANNO_MAX, "| scarto max vs totale:",
    round(scarto, 3), "\n")
for (b in bande) {
  v1 <- larghi[[b]][larghi$anno == ANNO_MIN]
  v2 <- larghi[[b]][larghi$anno == ANNO_MAX]
  cat(sprintf("  %-19s %4.1f%% -> %4.1f%%\n", b, v1, v2))
}
cat(sprintf("  Totale              %4.1f%% -> %4.1f%%\n",
            larghi$totale[larghi$anno == ANNO_MIN],
            larghi$totale[larghi$anno == ANNO_MAX]))
cat(sprintf("Picco Affari economici (superbonus): %d (%.1f%%)\n",
            larghi$anno[which.max(larghi$`Affari economici`)],
            max(larghi$`Affari economici`)))
