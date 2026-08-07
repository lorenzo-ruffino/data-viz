# Pensioni vigenti nel 2026 per regime di liquidazione — importo medio mensile
# Fonte: INPS, Osservatorio pensioni vigenti (export "Pensioni per regime di
# liquidazione", anno 2026). Il grafico mostra solo vecchiaia e superstiti:
# le pensioni di invalidità sono lette dal file ma escluse dalla figura.

library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------
font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO         <- "#1C1C1C"
COL_ROSSO        <- "#F12938"
COL_BLU          <- "#0478EA"
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

CAP_INPS <- "Elaborazione di Lorenzo Ruffino su dati Inps"

input_dir  <- "../input"
output_dir <- "../output"

# --- 1) Lettura e pulizia ---------------------------------------------------
# L'export INPS ha un'intestazione HTML sporca con campi multi-riga: si tengono
# solo le righe che iniziano con il nome di un regime di liquidazione.
raw_lines <- readLines(file.path(input_dir, "inps_pensioni_regime_2026_raw.csv"),
                       warn = FALSE)
data_lines <- raw_lines[grepl(paste0('^"(Retributivo|Misto riforma Dini|',
                                     'Misto riforma Fornero|Contributivo puro|',
                                     'Totale)";'),
                              raw_lines)]

raw <- read_delim(I(paste(data_lines, collapse = "\n")),
                  delim = ";", col_names = FALSE,
                  col_types = cols(.default = col_character()))

parse_num <- function(x) as.numeric(gsub(",", ".", gsub("\\.", "", trimws(x))))

df <- raw %>%
  transmute(
    regime         = trimws(X1),
    vecchiaia_n    = parse_num(X3),
    vecchiaia_imp  = parse_num(X4),
    invalidita_n   = parse_num(X5),
    invalidita_imp = parse_num(X6),
    superstiti_n   = parse_num(X7),
    superstiti_imp = parse_num(X8),
    totale_n       = parse_num(X9),
    totale_imp     = parse_num(X10)
  )

stopifnot(nrow(df) == 5,
          max(abs(df$vecchiaia_n + df$invalidita_n + df$superstiti_n -
                    df$totale_n)) == 0)

write_csv(df, file.path(output_dir, "pensioni_regime_liquidazione.csv"))

# --- 2) Dati per il grafico -------------------------------------------------
# Fuori il totale, fuori l'invalidità: restano vecchiaia e superstiti.
lv_regime <- c("Retributivo", "Misto riforma Dini",
               "Misto riforma Fornero", "Contributivo puro")
lv_cat <- c("Vecchiaia", "Superstiti")

plot_data <- df %>%
  filter(regime != "Totale") %>%
  select(regime, Vecchiaia = vecchiaia_imp, Superstiti = superstiti_imp) %>%
  pivot_longer(-regime, names_to = "categoria", values_to = "importo") %>%
  mutate(regime = factor(regime, levels = lv_regime),
         categoria = factor(categoria, levels = lv_cat))

colori <- c("Vecchiaia" = COL_ROSSO, "Superstiti" = COL_BLU)

fmt_euro <- function(x) paste0("€ ", format(round(x), big.mark = ".",
                                                 decimal.mark = ",", trim = TRUE))

# Nota richiesta dentro al grafico, nello spazio vuoto in alto a destra.
nota <- paste("Gli importi sono per singola pensione",
              "e un pensionato può percepirne più di una.",
              sep = "\n")

p <- ggplot(plot_data, aes(x = regime, y = importo, fill = categoria)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = fmt_euro(importo), color = categoria),
            position = position_dodge(width = 0.8), vjust = -0.7,
            size = 3.3, fontface = "bold", family = "Source Sans Pro",
            show.legend = FALSE) +
  annotate("text", x = 4.55, y = 2580, label = nota,
           hjust = 1, vjust = 1, size = 3.1, family = "Source Sans Pro",
           lineheight = 1.2, color = COL_GRIGIO_SCURO) +
  scale_fill_manual(values = colori) +
  scale_color_manual(values = colori) +
  scale_x_discrete(labels = c("Retributivo", "Misto\nriforma Dini",
                              "Misto\nriforma Fornero", "Contributivo\npuro")) +
  scale_y_continuous(breaks = seq(0, 2500, 500),
                     labels = function(x) paste0("€ ", format(x, big.mark = ".",
                                                                   decimal.mark = ",",
                                                                   trim = TRUE)),
                     limits = c(0, 2600), expand = expansion(mult = c(0, 0))) +
  guides(fill = guide_legend(keywidth = unit(0.45, "cm"),
                             keyheight = unit(0.45, "cm")),
         color = "none") +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "top", legend.justification = "left",
        legend.margin = margin(l = -0.2, b = 0.1, unit = "cm")) +
  labs(title = "Come cambia la pensione in base al regime di calcolo",
       subtitle = paste0("Importo medio mensile lordo delle pensioni di vecchiaia e ai superstiti per regime di liquidazione;\n",
                         "sono escluse le pensioni di invalidità, pensioni vigenti in Italia nel 2026"),
       caption = CAP_INPS)

ggsave(file.path(output_dir, "pensioni_regime_liquidazione.png"),
       plot = p, width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 3) Sanity check --------------------------------------------------------
tot <- df %>% filter(regime == "Totale")
cat("Pensioni vigenti 2026 (totale):", format(tot$totale_n, big.mark = ".", decimal.mark = ","), "\n")
cat("Escluse (invalidità):", format(tot$invalidita_n, big.mark = ".", decimal.mark = ","),
    "=", round(tot$invalidita_n / tot$totale_n * 100, 1), "%\n")
cat("Vecchiaia + superstiti:",
    format(tot$vecchiaia_n + tot$superstiti_n, big.mark = ".", decimal.mark = ","), "\n\n")

df %>%
  filter(regime != "Totale") %>%
  transmute(regime,
            vecchiaia = sprintf("%s (%s)", fmt_euro(vecchiaia_imp),
                                format(vecchiaia_n, big.mark = ".", decimal.mark = ",")),
            superstiti = sprintf("%s (%s)", fmt_euro(superstiti_imp),
                                 format(superstiti_n, big.mark = ".", decimal.mark = ","))) %>%
  print(n = Inf)

cat("\nRapporto vecchiaia misto Fornero / contributivo puro:",
    round(df$vecchiaia_imp[df$regime == "Misto riforma Fornero"] /
            df$vecchiaia_imp[df$regime == "Contributivo puro"], 1), "volte\n")
