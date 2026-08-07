# Pensioni vigenti nel 2026 per anno di decorrenza e categoria
# Fonte: INPS, Osservatorio pensioni vigenti (export "Pensioni per anno di decorrenza", anno 2026)
# "Invalidità e assegni sociali" = invalidità previdenziali + invalidi civili + pensioni/assegni sociali

library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------
font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"

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
# L'export INPS ha un'intestazione HTML sporca con campi multi-riga: si
# estraggono a mano le sole righe dati (anni + riga "Decorrenza anteriore").
raw_lines <- readLines(file.path(input_dir, "inps_pensioni_decorrenza_2026_raw.csv"),
                       warn = FALSE)
data_lines <- raw_lines[grepl('^"(Decorrenza anteriore|[0-9]{4}")', raw_lines)]

raw <- read_delim(I(paste(data_lines, collapse = "\n")),
                  delim = ";", col_names = FALSE,
                  col_types = cols(.default = col_character()))

parse_num <- function(x) as.numeric(gsub("\\.", "", trimws(x)))

df <- raw %>%
  transmute(
    anno_label       = gsub('"', "", X1),
    anno             = if_else(grepl("anteriore", anno_label), 1980L,
                               as.integer(anno_label)),
    vecchiaia        = parse_num(X3),
    invalidita       = parse_num(X6),
    superstite       = parse_num(X9),
    assegni_sociali  = parse_num(X12),
    invalidi_civili  = parse_num(X15),
    totale           = parse_num(X18)
  ) %>%
  mutate(invalidita_tot = invalidita + invalidi_civili)   # previdenziali + invalidi civili

stopifnot(nrow(df) == 46,
          max(abs(df$vecchiaia + df$superstite + df$invalidita_tot +
                    df$assegni_sociali - df$totale)) == 0)

write_csv(df %>% select(anno, vecchiaia, superstite, invalidita_tot,
                        assegni_sociali, totale),
          file.path(output_dir, "pensioni_decorrenza.csv"))

# --- 2) Dati per il grafico -------------------------------------------------
lv <- c("Assegni sociali", "Invalidità", "Superstite", "Vecchiaia")  # Vecchiaia in basso

plot_data <- df %>%
  select(anno, Vecchiaia = vecchiaia, Superstite = superstite,
         `Invalidità` = invalidita_tot, `Assegni sociali` = assegni_sociali) %>%
  pivot_longer(-anno, names_to = "categoria", values_to = "n") %>%
  mutate(categoria = factor(categoria, levels = lv))

colori <- c("Vecchiaia"       = COL_BLU,
            "Superstite"      = COL_GIALLO,
            "Invalidità"      = COL_ROSSO,
            "Assegni sociali" = COL_VIOLA)

# Etichette inline a destra, ai punti medi dei segmenti della barra 2025
ult <- df %>% filter(anno == 2025)
label_data <- tibble(
  categoria = factor(c("Vecchiaia", "Superstite", "Invalidità", "Assegni sociali"),
                     levels = lv),
  testo = c("Vecchiaia", "Superstite", "Invalidità", "Assegni\nsociali"),
  y = c(ult$vecchiaia / 2,
        ult$vecchiaia + ult$superstite / 2,
        ult$vecchiaia + ult$superstite + ult$invalidita_tot / 2,
        ult$vecchiaia + ult$superstite + ult$invalidita_tot + ult$assegni_sociali / 2)
)

# Annotazione: pensioni in pagamento da più di vent'anni (decorrenza <= 2005)
n_20anni   <- sum(df$totale[df$anno <= 2005])
pct_20anni <- round(n_20anni / sum(df$totale) * 100)
testo_20anni <- sprintf("Da più di vent'anni sono\nin pagamento %s mln di\npensioni, il %d%% del totale",
                        formatC(n_20anni / 1e6, format = "f", digits = 1, decimal.mark = ","),
                        pct_20anni)

fmt_conteggi <- function(x) case_when(
  x == 0      ~ "0",
  x < 1e6     ~ paste0(formatC(x / 1000, format = "d", big.mark = "."), " mila"),
  TRUE        ~ paste0(formatC(x / 1e6, format = "fg", decimal.mark = ","), " mln")
)

p <- ggplot(plot_data, aes(x = anno, y = n, fill = categoria)) +
  geom_col(width = 0.85) +
  geom_vline(xintercept = 2005.5, colour = COL_GRIGIO,
             linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 1981, y = 1220000, label = testo_20anni,
           hjust = 0, vjust = 1, size = 3.4, family = "Source Sans Pro",
           lineheight = 1.1, color = COL_NERO) +
  geom_text(data = label_data,
            aes(x = 2026.2, y = y, label = testo, color = categoria),
            hjust = 0, size = 3.4, fontface = "bold",
            family = "Source Sans Pro", lineheight = 0.95,
            show.legend = FALSE, inherit.aes = FALSE) +
  scale_fill_manual(values = colori) +
  scale_color_manual(values = colori) +
  scale_x_continuous(breaks = c(seq(1980, 2020, 10), 2025),
                     labels = c("≤1980", seq(1990, 2020, 10), 2025),
                     limits = c(1978.5, 2037), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1250000, 250000),
                     labels = fmt_conteggi,
                     limits = c(0, 1320000), expand = expansion(mult = c(0, 0.01))) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "Tre pensioni su dieci sono pagate da più di vent'anni",
       subtitle = "Numero di pensioni vigenti nel 2026 per anno di decorrenza; la prima barra raggruppa le decorrenze fino\nal 1980 e «invalidità» comprende anche le prestazioni agli invalidi civili, Italia, 2026",
       caption = CAP_INPS)

ggsave(file.path(output_dir, "pensioni_decorrenza.png"),
       plot = p, width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 3) Sanity check --------------------------------------------------------
tot <- sum(df$totale)
cat("Totale pensioni vigenti 2026:", format(tot, big.mark = "."), "\n")
cat("Quota decorrenza >= 2014:",
    round(sum(df$totale[df$anno >= 2014]) / tot * 100, 1), "%\n")
cat("Quota decorrenza <= 2000 (>= 25 anni):",
    round(sum(df$totale[df$anno <= 2000]) / tot * 100, 1), "%\n")
cat("Pensioni ante 1981:", format(df$totale[df$anno == 1980], big.mark = "."), "\n")
cat("Vecchiaia:", format(sum(df$vecchiaia), big.mark = "."),
    "| Superstite:", format(sum(df$superstite), big.mark = "."),
    "| Invalidità:", format(sum(df$invalidita_tot), big.mark = "."),
    "| Assegni sociali:", format(sum(df$assegni_sociali), big.mark = "."), "\n")
cat("Oltre 20 anni (<= 2005):", format(n_20anni, big.mark = "."), "=", pct_20anni, "%\n")
