# script/distribuzione_reddito.R — eseguito da `cd script && Rscript distribuzione_reddito.R`
# Densità del reddito disponibile equivalente (LIS DART, 2022, $ PPP 2021)

library(tidyverse)
library(showtext)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

palette_paesi <- c(
  "Italia"      = "#F12938",
  "Francia"     = "#A82DE3",
  "Germania"    = "#1C1C1C",
  "Spagna"      = "#E07700",
  "Stati Uniti" = "#0478EA"
)
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
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.15, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.5, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      plot.caption.position = "plot",
      ...
    )
}

# 1) Dati -------------------------------------------------------------------
df <- read_csv(file.path(input_dir, "lis_dart_density.csv"),
               col_types = cols(paese = col_character())) %>%
  mutate(pct_per_1000 = densita * 1000 * 100)   # quota di persone per fascia di 1.000 $

write_csv(df, file.path(output_dir, "distribuzione_reddito.csv"))

# Statistiche di controllo dalla densità (integrazione trapezoidale)
stats <- df %>% arrange(paese, reddito_usd_ppp) %>% group_by(paese) %>%
  mutate(dx = lead(reddito_usd_ppp) - reddito_usd_ppp,
         area = dx * (densita + lead(densita)) / 2,
         cum = cumsum(coalesce(area, 0))) %>%
  summarise(moda = reddito_usd_ppp[which.max(densita)],
            mediana = approx(cum, reddito_usd_ppp, xout = 0.5)$y,
            sotto_15k = 100 * approx(reddito_usd_ppp, cum, xout = 15000)$y,
            sotto_20k = 100 * approx(reddito_usd_ppp, cum, xout = 20000)$y,
            sopra_50k = 100 * (1 - approx(reddito_usd_ppp, cum, xout = 50000)$y),
            sopra_75k = 100 * (1 - approx(reddito_usd_ppp, cum, xout = 75000)$y),
            totale = max(cum), .groups = "drop")
print(stats, width = 120)

# 2) Grafico ----------------------------------------------------------------
X_MAX <- 150000
plot_df <- df %>% filter(reddito_usd_ppp <= X_MAX) %>%
  mutate(paese = factor(paese, levels = names(palette_paesi)))

labels <- tribble(
  ~paese,        ~x,      ~y,   ~hjust, ~vjust,
  "Italia",      17000,   3.95, 0.5,    0,
  "Francia",     33000,   3.05, 0,      0,
  "Spagna",      70000,   1.30, 0,      0,
  "Germania",    44000,   2.65, 0,      0,
  "Stati Uniti", 95000,   0.42, 0,      0
)

# Segmento guida per la Spagna: dall'etichetta al punto della curva a 62.000 $
y_at <- function(paese_sel, x0) {
  d <- filter(plot_df, paese == paese_sel)
  approx(d$reddito_usd_ppp, d$pct_per_1000, xout = x0)$y
}
guide <- tibble(paese = "Spagna", x = 69000, y = 1.28,
                xend = 62500, yend = y_at("Spagna", 62500) + 0.03)

fmt_usd <- function(x) paste0("$ ", format(x / 1000, big.mark = ".", decimal.mark = ","), "k")

p <- ggplot(plot_df, aes(reddito_usd_ppp, pct_per_1000, colour = paese)) +
  geom_area(data = filter(plot_df, paese == "Italia"),
            aes(fill = paese), alpha = 0.10, position = "identity", linewidth = 0) +
  geom_line(aes(linewidth = paese == "Italia")) +
  geom_segment(data = guide, aes(x = x, y = y, xend = xend, yend = yend),
               colour = "#9A9A9A", linewidth = 0.35) +
  geom_text(data = labels, aes(x = x, y = y, label = paese, hjust = hjust, vjust = vjust),
            size = 3.4, fontface = "bold", family = "Source Sans Pro") +
  scale_colour_manual(values = palette_paesi) +
  scale_fill_manual(values = palette_paesi) +
  scale_linewidth_manual(values = c(`TRUE` = 1.15, `FALSE` = 1.2)) +
  scale_x_continuous(limits = c(0, X_MAX), breaks = seq(25000, 150000, 25000),
                     labels = fmt_usd, expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 4.3), breaks = 1:4,
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "In Italia i redditi sono più bassi e più concentrati",
    subtitle = "Distribuzione del reddito disponibile per persona, in dollari a parità di potere d'acquisto del 2021, 2022",
    caption = "Elaborazione di Lorenzo Ruffino su dati Luxembourg Income Study (LIS)"
  ) +
  theme_linechart()

ggsave(file.path(output_dir, "distribuzione_reddito.png"),
       plot = p, width = 9, height = 6.5, units = "in", dpi = 220, bg = "white")

stopifnot(all(abs(stats$totale - 1) < 0.02))
cat("OK\n")
