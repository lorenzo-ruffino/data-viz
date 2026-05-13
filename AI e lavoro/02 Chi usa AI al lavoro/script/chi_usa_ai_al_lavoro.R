# Quota di lavoratori che ha usato l'AI generativa al lavoro, per paese.
#
# Fonte: Bick, Blandin, Deming, Fuchs-Schündeln, Jessen (2026),
# "Mind the Gap: AI Adoption in Europe and the US", Brookings Papers on
# Economic Activity, Spring 2026, Figura 2.
# https://www.brookings.edu/wp-content/uploads/2026/03/6_Bick-et-al_unembargoed.pdf
# Survey condotta gennaio-febbraio 2026, ~3.000 risposte per paese.

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

# ---------------------------------------------------------------------------
# Dati
# ---------------------------------------------------------------------------

df <- tibble(
  paese = c("Stati Uniti", "Regno Unito", "Svezia", "Paesi Bassi",
            "Germania", "Francia", "Italia"),
  pct   = c(43.0, 36.3, 35.6, 35.6, 31.5, 28.1, 25.6)
) %>%
  mutate(paese = factor(paese, levels = paese[order(pct)]))

write_csv(df, "../output/chi_usa_ai_al_lavoro.csv")

# ---------------------------------------------------------------------------
# Tema e palette
# ---------------------------------------------------------------------------

COL_BLU     <- "#0478EA"
COL_AZZURRO <- "#7FB8F0"
COL_ROSSO   <- "#F12938"

# Stati Uniti in blu (paese leader), Italia in rosso (fanalino),
# gli altri cinque in azzurro per fare risaltare i due estremi.
df <- df %>%
  mutate(colore = case_when(
    paese == "Stati Uniti" ~ COL_BLU,
    paese == "Italia"      ~ COL_ROSSO,
    TRUE                   ~ COL_AZZURRO
  ))

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 11, color = "#1C1C1C", hjust = 1),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.4, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

# ---------------------------------------------------------------------------
# Grafico
# ---------------------------------------------------------------------------

p <- ggplot(df, aes(x = pct, y = paese, fill = I(colore))) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(format(pct, decimal.mark = ","), "%")),
            hjust = -0.15, family = "Source Sans Pro",
            fontface = "bold", size = 4, color = "#1C1C1C") +
  scale_x_continuous(limits = c(0, 50),
                     expand = c(0, 0)) +
  labs(title = "In Italia usa l'AI al lavoro un lavoratore su quattro",
       subtitle = "Quota di lavoratori che ha usato l'AI generativa per lavoro, gennaio-febbraio 2026",
       caption = "Elaborazione di Lorenzo Ruffino su dati Bick et al. (2026)") +
  theme_linechart()

ggsave("../output/chi_usa_ai_al_lavoro.png", p,
       width = 8, height = 5.5, units = "in", dpi = 220, bg = "white")
