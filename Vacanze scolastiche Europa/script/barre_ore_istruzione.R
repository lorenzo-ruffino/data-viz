# Ore totali di istruzione obbligatoria annua: primaria vs secondaria I grado, paesi europei.
# Stacked bar chart. Eseguito da `cd script && Rscript barre_ore_istruzione.R`.
# Fonte: OECD Education at a Glance 2024.

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 220)

COL_PRIMARIA   <- "#4A90C4"
COL_SECONDARIA <- "#E6B422"
COL_NERO       <- "#1C1C1C"
COL_GRIGIO     <- "#5A5A5A"

base <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Vacanze scolastiche Europa"

df_raw <- read_csv(file.path(base, "input/ore_istruzione.csv"), show_col_types = FALSE)

ordine <- df_raw %>%
  mutate(tot = primaria + secondaria) %>%
  arrange(tot) %>%
  pull(paese)

df <- df_raw %>%
  pivot_longer(c(primaria, secondaria), names_to = "livello", values_to = "ore") %>%
  mutate(
    paese = factor(paese, levels = ordine),
    livello = factor(livello,
                     levels = c("secondaria", "primaria"),
                     labels = c("Secondaria I grado", "Primaria"))
  )

# Posizioni cumulative per le label dentro i segmenti
df_label <- df_raw %>%
  mutate(paese = factor(paese, levels = ordine)) %>%
  mutate(
    x_primaria   = primaria,
    x_secondaria = primaria + secondaria
  )

p <- ggplot(df, aes(ore, paese, fill = livello)) +
  geom_col(width = 0.72) +
  scale_fill_manual(
    values = c("Primaria" = COL_PRIMARIA, "Secondaria I grado" = COL_SECONDARIA),
    guide = guide_legend(title = NULL, reverse = TRUE)
  ) +
  geom_text(
    data = df_label,
    aes(x = x_primaria - 12, y = paese, label = primaria),
    hjust = 1, size = 3.2, family = "Source Sans Pro",
    colour = "white", fontface = "bold",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = df_label,
    aes(x = x_secondaria - 12, y = paese, label = secondaria),
    hjust = 1, size = 3.2, family = "Source Sans Pro",
    colour = "white", fontface = "bold",
    inherit.aes = FALSE
  ) +
  geom_text(
    aes(x = -28, y = paese, label = paese),
    data = tibble(paese = ordine) %>% mutate(paese = factor(paese, levels = ordine)),
    hjust = 1, size = 3.2, family = "Source Sans Pro", colour = COL_NERO,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(limits = c(-250, 2400), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Ore di istruzione obbligatoria all'anno, primaria e secondaria",
    subtitle = "Ore totali di insegnamento in un anno scolastico, istituti pubblici, 2022/2023",
    caption = "Elaborazione di Lorenzo Ruffino su dati OECD Education at a Glance 2024"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Source Sans Pro"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.91, 0.10),
    legend.justification = c(1, 0),
    legend.text = element_text(size = 11, color = COL_NERO),
    plot.title = element_text(size = 16, face = "bold", color = COL_NERO,
                              margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 11, color = COL_NERO, lineheight = 1.3,
                                 margin = margin(b = 0.3, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 11, color = COL_GRIGIO, hjust = 1,
                                margin = margin(t = 0.4, unit = "cm")),
    plot.title.position = "plot",
    plot.margin = unit(c(0.4, 0.6, 0.4, 0.4), "cm")
  )

ggsave(file.path(base, "output/barre_ore_istruzione.png"), p,
       width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("OK\n")
