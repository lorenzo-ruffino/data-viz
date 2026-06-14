library(tidyverse)
library(lubridate)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"

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
      plot.title = element_text(size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

# --- Dati -------------------------------------------------------------------

csv_url <- "https://raw.githubusercontent.com/marcodallastella/interventi_meloni/refs/heads/main/data/interventi_meloni.csv"
raw <- read_csv(csv_url, col_types = cols(.default = "c"), show_col_types = FALSE)

pattern_us    <- "stati uniti|(?<!sud)america(?!\\s+(latina|meridionale|centrale))|americano|americani|americana|americane|washington"
pattern_trump <- "trump"
pattern_biden <- "biden"

counts <- raw %>%
  filter(!is.na(date)) %>%
  mutate(
    month = str_sub(date, 1, 7),
    text  = str_to_lower(paste(title, content)),
    n_us    = str_count(text, regex(pattern_us, ignore_case = TRUE)),
    n_trump = str_count(text, regex(pattern_trump, ignore_case = TRUE)),
    n_biden = str_count(text, regex(pattern_biden, ignore_case = TRUE))
  ) %>%
  group_by(month) %>%
  summarise(
    `Stati Uniti`  = sum(n_us),
    `Donald Trump` = sum(n_trump),
    `Joe Biden`    = sum(n_biden),
    .groups = "drop"
  ) %>%
  pivot_longer(-month, names_to = "citazione", values_to = "n") %>%
  mutate(
    mese     = ym(month),
    citazione = factor(citazione, levels = c("Joe Biden", "Donald Trump", "Stati Uniti"))
  )

write_csv(
  counts %>% pivot_wider(names_from = citazione, values_from = n),
  "../output/meloni_citazioni_usa_trump.csv"
)

# --- Grafico ----------------------------------------------------------------

p <- ggplot(counts, aes(x = mese, y = n, fill = citazione)) +
  geom_col(width = 18, color = NA) +
  scale_fill_manual(
    values = c(
      "Stati Uniti"  = COL_BLU,
      "Donald Trump" = COL_ROSSO,
      "Joe Biden"    = COL_GIALLO
    )
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    breaks = seq(0, 75, 25),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 75), clip = "off") +
  theme_linechart() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    axis.text = element_text(size = 8),
    axis.text.x = element_text(hjust = 0.5, vjust = 0.5),
    legend.text = element_text(size = 8),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 8)
  ) +
  labs(
    title = "Quanto Giorgia Meloni parla di Trump",
    subtitle = "Numero di citazioni di Stati Uniti, Donald Trump e Joe Biden nei discorsi ufficiali di Giorgia Meloni, ottobre 2022 – giugno 2026",
    caption = "Elaborazione di Lorenzo Ruffino su dati del governo italiano"
  )

ggsave("../output/meloni_citazioni_usa_trump.png", p,
       width = 8, height = 6.5, dpi = 220, bg = "white")
