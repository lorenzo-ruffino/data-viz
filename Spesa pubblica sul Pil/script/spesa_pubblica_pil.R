# script/spesa_pubblica_pil.R — eseguito da `cd script && Rscript spesa_pubblica_pil.R`
# Spesa pubblica in % del PIL, 7 Paesi europei, 1950-2023.
# Fonte: FMI "Public Finances in Modern History", elaborata da Our World in Data.

library(tidyverse)
library(showtext)
library(ggrepel)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

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

palette_paesi <- c(
  "Italia"    = "#F12938",  # rosso
  "Francia"   = "#A82DE3",  # viola
  "Germania"  = "#1C1C1C",  # nero
  "Spagna"    = "#F2A900",  # giallo/ocra
  "Svezia"    = "#1B9E77",  # verde
  "Norvegia"  = "#0478EA",  # blu
  "Svizzera"  = "#E0529C"   # rosa
)

# --- Dati -------------------------------------------------------------------

nomi_it <- c(
  "France"      = "Francia",
  "Germany"     = "Germania",
  "Italy"       = "Italia",
  "Norway"      = "Norvegia",
  "Spain"       = "Spagna",
  "Sweden"      = "Svezia",
  "Switzerland" = "Svizzera"
)

df <- read_csv(file.path(input_dir, "owid_gov_spending_raw.csv"),
               show_col_types = FALSE) %>%
  transmute(
    paese  = nomi_it[entity],
    anno   = year,
    valore = expenditure
  ) %>%
  filter(!is.na(paese)) %>%
  group_by(paese) %>%
  arrange(anno, .by_group = TRUE) %>%
  # Media mobile centrata a tre anni; agli estremi resta il valore osservato.
  mutate(valore_ma = coalesce((lag(valore) + valore + lead(valore)) / 3, valore)) %>%
  ungroup()

write_csv(df, file.path(output_dir, "spesa_pubblica_pil.csv"))

anno_max <- max(df$anno)

label_data <- df %>%
  filter(anno == anno_max) %>%
  mutate(label = paste0(paese, " ", round(valore_ma), "%"))

# --- Grafico ----------------------------------------------------------------

p <- ggplot(df, aes(x = anno, y = valore_ma, color = paese)) +
  # Italia disegnata per ultima, sopra le altre, e con linea più spessa.
  geom_line(data = filter(df, paese != "Italia"), linewidth = 0.8) +
  geom_line(data = filter(df, paese == "Italia"), linewidth = 1.2) +
  geom_text_repel(
    data = label_data,
    aes(label = label),
    family = "Source Sans Pro", fontface = "bold", size = 3.2,
    hjust = 0, direction = "y", nudge_x = 1.5,
    segment.colour = "#9A9A9A", min.segment.length = 0,
    box.padding = 0.2, seed = 1
  ) +
  scale_color_manual(values = palette_paesi) +
  scale_x_continuous(limits = c(1950, 2036),
                     breaks = seq(1950, 2020, 10),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 70),
                     breaks = seq(0, 70, 10),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.01, 0)) +
  theme_linechart() +
  labs(
    title = "Quanto pesa la spesa pubblica sul Pil",
    subtitle = "Spesa pubblica totale in percentuale del Pil, media mobile a tre anni, sette Paesi, 1950-2023",
    caption = "Elaborazione di Lorenzo Ruffino su dati Fmi e Our World in Data"
  )

ggsave(file.path(output_dir, "spesa_pubblica_pil.png"),
       plot = p, width = 8, height = 6.5, dpi = 220, bg = "white")
