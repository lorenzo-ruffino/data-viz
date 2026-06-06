# Grafico 1 - Produzione industriale a confronto fra paesi
# Indice 2000 = 100, 5 paesi (IT, DE, FR, ES, UE)
# Fonte: Eurostat sts_inpr_a (B-D, industria escl. costruzioni)

library(tidyverse)
library(showtext)
library(ggrepel)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

palette_paesi <- c(
  "Italia"         = "#F12938",
  "Francia"        = "#A82DE3",
  "Germania"       = "#1C1C1C",
  "Spagna"         = "#F2A900",
  "Unione europea" = "#0478EA"
)

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

# --- Dati -------------------------------------------------------------------

input_path <- "../input/eurostat_produzione_industriale.csv"
df <- read_csv(input_path, show_col_types = FALSE) %>%
  filter(nace_r2 == "B-D", anno >= 2000, !is.na(indice)) %>%
  select(geo, anno, indice)

# Rebase a 2000 = 100
base_2000 <- df %>%
  filter(anno == 2000) %>%
  select(geo, base = indice)

df <- df %>%
  left_join(base_2000, by = "geo") %>%
  mutate(indice = indice / base * 100) %>%
  select(-base)

# Etichette italiane
df <- df %>%
  mutate(paese = case_when(
    geo == "IT" ~ "Italia",
    geo == "DE" ~ "Germania",
    geo == "FR" ~ "Francia",
    geo == "ES" ~ "Spagna",
    geo == "EU27_2020" ~ "Unione europea"
  )) %>%
  filter(!is.na(paese))

max_anno <- max(df$anno)

# Etichette inline a destra: paese + valore, con "Unione europea" su due righe
last_points <- df %>%
  filter(anno == max_anno) %>%
  mutate(label_text = case_when(
    paese == "Unione europea" ~ paste0("Unione\neuropea ", round(indice)),
    TRUE ~ paste0(paese, " ", round(indice))
  ))

# Esportazione dato pulito (long + wide)
write_csv(df, "../output/produzione_paesi_indice.csv")

df_wide <- df %>%
  select(anno, paese, indice) %>%
  mutate(indice = round(indice, 1)) %>%
  pivot_wider(names_from = paese, values_from = indice) %>%
  select(anno, Italia, Francia, Germania, Spagna, `Unione europea`) %>%
  arrange(anno)
write_csv(df_wide, "../output/produzione_paesi_indice_wide.csv")

# --- Grafico ----------------------------------------------------------------

p <- ggplot(df, aes(x = anno, y = indice, color = paese)) +
  geom_hline(yintercept = 100, colour = "#9A9A9A", linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(data = last_points, size = 1.8) +
  geom_text_repel(data = last_points,
                  aes(label = label_text),
                  hjust = 0, nudge_x = 0.5,
                  direction = "y",
                  segment.colour = "#9A9A9A",
                  segment.size = 0.3,
                  min.segment.length = 0,
                  box.padding = 0.25,
                  size = 3.4, fontface = "bold",
                  family = "Source Sans Pro",
                  seed = 1) +
  scale_color_manual(values = palette_paesi) +
  scale_x_continuous(breaks = seq(2000, 2025, 5),
                     limits = c(2000, max_anno + 4),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(70, 130, 10),
                     limits = c(70, 130),
                     expand = c(0.01, 0.01)) +
  labs(title = "L'Italia è il paese con il maggior calo industriale",
       subtitle = "Indice della produzione industriale (escluse costruzioni), 2000 = 100",
       caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat") +
  theme_linechart() +
  theme(plot.margin = unit(c(0.4, 1.5, 0.4, 0.4), "cm"))

ggsave("../output/01_produzione_paesi.png", p,
       width = 10, height = 7.7, dpi = 220, bg = "white")

message("Grafico 1 salvato: output/01_produzione_paesi.png")
