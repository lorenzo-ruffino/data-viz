# Grafico 2 - Settori vincenti e perdenti della manifattura italiana
# Barre orizzontali, variazione % produzione 2007-2024 per comparto
# Fonte: Istat 115_333 (indice mensile per settore, media annua)

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO <- "#1C1C1C"
COL_BLU <- "#0478EA"
COL_ROSSO <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10, color = "#1C1C1C", hjust = 1),
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

# --- Dati -------------------------------------------------------------------

input_path <- "../input/produzione_industriale_per_settore_mensile.csv"
df_raw <- read_csv(input_path, show_col_types = FALSE)

# Estraggo anno e media annua, escludo "Totale manifattura" dal grafico
df <- df_raw %>%
  filter(!is.na(indice), settore != "Totale manifattura") %>%
  mutate(anno = as.integer(substr(mese, 1, 4))) %>%
  filter(anno %in% c(2007, 2024)) %>%
  group_by(settore, anno) %>%
  summarise(indice = mean(indice), n_mesi = n(), .groups = "drop") %>%
  filter(n_mesi == 12) %>%
  select(-n_mesi) %>%
  pivot_wider(names_from = anno, values_from = indice, names_prefix = "y") %>%
  filter(!is.na(y2007), !is.na(y2024)) %>%
  mutate(var_pct = (y2024 / y2007 - 1) * 100)

# Etichette settori più leggibili
df <- df %>%
  mutate(settore_label = case_when(
    settore == "Altri mezzi di trasporto" ~ "Navi, treni, aerei",
    settore == "Riparazione/installazione macchine" ~ "Riparazione di macchinari",
    settore == "Elettronica e ottica" ~ "Elettronica",
    settore == "Apparecchiature elettriche" ~ "Apparecchi elettrici",
    settore == "Minerali non metalliferi" ~ "Vetro, cemento, ceramica",
    settore == "Prodotti in metallo" ~ "Lavorazioni in metallo",
    settore == "Gomma e plastica" ~ "Gomma e plastica",
    settore == "Pelle e calzature" ~ "Pelle e calzature",
    settore == "Altre manifatturiere" ~ "Altre manifatturiere",
    TRUE ~ settore
  )) %>%
  arrange(var_pct) %>%
  mutate(settore_label = factor(settore_label, levels = settore_label),
         positivo = var_pct >= 0)

# Esportazione dato pulito (long + wide leggibile)
write_csv(df, "../output/settori_variazione_2007_2024.csv")

df_wide <- df %>%
  transmute(settore = settore_label,
            `Indice 2007` = round(y2007, 1),
            `Indice 2024` = round(y2024, 1),
            `Variazione (%)` = round(var_pct, 1)) %>%
  arrange(`Variazione (%)`)
write_csv(df_wide, "../output/settori_variazione_2007_2024_wide.csv")

# --- Grafico ----------------------------------------------------------------

max_abs <- max(abs(df$var_pct))
xlim_max <- ceiling(max_abs / 10) * 10

# Etichette dei valori sulle barre, sempre all'esterno della barra
df <- df %>%
  mutate(label_text = paste0(ifelse(positivo, "+", ""), round(var_pct), "%"),
         label_x = ifelse(positivo, var_pct + 1.5, var_pct - 1.5),
         label_hjust = ifelse(positivo, 0, 1),
         label_color = ifelse(positivo, COL_BLU, COL_ROSSO))

p <- ggplot(df, aes(x = var_pct, y = settore_label, fill = positivo)) +
  geom_col(width = 0.75) +
  geom_vline(xintercept = 0, colour = "#1C1C1C", linewidth = 0.4) +
  geom_text(aes(x = label_x, label = label_text, hjust = label_hjust, color = label_color),
            family = "Source Sans Pro", fontface = "bold", size = 3.3) +
  scale_color_identity() +
  scale_fill_manual(values = c("TRUE" = COL_BLU, "FALSE" = COL_ROSSO)) +
  scale_x_continuous(limits = c(-xlim_max - 15, xlim_max + 15),
                     breaks = seq(-xlim_max, xlim_max, 20),
                     labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%"),
                     expand = c(0, 0)) +
  labs(title = "Crollano moda, mobili e auto, cresce la farmaceutica",
       subtitle = "Variazione della produzione industriale per comparto della manifattura italiana, 2007-2024",
       caption = "Elaborazione di Lorenzo Ruffino su dati Istat") +
  theme_linechart()

ggsave("../output/02_settori_vincenti_perdenti.png", p,
       width = 10, height = 7.7, dpi = 220, bg = "white")

message("Grafico 2 salvato: output/02_settori_vincenti_perdenti.png")
