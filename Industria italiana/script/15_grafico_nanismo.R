# Grafico 4 - Paradosso del nanismo dimensionale
# Tre barre per classe: quota imprese, quota dipendenti, quota valore aggiunto
# Fonte: Istat SBS 161_267 - statistiche d'impresa, manifattura (Ateco C)
# Ultimo anno disponibile: 2023

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_BLU      <- "#0478EA"
COL_ROSSO    <- "#F12938"
COL_GIALLO   <- "#F2A900"
COL_NERO     <- "#1C1C1C"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                                 lineheight = 1.0),
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

anno_ref <- 2023
raw <- read_csv("../input/imprese_manifattura_per_dimensione.csv",
                show_col_types = FALSE)

# Per il grafico aggrego 10-19 e 20-49 in "10-49 addetti" (piccole)
aggregato <- raw %>%
  filter(anno == anno_ref,
         variabile %in% c("imprese", "addetti", "valore_aggiunto_mgl_eur"),
         classe_codice != "TOTAL") %>%
  mutate(classe_aggregata = case_when(
    classe_codice == "W0_9" ~ "Microimprese\n1-9 addetti",
    classe_codice %in% c("W10_19", "W20_49") ~ "Piccole\n10-49 addetti",
    classe_codice == "W50_249" ~ "Medie\n50-249 addetti",
    classe_codice == "W_GE250" ~ "Grandi\n250 addetti e più"
  )) %>%
  group_by(variabile, classe_aggregata) %>%
  summarise(valore = sum(valore), .groups = "drop") %>%
  group_by(variabile) %>%
  mutate(quota = valore / sum(valore) * 100) %>%
  ungroup()

# Long format per ggplot
df <- aggregato %>%
  mutate(metrica = case_when(
    variabile == "imprese" ~ "Quota delle imprese",
    variabile == "addetti" ~ "Quota dei dipendenti",
    variabile == "valore_aggiunto_mgl_eur" ~ "Quota del valore aggiunto"
  )) %>%
  select(classe_aggregata, metrica, quota) %>%
  mutate(classe_aggregata = factor(classe_aggregata,
                                   levels = c("Grandi\n250 addetti e più",
                                              "Medie\n50-249 addetti",
                                              "Piccole\n10-49 addetti",
                                              "Microimprese\n1-9 addetti")),
         metrica = factor(metrica,
                          levels = c("Quota delle imprese",
                                     "Quota dei dipendenti",
                                     "Quota del valore aggiunto")))

write_csv(df, "../output/nanismo_imprese_va.csv")

df_wide <- df %>%
  mutate(classe = str_replace(classe_aggregata, "\n", " ")) %>%
  mutate(classe = factor(classe, levels = c("Microimprese 1-9 addetti",
                                            "Piccole 10-49 addetti",
                                            "Medie 50-249 addetti",
                                            "Grandi 250 addetti e più"))) %>%
  select(classe, metrica, quota) %>%
  mutate(quota = round(quota, 1)) %>%
  pivot_wider(names_from = metrica, values_from = quota) %>%
  arrange(classe) %>%
  select(classe,
         `Quota delle imprese`,
         `Quota dei dipendenti`,
         `Quota del valore aggiunto`)
write_csv(df_wide, "../output/nanismo_imprese_va_wide.csv")

# --- Grafico ----------------------------------------------------------------

fmt_pct <- function(v) {
  ifelse(v < 5,
         paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%"),
         paste0(round(v), "%"))
}

df <- df %>% mutate(label_text = fmt_pct(quota))

colori_metriche <- c(
  "Quota delle imprese"       = COL_ROSSO,
  "Quota dei dipendenti"      = COL_GIALLO,
  "Quota del valore aggiunto" = COL_BLU
)

p <- ggplot(df, aes(x = quota, y = classe_aggregata, fill = metrica)) +
  geom_col(width = 0.78, position = position_dodge(width = 0.82)) +
  geom_text(aes(label = label_text, color = metrica),
            position = position_dodge(width = 0.82),
            hjust = -0.15,
            family = "Source Sans Pro", fontface = "bold", size = 3.4) +
  scale_fill_manual(values = colori_metriche) +
  scale_color_manual(values = colori_metriche) +
  scale_x_continuous(limits = c(0, 95), expand = c(0, 0)) +
  guides(fill = guide_legend(reverse = TRUE),
         color = "none") +
  labs(title = "Tante microimprese, poche grandi che producono quasi metà del valore",
       subtitle = paste0("Quota di imprese, di dipendenti e di valore aggiunto della manifattura italiana per classe dimensionale, ",
                         anno_ref),
       caption = "Elaborazione di Lorenzo Ruffino su dati Istat") +
  theme_linechart() +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.margin = margin(b = 0.2, unit = "cm"))

ggsave("../output/04_nanismo_imprese_va.png", p,
       width = 10, height = 7.7, dpi = 220, bg = "white")

message("Grafico 4 salvato: output/04_nanismo_imprese_va.png")
