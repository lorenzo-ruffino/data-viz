library(tidyverse)
library(data.table)
library(showtext)


setwd("~/Documents/Progetti/data-viz/Affluenza Italia")

data = fread("Regionali_Affluenza.csv")


font_add_google("Source Sans Pro")
showtext_auto()

# Tema personalizzato
theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 10, color = "#1C1C1C", hjust = 0.5, margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      plot.margin = unit(c(b = 0.4, l = 0.4, t = 0.4, r = 0.4), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 13, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
      plot.title = element_text(size = 18, color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
      plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0, lineheight = 1.1, margin = margin(b = 0.2, l = 0, t = 0.15, r = 0, unit = "cm")),
      plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 0.5, r = 0, unit = "cm")),
      ...
    )
}

df_long = data %>%
  select(-Data) %>%
  pivot_longer(cols = -Anno, names_to = "Regione", values_to = "Affluenza") %>%
  mutate(Affluenza_perc = Affluenza * 100) %>%
  filter(!is.na(Affluenza))

df_long = df_long %>%
  mutate(Tipo = case_when(
    Regione == "Italia" ~ "Politiche",
    Regione == "Europee" ~ "Europee",
    TRUE ~ "Regionali"
  ))

png("affluenza_elezioni.png", width = 8, height = 7, units = "in", res = 300)
ggplot(df_long, aes(x = Anno, y = Affluenza_perc, group = Regione, color = Tipo)) +
  geom_line(aes(linewidth = Tipo, alpha = Tipo)) +
  scale_color_manual(values = c("Regionali" = "#808080", "Politiche" = "#E0301E", "Europee" = "#0478EA")) +
  scale_linewidth_manual(values = c("Regionali" = 0.5, "Politiche" = 1.2, "Europee" = 1.2)) +
  scale_alpha_manual(values = c("Regionali" = 0.4, "Politiche" = 1, "Europee" = 1)) +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(1970, 2025, by = 5), limits = c(1970, 2025)) +
  scale_y_continuous(expand = c(0.01, 0.01), breaks = seq(0, 100, by = 10), limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  theme_linechart() +
  labs(
    x = NULL,
    y = NULL,
    title = "Com'è cambiata l'affluenza alle elezioni in Italia",
    subtitle = "Affluenza alle elezioni regionali (regioni ordinarie), politiche (Camera) ed europee dal 1970 al 2025",
    caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Ministero dell'Interno"
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1)))

dev.off()



df_long %>%
  group_by(Regione)%>%
  filter(Anno == max(Anno))%>%
  arrange(desc(Affluenza))