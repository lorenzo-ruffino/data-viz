library(data.table)
library(tidyverse)
library(janitor)
library(readxl)


# Nascite 2025 - totale (italiani + stranieri) per comune: P2
nascite_2025_tot <- fread("P2_2025_it_Comuni.csv", skip = 1, encoding = "UTF-8") %>%
  as.data.frame() %>%
  clean_names() %>%
  select(codice_comune, nati_vivi_totale) %>%
  rename(nati_2025_tot = nati_vivi_totale) %>%
  mutate(codice_comune = sprintf("%06s", codice_comune))


# Nascite 2025 - stranieri per comune: P3
nascite_2025_str <- fread("P3_2025_it_Comuni.csv", skip = 1, encoding = "UTF-8") %>%
  as.data.frame() %>%
  clean_names() %>%
  select(codice_comune, nati_vivi_totale) %>%
  rename(nati_2025_str = nati_vivi_totale) %>%
  mutate(codice_comune = sprintf("%06s", codice_comune))


# Nascite 2025 - italiani = totali - stranieri
nascite_2025 <- nascite_2025_tot %>%
  left_join(nascite_2025_str, by = "codice_comune") %>%
  mutate(nati_2025_str = replace_na(nati_2025_str, 0L),
         nati_2025_ita = nati_2025_tot - nati_2025_str) %>%
  select(codice_comune, nati_2025_ita, nati_2025_str)


# Nascite 2008 per comune e cittadinanza (Istat RBD)
rbd_2008 <- fread("RBD-Dataset-2008.csv", encoding = "UTF-8") %>%
  as.data.frame() %>%
  clean_names() %>%
  filter(sesso == "Maschi e femmine") %>%
  mutate(codice_comune = sprintf("%06s", codice_comune))

nascite_2008 <- rbd_2008 %>%
  filter(cittadinanza == "Italiana") %>%
  group_by(codice_comune) %>%
  summarise(nati_2008_ita = sum(nati, na.rm = TRUE), .groups = "drop") %>%
  full_join(rbd_2008 %>%
              filter(cittadinanza == "Straniera") %>%
              group_by(codice_comune) %>%
              summarise(nati_2008_str = sum(nati, na.rm = TRUE), .groups = "drop"),
            by = "codice_comune") %>%
  mutate(nati_2008_ita = replace_na(nati_2008_ita, 0L),
         nati_2008_str = replace_na(nati_2008_str, 0L))


# Classificazione comuni per grado di urbanizzazione (Istat/Eurostat DEGURBA 2021)
# 1 = Aree urbane, 2 = Aree suburbane, 3 = Aree rurali
classificazione <- read_excel(
  "Classificazioni statistiche-e-dimensione-dei-comuni_01_01_2026.xlsx",
  sheet = "Comuni 01-01-2026"
) %>%
  clean_names() %>%
  select(codice_istat_del_comune_alfanumerico, grado_di_urbanizzazione_2021) %>%
  rename(codice_comune = codice_istat_del_comune_alfanumerico,
         grado_urb = grado_di_urbanizzazione_2021) %>%
  mutate(categoria = case_when(
    grado_urb == 1 ~ "Aree urbane",
    grado_urb == 2 ~ "Aree suburbane",
    grado_urb == 3 ~ "Aree rurali"
  )) %>%
  filter(!is.na(categoria))


# Merge
data_comuni <- classificazione %>%
  inner_join(nascite_2008, by = "codice_comune") %>%
  inner_join(nascite_2025, by = "codice_comune")


# Aggregazione per categoria + Italia
aggrega <- function(df, group_var) {
  df %>%
    group_by(across(all_of(group_var))) %>%
    summarise(n_comuni = n(),
              nati_2008_ita = sum(nati_2008_ita, na.rm = TRUE),
              nati_2008_str = sum(nati_2008_str, na.rm = TRUE),
              nati_2025_ita = sum(nati_2025_ita, na.rm = TRUE),
              nati_2025_str = sum(nati_2025_str, na.rm = TRUE),
              .groups = "drop")
}

aggregato <- aggrega(data_comuni, "categoria")
totale_italia <- aggrega(mutate(data_comuni, categoria = "Italia"), "categoria")

risultato <- bind_rows(aggregato, totale_italia) %>%
  mutate(var_perc_ita = (nati_2025_ita / nati_2008_ita - 1) * 100,
         var_perc_str = (nati_2025_str / nati_2008_str - 1) * 100,
         var_ass_ita  = nati_2025_ita - nati_2008_ita,
         var_ass_str  = nati_2025_str - nati_2008_str) %>%
  arrange(factor(categoria,
                 levels = c("Italia", "Aree urbane", "Aree suburbane", "Aree rurali")))

print(risultato)

write.csv(risultato, "nascite_tipologia_comune.csv", row.names = FALSE)


# ==============================================================================
# GRAFICO: calo delle nascite 2008-2025 per tipologia di comune e cittadinanza
# ==============================================================================

library(ggplot2)
library(ggtext)
library(showtext)
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()

theme_linechart = function(...) {
    theme_minimal() +
        theme(
            text = element_text(family = "Source Sans Pro"),
            legend.position = "top",
            axis.line = element_line(linewidth = 0.3),
            axis.text = element_text(size = 10, color = "#1C1C1C", hjust = 0.5,
                margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
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
            legend.text = element_text(size = 13, color = "#1C1C1C", hjust = 0,
                margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
            strip.text = element_text(size = 13, color = "#1C1C1C", hjust = 0,
                face = "bold",
                margin = margin(b = 0.2, l = 0, t = 0, r = 0, unit = "cm")),
            plot.title = element_text(size = 18, color = "#1C1C1C", hjust = 0,
                margin = margin(b = 0, l = 0, t = 0, r = 0, unit = "cm")),
            plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0,
                lineheight = 1.1,
                margin = margin(b = 0.2, l = 0, t = 0.15, r = 0, unit = "cm")),
            plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                margin = margin(b = 0, l = 0, t = 0.5, r = 0, unit = "cm")),
            ...
        )
}

plot_data <- risultato %>%
  select(categoria, var_perc_ita, var_perc_str) %>%
  pivot_longer(cols = c(var_perc_ita, var_perc_str),
               names_to = "cittadinanza", values_to = "var_perc") %>%
  mutate(cittadinanza = factor(
             ifelse(cittadinanza == "var_perc_ita", "Italiani", "Stranieri"),
             levels = c("Stranieri", "Italiani")),  # Italiani sopra (ultimo = in alto)
         categoria_label = ifelse(categoria == "Italia", "**Italia**", categoria),
         categoria = factor(categoria_label,
                            levels = c("Aree rurali", "Aree suburbane", "Aree urbane", "**Italia**")),
         perc_txt = paste0(gsub("\\.", ",", sprintf("%.1f", var_perc)), "%"))

colori_cit <- c("Italiani" = "#0478EA", "Stranieri" = "#E74C3C")

png("nascite_tipologia_comune.png", width = 7, height = 7, units = "in", res = 300)
ggplot(plot_data, aes(y = categoria, x = var_perc, fill = cittadinanza)) +
  geom_col(width = 0.75, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = perc_txt),
            position = position_dodge(width = 0.8),
            hjust = -0.15, family = "Source Sans Pro",
            fontface = "bold", size = 4.2, color = "white") +
  geom_vline(xintercept = 0, color = "#1C1C1C", linewidth = 0.6) +
  scale_fill_manual(values = colori_cit,
                    breaks = c("Italiani", "Stranieri")) +
  scale_x_continuous(
      limits = c(-50, 2),
      expand = c(0.01, 0.01)
  ) +
  scale_y_discrete(position = "right") +
  theme_linechart() +
  theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      axis.text.y.right = element_markdown(size = 13, color = "#1C1C1C",
                                           hjust = 0,
                                           margin = margin(l = 0.05, unit = "cm")),
      legend.text = element_text(size = 11, color = "#1C1C1C")
  ) +
  labs(
      x = NULL, y = NULL,
      title = "Il calo delle nascite è più forte nei piccoli comuni",
      subtitle = "Variazione percentuale dei nati tra il 2008 e il 2025 per tipologia di comune e cittadinanza",
      caption = "Elaborazione di Lorenzo Ruffino | Fonte: Istat"
  )
dev.off()
