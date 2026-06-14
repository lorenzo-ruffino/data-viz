library(tidyverse); library(showtext)
font_add_google("Source Sans 3", "Source Sans Pro"); showtext_auto(); showtext_opts(dpi = 220)
COL_ROSSO <- "#F12938"; COL_GRIGIO <- "#C9C9C9"
base <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Vacanze scolastiche Europa"

eu <- c("Austria","Belgio","Bulgaria","Cechia","Cipro","Croazia","Danimarca","Estonia","Finlandia",
        "Francia","Germania","Grecia","Irlanda","Italia","Lettonia","Lituania","Lussemburgo","Malta",
        "Paesi Bassi","Polonia","Portogallo","Romania","Slovacchia","Slovenia","Spagna","Svezia","Ungheria")

df <- read_csv(file.path(base, "input/quota_estate.csv"), show_col_types = FALSE) %>%
  filter(paese %in% eu) %>%
  transmute(paese, giorni = round(totale_gg)) %>%
  mutate(paese = fct_reorder(paese, giorni),
         hl = paese == "Italia",
         col = ifelse(hl, COL_ROSSO, COL_GRIGIO),
         lab_col = ifelse(hl, COL_ROSSO, "#1C1C1C"),
         face = ifelse(hl, "bold", "plain"))

p <- ggplot(df, aes(giorni, paese)) +
  geom_col(aes(fill = col), width = 0.72) +
  scale_fill_identity() +
  geom_text(aes(label = giorni, colour = lab_col, fontface = face),
            hjust = -0.25, size = 3.5, family = "Source Sans Pro") +
  geom_text(aes(x = -1.5, label = paese, colour = lab_col, fontface = face),
            hjust = 1, size = 3.5, family = "Source Sans Pro") +
  scale_colour_identity() +
  scale_x_continuous(limits = c(-30, 150), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Quanti giorni di vacanza fa la scuola in un anno",
    subtitle = "Giorni di calendario di chiusura per vacanza (estate più tutte le altre pause), scuole secondarie, 2023/2024\nInclusi i weekend dentro le vacanze; per i paesi con calendari regionali il dato è indicativo",
    caption = "Elaborazione di Lorenzo Ruffino su dati Eurydice"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Source Sans Pro"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 17, face = "bold", color = "#1C1C1C", margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 11, color = "#1C1C1C", lineheight = 1.3, margin = margin(b = 0.3, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 11, color = "#5A5A5A", hjust = 1, margin = margin(t = 0.4, unit = "cm")),
    plot.title.position = "plot",
    plot.margin = unit(c(0.4, 0.6, 0.4, 0.4), "cm")
  )

ggsave(file.path(base, "output/barre_giorni_vacanza.png"), p,
       width = 9, height = 8, units = "in", dpi = 220, bg = "white")
cat("OK\n")
