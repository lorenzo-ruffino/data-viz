library(eurostat)
library(tidyverse)
library(showtext)
library(ggrepel)


# --- Dati Eurostat (earn_nt_net) ---------------------------------------------

selected_countries <- c("ES", "EU27_2020", "DE", "FR", "IT")

country_labels <- c(
  "ES"        = "Spagna",
  "EU27_2020" = "Unione europea",
  "DE"        = "Germania",
  "FR"        = "Francia",
  "IT"        = "Italia"
)

ecase_pct <- c(
  "P1_NCH_AW50"  = 50,
  "P1_NCH_AW67"  = 67,
  "P1_NCH_AW80"  = 80,
  "P1_NCH_AW100" = 100,
  "P1_NCH_AW125" = 125,
  "P1_NCH_AW167" = 167
)

raw <- get_eurostat("earn_nt_net", time_format = "num")

dat <- raw %>%
  filter(currency == "PPS",
         geo %in% selected_countries,
         ecase %in% names(ecase_pct),
         estruct %in% c("GRS", "NET", "TOTAL")) %>%
  group_by(geo, ecase) %>%
  filter(TIME_PERIOD == max(TIME_PERIOD)) %>%
  ungroup() %>%
  select(geo, ecase, estruct, anno = TIME_PERIOD, values) %>%
  pivot_wider(names_from = estruct, values_from = values) %>%
  mutate(
    paese    = factor(country_labels[geo],
                      levels = c("Italia", "Francia", "Germania", "Spagna", "Unione europea")),
    pct      = ecase_pct[ecase],
    rapporto = TOTAL / NET
  ) %>%
  arrange(paese, pct)

write.csv(dat, file = "../output/cuneo_fiscale_europa.csv", row.names = FALSE)


# --- Tema grafico -------------------------------------------------------------

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
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 11, color = "#1C1C1C", hjust = 0.5),
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
      legend.text = element_text(size = 12, color = "#1C1C1C", hjust = 0),
      plot.title = element_text(size = 18, color = "#1C1C1C", hjust = 0),
      plot.subtitle = element_text(size = 10, color = "#1C1C1C", hjust = 0, lineheight = 1.1,
                                   margin = margin(b = 0.2, t = 0.15, unit = "cm")),
      plot.caption = element_text(size = 10, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

caption_fonte <- "Elaborazione di Lorenzo Ruffino su dati Eurostat (earn_nt_net)"


# --- Etichette ----------------------------------------------------------------

# Etichetta del paese: sul punto al 167% del salario medio
labels_paese <- dat %>% filter(pct == 167)

anno_dato <- max(dat$anno)


# --- Grafico ------------------------------------------------------------------

png("../output/cuneo_fiscale_europa.png",
    width = 8, height = 7.5, units = "in", res = 220, bg = "white")
print(
  ggplot(dat, aes(x = NET, y = rapporto)) +
    geom_line(aes(group = paese, colour = paese),
              linewidth = 0.7) +
    geom_point(aes(colour = paese), size = 2.4) +
    geom_text_repel(data = labels_paese,
                    aes(label = as.character(paese), colour = paese),
                    nudge_x = 3000, direction = "y", hjust = 0,
                    segment.size = 0.3, segment.colour = "#9A9A9A",
                    min.segment.length = 0, point.padding = 0.4,
                    box.padding = 0.4, force = 4, seed = 7,
                    size = 4.2, fontface = "bold",
                    family = "Source Sans Pro",
                    show.legend = FALSE) +
    scale_color_manual(values = palette_paesi) +
    scale_x_continuous(
      labels = function(x) paste0(format(x / 1000, big.mark = ".", decimal.mark = ","), "k"),
      breaks = seq(15000, 60000, by = 10000),
      limits = c(13000, 70000),
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
      labels = function(x) format(round(x, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
      breaks = seq(1.2, 2.3, by = 0.1),
      limits = c(1.2, 2.25),
      expand = c(0.02, 0.02)
    ) +
    labs(
      x = "Stipendio netto annuo, a parità di potere d'acquisto (PPS)",
      y = "Costo totale del lavoro per ogni euro di stipendio netto",
      title = "Quanto pesa il fisco sugli stipendi in Europa",
      subtitle = paste0("Rapporto tra costo totale del lavoro e stipendio netto sull'asse Y e stipendio netto in PPS sull'asse X, single\n",
                        "senza figli, sei livelli salariali (50%, 67%, 80%, 100%, 125% e 167% del salario medio nazionale), anno ", anno_dato),
      caption = caption_fonte
    ) +
    theme_linechart() +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 11, color = "#1C1C1C", hjust = 0.5,
                                  margin = margin(t = 0.3, unit = "cm")),
      axis.title.y = element_text(size = 11, color = "#1C1C1C", hjust = 0.5,
                                  angle = 90, margin = margin(r = 0.3, unit = "cm"))
    )
)
dev.off()
