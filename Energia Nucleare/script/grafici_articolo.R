library(tidyverse)
library(showtext)
library(sf)
library(rnaturalearth)
library(viridis)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

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

CAP_OWID <- "Elaborazione di Lorenzo Ruffino su dati Our World in Data"

# === GRAFICO 1: Mappa quota di elettricità da nucleare =====================

raw_share <- read_csv(
  "/Users/lorenzoruffino/Desktop/Nucleare/share-electricity-nuclear.filtered/share-electricity-nuclear.csv",
  show_col_types = FALSE
)

share_data <- raw_share %>%
  filter(!is.na(Code), Code != "") %>%
  rename(iso_a3 = Code, quota = Nuclear) %>%
  select(iso_a3, quota)

write_csv(
  share_data %>% rename(quota_nucleare = quota),
  "../output/01_quota_nucleare.csv"
)

europe <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "Europe", !name %in% c("Russia", "Iceland"))

europe_data <- europe %>%
  left_join(share_data, by = c("iso_a3_eh" = "iso_a3")) %>%
  mutate(quota_plot = ifelse(is.na(quota) | quota == 0, NA_real_, quota))

# Proiezione Robinson, Russia e Islanda escluse, Ucraina interamente visibile
europe_robin <- st_transform(europe_data, crs = "+proj=robin")
europe_bbox <- st_bbox(c(xmin = -1000000, ymin = 3700000,
                         xmax = 3300000, ymax = 7500000),
                       crs = "+proj=robin")
europe_robin <- st_crop(europe_robin, europe_bbox)

p1 <- ggplot(europe_robin) +
  geom_sf(aes(fill = quota_plot), colour = "#9A9A9A", linewidth = 0.15) +
  scale_fill_viridis_b(
    option = "mako",
    direction = -1,
    breaks = seq(10, 70, 10),
    limits = c(0, 70),
    na.value = "white",
    labels = function(x) paste0(x, "%"),
    guide = guide_colorsteps(
      barwidth = 11,
      barheight = 0.5,
      title.position = "top",
      label.position = "bottom",
      ticks = FALSE,
      frame.colour = NA
    )
  ) +
  labs(
    title = "Quanto è usata l'energia nucleare per l'elettricità",
    subtitle = "Quota di elettricità prodotta da nucleare, paesi in bianco senza nucleare, Europa, 2025",
    caption = CAP_OWID
  ) +
  theme_linechart() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.03, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
  )

ggsave(
  "../output/01_quota_nucleare.png",
  plot = p1, width = 8, height = 6.5, dpi = 300, bg = "white"
)

# === GRAFICO 2: Sicurezza delle fonti di energia ===========================

raw_deaths <- read_csv(
  "/Users/lorenzoruffino/Desktop/Nucleare/death-rates-from-energy-production-per-twh.filtered/death-rates-from-energy-production-per-twh.csv",
  show_col_types = FALSE
)

fonti_traduzione <- c(
  "Brown coal" = "Lignite",
  "Coal"       = "Carbone",
  "Oil"        = "Petrolio",
  "Biomass"    = "Biomasse",
  "Gas"        = "Gas",
  "Hydropower" = "Idroelettrico",
  "Wind"       = "Eolico",
  "Nuclear"    = "Nucleare",
  "Solar"      = "Solare"
)

deaths_plot <- raw_deaths %>%
  rename(morti = `Deaths per terawatt-hour of energy production`) %>%
  mutate(
    fonte = fonti_traduzione[Entity],
    is_nucleare = fonte == "Nucleare"
  ) %>%
  arrange(desc(morti)) %>%
  mutate(
    fonte = factor(fonte, levels = rev(fonte)),
    colore = ifelse(is_nucleare, COL_ROSSO, COL_BLU)
  )

write_csv(
  deaths_plot %>% select(fonte, anno = Year, morti_per_twh = morti),
  "../output/02_sicurezza_nucleare.csv"
)

format_morti <- function(x) {
  sapply(x, function(v) {
    if (v >= 10) {
      formatC(round(v, 1), format = "f", digits = 1, big.mark = ".", decimal.mark = ",")
    } else if (v >= 1) {
      formatC(round(v, 1), format = "f", digits = 1, decimal.mark = ",")
    } else {
      formatC(v, format = "f", digits = 3, decimal.mark = ",")
    }
  })
}

p2 <- ggplot(deaths_plot, aes(x = fonte, y = morti, fill = colore)) +
  geom_col(width = 0.72) +
  geom_text(
    aes(
      label = format_morti(morti),
      hjust = ifelse(morti < 4, -0.25, 1.15),
      colour = ifelse(morti < 4, COL_NERO, "white")
    ),
    size = 3.4, family = "Source Sans Pro", fontface = "bold"
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_y_continuous(
    limits = c(0, 36),
    expand = c(0, 0)
  ) +
  coord_flip() +
  labs(
    title = "L'energia nucleare è molto sicura",
    subtitle = "Morti per terawattora di energia prodotta, per fonte, 2021",
    caption = CAP_OWID
  ) +
  theme_linechart() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      size = 10, hjust = 1,
      colour = ifelse(levels(deaths_plot$fonte) == "Nucleare", COL_ROSSO, COL_NERO),
      face = ifelse(levels(deaths_plot$fonte) == "Nucleare", "bold", "plain")
    )
  )

ggsave(
  "../output/02_sicurezza_nucleare.png",
  plot = p2, width = 8, height = 6.5, dpi = 220, bg = "white"
)

cat("Grafici salvati in ../output/\n")
