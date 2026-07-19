# =============================================================================
# Spesa pubblica in percentuale del PIL nei paesi UE27 + EFTA, 2024
# Fonte: Eurostat, gov_10a_main (na_item = TE, unit = PC_GDP, sector = S13)
# Dato gia' scaricato in ../input/eurostat_gov_10a_main.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO         <- "#1C1C1C"
COL_BLU          <- "#0478EA"
COL_ROSSO        <- "#F12938"
COL_GRIGIO_BARRA <- "#CBCBCB"

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

# --- Dati --------------------------------------------------------------------
df <- read_csv("../input/eurostat_gov_10a_main.csv", show_col_types = FALSE)

nomi <- c(
  AT = "Austria", BE = "Belgio", BG = "Bulgaria", CY = "Cipro", CZ = "Cechia",
  DE = "Germania", DK = "Danimarca", EE = "Estonia", EL = "Grecia", ES = "Spagna",
  FI = "Finlandia", FR = "Francia", HR = "Croazia", HU = "Ungheria", IE = "Irlanda",
  IT = "Italia", LT = "Lituania", LU = "Lussemburgo", LV = "Lettonia", MT = "Malta",
  NL = "Paesi Bassi", PL = "Polonia", PT = "Portogallo", RO = "Romania", SE = "Svezia",
  SI = "Slovenia", SK = "Slovacchia",
  CH = "Svizzera", IS = "Islanda", NO = "Norvegia",
  EU27_2020 = "Media UE27"
)

plot_data <- df %>%
  mutate(anno = as.integer(substr(as.character(TIME_PERIOD), 1, 4))) %>%
  filter(anno == 2024, geo %in% names(nomi)) %>%
  transmute(
    geo,
    nome   = unname(nomi[geo]),
    valore = OBS_VALUE,
    tipo   = case_when(
      geo == "IT"        ~ "Italia",
      geo == "EU27_2020" ~ "Media UE27",
      TRUE               ~ "Altro"
    )
  ) %>%
  mutate(nome = fct_reorder(nome, valore))

# Dato pulito esportato
plot_data %>%
  arrange(desc(valore)) %>%
  transmute(codice = geo, paese = as.character(nome), spesa_pubblica_pct_pil = valore) %>%
  write_csv("../output/spesa_pubblica_pil_europa.csv")

col_fill <- c("Italia" = COL_ROSSO, "Media UE27" = COL_BLU, "Altro" = COL_GRIGIO_BARRA)
col_text <- c("Italia" = COL_ROSSO, "Media UE27" = COL_BLU, "Altro" = COL_NERO)

fmt_pct <- function(v) paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%")

p <- ggplot(plot_data, aes(x = valore, y = nome)) +
  geom_col(aes(fill = tipo), width = 0.72) +
  geom_text(aes(label = fmt_pct(valore), color = tipo),
            hjust = -0.15, size = 3.0, fontface = "bold",
            family = "Source Sans Pro") +
  scale_fill_manual(values = col_fill) +
  scale_color_manual(values = col_text) +
  scale_x_continuous(limits = c(0, 64), expand = c(0, 0)) +
  labs(
    title = "La spesa pubblica supera il 50% del PIL in sei paesi",
    subtitle = "Spesa delle amministrazioni pubbliche in percentuale del PIL, paesi UE ed EFTA, 2024",
    caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat"
  ) +
  theme_linechart() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_text(size = 9, color = "#1C1C1C", hjust = 1)
  )

ggsave("../output/spesa_pubblica_pil_europa.png", p,
       width = 8, height = 9.5, dpi = 220, bg = "white")

cat("Fatto. Paesi:", nrow(plot_data),
    "| EU27:", plot_data$valore[plot_data$geo == "EU27_2020"],
    "| Italia:", plot_data$valore[plot_data$geo == "IT"], "\n")
