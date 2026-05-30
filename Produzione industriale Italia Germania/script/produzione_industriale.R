# produzione_industriale.R
# Eseguito da: cd script && Rscript produzione_industriale.R
# Confronto Italia-Germania sulla produzione industriale (industria in senso
# stretto, B-D), dato mensile destagionalizzato, media mobile a 6 mesi,
# numero indice con base 1991 = 100. Entrambe le serie partono dal 1991.

library(eurostat)
library(tidyverse)
library(showtext)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

palette_paesi <- c(
  "Italia"   = "#F12938",
  "Germania" = "#0478EA"
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

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

# --- 1) Dati ----------------------------------------------------------------

raw <- get_eurostat(
  "sts_inpr_m",
  time_format = "date",
  filters = list(geo = c("IT", "DE"), nace_r2 = "B-D",
                 indic_bt = "PRD", unit = "I21", s_adj = "SCA"),
  cache = FALSE, update_cache = TRUE
)

# Il nome della colonna tempo cambia tra versioni del pacchetto eurostat
col_tempo <- intersect(c("TIME_PERIOD", "time"), names(raw))[1]
raw$.data_periodo <- as.Date(raw[[col_tempo]])

df <- raw %>%
  transmute(
    paese = recode(geo, IT = "Italia", DE = "Germania"),
    data  = .data_periodo,
    anno  = as.integer(format(.data_periodo, "%Y")),
    indice_grezzo = values
  ) %>%
  filter(!is.na(indice_grezzo), anno >= 1991) %>%
  arrange(paese, data)

# 1) Media mobile a 6 mesi sul dato grezzo (trailing, solo base R)
ma6 <- function(x) as.numeric(stats::filter(x, rep(1/6, 6), sides = 1))

df <- df %>%
  group_by(paese) %>%
  arrange(data, .by_group = TRUE) %>%
  mutate(indice_ma = ma6(indice_grezzo)) %>%
  ungroup()

# 2) Ribasa la media mobile: il primo valore disponibile = 100 per ciascun
#    paese, cosi' entrambe le serie partono esattamente da 100
plot_data <- df %>%
  filter(!is.na(indice_ma)) %>%
  group_by(paese) %>%
  arrange(data, .by_group = TRUE) %>%
  mutate(indice = indice_ma / first(indice_ma) * 100) %>%
  ungroup()

# Esporta il dato pulito
plot_data %>%
  transmute(paese, data, indice_1991_100 = round(indice, 2)) %>%
  write_csv(file.path(output_dir, "produzione_industriale.csv"))

# --- Diagnostica console ----------------------------------------------------

cat("\n--- Valori chiave (media mobile 6m, base 1991 = 100) ---\n")
diag <- plot_data %>%
  group_by(paese) %>%
  summarise(
    primo = first(indice),
    picco = max(indice),
    anno_picco = anno[which.max(indice)],
    ultimo = last(indice),
    ultima_data = max(data),
    .groups = "drop"
  )
print(diag, width = Inf)

# --- 2) Grafico -------------------------------------------------------------

ultimo_punto <- plot_data %>%
  group_by(paese) %>%
  slice_max(data, n = 1) %>%
  ungroup()

x_min <- min(plot_data$data)
x_max <- max(plot_data$data)

p <- ggplot(plot_data, aes(data, indice, color = paese)) +
  geom_hline(yintercept = 100, colour = "#9A9A9A",
             linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(data = ultimo_punto, size = 1.8) +
  geom_text(data = ultimo_punto,
            aes(label = paese), hjust = 0, nudge_x = 120,
            size = 3.6, fontface = "bold", family = "Source Sans Pro") +
  scale_color_manual(values = palette_paesi) +
  scale_x_date(
    breaks = seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 years"),
    date_labels = "%Y",
    limits = c(x_min, x_max + 2600),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    breaks = seq(60, 160, 10),
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "L'industria italiana produce meno che nel 1991",
    subtitle = paste0(
      "Indice della produzione industriale, media mobile a 6 mesi dei dati mensili destagionalizzati,\n",
      "numero indice con base 1991 = 100, Italia e Germania, 1991-2026"
    ),
    caption = CAP_EUROSTAT
  ) +
  theme_linechart()

ggsave(file.path(output_dir, "produzione_industriale.png"),
       plot = p, width = 8, height = 6.5, dpi = 220, bg = "white")

cat("\nPNG salvato in", file.path(output_dir, "produzione_industriale.png"), "\n")
