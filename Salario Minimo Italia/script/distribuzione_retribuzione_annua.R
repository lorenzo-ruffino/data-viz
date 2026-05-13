library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 9, color = COL_NERO, hjust = 0.5),
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
      legend.text = element_text(size = 10, color = COL_NERO, hjust = 0),
      plot.title = element_text(size = 14, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP_INPS <- "Elaborazione di Lorenzo Ruffino su dati Inps"

# --- Dati -------------------------------------------------------------------
# Inps 2024, dipendenti privati. FYFT (full-time tutto l'anno) dal file con
# filtro "Presenza tempo parziale: No", colonna "Annointero". Totale dal file
# senza filtri, colonna "Totale". "Altri" = totale - FYFT.

dati <- tribble(
  ~classe,         ~midpoint,   ~fyft,    ~totale,
  "Fino a 5000",       2500,        0,   2309118,
  "5000-9999",         7500,     3834,   1838351,
  "10000-14999",      12500,    17749,   1970218,
  "15000-19999",      17500,   252942,   2056366,
  "20000-24999",      22500,  1234326,   2489782,
  "25000-29999",      27500,  1697969,   2337923,
  "30000-34999",      32500,  1195974,   1486142,
  "35000-39999",      37500,   743158,    887367,
  "40000-44999",      42500,   504507,    587085,
  "45000-49999",      47500,   360676,    411743,
  "50000-59999",      55000,   465338,    520834,
  "60000-79999",      70000,   415413,    455631,
  "80000+",           90000,   352055,    380442
)

dati <- dati %>%
  mutate(
    altri = totale - fyft,
    pct_fyft = fyft / sum(totale) * 100,
    pct_altri = altri / sum(totale) * 100
  )

write_csv(dati, "../output/distribuzione_retribuzione_annua.csv")

# Long format per ggplot
plot_data <- dati %>%
  select(midpoint, pct_fyft, pct_altri) %>%
  pivot_longer(cols = c(pct_fyft, pct_altri),
               names_to = "gruppo", values_to = "pct") %>%
  mutate(gruppo = if_else(gruppo == "pct_fyft",
                          "A tempo pieno per tutto l'anno",
                          "Part-time o meno di un anno"))

# Ancoraggio a (0, 0) per le linee/aree, così non sembrano nascere a metà grafico
anchor <- tibble(
  midpoint = 0,
  gruppo = c("A tempo pieno per tutto l'anno", "Part-time o meno di un anno"),
  pct = 0
)

plot_data_full <- bind_rows(anchor, plot_data) %>%
  arrange(gruppo, midpoint)

# Etichette inline (testo personalizzato su due righe)
label_data <- tibble(
  gruppo = c("A tempo pieno per tutto l'anno", "Part-time o meno di un anno"),
  label_text = c("Lavorano a tempo pieno\nper tutto l'anno",
                 "Lavorano part-time o\nper meno di un anno"),
  midpoint = c(70000, 7500),
  pct = c(3.5, 10.8),
  hjust = c(1, 0),
  vjust = c(-0.4, -0.4)
)

# --- Grafico ---------------------------------------------------------------

p <- ggplot(plot_data_full, aes(x = midpoint, y = pct, color = gruppo, fill = gruppo)) +
  geom_area(alpha = 0.25, position = "identity", linewidth = 0) +
  geom_line(linewidth = 0.9) +
  geom_point(data = plot_data, size = 1.8) +
  geom_text(data = label_data,
            aes(label = label_text, hjust = hjust, vjust = vjust),
            fontface = "bold", size = 3.6,
            family = "Source Sans Pro", lineheight = 0.95) +
  scale_color_manual(values = c(
    "A tempo pieno per tutto l'anno" = COL_BLU,
    "Part-time o meno di un anno" = COL_ROSSO
  )) +
  scale_fill_manual(values = c(
    "A tempo pieno per tutto l'anno" = COL_BLU,
    "Part-time o meno di un anno" = COL_ROSSO
  )) +
  scale_x_continuous(
    breaks = c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 90000),
    labels = function(x) {
      ifelse(x == 0, "0",
             ifelse(x == 90000, "≥ € 80k",
                    paste0("€ ", format(x / 1000, big.mark = ".", decimal.mark = ","), "k")))
    },
    limits = c(0, 95000),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(format(x, decimal.mark = ","), "%"),
    limits = c(0, 14),
    breaks = seq(0, 12, 3),
    expand = c(0, 0)
  ) +
  labs(
    title = "I bassi salari sono concentrati tra chi non lavora a tempo pieno",
    subtitle = "Distribuzione dei lavoratori dipendenti privati per classe di retribuzione annua lorda, Italia, 2024",
    caption = CAP_INPS
  ) +
  theme_linechart() +
  theme(legend.position = "none",
        axis.line.x = element_line(linewidth = 0.3),
        axis.line.y = element_line(linewidth = 0.3))

ggsave("../output/distribuzione_retribuzione_annua.png",
       plot = p,
       width = 8, height = 6, units = "in", dpi = 220, bg = "white")

cat("Grafico salvato.\n")
cat("Lavoratori FYFT:", format(sum(dati$fyft), big.mark = "."), "\n")
cat("Lavoratori altri:", format(sum(dati$altri), big.mark = "."), "\n")
cat("Totale:", format(sum(dati$totale), big.mark = "."), "\n")
