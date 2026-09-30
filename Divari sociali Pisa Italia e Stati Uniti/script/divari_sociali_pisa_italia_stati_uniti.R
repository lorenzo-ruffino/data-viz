# script/divari_sociali_pisa_italia_stati_uniti.R
# Punteggi medi Pisa del quarto di studenti con lo status socio-economico più
# alto e di quello più basso, 2015-2025, in Italia e negli Stati Uniti.
# Fonte: OCSE, PISA 2025 Results (Volume I), tabelle I.B1.2b.22/23/24.
# Eseguito da: cd script && Rscript divari_sociali_pisa_italia_stati_uniti.R

library(tidyverse)
library(readxl)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

palette_paesi <- c("Italia" = "#F12938", "Stati Uniti" = "#0478EA")
tipo_linea    <- c("Il 25% più benestante" = "solid", "Il 25% più povero" = "dashed")

# Corpi nominali di grafici-r (14/9/9/9/10): la tela è larga 10 in perché i
# pannelli sono tre, ma ognuno è più stretto di un grafico singolo da 8 in.
# Riscalare i testi con la larghezza della tela li farebbe pesare più che
# altrove nel repo, non meno.
theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_blank(),
      axis.line.y = element_line(linewidth = 0.3),
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
      legend.title = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(0.1, 0.15, 0.1, 0.1), "cm"),
      plot.title.position = "plot",
      legend.text = element_text(size = 10, color = "#1C1C1C", hjust = 0),
      ...
    )
}

CAP_OCSE <- "Elaborazione di Lorenzo Ruffino su dati OCSE"

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")
xlsx       <- file.path(input_dir, "pisa2025_volumeI_annexB1_cap2b.xlsx")

# --- 1) Dati ----------------------------------------------------------------

# Ogni ciclo Pisa occupa dieci colonne: i quattro quarti di status
# socio-economico (media e errore standard) più la differenza fra il quarto
# più alto e quello più basso. Servono il primo e il quarto quarto.
cicli <- c(2015, 2018, 2022, 2025)

leggi_tabella <- function(foglio, dominio) {
  raw <- read_excel(xlsx, sheet = foglio, col_names = FALSE, skip = 9,
                    .name_repair = "minimal")
  paesi <- raw[[1]]
  map_dfr(seq_along(cicli), function(i) {
    base <- 10 * (i - 1)
    tibble(
      paese_en = paesi,
      anno     = cicli[i],
      `Il 25% più povero`     = suppressWarnings(as.numeric(raw[[2 + base]])),
      `Il 25% più benestante` = suppressWarnings(as.numeric(raw[[8 + base]]))
    )
  }) |>
    filter(!is.na(paese_en)) |>
    mutate(dominio = dominio)
}

nomi_paesi <- c("Italy" = "Italia", "United States*" = "Stati Uniti")

plot_data <- bind_rows(
  leggi_tabella("Table I.B1.2b.22", "Scienze"),
  leggi_tabella("Table I.B1.2b.23", "Lettura"),
  leggi_tabella("Table I.B1.2b.24", "Matematica")
) |>
  filter(paese_en %in% names(nomi_paesi)) |>
  pivot_longer(starts_with("Il 25%"), names_to = "gruppo", values_to = "punteggio") |>
  filter(!is.na(punteggio)) |>
  mutate(
    paese   = factor(nomi_paesi[paese_en], levels = names(palette_paesi)),
    gruppo  = factor(gruppo, levels = names(tipo_linea)),
    dominio = factor(dominio, levels = c("Scienze", "Lettura", "Matematica"))
  ) |>
  select(paese, gruppo, dominio, anno, punteggio) |>
  arrange(dominio, paese, gruppo, anno)

write_csv(plot_data |> mutate(punteggio = round(punteggio, 1)),
          file.path(output_dir, "divari_sociali_pisa_italia_stati_uniti.csv"))

divari <- plot_data |>
  pivot_wider(names_from = gruppo, values_from = punteggio) |>
  mutate(divario = round(`Il 25% più benestante` - `Il 25% più povero`, 1))

# Controllo: nel 2025 il divario italiano vale poco meno di 80 punti in tutti
# e tre gli ambiti (79,5 in scienze, 79,1 in lettura, 79,2 in matematica).
stopifnot(all(between(filter(divari, paese == "Italia", anno == 2025)$divario, 79, 80)))

# --- 2) Grafico -------------------------------------------------------------

etichette_x <- c("2015", "'18", "'22", "2025")

y_lim    <- c(415, 570)
y_breaks <- seq(420, 560, 20)

grafico <- ggplot(plot_data,
                  aes(anno, punteggio, colour = paese, linetype = gruppo)) +
  annotate("segment", x = 2014.6, xend = 2025.4,
           y = y_lim[1], yend = y_lim[1], linewidth = 0.3, colour = "#1C1C1C") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  facet_wrap(~ dominio, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = palette_paesi, drop = FALSE) +
  scale_linetype_manual(values = tipo_linea, drop = FALSE) +
  scale_x_continuous(breaks = cicli, labels = etichette_x,
                     limits = c(2014.6, 2025.4), expand = c(0, 0)) +
  scale_y_continuous(limits = y_lim, breaks = y_breaks, expand = c(0, 0)) +
  guides(
    colour = guide_legend(order = 1, nrow = 1,
                          override.aes = list(linetype = "solid", linewidth = 0.9)),
    linetype = guide_legend(order = 2, nrow = 1,
                            override.aes = list(colour = "#1C1C1C", linewidth = 0.6))
  ) +
  labs(
    title = "Il distacco dagli Stati Uniti è tutto tra i benestanti",
    subtitle = paste0("Punteggio medio degli studenti quindicenni nelle prove ",
                      "Pisa per quarto di status socio-economico, 2015-2025"),
    caption = CAP_OCSE
  ) +
  theme_linechart(
    strip.background = element_blank(),
    strip.text = element_text(family = "Source Sans Pro SemiBold", size = 11,
                              color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.2, t = 0.25, unit = "cm")),
    panel.spacing.x = unit(0.7, "cm"),
    legend.justification = "left",
    legend.box.spacing = unit(0.35, "cm"),
    legend.spacing.x = unit(0.5, "cm")
  ) +
  theme(
    plot.title = element_text(family = "Source Sans Pro SemiBold",
                              size = 14, color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                 lineheight = 1.35,
                                 margin = margin(b = 0.1, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                margin = margin(t = 0.5, unit = "cm")),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )

ggsave(file.path(output_dir, "divari_sociali_pisa_italia_stati_uniti.png"),
       plot = grafico, width = 10, height = 6.3, dpi = 220, bg = "white")

cat("Fatto.\n")
print(divari, n = 30)
