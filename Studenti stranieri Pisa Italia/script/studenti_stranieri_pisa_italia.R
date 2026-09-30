# script/studenti_stranieri_pisa_italia.R
# Punteggi medi Pisa in Italia per origine migratoria, 2015-2025.
# Fonte: OCSE, PISA 2025 Results (Volume I), tabelle I.B1.2d.7/8/9.
# Eseguito da: cd script && Rscript studenti_stranieri_pisa_italia.R

library(tidyverse)
library(readxl)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

palette_gruppi <- c(
  "Senza origini straniere"              = "#1C1C1C",
  "Nati in Italia da genitori stranieri" = "#0478EA",
  "Nati all'estero"                      = "#F12938"
)

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
xlsx       <- file.path(input_dir, "pisa2025_volumeI_annexB1_cap2d.xlsx")

# --- 1) Dati ----------------------------------------------------------------

# Ogni ciclo Pisa occupa diciassette colonne: tutti gli studenti, quelli senza
# origini straniere, quelli con origini straniere (in totale, di seconda e di
# prima generazione) e la differenza fra gli ultimi due gruppi, ciascuno con il
# proprio errore standard. Servono la colonna dei non immigrati e quelle delle
# due generazioni.
cicli <- c(2015, 2018, 2022, 2025)

leggi_tabella <- function(foglio, dominio) {
  raw <- read_excel(xlsx, sheet = foglio, col_names = FALSE, skip = 9,
                    .name_repair = "minimal")
  paesi <- raw[[1]]
  map_dfr(seq_along(cicli), function(i) {
    base <- 17 * (i - 1)
    tibble(
      paese_en = paesi,
      anno     = cicli[i],
      `Senza origini straniere`              = suppressWarnings(as.numeric(raw[[4  + base]])),
      `Nati in Italia da genitori stranieri` = suppressWarnings(as.numeric(raw[[10 + base]])),
      `Nati all'estero`                      = suppressWarnings(as.numeric(raw[[13 + base]]))
    )
  }) |>
    filter(!is.na(paese_en)) |>
    mutate(dominio = dominio)
}

plot_data <- bind_rows(
  leggi_tabella("Table I.B1.2d.7", "Scienze"),
  leggi_tabella("Table I.B1.2d.8", "Lettura"),
  leggi_tabella("Table I.B1.2d.9", "Matematica")
) |>
  filter(paese_en == "Italy") |>
  pivot_longer(all_of(names(palette_gruppi)), names_to = "gruppo", values_to = "punteggio") |>
  filter(!is.na(punteggio)) |>
  mutate(
    gruppo  = factor(gruppo, levels = names(palette_gruppi)),
    dominio = factor(dominio, levels = c("Scienze", "Lettura", "Matematica"))
  ) |>
  select(gruppo, dominio, anno, punteggio) |>
  arrange(dominio, gruppo, anno)

write_csv(plot_data |> mutate(punteggio = round(punteggio, 1)),
          file.path(output_dir, "studenti_stranieri_pisa_italia.csv"))

# Controllo: nel 2025 in scienze i tre gruppi fanno 491,5 / 454,0 / 420,2.
check <- plot_data |> filter(dominio == "Scienze", anno == 2025) |> pull(punteggio)
stopifnot(all(abs(check - c(491.5, 454.0, 420.2)) < 0.1))

# --- 2) Grafico -------------------------------------------------------------

etichette_x <- c("2015", "'18", "'22", "2025")

y_lim    <- c(400, 500)
y_breaks <- seq(400, 500, 20)

grafico <- ggplot(plot_data, aes(anno, punteggio, colour = gruppo)) +
  annotate("segment", x = 2014.6, xend = 2025.4,
           y = y_lim[1], yend = y_lim[1], linewidth = 0.3, colour = "#1C1C1C") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  facet_wrap(~ dominio, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = palette_gruppi, drop = FALSE) +
  scale_x_continuous(breaks = cicli, labels = etichette_x,
                     limits = c(2014.6, 2025.4), expand = c(0, 0)) +
  scale_y_continuous(limits = y_lim, breaks = y_breaks, expand = c(0, 0)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 0.9))) +
  labs(
    title = "Gli studenti nati all'estero sono indietro di quasi 70 punti",
    subtitle = paste0("Punteggio medio degli studenti quindicenni nelle prove ",
                      "Pisa per origine e ciclo, Italia, 2015-2025"),
    caption = CAP_OCSE
  ) +
  theme_linechart(
    strip.background = element_blank(),
    strip.text = element_text(family = "Source Sans Pro SemiBold", size = 11,
                              color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.2, t = 0.25, unit = "cm")),
    panel.spacing.x = unit(0.7, "cm"),
    legend.justification = "left",
    legend.box.spacing = unit(0.35, "cm")
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

ggsave(file.path(output_dir, "studenti_stranieri_pisa_italia.png"),
       plot = grafico, width = 10, height = 6.3, dpi = 220, bg = "white")

cat("Fatto.\n")
print(plot_data |> filter(anno == 2025) |> mutate(punteggio = round(punteggio, 1)), n = 12)
