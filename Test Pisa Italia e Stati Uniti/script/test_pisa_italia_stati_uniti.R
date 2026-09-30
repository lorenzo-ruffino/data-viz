# script/test_pisa_italia_stati_uniti.R
# Punteggi medi Pisa in scienze, lettura e matematica, 2006-2025,
# in Italia, negli Stati Uniti e nella media OCSE.
# Fonte: OCSE, PISA 2025 Results (Volume I), tabelle I.B1.2a.36/37/38.
# Eseguito da: cd script && Rscript test_pisa_italia_stati_uniti.R

library(tidyverse)
library(readxl)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

palette_serie <- c(
  "Italia"                = "#F12938",
  "Stati Uniti"           = "#0478EA",
  "Media OCSE (23 paesi)" = "#5A5A5A"
)

tipo_linea <- c(
  "Italia"                = "solid",
  "Stati Uniti"           = "solid",
  "Media OCSE (23 paesi)" = "dashed"
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
xlsx       <- file.path(input_dir, "pisa2025_volumeI_annexB1_cap2.xlsx")

# --- 1) Dati ----------------------------------------------------------------

# Nelle tabelle di tendenza dell'Annesso B1 la prima colonna è il paese e le
# successive alternano punteggio medio e errore standard, un ciclo Pisa alla
# volta. Le celle "m" (dato non disponibile) diventano NA: per gli Stati Uniti
# manca la lettura 2006, annullata dall'OCSE per un errore di stampa dei
# fascicoli.
leggi_tabella <- function(foglio, cicli, dominio) {
  raw <- read_excel(xlsx, sheet = foglio, col_names = FALSE, skip = 9,
                    .name_repair = "minimal")
  paesi <- raw[[1]]
  map_dfr(seq_along(cicli), function(i) {
    tibble(
      paese_en  = paesi,
      anno      = cicli[i],
      punteggio = suppressWarnings(as.numeric(raw[[2 * i]]))
    )
  }) |>
    filter(!is.na(paese_en)) |>
    mutate(dominio = dominio)
}

pisa <- bind_rows(
  leggi_tabella("Table I.B1.2a.36",
                c(2006, 2009, 2012, 2015, 2018, 2022, 2025), "Scienze"),
  leggi_tabella("Table I.B1.2a.37",
                c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025), "Lettura"),
  leggi_tabella("Table I.B1.2a.38",
                c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025), "Matematica")
)

# La media OCSE con serie storica completa è quella sui 23 paesi presenti in
# tutti i cicli, la stessa che l'OCSE usa nei suoi grafici di tendenza.
nomi_serie <- c("Italy" = "Italia", "United States*" = "Stati Uniti",
                "OECD average-23" = "Media OCSE (23 paesi)")

plot_data <- pisa |>
  filter(paese_en %in% names(nomi_serie), anno >= 2006, !is.na(punteggio)) |>
  mutate(
    serie   = factor(nomi_serie[paese_en], levels = names(palette_serie)),
    dominio = factor(dominio, levels = c("Scienze", "Lettura", "Matematica"))
  ) |>
  select(serie, dominio, anno, punteggio) |>
  arrange(dominio, serie, anno)

write_csv(plot_data |> mutate(punteggio = round(punteggio, 1)),
          file.path(output_dir, "test_pisa_italia_stati_uniti.csv"))

# Controllo: i punteggi italiani del 2025 devono essere quelli del comunicato.
check <- plot_data |> filter(serie == "Italia", anno == 2025) |>
  mutate(punteggio = round(punteggio))
stopifnot(identical(sort(check$punteggio), c(468, 474, 483)))

# --- 2) Grafico -------------------------------------------------------------

cicli_x     <- c(2006, 2009, 2012, 2015, 2018, 2022, 2025)
etichette_x <- c("2006", "'09", "'12", "'15", "'18", "'22", "2025")

y_lim    <- c(455, 512)
y_breaks <- seq(460, 510, 10)

grafico <- ggplot(plot_data,
                  aes(anno, punteggio, colour = serie, linetype = serie)) +
  annotate("segment", x = 2005.4, xend = 2025.6,
           y = y_lim[1], yend = y_lim[1], linewidth = 0.3, colour = "#1C1C1C") +
  geom_line(aes(linewidth = serie == "Italia")) +
  geom_point(aes(size = serie == "Italia")) +
  facet_wrap(~ dominio, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = palette_serie, drop = FALSE) +
  scale_linetype_manual(values = tipo_linea, drop = FALSE) +
  scale_linewidth_manual(values = c("TRUE" = 0.95, "FALSE" = 0.7), guide = "none") +
  scale_size_manual(values = c("TRUE" = 1.8, "FALSE" = 1.4), guide = "none") +
  scale_x_continuous(breaks = cicli_x, labels = etichette_x,
                     limits = c(2005.4, 2025.6), expand = c(0, 0)) +
  scale_y_continuous(limits = y_lim, breaks = y_breaks, expand = c(0, 0)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 0.9)),
         linetype = guide_legend(nrow = 1)) +
  labs(
    title = "Gli Stati Uniti fanno meglio dell'Italia a scuola",
    subtitle = paste0("Punteggio medio degli studenti quindicenni nelle prove ",
                      "Pisa dell'OCSE, per ambito e ciclo, 2006-2025"),
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

ggsave(file.path(output_dir, "test_pisa_italia_stati_uniti.png"),
       plot = grafico, width = 10, height = 6.3, dpi = 220, bg = "white")

cat("Fatto.\n")
print(plot_data |> filter(anno == 2025) |> mutate(punteggio = round(punteggio, 1)), n = 20)
