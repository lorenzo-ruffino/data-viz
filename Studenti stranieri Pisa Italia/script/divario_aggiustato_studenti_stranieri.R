# script/divario_aggiustato_studenti_stranieri.R
# Divario di punteggio Pisa fra studenti con e senza origini straniere in
# Italia nel 2025, prima e dopo aver tenuto conto della condizione sociale e
# della lingua parlata a casa.
# Fonte: OCSE, PISA 2025 Results (Volume I), tabelle I.B1.2d.10/11/12.
# Eseguito da: cd script && Rscript divario_aggiustato_studenti_stranieri.R

library(tidyverse)
library(readxl)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

palette_stadi <- c(
  "Divario grezzo"                          = "#F12938",
  "A parità di condizione sociale"          = "#F2A900",
  "A parità anche di lingua parlata a casa" = "#0478EA"
)

# Corpi nominali di grafici-r (14/9/9/9/10): la tela è larga 10 in perché i
# pannelli sono tre, ma ognuno è più stretto di un grafico singolo da 8 in.
theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
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

# Ogni ciclo Pisa occupa nove colonne: il divario grezzo, quello al netto dello
# status socio-economico e quello al netto anche della lingua parlata a casa,
# ciascuno con il proprio errore standard. Il 2025 è il quarto blocco.
cicli <- c(2015, 2018, 2022, 2025)

leggi_tabella <- function(foglio, dominio) {
  raw <- read_excel(xlsx, sheet = foglio, col_names = FALSE, skip = 9,
                    .name_repair = "minimal")
  paesi <- raw[[1]]
  map_dfr(seq_along(cicli), function(i) {
    base <- 9 * (i - 1)
    tibble(
      paese_en = paesi,
      anno     = cicli[i],
      `Divario grezzo`                          = suppressWarnings(as.numeric(raw[[2 + base]])),
      `A parità di condizione sociale`          = suppressWarnings(as.numeric(raw[[5 + base]])),
      `A parità anche di lingua parlata a casa` = suppressWarnings(as.numeric(raw[[8 + base]]))
    )
  }) |>
    filter(!is.na(paese_en)) |>
    mutate(dominio = dominio)
}

divari <- bind_rows(
  leggi_tabella("Table I.B1.2d.10", "Scienze"),
  leggi_tabella("Table I.B1.2d.11", "Lettura"),
  leggi_tabella("Table I.B1.2d.12", "Matematica")
) |>
  filter(paese_en == "Italy") |>
  pivot_longer(all_of(names(palette_stadi)), names_to = "stadio", values_to = "divario") |>
  mutate(
    stadio  = factor(stadio, levels = names(palette_stadi)),
    dominio = factor(dominio, levels = c("Scienze", "Lettura", "Matematica"))
  ) |>
  select(dominio, anno, stadio, divario) |>
  arrange(dominio, anno, stadio)

write_csv(divari |> mutate(divario = round(divario, 1)),
          file.path(output_dir, "divario_aggiustato_studenti_stranieri.csv"))

plot_data <- filter(divari, anno == 2025)

# Controllo: nel 2025 il divario grezzo in scienze vale -45,0 punti e si
# annulla una volta tenuto conto di condizione sociale e lingua.
check <- plot_data |> filter(dominio == "Scienze") |> pull(divario)
stopifnot(all(abs(check - c(-45.0, -18.5, -1.6)) < 0.1))

# --- 2) Grafico -------------------------------------------------------------

# Il numero sta dentro la barra, in bianco, quando la barra è abbastanza lunga
# da contenerlo; altrimenti fuori, nel colore della serie.
SOGLIA <- 12

fmt <- function(v) paste0(ifelse(v > 0, "+", "−"),
                          formatC(abs(round(v, 1)), format = "f", digits = 1,
                                  decimal.mark = ","))

plot_data <- plot_data |>
  mutate(
    etichetta = fmt(divario),
    dentro    = abs(divario) >= SOGLIA,
    x_lab     = if_else(dentro, divario + 1.8,
                        if_else(divario < 0, divario - 1.2, divario + 1.2)),
    h_lab     = if_else(dentro, 0, if_else(divario < 0, 1, 0)),
    col_lab   = if_else(dentro, "#FFFFFF", palette_stadi[as.character(stadio)])
  )

grafico <- ggplot(plot_data, aes(divario, fct_rev(stadio), fill = stadio)) +
  geom_col(width = 0.62) +
  geom_vline(xintercept = 0, colour = "#1C1C1C", linewidth = 0.3) +
  geom_text(aes(x = x_lab, label = etichetta, hjust = h_lab, colour = col_lab),
            size = 3.1, fontface = "bold", family = "Source Sans Pro") +
  facet_wrap(~ dominio, nrow = 1) +
  scale_fill_manual(values = palette_stadi, drop = FALSE) +
  scale_colour_identity() +
  # L'asse x resta muto: i valori sono già scritti sulle barre e l'unico
  # riferimento che serve è la linea dello zero.
  scale_x_continuous(limits = c(-52, 12), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title = "Nei test Pisa il divario degli studenti stranieri è sociale",
    subtitle = paste0("Differenza di punteggio tra studenti quindicenni con e ",
                      "senza origini straniere, per ambito, Italia, 2025"),
    caption = CAP_OCSE
  ) +
  theme_barchart(
    strip.background = element_blank(),
    strip.text = element_text(family = "Source Sans Pro SemiBold", size = 11,
                              color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.25, t = 0.25, unit = "cm")),
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

ggsave(file.path(output_dir, "divario_aggiustato_studenti_stranieri.png"),
       plot = grafico, width = 10, height = 5, dpi = 220, bg = "white")

cat("Fatto.\n")
print(divari |> pivot_wider(names_from = stadio, values_from = divario) |>
        mutate(across(where(is.numeric) & !anno, \(x) round(x, 1))), n = 12)
