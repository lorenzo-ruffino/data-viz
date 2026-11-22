# Tema, palette e funzioni comuni ai grafici dell'articolo sul costo delle case.
# Ogni script grafici_XX_*.R lo carica con source("grafici_00_tema.R") e va
# eseguito dalla cartella script/ (percorsi relativi ../input e ../output).

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
  library(ggrepel)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

IN  <- "../input"
OUT <- "../output"

COL_NERO         <- "#1C1C1C"
COL_BLU          <- "#0478EA"
COL_VIOLA        <- "#A82DE3"
COL_ROSSO        <- "#F12938"
COL_GIALLO       <- "#F2A900"
COL_GRIGIO       <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"
COL_GRIGIO_CHIARO <- "#C9C9C9"
COL_VERDE        <- "#1B9E77"

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
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = "#1C1C1C", hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = "#1C1C1C", hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

# Numeri in formato italiano (virgola decimale, punto per le migliaia)
fmt_it <- function(x, dec = 0)
  formatC(round(x, dec), format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")

# Variazioni con segno esplicito: +27,5% / -11,1%
fmt_var <- function(x, dec = 0)
  paste0(ifelse(x > 0, "+", ifelse(x < 0, "−", "")), fmt_it(abs(x), dec), "%")

salva <- function(p, nome, width = 8, height = 6.5) {
  ggsave(file.path(OUT, nome), p, width = width, height = height,
         units = "in", dpi = 220, bg = "white")
}
