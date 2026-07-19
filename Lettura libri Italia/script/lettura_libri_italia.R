# Lettura di libri in Italia per classe di età, ultimo anno disponibile.
# Eseguito da `cd script && Rscript lettura_libri_italia.R`.
#
# Barchart orizzontale: quota di persone di 6 anni e più che hanno letto almeno
# un libro nei 12 mesi, per classe di età, con il totale staccato in fondo.
# Fonte: Istat, indagine "Aspetti della vita quotidiana" (dataflow AVQ_PERSONE_233,
# scaricato via SDMX in input/istat_avq_persone_233.tsv).

suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
})

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO  <- "#1C1C1C"
COL_BLU2  <- "#3686D6"
COL_ROSSO <- "#F12938"
CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

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

input_dir  <- ".."
tsv <- read_tsv(file.path(input_dir, "input", "istat_avq_persone_233.tsv"),
                show_col_types = FALSE)

anno <- max(tsv$TIME_PERIOD)

# Mappa codici età -> etichette italiane. Y_GE6 è il totale 6 anni e più.
eta_labels <- c(
  "Y6-10"  = "6-10 anni",  "Y11-14" = "11-14 anni", "Y15-17" = "15-17 anni",
  "Y18-19" = "18-19 anni", "Y20-24" = "20-24 anni", "Y25-34" = "25-34 anni",
  "Y35-44" = "35-44 anni", "Y45-54" = "45-54 anni", "Y55-59" = "55-59 anni",
  "Y60-64" = "60-64 anni", "Y65-74" = "65-74 anni", "Y_GE75" = "75 anni e più",
  "Y_GE6"  = "Totale"
)

dati <- tsv %>%
  filter(TIME_PERIOD == anno) %>%
  transmute(eta = eta_labels[AGE], valore = OBS_VALUE,
            is_totale = AGE == "Y_GE6")

# Ordine verticale (in ggplot il primo livello sta in basso): Totale in fondo,
# poi le classi dalla più anziana (basso) alla più giovane (alto).
ordine <- c("Totale", "75 anni e più", "65-74 anni", "60-64 anni",
            "55-59 anni", "45-54 anni", "35-44 anni", "25-34 anni",
            "20-24 anni", "18-19 anni", "15-17 anni", "11-14 anni", "6-10 anni")
dati$eta <- factor(dati$eta, levels = ordine)

# Esporta dato pulito
write_csv(
  dati %>% filter(!is.na(valore)) %>%
    transmute(classe_eta = as.character(eta), anno = anno,
              quota_lettori = valore) %>%
    arrange(desc(quota_lettori)),
  "../output/lettura_libri_italia.csv"
)

cat("Anno:", anno, "\n")
cat("Totale:", dati$valore[dati$eta == "Totale"], "\n")
cat("Range classi:", paste(range(dati$valore[dati$eta != "Totale"], na.rm = TRUE),
                           collapse = " - "), "\n")

# --- Grafico ----------------------------------------------------------------

fmt_pct <- function(v) paste0(formatC(v, format = "f", digits = 1,
                                      decimal.mark = ","), "%")

p <- ggplot(dati %>% filter(!is.na(valore)),
            aes(x = valore, y = eta, fill = is_totale)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = fmt_pct(valore)),
            hjust = 1, nudge_x = -0.7, color = "white", fontface = "bold",
            size = 3.1, family = "Source Sans Pro") +
  scale_fill_manual(values = c("FALSE" = COL_BLU2, "TRUE" = COL_ROSSO),
                    guide = "none") +
  scale_x_continuous(limits = c(0, 58), expand = c(0.005, 0.005)) +
  scale_y_discrete(drop = FALSE) +
  theme_linechart() +
  theme(panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9.5, color = "#1C1C1C", hjust = 0)) +
  labs(
    title = "I giovani leggono più degli anziani",
    subtitle = paste0(
      "Persone di 6 anni e più che hanno letto almeno un libro nei 12 mesi, ",
      "per classe di età, Italia, ", anno, "."),
    caption = CAP_ISTAT,
    x = NULL, y = NULL
  )

ggsave("../output/lettura_libri_italia.png",
       plot = p, width = 8, height = 7.5, units = "in", dpi = 220, bg = "white")

cat("Grafico salvato in ../output/lettura_libri_italia.png\n")
