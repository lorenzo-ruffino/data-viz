# Modello Svedese — Grafico 02
# Salari medi annui reali (dollari a parità di potere d'acquisto, prezzi
# costanti) in Italia e Svezia, 1990-2024. Fonte: OCSE, dataset AV_AN_WAGE.

library(tidyverse)
library(showtext)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

palette_serie <- c("Italia" = COL_ROSSO, "Svezia" = COL_BLU)

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

CAP_OCSE <- "Elaborazione di Lorenzo Ruffino su dati OCSE"

# --- Dati -------------------------------------------------------------------

grezzi <- read_csv("../input/oecd_salari_annui_usdppp_costanti.csv",
                   col_types = cols(time = col_integer(),
                                    ITA = col_double(),
                                    SWE = col_double()))

dati <- grezzi %>%
  rename(anno = time, Italia = ITA, Svezia = SWE) %>%
  pivot_longer(-anno, names_to = "paese", values_to = "salario") %>%
  filter(!is.na(salario)) %>%
  mutate(paese = factor(paese, levels = c("Italia", "Svezia")))

ANNO_MIN <- min(dati$anno)
ANNO_MAX <- max(dati$anno)

# Livello italiano del primo anno: linea di riferimento orizzontale
ITA_1990 <- dati %>% filter(paese == "Italia", anno == ANNO_MIN) %>% pull(salario)

# Anno del sorpasso: primo anno in cui la Svezia supera l'Italia
larghi <- grezzi %>% rename(anno = time)
ANNO_SORPASSO <- larghi %>% filter(SWE > ITA) %>% slice_min(anno) %>% pull(anno)
SWE_SORPASSO  <- larghi %>% filter(anno == ANNO_SORPASSO) %>% pull(SWE)

# --- Etichette --------------------------------------------------------------

fmt_dollari <- function(x) paste0("$ ", format(round(x, -2), big.mark = ".",
                                               decimal.mark = ",",
                                               scientific = FALSE, trim = TRUE))

etichette_fine <- dati %>%
  filter(anno == ANNO_MAX) %>%
  mutate(testo = paste0(paese, "\n", fmt_dollari(salario)),
         # l'etichetta italiana scende un po' per non toccare la linea
         # tratteggiata del livello 1990
         y_label = if_else(paese == "Italia", salario - 900, salario))

# --- Grafico ----------------------------------------------------------------

p <- ggplot(dati, aes(x = anno, y = salario, colour = paese)) +
  # riferimento: livello italiano del 1990
  annotate("segment", x = ANNO_MIN, xend = ANNO_MAX - 0.3,
           y = ITA_1990, yend = ITA_1990,
           colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2001, y = ITA_1990 - 900, label = "Italia 1990",
           family = "Source Sans Pro", size = 3.2, colour = COL_GRIGIO,
           hjust = 0.5, vjust = 1) +
  geom_line(linewidth = 0.9) +
  # annotazione sul sorpasso
  geom_point(data = tibble(anno = ANNO_SORPASSO, salario = SWE_SORPASSO,
                           paese = factor("Svezia", levels = levels(dati$paese))),
             size = 2.4) +
  annotate("segment", x = ANNO_SORPASSO - 0.4, xend = ANNO_SORPASSO - 0.05,
           y = 57500, yend = 55400,
           colour = COL_GRIGIO, linewidth = 0.4) +
  annotate("text", x = ANNO_SORPASSO - 0.7, y = 57900,
           label = paste0("Sorpasso svedese, ", ANNO_SORPASSO),
           family = "Source Sans Pro", fontface = "bold", size = 3.4,
           colour = COL_NERO, hjust = 1, vjust = 0) +
  # etichette inline a fine linea
  geom_text(data = etichette_fine, aes(label = testo, y = y_label),
            hjust = 0, nudge_x = 0.45, size = 3.6, lineheight = 1.1,
            fontface = "bold", family = "Source Sans Pro") +
  scale_colour_manual(values = palette_serie) +
  scale_x_continuous(limits = c(ANNO_MIN, ANNO_MAX + 5),
                     breaks = seq(1990, 2020, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(34000, 64000),
                     breaks = seq(35000, 60000, 5000),
                     labels = function(x) paste0("$ ", format(x / 1000, big.mark = ".",
                                                              decimal.mark = ",",
                                                              trim = TRUE), "k"),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(
    title = "Il salario medio svedese era tre quarti di quello italiano, oggi lo supera",
    subtitle = "Salario medio annuo a parità di potere d'acquisto, dollari a prezzi costanti, Italia e Svezia, 1990-2024",
    caption = CAP_OCSE
  )

ggsave("../output/02_salari.png", p, width = 9, height = 6.5,
       dpi = 220, bg = "white")

write_csv(dati %>% arrange(paese, anno), "../output/02_salari.csv")

# --- Sanity check -----------------------------------------------------------

chiave <- larghi %>% filter(anno %in% c(1990, 1995, 2011, 2024))
cat("\nValori chiave (dollari PPP a prezzi costanti):\n")
print(as.data.frame(chiave))
cat("\nRapporto Svezia/Italia 1990: ",
    round(chiave$SWE[chiave$anno == 1990] / chiave$ITA[chiave$anno == 1990], 3), "\n")
cat("Anno del sorpasso: ", ANNO_SORPASSO, "\n")
cat("Italia 2024 vs 1990: ",
    round(chiave$ITA[chiave$anno == 2024] - chiave$ITA[chiave$anno == 1990]), " dollari\n")
cat("Svezia 2024 vs 1990: ",
    round(chiave$SWE[chiave$anno == 2024] - chiave$SWE[chiave$anno == 1990]), " dollari\n")
