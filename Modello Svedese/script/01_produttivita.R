# Modello Svedese — Grafico 01
# PIL per ora lavorata in dollari a parità di potere d'acquisto, prezzi
# costanti, in Italia e Svezia, 1995-2024. Fonte: OCSE, dataset PDB_LV.

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

grezzi <- read_csv("../input/oecd_pil_per_ora_usdppp_costanti.csv",
                   col_types = cols(time = col_integer(),
                                    ITA = col_double(),
                                    SWE = col_double())) %>%
  rename(anno = time) %>%
  filter(anno >= 1995, !is.na(ITA), !is.na(SWE))

dati <- grezzi %>%
  rename(Italia = ITA, Svezia = SWE) %>%
  pivot_longer(-anno, names_to = "paese", values_to = "prod") %>%
  mutate(paese = factor(paese, levels = c("Italia", "Svezia")))

ANNO_MIN <- min(dati$anno)
ANNO_MAX <- max(dati$anno)

# Anno del sorpasso: primo anno in cui la Svezia supera l'Italia
ANNO_SORPASSO <- grezzi %>% filter(SWE > ITA) %>% slice_min(anno) %>% pull(anno)
SWE_SORPASSO  <- grezzi %>% filter(anno == ANNO_SORPASSO) %>% pull(SWE)

# Crescita cumulata dal primo anno della serie
crescita <- dati %>%
  group_by(paese) %>%
  summarise(inizio = prod[anno == ANNO_MIN],
            fine   = prod[anno == ANNO_MAX],
            var_pc = (fine / inizio - 1) * 100,
            .groups = "drop")

# --- Etichette --------------------------------------------------------------

fmt_dollari <- function(x) paste0("$ ", format(round(x), big.mark = ".",
                                               decimal.mark = ",",
                                               scientific = FALSE, trim = TRUE))

etichette_fine <- dati %>%
  filter(anno == ANNO_MAX) %>%
  left_join(crescita, by = "paese") %>%
  mutate(testo = paste0(paese, "\n", fmt_dollari(prod), "\n",
                        "+", round(var_pc), "% dal ", ANNO_MIN))

# --- Grafico ----------------------------------------------------------------

p <- ggplot(dati, aes(x = anno, y = prod, colour = paese)) +
  geom_line(linewidth = 0.9) +
  # annotazione sul sorpasso
  geom_point(data = tibble(anno = ANNO_SORPASSO, prod = SWE_SORPASSO,
                           paese = factor("Svezia", levels = levels(dati$paese))),
             size = 2.4) +
  annotate("segment", x = ANNO_SORPASSO + 1.0, xend = ANNO_SORPASSO + 0.15,
           y = 63.4, yend = SWE_SORPASSO - 1.1,
           colour = COL_GRIGIO, linewidth = 0.4) +
  annotate("text", x = ANNO_SORPASSO + 1.2, y = 62.9,
           label = paste0("Sorpasso svedese, ", ANNO_SORPASSO),
           family = "Source Sans Pro", fontface = "bold", size = 3.4,
           colour = COL_NERO, hjust = 0, vjust = 1) +
  # etichette inline a fine linea: nome, livello e crescita cumulata
  geom_text(data = etichette_fine, aes(label = testo),
            hjust = 0, nudge_x = 0.6, size = 3.6, lineheight = 1.1,
            fontface = "bold", family = "Source Sans Pro") +
  scale_colour_manual(values = palette_serie) +
  scale_x_continuous(limits = c(ANNO_MIN, ANNO_MAX + 7),
                     breaks = seq(1995, 2020, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(52, 86),
                     breaks = seq(55, 85, 5),
                     labels = function(x) paste0("$ ", format(x, trim = TRUE)),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(
    title = "La produttività svedese è cresciuta di metà in trent'anni, quella italiana è ferma",
    subtitle = paste0("PIL per ora lavorata a parità di potere d'acquisto, dollari a prezzi costanti, Italia e Svezia, ",
                      ANNO_MIN, "-", ANNO_MAX),
    caption = CAP_OCSE
  )

ggsave("../output/01_produttivita.png", p, width = 10, height = 6.5,
       dpi = 220, bg = "white")

write_csv(dati %>% arrange(paese, anno), "../output/01_produttivita.csv")

# --- Sanity check -----------------------------------------------------------

chiave <- grezzi %>% filter(anno %in% c(ANNO_MIN, ANNO_SORPASSO, ANNO_MAX))
cat("\nValori chiave (dollari PPP per ora, prezzi costanti):\n")
print(as.data.frame(chiave))
cat("\nAnno del sorpasso: ", ANNO_SORPASSO, "\n")
cat("Crescita cumulata ", ANNO_MIN, "-", ANNO_MAX, ":\n", sep = "")
print(as.data.frame(crescita %>% mutate(across(where(is.numeric), ~round(.x, 1)))))
