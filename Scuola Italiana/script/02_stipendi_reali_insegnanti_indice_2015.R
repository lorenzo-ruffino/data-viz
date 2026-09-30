# Scuola Italiana — 02
# Indice degli stipendi reali degli insegnanti, 2015 = 100, 2015-2024.
# Fonte: OCSE, DSD_EAG_SAL_TREND (oecd_EAG_SAL_TREND_indici_stipendi_2000_2025.csv).
# Misura scelta: MEASURE = SAL_STA (stipendi previsti dal contratto, non effettivi), UNIT_MEASURE = IX
# (indice in termini reali, TRANSFORMATION = MIX_100, BASE_PER = 2015),
# EDUCATION_LEV = ISCED11_1 (primaria), PERS_EXP_LEV = EXP15 (docente con 15 anni
# di esperienza), PERS_QUAL_LEV = TYP_EXP, INST_TYPE_EDU = INST_EDU_PUB.
# La media OCSE è l'area REF_AREA = OECD_REP (media dei paesi che riportano);
# non ha il dato 2025, quindi la serie si ferma al 2024.

suppressMessages({
  library(tidyverse)
  library(showtext)
  library(ggrepel)
})

BASE   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Scuola Italiana"
INPUT  <- file.path(BASE, "input")
OUTPUT <- file.path(BASE, "output")

FIG_W <- 9.5
FIG_H <- 6.5
wrap_titolo <- function(x) str_wrap(x, width = floor(FIG_W * 9.2))
wrap_sub    <- function(x) str_wrap(x, width = floor(FIG_W * 12.3))

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_GRIGIO <- "#9A9A9A"

palette_paesi <- c(
  "Italia"         = "#F12938",
  "Francia"        = "#A82DE3",
  "Germania"       = "#1C1C1C",
  "Spagna"         = "#F2A900",
  "Media OCSE"     = "#0478EA"
)

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
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = COL_NERO, hjust = 0,
                                lineheight = 1.2,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP <- "Elaborazione di Lorenzo Ruffino su dati OCSE"

# --- Dati -------------------------------------------------------------------

nomi <- c("ITA" = "Italia", "OECD_REP" = "Media OCSE", "FRA" = "Francia",
          "DEU" = "Germania", "ESP" = "Spagna")

ANNO_MIN <- 2015
ANNO_MAX <- 2024

dati <- read_csv(file.path(INPUT, "oecd_EAG_SAL_TREND_indici_stipendi_2000_2025.csv"),
                 show_col_types = FALSE) %>%
  filter(MEASURE == "SAL_STA",
         UNIT_MEASURE == "IX",
         TRANSFORMATION == "MIX_100",
         EDUCATION_LEV == "ISCED11_1",
         PERS_TYPE == "TE",
         PERS_QUAL_LEV == "TYP_EXP",
         PERS_EXP_LEV == "EXP15",
         INST_TYPE_EDU == "INST_EDU_PUB",
         REF_AREA %in% names(nomi),
         TIME_PERIOD >= ANNO_MIN, TIME_PERIOD <= ANNO_MAX,
         !is.na(OBS_VALUE)) %>%
  transmute(paese = factor(nomi[REF_AREA], levels = names(palette_paesi)),
            anno = TIME_PERIOD,
            indice = OBS_VALUE)

fmt_it <- function(v) format(round(v, 1), nsmall = 1, decimal.mark = ",")

etichette <- dati %>%
  filter(anno == ANNO_MAX) %>%
  mutate(testo = paste0(paese, "  ", fmt_it(indice)))

p <- ggplot(dati, aes(x = anno, y = indice, colour = paese)) +
  geom_hline(yintercept = 100, colour = COL_GRIGIO,
             linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  geom_text_repel(data = etichette, aes(label = testo),
                  hjust = 0, nudge_x = 0.25, direction = "y",
                  size = 3.4, fontface = "bold", family = "Source Sans Pro",
                  segment.colour = "#9A9A9A", min.segment.length = 0,
                  box.padding = 0.2, seed = 1) +
  scale_colour_manual(values = palette_paesi) +
  scale_x_continuous(limits = c(ANNO_MIN, ANNO_MAX + 3.4),
                     breaks = seq(ANNO_MIN, ANNO_MAX, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(88, 110), breaks = seq(88, 108, 4),
                     labels = function(x) format(x, decimal.mark = ","),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = wrap_titolo(
         "L'Italia è il paese dove gli stipendi degli insegnanti calano di più"),
       subtitle = wrap_sub(paste0(
         "Indice degli stipendi previsti dal contratto al netto dell'inflazione per un docente della ",
         "scuola primaria con 15 anni di esperienza, 2015 = 100, Italia e principali paesi, 2015-2024")),
       caption = CAP)

ggsave(file.path(OUTPUT, "02_stipendi_reali_insegnanti_indice_2015.png"), p,
       width = FIG_W, height = FIG_H, dpi = 220, bg = "white")

write_csv(dati, file.path(OUTPUT, "02_stipendi_reali_insegnanti_indice_2015.csv"))

cat("--- Indice 2024 ---\n")
print(dati %>% filter(anno == ANNO_MAX) %>% arrange(desc(indice)) %>% as.data.frame())
cat("--- Italia, minimo e ultimo ---\n")
print(dati %>% filter(paese == "Italia") %>% as.data.frame())
