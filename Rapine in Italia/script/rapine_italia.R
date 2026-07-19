library(tidyverse)
library(showtext)
library(ggrepel)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"

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

fmt_k <- function(x) ifelse(x == 0, "0",
                            paste0(formatC(x / 1000, format = "d", big.mark = "."), "k"))

# --- Dati -------------------------------------------------------------------

raw <- read_csv("../input/istat_73_67_DF_DCCV_DELITTIPS_1.csv", show_col_types = FALSE)

etichette_tipo <- c(
  ROBBER    = "Totale",
  STREETROB = "In pubblica via",
  SHOPROB   = "In esercizi commerciali",
  HOUSEROB  = "In abitazione",
  BANKROB   = "In banca",
  POSTROB   = "In uffici postali"
)

dati <- raw %>%
  transmute(anno = as.integer(TIME_PERIOD),
            codice = TYPE_CRIME,
            tipologia = etichette_tipo[TYPE_CRIME],
            rapine = OBS_VALUE)

dati %>%
  select(anno, tipologia, rapine) %>%
  arrange(tipologia, anno) %>%
  write_csv("../output/rapine_italia.csv")

# --- Grafico 1: totale ------------------------------------------------------

tot <- dati %>% filter(codice == "ROBBER")

label_tot <- tot %>%
  filter(anno %in% c(2007, 2020, 2024)) %>%
  mutate(vj = ifelse(anno == 2020, 2.1, -1.2),
         hj = case_when(anno == 2007 ~ 0.2,
                        anno == 2024 ~ 1,
                        TRUE         ~ 0.5))

p1 <- ggplot(tot, aes(anno, rapine)) +
  geom_line(color = COL_BLU, linewidth = 1.0) +
  geom_point(color = COL_BLU, size = 2.4) +
  geom_text(data = label_tot,
            aes(label = formatC(rapine, format = "d", big.mark = "."),
                vjust = vj, hjust = hj),
            family = "Source Sans Pro", fontface = "bold",
            size = 3.6, color = COL_NERO) +
  scale_x_continuous(breaks = seq(2006, 2024, 2),
                     limits = c(2005.7, 2024.3), expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 50000, 10000), labels = fmt_k,
                     limits = c(0, 56000), expand = c(0.01, 0.01)) +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "Rapine quasi dimezzate dal 2007, in risalita dal 2020",
       subtitle = "Numero di rapine denunciate dalle forze di polizia all'autorità giudiziaria, Italia, 2006-2024",
       caption = CAP_ISTAT)

ggsave("../output/01_rapine_totale.png", p1,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- Grafico 2: per tipologia -----------------------------------------------

livelli_tipi <- c("In pubblica via", "In esercizi commerciali",
                  "In abitazione", "In banca", "In uffici postali")

tipi <- dati %>%
  filter(codice != "ROBBER") %>%
  mutate(tipologia = factor(tipologia, levels = livelli_tipi))

pal_tipi <- c(
  "In pubblica via"         = COL_BLU,
  "In esercizi commerciali" = COL_GIALLO,
  "In abitazione"           = COL_VIOLA,
  "In banca"                = COL_ROSSO,
  "In uffici postali"       = COL_GRIGIO
)

label_tipi <- tipi %>%
  filter(anno == 2024) %>%
  mutate(label = ifelse(tipologia == "In esercizi commerciali",
                        "In esercizi\ncommerciali", as.character(tipologia)))

p2 <- ggplot(tipi, aes(anno, rapine, color = tipologia)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_text_repel(data = label_tipi,
                  aes(label = label),
                  hjust = 0, nudge_x = 0.3, direction = "y",
                  size = 3.4, fontface = "bold", family = "Source Sans Pro",
                  segment.colour = "#9A9A9A", min.segment.length = 0,
                  box.padding = 0.15, seed = 1, lineheight = 0.95) +
  scale_color_manual(values = pal_tipi) +
  scale_x_continuous(breaks = seq(2006, 2024, 2),
                     limits = c(2006, 2026.6), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 25000, 5000), labels = fmt_k,
                     limits = c(0, 27000), expand = c(0.01, 0.01)) +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "Le rapine in banca sono praticamente scomparse",
       subtitle = "Numero di rapine denunciate dalle forze di polizia all'autorità giudiziaria, per luogo,\nprincipali tipologie, Italia, 2006-2024",
       caption = CAP_ISTAT)

ggsave("../output/02_rapine_per_tipologia.png", p2,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")
