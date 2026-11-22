# script/governi_america_latina.R — eseguito da `cd script && Rscript governi_america_latina.R`
#
# Orientamento politico del presidente in carica in 18 paesi dell'America
# Latina dal 2000 a oggi. Il dato e' costruito a mano (input/presidenti.csv):
# una riga per presidenza, con la data di insediamento; la presidenza dura fino
# all'insediamento successivo. Tre categorie: sinistra (comprende il
# centro-sinistra), destra (comprende il centro-destra), centro per i governi
# senza un orientamento netto o di transizione.
# Restano solo le democrazie: esclusi Cuba (senza elezioni competitive),
# Venezuela e Nicaragua (regimi autoritari) e Haiti.

library(tidyverse)
library(showtext)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO_CHIARO <- "#C8C8C8"

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "top",
      legend.justification = "left",
      axis.line = element_blank(),
      axis.text = element_text(size = 9, color = COL_NERO),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 9, color = COL_NERO, hjust = 0,
                                 margin = margin(r = 10)),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(0.2, "cm"),
      plot.margin = unit(c(0.4, 0.5, 0.4, 0.4), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(family = "Source Sans Pro SemiBold",
                                size = 14, color = COL_NERO, hjust = 0,
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.4, unit = "cm")),
      ...
    )
}

CAP <- "Elaborazione di Lorenzo Ruffino"

output_dir <- file.path("..", "output")
DA   <- as.Date("2000-01-01")
OGGI <- as.Date("2026-10-03")

# ── Dati ─────────────────────────────────────────────────────────────────────
ESCLUSI <- c("Venezuela", "Nicaragua")

pres <- read_csv(file.path("..", "input", "presidenti.csv"), show_col_types = FALSE) %>%
  filter(!paese %in% ESCLUSI) %>%
  arrange(paese, inizio) %>%
  group_by(paese) %>%
  mutate(fine = lead(inizio, default = OGGI)) %>%
  ungroup() %>%
  filter(fine > DA) %>%
  mutate(da = pmax(inizio, DA),
         orientamento = factor(orientamento, levels = c("sinistra", "centro", "destra"),
                               labels = c("Sinistra e centro-sinistra", "Centro",
                                          "Destra e centro-destra")))

paesi <- sort(unique(pres$paese), decreasing = TRUE)   # A in alto
pres <- pres %>% mutate(y = match(paese, paesi))

write_csv(select(pres, paese, presidente, inizio, fine, orientamento),
          file.path(output_dir, "governi_america_latina.csv"))

# ── Conteggi per il testo ────────────────────────────────────────────────────
conta <- function(giorno) {
  pres %>% filter(inizio <= giorno, fine > giorno | fine == OGGI) %>% count(orientamento) %>%
    mutate(giorno = giorno)
}
giorni <- c(as.Date(paste0(2000:2026, "-07-01")), OGGI)
serie <- map_dfr(giorni, conta) %>%
  pivot_wider(names_from = orientamento, values_from = n, values_fill = 0)
print(serie, n = 40)

# ── Grafico ──────────────────────────────────────────────────────────────────
colori <- c("Sinistra e centro-sinistra" = COL_ROSSO, "Centro" = COL_GRIGIO_CHIARO,
            "Destra e centro-destra" = COL_BLU)

anni <- as.Date(paste0(seq(2000, 2025, 5), "-01-01"))

p <- ggplot(pres) +
  geom_rect(aes(xmin = da, xmax = fine, ymin = y - 0.42, ymax = y + 0.42,
                fill = orientamento), colour = "white", linewidth = 0.35) +
  scale_fill_manual(values = colori) +
  scale_x_date(breaks = anni, labels = format(anni, "%Y"),
               limits = c(DA, OGGI), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq_along(paesi), labels = paesi,
                     expand = c(0, 0.3)) +
  labs(title = "In America Latina destra e sinistra arrivano a ondate",
       subtitle = "Orientamento del presidente in carica, 16 democrazie dell'America Latina, 2000-2026",
       caption = CAP) +
  guides(fill = guide_legend(keywidth = unit(0.5, "cm"), keyheight = unit(0.3, "cm"))) +
  theme_linechart(axis.text.y = element_text(hjust = 1, size = 9, color = COL_NERO))

ggsave(file.path(output_dir, "governi_america_latina.png"),
       plot = p, width = 9, height = 7, dpi = 220, bg = "white")
