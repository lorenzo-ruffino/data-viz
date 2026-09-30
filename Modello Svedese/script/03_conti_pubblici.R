# =============================================================================
# Modello Svedese — Grafico 3
# "Lo Stato svedese è dimagrito, quello italiano no"
# Conti pubblici di Svezia e Italia in percentuale del PIL, 1995-2025
# Fonte: Eurostat (gov_10a_main, gov_10a_taxag, gov_10dd_edpt1)
# =============================================================================

library(tidyverse)
library(showtext)
library(ggrepel)

# --- Tema --------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

palette_paesi <- c(
  "Italia"         = "#F12938",  # rosso
  "Francia"        = "#A82DE3",  # viola
  "Germania"       = "#1C1C1C",  # nero
  "Spagna"         = "#F2A900",  # giallo/ocra
  "Unione europea" = "#0478EA"   # blu
)

COL_NERO   <- "#1C1C1C"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

# Italia = rosso della palette, Svezia = blu (coerenza con gli altri
# grafici della serie sul modello svedese)
COL_ITALIA <- "#F12938"
COL_SVEZIA <- "#0478EA"

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

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

DIR_INPUT  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Modello Svedese/input"
DIR_OUTPUT <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Modello Svedese/output"

# --- Dati --------------------------------------------------------------------

leggi <- function(file, pannello) {
  read_csv(file.path(DIR_INPUT, file),
           col_types = cols(time = col_integer(), .default = col_double())) %>%
    select(anno = time, Italia = IT, Svezia = SE) %>%
    pivot_longer(c(Italia, Svezia), names_to = "paese", values_to = "valore") %>%
    mutate(pannello = pannello)
}

pannelli <- c("Spesa pubblica", "Pressione fiscale",
              "Debito pubblico", "Spesa per interessi")

dati <- bind_rows(
  leggi("spesa_pubblica_pc_pil.csv",   "Spesa pubblica"),
  leggi("pressione_fiscale_pc_pil.csv","Pressione fiscale"),
  leggi("debito_pc_pil.csv",           "Debito pubblico"),
  leggi("interessi_pc_pil.csv",        "Spesa per interessi")
) %>%
  filter(anno >= 1995, anno <= 2025, !is.na(valore)) %>%
  mutate(pannello = factor(pannello, levels = pannelli),
         paese    = factor(paese, levels = c("Italia", "Svezia")))

ANNO_MIN <- min(dati$anno)
ANNO_MAX <- max(dati$anno)

# --- Formattazione numeri ----------------------------------------------------

# Un decimale solo dove serve (spesa per interessi), altrimenti interi
fmt_valore <- function(valore, pannello) {
  ifelse(pannello == "Spesa per interessi",
         paste0(formatC(valore, format = "f", digits = 1, decimal.mark = ",")),
         paste0(formatC(round(valore), format = "d")))
}

fmt_asse <- function(x) {
  ifelse(is.na(x), NA_character_,
         ifelse(x %% 1 == 0,
                paste0(formatC(x, format = "d"), "%"),
                paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")))
}

# --- Etichette a fine linea --------------------------------------------------

etichette <- dati %>%
  filter(anno == ANNO_MAX) %>%
  mutate(label = paste0(paese, " ", fmt_valore(valore, as.character(pannello))))

# --- Annotazioni dei sorpassi ------------------------------------------------

# Anno in cui l'Italia supera la Svezia in ciascuna delle due grandezze
sorpassi <- tibble(
  pannello = factor(c("Spesa pubblica", "Pressione fiscale"), levels = pannelli),
  anno     = c(2020, 2012),
  valore   = c(58.6, 45.4),
  label    = c("2020", "2012")
)

righe_sorpasso <- sorpassi %>% select(pannello, anno)

# Headroom per non far uscire etichette e annotazioni dai pannelli
limiti <- dati %>%
  group_by(pannello) %>%
  summarise(min_v = min(valore), max_v = max(valore), .groups = "drop") %>%
  mutate(range_v = max_v - min_v,
         basso = min_v - range_v * 0.12,
         alto  = max_v + range_v * 0.16) %>%
  select(pannello, basso, alto) %>%
  pivot_longer(c(basso, alto), values_to = "valore") %>%
  mutate(anno = ANNO_MIN, paese = factor("Italia", levels = c("Italia", "Svezia")))

# --- Grafico -----------------------------------------------------------------

p <- ggplot(dati, aes(x = anno, y = valore, colour = paese)) +
  geom_blank(data = limiti) +
  geom_vline(data = righe_sorpasso, aes(xintercept = anno),
             colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(data = filter(dati, anno == ANNO_MAX), size = 1.8) +
  geom_text(data = sorpassi, aes(x = anno, y = valore, label = label),
            inherit.aes = FALSE, family = "Source Sans Pro", size = 3.0,
            colour = COL_GRIGIO_SCURO, hjust = 1.15) +
  geom_text_repel(data = etichette, aes(label = label),
                  family = "Source Sans Pro", fontface = "bold", size = 3.4,
                  hjust = 0, nudge_x = 0.8, direction = "y",
                  segment.colour = COL_GRIGIO, min.segment.length = 0,
                  box.padding = 0.15, point.padding = 0.1, seed = 1) +
  facet_wrap(~ pannello, nrow = 2, scales = "free_y") +
  scale_colour_manual(values = c("Italia" = COL_ITALIA, "Svezia" = COL_SVEZIA)) +
  scale_x_continuous(limits = c(ANNO_MIN, ANNO_MAX + 11),
                     breaks = c(1995, 2005, 2015, 2025),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = fmt_asse,
                     expand = c(0.02, 0.02)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(
    legend.position = "none",
    strip.text = element_text(family = "Source Sans Pro", face = "bold", size = 11,
                              colour = COL_NERO, hjust = 0,
                              margin = margin(b = 0.15, t = 0.3, unit = "cm")),
    panel.spacing.x = unit(1.0, "cm"),
    panel.spacing.y = unit(1.0, "cm")
  ) +
  labs(
    title    = "Lo Stato svedese si è ridotto, quello italiano no",
    subtitle = "Conti pubblici di Svezia e Italia in percentuale del PIL, 1995-2025",
    caption  = CAP_EUROSTAT
  )

ggsave(file.path(DIR_OUTPUT, "03_conti_pubblici.png"), p,
       width = 10, height = 7.5, dpi = 220, bg = "white")

# --- Dato pulito -------------------------------------------------------------

dati %>%
  pivot_wider(names_from = paese, values_from = valore) %>%
  arrange(pannello, anno) %>%
  write_csv(file.path(DIR_OUTPUT, "03_conti_pubblici.csv"))

# --- Sanity check ------------------------------------------------------------

check <- dati %>%
  filter(anno %in% c(ANNO_MIN, ANNO_MAX)) %>%
  arrange(pannello, paese, anno)

cat("\n--- Valori 1995 e 2025 (in % del PIL) ---\n")
print(as.data.frame(check), row.names = FALSE)

cat("\nSorpassi Italia > Svezia:\n")
dati %>%
  pivot_wider(names_from = paese, values_from = valore) %>%
  group_by(pannello) %>%
  filter(Italia > Svezia, lag(Italia, default = -Inf) <= lag(Svezia, default = Inf)) %>%
  ungroup() %>%
  select(pannello, anno, Italia, Svezia) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
