# ---------------------------------------------------------------------------
# Grafici per l'articolo "Perché la Spagna cresce più dell'Italia".
#
# Legge i CSV già prodotti da 10_decomposizione_crescita.py e
# 11_demografia_pensioni.py in output/dati/ e produce quattro PNG:
#   01_decomposizione_crescita.png  contributi alla crescita del Pil 2000-2025
#   02_pil_procapite_pps.png        Pil pro capite in PPS, media UE = 100
#   03_saldo_naturale_migratorio.png componenti della crescita demografica ES
#   04_dipendenza_anziani.png       indice di dipendenza, storico + proiezioni
#
# Fonte dei dati: Eurostat (conti nazionali, demografia, Europop2023).
# ---------------------------------------------------------------------------

library(tidyverse)
library(showtext)

# --- Tema --------------------------------------------------------------------

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
COL_GRIGIO_SCURO <- "#5A5A5A"

palette_paesi <- c(
  "Italia"         = "#F12938",
  "Francia"        = "#A82DE3",
  "Germania"       = "#1C1C1C",
  "Spagna"         = "#F2A900",
  "Unione europea" = "#0478EA"
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
                                margin = margin(b = 0.1, unit = "cm")),
      plot.subtitle = element_text(size = 9, color = COL_NERO, hjust = 0,
                                   lineheight = 1.35,
                                   margin = margin(b = 0.25, t = 0.1, unit = "cm")),
      plot.caption = element_text(size = 9, color = COL_NERO, hjust = 1,
                                  margin = margin(t = 0.5, unit = "cm")),
      ...
    )
}

CAP <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

DATI <- "../output/dati"
OUT  <- "../output"

fmt_pp <- function(x, segno = TRUE) {
  s <- format(round(x), big.mark = ".", decimal.mark = ",")
  ifelse(segno & x > 0, paste0("+", s, "%"), paste0(s, "%"))
}

# ===========================================================================
# 01 — Decomposizione della crescita del Pil, 2000-2025
# ===========================================================================

dec <- read_csv(file.path(DATI, "crescita_decomposizione_periodi.csv"),
                show_col_types = FALSE) %>%
  filter(periodo == "2000-2025")

ordine_paesi <- dec %>%
  distinct(paese, crescita_pil_pp) %>%
  arrange(crescita_pil_pp) %>%
  pull(paese)

# l'ordine dei livelli è quello con cui le barre si impilano da sinistra:
# la produttività parte da zero in tutti i paesi, così è confrontabile a colpo
# d'occhio; le ore, unica componente negativa, restano a sinistra dello zero
comp_levels <- c("Produttività oraria", "Occupati su popolazione",
                 "Popolazione", "Ore per occupato")
comp_colori <- c(
  "Popolazione"             = COL_GIALLO,
  "Occupati su popolazione" = COL_BLU,
  "Ore per occupato"        = COL_GRIGIO,
  "Produttività oraria"     = COL_ROSSO
)

dec_plot <- dec %>%
  mutate(paese = factor(paese, levels = ordine_paesi),
         etichetta = factor(etichetta, levels = comp_levels))

totali <- dec %>%
  group_by(paese) %>%
  summarise(totale = first(crescita_pil_pp), .groups = "drop") %>%
  mutate(paese = factor(paese, levels = ordine_paesi),
         label = fmt_pp(totale))

p1 <- ggplot(dec_plot, aes(x = contributo_pp, y = paese, fill = etichetta)) +
  geom_col(width = 0.58, position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_point(data = totali,
             aes(x = totale, y = paese, shape = "Crescita totale del Pil"),
             inherit.aes = FALSE, size = 3.8, colour = COL_NERO) +
  geom_text(data = totali,
            aes(x = totale, y = paese, label = label),
            inherit.aes = FALSE, hjust = 0.5, vjust = 0, nudge_y = 0.38,
            family = "Source Sans Pro", fontface = "bold", size = 3.6,
            colour = COL_NERO) +
  scale_fill_manual(values = comp_colori) +
  scale_shape_manual(values = c("Crescita totale del Pil" = 18)) +
  scale_x_continuous(breaks = seq(-20, 60, 20),
                     labels = function(x) paste0(x, "%"),
                     limits = c(-15, 68), expand = c(0, 0)) +
  guides(fill = guide_legend(order = 1, nrow = 1),
         shape = guide_legend(order = 2)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 1, size = 10),
        axis.line.y = element_blank(),
        legend.box = "vertical",
        legend.justification = "left",
        legend.spacing.y = unit(0, "cm"),
        legend.margin = margin(b = 0.1, unit = "cm")) +
  labs(title = "La Spagna cresce molto grazie all'immigrazione",
       subtitle = "Contributi alla crescita del Pil reale in punti percentuali, 2000-2025",
       caption = CAP)

ggsave(file.path(OUT, "01_decomposizione_crescita.png"), p1,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# ===========================================================================
# 02 — Pil pro capite in standard di potere d'acquisto, media UE = 100
# ===========================================================================

pps <- read_csv(file.path(DATI, "crescita_pil_procapite_serie.csv"),
                show_col_types = FALSE) %>%
  filter(indicatore == "pil_pc_pps_eu27")

pps_ue <- pps %>% filter(paese == "Unione europea") %>%
  select(anno, ue = valore)

pps_plot <- pps %>%
  filter(paese %in% c("Spagna", "Italia")) %>%
  left_join(pps_ue, by = "anno") %>%
  mutate(indice = valore / ue * 100)

anno_max_2 <- max(pps_plot$anno)
picco_es <- pps_plot %>% filter(paese == "Spagna") %>% slice_max(indice, n = 1)

label_valori_2 <- pps_plot %>%
  filter((anno == min(anno)) | (anno == anno_max_2) |
           (paese == "Spagna" & anno == picco_es$anno)) %>%
  mutate(label = paste0(round(indice), "%"),
         hjust = case_when(anno == min(pps_plot$anno) ~ 0,
                           anno == anno_max_2 ~ 1,
                           TRUE ~ 0.5),
         vjust = if_else(paese == "Spagna" & anno == min(pps_plot$anno), 2.0, -1.3))

p2 <- ggplot(pps_plot, aes(x = anno, y = indice, colour = paese)) +
  geom_hline(yintercept = 100, colour = COL_GRIGIO, linewidth = 0.4,
             linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(data = label_valori_2, size = 1.8) +
  geom_text(data = label_valori_2,
            aes(label = label, hjust = hjust, vjust = vjust),
            family = "Source Sans Pro", fontface = "bold", size = 3.5,
            show.legend = FALSE) +
  geom_text(data = filter(pps_plot, anno == anno_max_2),
            aes(label = paese), hjust = 0, nudge_x = 0.4,
            family = "Source Sans Pro", fontface = "bold", size = 3.6,
            show.legend = FALSE) +
  annotate("text", x = 1995, y = 100, label = "media dell'Unione europea",
           hjust = 0, vjust = -0.8, family = "Source Sans Pro",
           size = 3.2, colour = COL_GRIGIO_SCURO) +
  scale_colour_manual(values = palette_paesi) +
  scale_x_continuous(breaks = seq(1995, 2025, 5),
                     limits = c(1995, anno_max_2 + 3), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(80, 130, 10),
                     labels = function(x) paste0(x, "%"),
                     limits = c(80, 133), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "La Spagna non ha recuperato terreno sull'Europa",
       subtitle = "Pil pro capite in standard di potere d'acquisto, media dell'Unione europea = 100, 1995-2025",
       caption = CAP)

ggsave(file.path(OUT, "02_pil_procapite_pps.png"), p2,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# ===========================================================================
# 03 — Saldo naturale e saldo migratorio della Spagna
# ===========================================================================

demo <- read_csv(file.path(DATI, "demo_crescita_pop_componenti.csv"),
                 show_col_types = FALSE) %>%
  filter(geo == "ES", anno <= 2025)

comp3_levels <- c("Saldo migratorio", "Saldo naturale")
comp3_colori <- c("Saldo migratorio" = COL_BLU, "Saldo naturale" = COL_ROSSO)

demo_comp <- demo %>%
  filter(indicatore %in% c("saldo_naturale", "saldo_migratorio")) %>%
  mutate(componente = factor(if_else(indicatore == "saldo_migratorio",
                                     "Saldo migratorio", "Saldo naturale"),
                             levels = comp3_levels))

demo_tot <- demo %>% filter(indicatore == "variazione_totale")

fmt_mila <- function(x) {
  ifelse(x == 0, "0",
         paste0(format(x / 1000, big.mark = ".", decimal.mark = ","), " mila"))
}

p3 <- ggplot() +
  geom_col(data = demo_comp,
           aes(x = anno, y = valore, fill = componente), width = 0.72) +
  geom_hline(yintercept = 0, colour = COL_NERO, linewidth = 0.3) +
  geom_line(data = demo_tot, aes(x = anno, y = valore),
            colour = COL_NERO, linewidth = 0.7) +
  annotate("text", x = 1990.3, y = 700000,
           label = "variazione totale\ndella popolazione",
           hjust = 0, vjust = 1, family = "Source Sans Pro", fontface = "bold",
           size = 3.3, lineheight = 1.2, colour = COL_NERO) +
  geom_curve(aes(x = 1993.4, y = 560000, xend = 1996.6, yend = 215000),
             curvature = 0.28, linewidth = 0.35, colour = COL_GRIGIO_SCURO,
             arrow = arrow(length = unit(0.16, "cm"), type = "closed")) +
  scale_fill_manual(values = comp3_colori) +
  scale_x_continuous(breaks = seq(1990, 2025, 5), expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(-200000, 800000, 200000),
                     labels = fmt_mila,
                     limits = c(-300000, 950000), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.justification = "left") +
  labs(title = "Senza immigrazione la Spagna perderebbe abitanti",
       subtitle = "Componenti della variazione annua della popolazione, Spagna, 1990-2025",
       caption = CAP)

ggsave(file.path(OUT, "03_saldo_naturale_migratorio.png"), p3,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# ===========================================================================
# 04 — Indice di dipendenza degli anziani, storico e proiezioni
# ===========================================================================

odr <- read_csv(file.path(DATI, "demo_olddep_storico_proiezioni.csv"),
                show_col_types = FALSE) %>%
  filter(paese %in% c("Spagna", "Italia"), anno <= 2050)

ultimo_storico <- odr %>% filter(tipo == "storico") %>% pull(anno) %>% max()

# la proiezione riparte dall'ultimo punto osservato, così la linea non si spezza
ponte <- odr %>% filter(tipo == "storico", anno == ultimo_storico) %>%
  mutate(tipo = "proiezione")

odr_plot <- bind_rows(odr, ponte) %>%
  mutate(serie = paste(paese, tipo))

anno_max_4 <- max(odr_plot$anno)
it_oggi <- odr %>% filter(paese == "Italia", anno == ultimo_storico) %>% pull(valore)
es_sorpasso <- odr %>%
  filter(paese == "Spagna", tipo == "proiezione", valore >= it_oggi) %>%
  slice_min(anno, n = 1)

label_valori_4 <- odr %>%
  filter(anno %in% c(ultimo_storico, anno_max_4)) %>%
  mutate(label = as.character(round(valore)),
         vjust = case_when(paese == "Italia" ~ -1.2,
                           anno == anno_max_4 ~ 3.4,
                           TRUE ~ 2.0),
         hjust = if_else(anno == anno_max_4, 1, 0.5))

p4 <- ggplot(odr_plot, aes(x = anno, y = valore, colour = paese,
                           linetype = tipo, group = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = es_sorpasso, aes(x = anno, y = valore),
             inherit.aes = FALSE, size = 2.4, colour = palette_paesi[["Spagna"]]) +
  geom_point(data = label_valori_4, aes(x = anno, y = valore, colour = paese),
             inherit.aes = FALSE, size = 1.8) +
  geom_text(data = label_valori_4,
            aes(x = anno, y = valore, label = label, colour = paese,
                vjust = vjust, hjust = hjust),
            inherit.aes = FALSE, family = "Source Sans Pro",
            fontface = "bold", size = 3.5, show.legend = FALSE) +
  annotate("text", x = es_sorpasso$anno + 1, y = es_sorpasso$valore - 1,
           label = paste0("nel ", es_sorpasso$anno, " la Spagna raggiunge\nil livello italiano di oggi"),
           hjust = 0, vjust = 1, family = "Source Sans Pro", size = 3.2,
           lineheight = 1.2, colour = COL_GRIGIO_SCURO) +
  geom_text(data = filter(odr_plot, anno == anno_max_4),
            aes(label = paese), hjust = 0, nudge_x = 0.6,
            family = "Source Sans Pro", fontface = "bold", size = 3.6,
            show.legend = FALSE) +
  scale_colour_manual(values = palette_paesi) +
  scale_linetype_manual(values = c("storico" = "solid", "proiezione" = "22")) +
  scale_x_continuous(breaks = seq(1990, 2050, 10),
                     limits = c(1990, anno_max_4 + 5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(20, 60, 10),
                     limits = c(18, 66), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "L'invecchiamento rinviato dalla Spagna arriva adesso",
       subtitle = "Persone con 65 anni e più ogni 100 in età da lavoro, dal 2026 proiezioni, 1990-2050",
       caption = CAP)

ggsave(file.path(OUT, "04_dipendenza_anziani.png"), p4,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Controlli ---------------------------------------------------------------

cat("\n[01] crescita totale 2000-2025 e quota da popolazione + occupati:\n")
dec %>%
  group_by(paese) %>%
  summarise(totale = first(crescita_pil_pp),
            persone = sum(contributo_pp[componente %in% c("popolazione", "tasso_occupazione")]),
            produttivita = sum(contributo_pp[componente == "produttivita_oraria"]),
            .groups = "drop") %>%
  mutate(quota_persone = round(persone / totale * 100)) %>%
  arrange(desc(totale)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n[02] Pil pro capite in PPS, media UE = 100:\n")
pps_plot %>% filter(anno %in% c(1995, 2006, 2013, 2019, 2025)) %>%
  select(paese, anno, indice) %>%
  mutate(indice = round(indice, 1)) %>%
  pivot_wider(names_from = paese, values_from = indice) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n[03] Spagna, saldi cumulati 1995-2025 (milioni):\n")
demo %>% filter(anno >= 1995, indicatore != "popolazione_1gen") %>%
  group_by(indicatore) %>% summarise(mln = round(sum(valore) / 1e6, 2), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n[04] indice di dipendenza:\n")
odr %>% filter(anno %in% c(2000, 2009, ultimo_storico, 2040, 2050)) %>%
  select(paese, anno, valore) %>%
  pivot_wider(names_from = paese, values_from = valore) %>%
  as.data.frame() %>% print(row.names = FALSE)
cat("     sorpasso Spagna sul livello italiano di oggi (", it_oggi, "%): ",
    es_sorpasso$anno, "\n", sep = "")
cat("\nPNG scritti in output/\n")
