# Grafici per l'articolo "Perché bisogna costruire più data center"
# 01 - quota mondiale della capacità di calcolo per l'AI, per paese
# 02 - consumo elettrico dei data center negli Stati Uniti, 2014-2028
# 03 - acqua consumata ogni giorno negli Stati Uniti, a confronto

library(tidyverse)
library(showtext)

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

IN  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Data center/input"
OUT <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Data center/output"

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

fmt_it <- function(x, dec = 0)
  format(round(x, dec), big.mark = ".", decimal.mark = ",", nsmall = dec, trim = TRUE)

fmt_pct <- function(v, dec = 1)
  paste0(formatC(v, format = "f", digits = dec, decimal.mark = ","), "%")

# ============================================================================
# GRAFICO 01 - Chi ha la capacità di calcolo per l'intelligenza artificiale
# ============================================================================
# Fonte: Epoch AI, GPU Clusters (CC-BY), snapshot dei sistemi operativi al
# 31 maggio 2025, metrica FLOP/s a 16 bit. I sistemi senza paese attribuito
# (2,1% del totale, operatori multinazionali) escono dal denominatore: è
# l'ipotesi implicita del rapporto Carnegie, che sugli stessi dati dà Stati
# Uniti a tre quarti, Cina al 14 per cento e Unione europea sotto il 5.

paesi <- read_csv(file.path(IN, "capacita_calcolo_ai_per_paese.csv"),
                  show_col_types = FALSE)
ue27  <- read_csv(file.path(IN, "capacita_calcolo_ai_ue27.csv"),
                  show_col_types = FALSE)

tot_attribuito <- paesi %>%
  filter(paese != "Non attribuito") %>%
  summarise(t = sum(capacita_flops_16bit)) %>%
  pull(t)

quota_italia <- ue27 %>% filter(paese == "Italia") %>%
  summarise(q = capacita_flops_16bit / tot_attribuito * 100) %>% pull(q)

# L'Italia resta dentro l'aggregato europeo e non si evidenzia: il suo 1 per
# cento è per il 97 per cento hardware pre-Hopper di supercomputer scientifici
# e industriali (Eni HPC6, Leonardo del Cineca), non calcolo per l'AI. Il
# Carnegie la elenca infatti tra i paesi senza concentrazioni rilevanti.

calcolo <- paesi %>%
  filter(paese != "Non attribuito") %>%
  mutate(quota = capacita_flops_16bit / tot_attribuito * 100) %>%
  # accorpo le code sotto mezzo punto per non affollare l'asse
  mutate(gruppo = ifelse(quota < 0.5, "Altri paesi", paese)) %>%
  group_by(gruppo) %>%
  summarise(quota = sum(quota), .groups = "drop") %>%
  rename(paese = gruppo) %>%
  arrange(quota) %>%
  mutate(paese = factor(paese, levels = paese))

etichette <- calcolo %>%
  mutate(lab = ifelse(quota >= 10, fmt_pct(quota, 0), fmt_pct(quota, 1)))

g1 <- ggplot(calcolo, aes(quota, paese)) +
  geom_col(width = 0.72, fill = COL_BLU) +
  geom_text(data = etichette, aes(label = lab),
            hjust = -0.18, fontface = "bold", family = "Source Sans Pro",
            size = 3.3, colour = COL_NERO) +
  scale_x_continuous(limits = c(0, 84), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) +
  coord_cartesian(clip = "off") +
  labs(title = "Gli Stati Uniti hanno tre quarti del calcolo mondiale",
       subtitle = "Quota della capacità dei cluster di calcolo per l'intelligenza artificiale nel mondo, maggio 2025",
       caption = "Elaborazione di Lorenzo Ruffino su dati Epoch AI") +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 10),
        axis.text.x = element_blank(),
        axis.line = element_blank())

ggsave(file.path(OUT, "01_capacita_calcolo_per_paese.png"), g1,
       width = 9, height = 6.5, dpi = 220, bg = "white")

write_csv(calcolo, file.path(OUT, "capacita_calcolo_per_paese.csv"))

cat("G1 | Stati Uniti:", fmt_pct(calcolo$quota[calcolo$paese == "Stati Uniti"]),
    "| Cina:", fmt_pct(calcolo$quota[calcolo$paese == "Cina"]),
    "| UE:", fmt_pct(calcolo$quota[calcolo$paese == "Unione europea"]),
    "| Italia:", fmt_pct(quota_italia), "\n")

# ============================================================================
# GRAFICO 02 - Quanta elettricità consumano i data center americani
# ============================================================================
# Fonte: Lawrence Berkeley National Laboratory, "2024 United States Data Center
# Energy Usage Report" (dicembre 2024), figura ES-1. Lo storico 2014-2023 è una
# serie unica; dal 2024 il rapporto non dà uno scenario centrale ma solo la
# forbice tra lo scenario basso e quello alto. I consumi delle criptovalute
# sono esclusi: il rapporto li stima a parte.

energia <- read_csv(file.path(IN, "lbnl_2024_consumo_data_center_usa.csv"),
                    show_col_types = FALSE)

storico    <- energia %>% filter(!is.na(twh_centrale))
proiezione <- energia %>% filter(anno >= 2023)

ann_storico <- storico %>% filter(anno %in% c(2014, 2023))

# Termine di paragone per il lettore italiano: i consumi elettrici finali
# dell'Italia in un anno, al netto delle perdite di rete (Terna, 2024). È la
# grandezza omogenea, perché anche il dato LBNL è elettricità misurata al
# contatore degli impianti. La richiesta sulla rete, 311,3 TWh nel 2025,
# comprende invece le perdite e gonfierebbe il confronto.
ITALIA_TWH <- 292.7

g2 <- ggplot() +
  geom_hline(yintercept = ITALIA_TWH, colour = COL_GRIGIO_SCURO,
             linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2014.1, y = ITALIA_TWH,
           label = "consumo elettrico di tutta l'Italia in un anno",
           hjust = 0, vjust = -0.75, family = "Source Sans Pro",
           size = 3.1, colour = COL_GRIGIO_SCURO) +
  geom_ribbon(data = proiezione, aes(anno, ymin = twh_basso, ymax = twh_alto),
              fill = COL_ROSSO, alpha = 0.16) +
  geom_line(data = proiezione, aes(anno, twh_basso),
            colour = COL_ROSSO, linewidth = 0.5, linetype = "dashed") +
  geom_line(data = proiezione, aes(anno, twh_alto),
            colour = COL_ROSSO, linewidth = 0.5, linetype = "dashed") +
  geom_line(data = storico, aes(anno, twh_centrale),
            colour = COL_ROSSO, linewidth = 1.0) +
  geom_point(data = ann_storico, aes(anno, twh_centrale),
             colour = COL_ROSSO, size = 2.4) +
  geom_text(data = ann_storico,
            aes(anno, twh_centrale,
                label = paste0(fmt_it(twh_centrale), " TWh"),
                hjust = ifelse(anno == 2014, -0.18, 1.1)),
            vjust = -1.2, fontface = "bold", family = "Source Sans Pro",
            size = 3.4, colour = COL_NERO) +
  annotate("text", x = 2028.3, y = 580.8, label = "581 TWh",
           hjust = 0, vjust = 0.4, fontface = "bold",
           family = "Source Sans Pro", size = 3.4, colour = COL_NERO) +
  annotate("text", x = 2028.3, y = 323.7, label = "324 TWh",
           hjust = 0, vjust = 0.4, fontface = "bold",
           family = "Source Sans Pro", size = 3.4, colour = COL_NERO) +
  annotate("text", x = 2025.5, y = 140,
           label = "scenari di proiezione", hjust = 0, vjust = 0.5,
           family = "Source Sans Pro", size = 3.1, colour = COL_GRIGIO_SCURO) +
  scale_x_continuous(breaks = seq(2014, 2028, 2), limits = c(2014, 2030.2),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(100, 600, 100), limits = c(0, 640),
                     labels = function(x) paste0(fmt_it(x), " TWh"),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  labs(title = "I consumi dei data center americani sono triplicati",
       subtitle = "Elettricità consumata dai data center negli Stati Uniti, con la forbice delle proiezioni fino al 2028",
       caption = "Elaborazione di Lorenzo Ruffino su dati Lawrence Berkeley National Laboratory e Terna") +
  theme_linechart()

ggsave(file.path(OUT, "02_consumo_elettrico_data_center_usa.png"), g2,
       width = 9, height = 6.5, dpi = 220, bg = "white")

write_csv(energia, file.path(OUT, "consumo_data_center_usa.csv"))

cat("G2 | 2014:", storico$twh_centrale[storico$anno == 2014],
    "| 2023:", storico$twh_centrale[storico$anno == 2023],
    "| 2028:", proiezione$twh_basso[proiezione$anno == 2028], "-",
    proiezione$twh_alto[proiezione$anno == 2028], "TWh\n")

# ============================================================================
# GRAFICO 03 - Quanta acqua consumano i data center, a confronto
# ============================================================================
# Tutte le voci sono acqua che evapora e non torna, tranne i campi da golf,
# per cui esiste solo l'acqua applicata (quindi la loro barra è generosa).
# Per i data center vale l'acqua evaporata in sito, l'unico perimetro
# confrontabile con le altre voci: nessuna di queste conta l'acqua delle
# centrali che producono l'energia usata.
#
# Scala lineare e tutte e quattro le voci a barre: la barra dei data center
# esce larga meno di un pixel ed è esattamente il punto del grafico, cioè
# che il loro consumo non si vede. Golf e Arizona sono compresi nel totale
# nazionale dell'irrigazione.

acqua <- read_csv(file.path(IN, "acqua_usa_confronto.csv"), show_col_types = FALSE) %>%
  mutate(
    voce_it = recode(voce,
      "Data center USA (acqua evaporata in sito)" = "Data center",
      "Campi da golf USA"                         = "Campi da golf",
      "Colture irrigue dell'Arizona"              = "Campi irrigati della sola Arizona",
      "Irrigazione agricola USA"                  = "Irrigazione agricola"),
    mld = litri_giorno / 1e9,
    lab = case_when(
      mld < 1    ~ paste0(fmt_it(litri_giorno / 1e6), " mln"),
      mld >= 100 ~ paste0(fmt_it(mld, 0), " mld"),
      TRUE       ~ paste0(fmt_it(mld, 1), " mld")),
    is_dc = voce_it == "Data center"
  ) %>%
  arrange(mld)

rapporto <- max(acqua$mld) / min(acqua$mld)

barre <- acqua %>%
  mutate(voce_it = recode(voce_it,
           "Irrigazione agricola" = "Irrigazione dei campi, tutti gli Stati Uniti"),
         voce_it = factor(voce_it, levels = voce_it),
         # solo la barra dell'irrigazione è abbastanza lunga da contenere
         # l'etichetta al suo interno, in bianco
         dentro = mld >= 100)

quota_dc <- barre$mld[barre$is_dc] / max(barre$mld) * 100

g3 <- ggplot(barre, aes(mld, voce_it, fill = is_dc)) +
  geom_col(width = 0.62) +
  geom_text(data = filter(barre, dentro), aes(label = lab),
            hjust = 1.25, colour = "white",
            fontface = "bold", family = "Source Sans Pro", size = 3.3) +
  geom_text(data = filter(barre, !dentro), aes(label = lab, colour = is_dc),
            hjust = -0.14, fontface = "bold",
            family = "Source Sans Pro", size = 3.3) +
  annotate("curve", x = 30, y = 0.44, xend = 1.5, yend = 0.84,
           curvature = -0.32, linewidth = 0.35, colour = COL_GRIGIO_SCURO,
           arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
  annotate("text", x = 33, y = 0.42,
           label = paste0("lo ", fmt_it(quota_dc, 2), " per cento dell'irrigazione"),
           hjust = 0, vjust = 0.5, family = "Source Sans Pro",
           size = 3.1, colour = COL_GRIGIO_SCURO) +
  scale_fill_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_BLU),
                    guide = "none") +
  scale_colour_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_NERO),
                      guide = "none") +
  scale_x_continuous(limits = c(0, 300), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.95, 0.6))) +
  coord_cartesian(clip = "off") +
  labs(title = "L'agricoltura consuma 1.500 volte l'acqua dei data center",
       subtitle = "Litri d'acqua che ogni giorno evaporano negli Stati Uniti e non tornano a fiumi e falde.\nPer i campi da golf il dato disponibile è l'acqua usata per innaffiare",
       caption = "Elaborazione di Lorenzo Ruffino su dati Lawrence Berkeley National Laboratory, USGS e HortTechnology") +
  theme_linechart() +
  theme(axis.text.y = element_text(hjust = 0, size = 10),
        axis.text.x = element_blank(),
        axis.line = element_blank())

ggsave(file.path(OUT, "03_acqua_confronto_usa.png"), g3,
       width = 9, height = 5.4, dpi = 220, bg = "white")

write_csv(acqua %>% select(voce_it, litri_giorno, gal_giorno, misura, anno_riferimento),
          file.path(OUT, "acqua_confronto_usa.csv"))

cat("G3 | rapporto irrigazione/data center:", round(rapporto), "volte |",
    "data center:", round(acqua$litri_giorno[acqua$is_dc] / 1e6), "milioni di litri\n")
