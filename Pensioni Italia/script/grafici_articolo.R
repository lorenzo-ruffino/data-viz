# Grafici per l'articolo "Perché le pensioni italiane non sono sostenibili"
# 01 concentrazione (pensionati vs spesa per fascia) — Casellario/Itinerari, 2024
# 02 età effettiva vs legale di pensionamento 1995-2024 — Inps + normativa
# 03 la "gobba" spesa pensioni/PIL, proiezioni 2025-2070 — Ragioneria + UE
# 04 aliquota contributiva dipendenti 1960-2025 — Inps

library(tidyverse)
library(showtext)
library(ggrepel)

# --- Tema -------------------------------------------------------------------
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_VIOLA  <- "#A82DE3"
COL_ROSSO  <- "#F12938"
COL_GIALLO <- "#F2A900"
COL_GRIGIO <- "#9A9A9A"
COL_GRIGIO_SCURO <- "#5A5A5A"

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

pct_it <- function(x, dec = 1) paste0(format(round(x, dec), nsmall = dec, decimal.mark = ","), "%")

out <- "../output"

# ============================================================================
# 01 — CONCENTRAZIONE: barre 100% (pensionati / reddito) + flussi diagonali
#     Fonte: Inps, Casellario centrale dei pensionati 2024 (reddito per persona)
# ============================================================================

# Classi del Casellario (multipli del minimo 2024 = 598,61 €/mese)
classi <- tribble(
  ~sup,      ~n,         ~importo,
  598.61,    2264759,    10082078243,
  1197.22,   3803807,    42699923315,
  1795.83,   3821491,    73730695951,
  2394.44,   2822555,    76545535703,
  2993.05,   1876880,    65470817166,
  3591.66,   785401,     33287513785,
  4190.27,   343908,     17232642271,
  4788.88,   181696,     10552455945,
  5387.49,   113293,     7466047597,
  5986.10,   89029,      6568265653,
  6584.71,   67262,      5480142727,
  7183.32,   43293,      3861214195,
  7781.93,   28027,      2720143887,
  8380.54,   18629,      1952617981,
  8979.15,   12690,      1428365424,
  9577.76,   8330,       1002647074,
  11972.20,  14919,      2040625269,
  17958.30,  8891,       1507945760,
  29930.50,  1968,       526100000,
  99999.00,  247,        138964404
)

fasce_lv <- c("fino a 600", "600-1.200", "1.200-1.800", "1.800-2.400",
              "2.400-3.000", "3.000-3.600", "oltre 3.600")
# etichette da mostrare, con il simbolo € davanti agli importi
fasce_disp <- c("fino a €600", "€600-1.200", "€1.200-1.800", "€1.800-2.400",
                "€2.400-3.000", "€3.000-3.600", "oltre €3.600")
names(fasce_disp) <- fasce_lv

conc <- classi %>%
  mutate(fascia = case_when(
    sup <= 598.61  ~ "fino a 600",
    sup <= 1197.22 ~ "600-1.200",
    sup <= 1795.83 ~ "1.200-1.800",
    sup <= 2394.44 ~ "1.800-2.400",
    sup <= 2993.05 ~ "2.400-3.000",
    sup <= 3591.66 ~ "3.000-3.600",
    TRUE           ~ "oltre 3.600"
  )) %>%
  group_by(fascia) %>%
  summarise(n = sum(n), importo = sum(importo), .groups = "drop") %>%
  mutate(fascia = factor(fascia, levels = fasce_lv),
         q_pens = n / sum(n) * 100,
         q_spesa = importo / sum(importo) * 100) %>%
  arrange(fascia)

pal <- c("fino a 600"   = "#4575B4",
         "600-1.200"    = "#91BFDB",
         "1.200-1.800"  = "#C6DBEF",
         "1.800-2.400"  = "#FEE9A8",
         "2.400-3.000"  = "#FDBB84",
         "3.000-3.600"  = "#FC8D59",
         "oltre 3.600"  = "#D7301F")
txt_bianco <- c("fino a 600", "3.000-3.600", "oltre 3.600")

TOP_MIN <- 3.0; TOP_MAX <- 4.0
BOT_MIN <- 0.0; BOT_MAX <- 1.0

d <- conc %>%
  mutate(pens_end  = cumsum(q_pens),  pens_start  = lag(pens_end,  default = 0),
         spesa_end = cumsum(q_spesa), spesa_start = lag(spesa_end, default = 0))

# barre (rettangoli)
bars <- bind_rows(
  d %>% transmute(fascia, xmin = pens_start,  xmax = pens_end,  ymin = TOP_MIN, ymax = TOP_MAX,
                  quota = q_pens),
  d %>% transmute(fascia, xmin = spesa_start, xmax = spesa_end, ymin = BOT_MIN, ymax = BOT_MAX,
                  quota = q_spesa)
) %>%
  mutate(xmid = (xmin + xmax) / 2, ymid = (ymin + ymax) / 2, width = xmax - xmin,
         col_txt = ifelse(fascia %in% txt_bianco, "white", COL_NERO),
         # tutte le etichette dentro la barra, in bianco sui segmenti scuri;
         # le fasce strette sconfinano un po', va bene
         lab_y = ymid,
         lab_col = col_txt,
         # la fascia più stretta a sinistra: allineata a sinistra per non uscire dal bordo
         lab_x = ifelse(xmid < 2.5, 0.5, xmid),
         lab_hjust = ifelse(xmid < 2.5, 0, 0.5))

# flussi diagonali (quadrilateri) tra le due barre
ribbons <- pmap_dfr(
  list(as.character(d$fascia), d$pens_start, d$pens_end, d$spesa_start, d$spesa_end),
  function(f, ps, pe, ss, se) tibble(
    fascia = f,
    x = c(ps, pe, se, ss),
    y = c(TOP_MIN, TOP_MIN, BOT_MAX, BOT_MAX))
) %>% mutate(fascia = factor(fascia, levels = fasce_lv))

# etichette delle due righe: sopra la barra alta e sotto la barra bassa
righe <- tibble(
  x = 0,
  y = c(TOP_MAX + 0.34, BOT_MIN - 0.30),
  txt = c("Quota dei pensionati", "Quota della spesa pensionistica")
)

tit_conc <- "Un quinto dei pensionati prende più di 2.400 euro al mese"
sub_conc <- "Quota dei pensionati e della spesa pensionistica per fascia di reddito mensile lordo, Italia, 2024"
cap_conc <- "Elaborazione di Lorenzo Ruffino su dati Inps"

# elementi comuni alle due versioni
layer_flussi <- geom_polygon(data = ribbons, aes(x, y, group = fascia, fill = fascia),
                             alpha = 0.32, color = NA)
layer_barre  <- geom_rect(data = bars, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                           fill = fascia), color = "white", linewidth = 0.4)
layer_valori <- geom_text(data = bars, aes(lab_x, lab_y, label = pct_it(quota),
                                           color = I(lab_col), hjust = lab_hjust),
                          size = 3.0, fontface = "bold", family = "Source Sans Pro")
layer_righe  <- geom_text(data = righe, aes(x, y, label = txt), hjust = 0, size = 3.5,
                          fontface = "bold", family = "Source Sans Pro", color = COL_NERO)

# --- Versione 1: legenda in alto -------------------------------------------
p1 <- ggplot() +
  layer_flussi + layer_barre + layer_valori + layer_righe +
  scale_fill_manual(values = pal, breaks = fasce_lv, labels = fasce_disp,
                    guide = guide_legend(nrow = 1, label.position = "right",
                                         keywidth = unit(0.3, "cm"), keyheight = unit(0.3, "cm"))) +
  scale_x_continuous(limits = c(0, 100), expand = c(0.004, 0.004)) +
  coord_cartesian(ylim = c(-0.7, 4.6), clip = "off") +
  labs(title = tit_conc, subtitle = sub_conc, caption = cap_conc) +
  theme_linechart() +
  theme(axis.text = element_blank(), axis.line = element_blank(),
        legend.position = "top", legend.text = element_text(size = 7.5))

ggsave(file.path(out, "01_concentrazione.png"), p1,
       width = 8.7, height = 5.3, units = "in", dpi = 220, bg = "white")

# --- Versione 2: etichette fasce sul grafico, niente legenda ---------------
# fasce basse (fino a 2.400) sotto la barra alta; fasce alte sopra la barra bassa
band_lab <- bind_rows(
  d %>% slice(1:4) %>% transmute(fascia, x = (pens_start + pens_end) / 2,   y = TOP_MIN - 0.20),
  d %>% slice(5:7) %>% transmute(fascia, x = (spesa_start + spesa_end) / 2, y = BOT_MAX + 0.20)
) %>% mutate(lab = fasce_disp[as.character(fascia)])

p1b <- ggplot() +
  layer_flussi + layer_barre + layer_valori + layer_righe +
  geom_text(data = band_lab, aes(x, y, label = lab), size = 3.0, fontface = "bold",
            family = "Source Sans Pro", color = COL_NERO) +
  scale_fill_manual(values = pal, breaks = fasce_lv, guide = "none") +
  scale_x_continuous(limits = c(0, 100), expand = c(0.004, 0.004)) +
  coord_cartesian(ylim = c(-0.7, 4.6), clip = "off") +
  labs(title = tit_conc, subtitle = sub_conc, caption = cap_conc) +
  theme_linechart() +
  theme(axis.text = element_blank(), axis.line = element_blank(), legend.position = "none")

ggsave(file.path(out, "01b_concentrazione_alt.png"), p1b,
       width = 8.7, height = 5.5, units = "in", dpi = 220, bg = "white")

# ============================================================================
# 02 — ETÀ EFFETTIVA vs LEGALE di pensionamento, 1995-2024
# ============================================================================

eta_eff <- tibble(
  anno = c(1995, 2000, 2005, 2010, 2015, 2019, 2020, 2022, 2024),
  eta  = c(57.6, 59.1, 59.9, 60.8, 61.7, 63.0, 63.1, 63.5, 63.8),
  serie = "Età effettiva di uscita"
)
eta_leg <- tibble(
  anno = c(1995, 2000, 2010, 2011, 2012, 2013, 2016, 2019, 2024),
  eta  = c(60.0, 65.0, 65.0, 65.0, 66.0, 66.25, 66.58, 67.0, 67.0),
  serie = "Età legale per la vecchiaia"
)

lab2 <- bind_rows(
  eta_eff %>% slice_max(anno) %>% mutate(txt = "Età effettiva\ndi uscita", col = COL_ROSSO),
  eta_leg %>% slice_max(anno) %>% mutate(txt = "Età legale per\nla vecchiaia", col = COL_NERO)
)

p2 <- ggplot() +
  geom_step(data = eta_leg, aes(anno, eta), color = COL_NERO,
            linewidth = 0.9, direction = "hv") +
  geom_line(data = eta_eff, aes(anno, eta), color = COL_ROSSO, linewidth = 1.0) +
  geom_point(data = eta_eff, aes(anno, eta), color = COL_ROSSO, size = 1.8) +
  # valori estremi: effettiva in rosso, legale in nero
  geom_text(data = eta_eff %>% slice_min(anno),
            aes(anno, eta, label = format(eta, nsmall = 1, decimal.mark = ",")),
            hjust = 0, nudge_x = 0.5, vjust = 1.9, size = 3.1, fontface = "bold",
            family = "Source Sans Pro", color = COL_ROSSO) +
  geom_text(data = eta_eff %>% slice_max(anno),
            aes(anno, eta, label = format(eta, nsmall = 1, decimal.mark = ",")),
            hjust = 1, nudge_x = -0.4, vjust = -1.1, size = 3.1, fontface = "bold",
            family = "Source Sans Pro", color = COL_ROSSO) +
  geom_text(data = eta_leg %>% slice_min(anno),
            aes(anno, eta, label = format(eta, nsmall = 1, decimal.mark = ",")),
            hjust = 0, nudge_x = 0.5, vjust = -0.8, size = 3.1, fontface = "bold",
            family = "Source Sans Pro", color = COL_NERO) +
  geom_text(data = eta_leg %>% slice_max(anno),
            aes(anno, eta, label = format(eta, nsmall = 1, decimal.mark = ",")),
            hjust = 1, nudge_x = -0.4, vjust = -1.1, size = 3.1, fontface = "bold",
            family = "Source Sans Pro", color = COL_NERO) +
  geom_text(data = lab2, aes(anno, eta, label = txt, color = I(col)),
            hjust = 0, nudge_x = 0.6, vjust = 0.5, size = 3.3, lineheight = 0.9,
            fontface = "bold", family = "Source Sans Pro") +
  scale_x_continuous(limits = c(1995, 2032), expand = c(0, 0),
                     breaks = seq(1995, 2024, 5)) +
  scale_y_continuous(limits = c(56, 69), breaks = seq(56, 68, 2),
                     labels = function(x) paste0(x, " anni")) +
  labs(
    title = "Si va in pensione prima di quando si dovrebbe",
    subtitle = "Età legale per la pensione di vecchiaia ed età media effettiva di uscita, Italia, 1995-2024",
    caption = "Elaborazione di Lorenzo Ruffino su dati Inps e normativa vigente"
  ) +
  theme_linechart() +
  theme(legend.position = "none")

ggsave(file.path(out, "02_eta_pensionamento.png"), p2,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# ============================================================================
# 03 — LA "GOBBA": spesa pensionistica in % PIL, proiezioni 2025-2070
#     Ragioneria generale dello Stato (Rapporto 26/2025) + Commissione UE
# ============================================================================

rgs <- tibble(
  anno = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070),
  val  = c(15.3, 15.7, 16.6, 17.1, 16.8, 15.9, 14.9, 14.1, 13.8, 14.0),
  serie = "Ragioneria generale dello Stato"
)
ue <- tibble(
  anno = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070),
  val  = c(15.4, 15.7, 16.3, 17.3, 17.0, 16.6, 16.2, 15.8, 15.5, 15.2),
  serie = "Commissione UE"
)
gobba <- bind_rows(rgs, ue)

lab3 <- bind_rows(rgs %>% slice_max(anno), ue %>% slice_max(anno)) %>%
  mutate(txt = c("Ragioneria\ngenerale", "Commissione\nUE"))
picco <- tibble(anno = 2040, val = 17.1)

p3 <- ggplot(gobba, aes(anno, val, color = serie)) +
  geom_line(aes(linewidth = serie)) +
  geom_point(size = 1.6) +
  geom_point(data = picco, aes(anno, val), inherit.aes = FALSE,
             color = COL_ROSSO, size = 2.6) +
  annotate("text", x = 2040, y = 17.75, label = "Picco del 2040",
           size = 3.3, fontface = "bold", family = "Source Sans Pro", color = COL_NERO) +
  geom_text(data = lab3, aes(anno, val, label = txt, color = serie),
            hjust = 0, nudge_x = 1.3, vjust = 0.5, size = 3.3,
            fontface = "bold", family = "Source Sans Pro", lineheight = 0.9) +
  scale_color_manual(values = c("Ragioneria generale dello Stato" = COL_ROSSO,
                                "Commissione UE" = COL_BLU)) +
  scale_linewidth_manual(values = c("Ragioneria generale dello Stato" = 1.0,
                                    "Commissione UE" = 0.7)) +
  scale_x_continuous(limits = c(2025, 2083), expand = c(0, 0),
                     breaks = seq(2030, 2070, 10)) +
  scale_y_continuous(limits = c(13.5, 18), breaks = seq(14, 18, 1),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "La spesa pensionistica salirà fino al 2040",
    subtitle = "Spesa pensionistica in percentuale del PIL, proiezioni, Italia, 2025-2070",
    caption = "Elaborazione di Lorenzo Ruffino su dati RGS e Commissione UE"
  ) +
  theme_linechart() +
  theme(legend.position = "none")

ggsave(file.path(out, "03_gobba_spesa_pil.png"), p3,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# ============================================================================
# 04 — ALIQUOTA CONTRIBUTIVA dipendenti privati, 1960-2025
# ============================================================================

anni_dip <- 1960:2024
aliq_dip <- c(14.41,14.41,16.42,19.10,18.80,18.58,18.56,18.10,19.67,20.56,20.56,
              18.91,19.01,19.01,19.95,20.77,23.31,23.31,23.31,23.31,23.90,24.01,
              24.17,24.51,24.51,24.51,24.51,24.51,24.51,25.92,25.92,26.09,26.49,
              26.97,26.97,27.16,32.70,32.70,32.70,32.70,32.70,32.70,32.70,32.70,
              32.70,32.70,32.70,33.00,33.00,33.00,33.00,33.00,33.00,33.00,33.00,
              33.00,33.00,33.00,33.00,33.00,33.00,33.00,33.00,33.00,33.00)
dip <- tibble(anno = c(anni_dip, 2025), aliq = c(aliq_dip, 33.00))

p4 <- ggplot(dip, aes(anno, aliq)) +
  geom_line(color = COL_ROSSO, linewidth = 1.0) +
  # freccia sul salto della riforma Dini
  annotate("curve", x = 2004, y = 29.4, xend = 1997, yend = 32.4,
           curvature = -0.25, linewidth = 0.4, color = COL_NERO,
           arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
  annotate("text", x = 2005, y = 28.7, label = "Riforma Dini",
           hjust = 0, size = 3.3, fontface = "bold", family = "Source Sans Pro",
           color = COL_NERO) +
  annotate("text", x = 1960, y = 12.6, label = "14,4%", hjust = 0, vjust = 1,
           size = 3.1, fontface = "bold", family = "Source Sans Pro", color = COL_ROSSO) +
  annotate("text", x = 2025, y = 33.0, label = "33%", hjust = 0, nudge_x = 0.5,
           vjust = 0.4, size = 3.3, fontface = "bold", family = "Source Sans Pro",
           color = COL_ROSSO) +
  scale_x_continuous(limits = c(1960, 2030), expand = c(0, 0),
                     breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(limits = c(0, 36), breaks = seq(0, 35, 5),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "L'aliquota dei dipendenti è più che raddoppiata",
    subtitle = "Aliquota contributiva per la pensione dei dipendenti privati, Italia, 1960-2025",
    caption = "Elaborazione di Lorenzo Ruffino su dati Inps"
  ) +
  theme_linechart()

ggsave(file.path(out, "04_aliquota_contributiva.png"), p4,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# ============================================================================
# VERSIONI IN INGLESE (_eng)
# ============================================================================
pct_en  <- function(x, dec = 1) paste0(format(round(x, dec), nsmall = dec, decimal.mark = "."), "%")
fmt1_en <- function(v) format(v, nsmall = 1, decimal.mark = ".")
cap_inps_en <- "Chart by Lorenzo Ruffino based on INPS data"

# --- 01 concentrazione (EN) -------------------------------------------------
fasce_en <- c("up to €600", "€600-1,200", "€1,200-1,800", "€1,800-2,400",
              "€2,400-3,000", "€3,000-3,600", "over €3,600")
names(fasce_en) <- fasce_lv

tit_conc_en <- "A fifth of pensioners get more than €2,400 a month"
sub_conc_en <- "Share of pensioners and of pension spending by monthly gross pension income band, Italy, 2024"

righe_en <- tibble(x = 0, y = c(TOP_MAX + 0.34, BOT_MIN - 0.30),
                   txt = c("Share of pensioners", "Share of pension spending"))

layer_valori_en <- geom_text(data = bars, aes(lab_x, lab_y, label = pct_en(quota),
                                              color = I(lab_col), hjust = lab_hjust),
                             size = 3.0, fontface = "bold", family = "Source Sans Pro")
layer_righe_en  <- geom_text(data = righe_en, aes(x, y, label = txt), hjust = 0, size = 3.5,
                             fontface = "bold", family = "Source Sans Pro", color = COL_NERO)

p1_en <- ggplot() +
  layer_flussi + layer_barre + layer_valori_en + layer_righe_en +
  scale_fill_manual(values = pal, breaks = fasce_lv, labels = fasce_en,
                    guide = guide_legend(nrow = 1, label.position = "right",
                                         keywidth = unit(0.3, "cm"), keyheight = unit(0.3, "cm"))) +
  scale_x_continuous(limits = c(0, 100), expand = c(0.004, 0.004)) +
  coord_cartesian(ylim = c(-0.7, 4.6), clip = "off") +
  labs(title = tit_conc_en, subtitle = sub_conc_en, caption = cap_inps_en) +
  theme_linechart() +
  theme(axis.text = element_blank(), axis.line = element_blank(),
        legend.position = "top", legend.text = element_text(size = 7.5))
ggsave(file.path(out, "01_concentrazione_eng.png"), p1_en,
       width = 8.7, height = 5.3, units = "in", dpi = 220, bg = "white")

band_lab_en <- band_lab %>% mutate(lab = fasce_en[as.character(fascia)])
p1b_en <- ggplot() +
  layer_flussi + layer_barre + layer_valori_en + layer_righe_en +
  geom_text(data = band_lab_en, aes(x, y, label = lab), size = 3.0, fontface = "bold",
            family = "Source Sans Pro", color = COL_NERO) +
  scale_fill_manual(values = pal, breaks = fasce_lv, guide = "none") +
  scale_x_continuous(limits = c(0, 100), expand = c(0.004, 0.004)) +
  coord_cartesian(ylim = c(-0.7, 4.6), clip = "off") +
  labs(title = tit_conc_en, subtitle = sub_conc_en, caption = cap_inps_en) +
  theme_linechart() +
  theme(axis.text = element_blank(), axis.line = element_blank(), legend.position = "none")
ggsave(file.path(out, "01b_concentrazione_alt_eng.png"), p1b_en,
       width = 8.7, height = 5.5, units = "in", dpi = 220, bg = "white")

# --- 02 età (EN) ------------------------------------------------------------
lab2_en <- bind_rows(
  eta_eff %>% slice_max(anno) %>% mutate(txt = "Effective\nretirement age", col = COL_ROSSO),
  eta_leg %>% slice_max(anno) %>% mutate(txt = "Statutory\nretirement age", col = COL_NERO)
)
p2_en <- ggplot() +
  geom_step(data = eta_leg, aes(anno, eta), color = COL_NERO, linewidth = 0.9, direction = "hv") +
  geom_line(data = eta_eff, aes(anno, eta), color = COL_ROSSO, linewidth = 1.0) +
  geom_point(data = eta_eff, aes(anno, eta), color = COL_ROSSO, size = 1.8) +
  geom_text(data = eta_eff %>% slice_min(anno), aes(anno, eta, label = fmt1_en(eta)),
            hjust = 0, nudge_x = 0.5, vjust = 1.9, size = 3.1, fontface = "bold",
            family = "Source Sans Pro", color = COL_ROSSO) +
  geom_text(data = eta_eff %>% slice_max(anno), aes(anno, eta, label = fmt1_en(eta)),
            hjust = 1, nudge_x = -0.4, vjust = -1.1, size = 3.1, fontface = "bold",
            family = "Source Sans Pro", color = COL_ROSSO) +
  geom_text(data = eta_leg %>% slice_min(anno), aes(anno, eta, label = fmt1_en(eta)),
            hjust = 0, nudge_x = 0.5, vjust = -0.8, size = 3.1, fontface = "bold",
            family = "Source Sans Pro", color = COL_NERO) +
  geom_text(data = eta_leg %>% slice_max(anno), aes(anno, eta, label = fmt1_en(eta)),
            hjust = 1, nudge_x = -0.4, vjust = -1.1, size = 3.1, fontface = "bold",
            family = "Source Sans Pro", color = COL_NERO) +
  geom_text(data = lab2_en, aes(anno, eta, label = txt, color = I(col)),
            hjust = 0, nudge_x = 0.6, vjust = 0.5, size = 3.3, lineheight = 0.9,
            fontface = "bold", family = "Source Sans Pro") +
  scale_x_continuous(limits = c(1995, 2032), expand = c(0, 0), breaks = seq(1995, 2024, 5)) +
  scale_y_continuous(limits = c(56, 69), breaks = seq(56, 68, 2),
                     labels = function(x) paste0(x, " years")) +
  labs(title = "People retire earlier than they should",
       subtitle = "Statutory old-age pension age and average effective retirement age, Italy, 1995-2024",
       caption = "Chart by Lorenzo Ruffino based on INPS data and current legislation") +
  theme_linechart() + theme(legend.position = "none")
ggsave(file.path(out, "02_eta_pensionamento_eng.png"), p2_en,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 03 gobba (EN) ----------------------------------------------------------
lab3_en <- bind_rows(rgs %>% slice_max(anno), ue %>% slice_max(anno)) %>%
  mutate(txt = c("State Accounting\nOffice", "European\nCommission"))
p3_en <- ggplot(gobba, aes(anno, val, color = serie)) +
  geom_line(aes(linewidth = serie)) +
  geom_point(size = 1.6) +
  geom_point(data = picco, aes(anno, val), inherit.aes = FALSE, color = COL_ROSSO, size = 2.6) +
  annotate("text", x = 2040, y = 17.75, label = "2040 peak", size = 3.3, fontface = "bold",
           family = "Source Sans Pro", color = COL_NERO) +
  geom_text(data = lab3_en, aes(anno, val, label = txt, color = serie), hjust = 0, nudge_x = 1.3,
            vjust = 0.5, size = 3.2, fontface = "bold", family = "Source Sans Pro", lineheight = 0.9) +
  scale_color_manual(values = c("Ragioneria generale dello Stato" = COL_ROSSO, "Commissione UE" = COL_BLU)) +
  scale_linewidth_manual(values = c("Ragioneria generale dello Stato" = 1.0, "Commissione UE" = 0.7)) +
  scale_x_continuous(limits = c(2025, 2092), expand = c(0, 0), breaks = seq(2030, 2070, 10)) +
  scale_y_continuous(limits = c(13.5, 18), breaks = seq(14, 18, 1), labels = function(x) paste0(x, "%")) +
  labs(title = "Pension spending will rise until 2040",
       subtitle = "Pension spending as a share of GDP, projections, Italy, 2025-2070",
       caption = "Chart by Lorenzo Ruffino based on RGS and European Commission data") +
  theme_linechart() + theme(legend.position = "none")
ggsave(file.path(out, "03_gobba_spesa_pil_eng.png"), p3_en,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- 04 aliquota (EN) -------------------------------------------------------
p4_en <- ggplot(dip, aes(anno, aliq)) +
  geom_line(color = COL_ROSSO, linewidth = 1.0) +
  annotate("curve", x = 2004, y = 29.4, xend = 1997, yend = 32.4, curvature = -0.25,
           linewidth = 0.4, color = COL_NERO, arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
  annotate("text", x = 2005, y = 28.7, label = "Dini reform", hjust = 0, size = 3.3,
           fontface = "bold", family = "Source Sans Pro", color = COL_NERO) +
  annotate("text", x = 1960, y = 12.6, label = "14.4%", hjust = 0, vjust = 1, size = 3.1,
           fontface = "bold", family = "Source Sans Pro", color = COL_ROSSO) +
  annotate("text", x = 2025.5, y = 33.0, label = "33%", hjust = 0, vjust = 0.4, size = 3.3,
           fontface = "bold", family = "Source Sans Pro", color = COL_ROSSO) +
  scale_x_continuous(limits = c(1960, 2030), expand = c(0, 0), breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(limits = c(0, 36), breaks = seq(0, 35, 5), labels = function(x) paste0(x, "%")) +
  labs(title = "Employee contribution rate has more than doubled",
       subtitle = "Pension contribution rate for private-sector employees, Italy, 1960-2025",
       caption = cap_inps_en) +
  theme_linechart()
ggsave(file.path(out, "04_aliquota_contributiva_eng.png"), p4_en,
       width = 8, height = 6.5, units = "in", dpi = 220, bg = "white")

# --- Esporta i dati puliti ---------------------------------------------------
write_csv(conc, file.path(out, "01_concentrazione.csv"))
bind_rows(mutate(eta_eff, tipo = "effettiva"), mutate(eta_leg, tipo = "legale")) %>%
  write_csv(file.path(out, "02_eta_pensionamento.csv"))
write_csv(gobba, file.path(out, "03_gobba_spesa_pil.csv"))
write_csv(dip, file.path(out, "04_aliquota_contributiva.csv"))

cat("Fatto. Concentrazione (cumulate dall'alto):\n")
d %>% transmute(fascia, q_pens, q_spesa,
                cum_pens = pens_end, cum_spesa = spesa_end) %>% print()
