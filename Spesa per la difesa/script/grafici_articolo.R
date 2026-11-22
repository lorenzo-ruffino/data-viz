# Grafici per l'articolo "Quanto spende l'Italia per la difesa e per cosa"
# Esegui da script/: Rscript grafici_articolo.R

library(tidyverse)
library(showtext)
library(patchwork)
library(treemapify)

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
COL_GRIGIO_SCURO <- "#5A5A5A"
COL_VERDE  <- "#1B9E77"
COL_ARANCIO <- "#E07700"

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
                                  margin = margin(t = 0.5, unit = "cm"))
    ) +
    theme(...)
}

CAP_SIPRI    <- "Elaborazione di Lorenzo Ruffino su dati Sipri"
CAP_NATO     <- "Elaborazione di Lorenzo Ruffino su dati Nato ed Eurostat"
CAP_DIFESA   <- "Elaborazione di Lorenzo Ruffino su dati del ministero della Difesa ed Eurostat"
CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

fmt_mld <- function(x) if_else(x >= 10, num_it(round(x), 0), num_it(x))
num_it <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")

OUT <- "../output"
IN  <- "../input"

# =============================================================================
# 01 — Spesa militare in % del Pil, 1960-2025 (Sipri)
# =============================================================================

nomi_sipri <- c(
  "Italy" = "Italia", "Germany" = "Germania", "France" = "Francia",
  "United Kingdom" = "Regno Unito", "Poland" = "Polonia",
  "United States of America" = "Stati Uniti"
)
pal_01 <- c("Italia" = COL_ROSSO, "Germania" = COL_NERO, "Francia" = COL_VIOLA,
            "Regno Unito" = COL_GIALLO, "Polonia" = COL_VERDE,
            "Stati Uniti" = COL_GRIGIO)

d01 <- read_csv(file.path(IN, "sipri_quota_pil.csv"), show_col_types = FALSE) |>
  filter(paese %in% names(nomi_sipri), anno >= 1960, !is.na(pil_pct)) |>
  mutate(paese = factor(nomi_sipri[paese], levels = names(pal_01)))
write_csv(d01, file.path(OUT, "01_spesa_militare_pil_1960_2025.csv"))

lab01 <- d01 |> group_by(paese) |> filter(anno == max(anno)) |> ungroup() |>
  mutate(label = paste0(paese, " ", num_it(pil_pct), "%"))

p01 <- ggplot(d01, aes(anno, pil_pct, colour = paese)) +
  geom_line(data = filter(d01, paese != "Italia"), linewidth = 0.7, alpha = 0.85) +
  geom_line(data = filter(d01, paese == "Italia"), linewidth = 1.1) +
  ggrepel::geom_text_repel(
    data = lab01, aes(label = label), hjust = 0, direction = "y",
    nudge_x = 1.2, size = 3.3, fontface = "bold", family = "Source Sans Pro",
    segment.colour = COL_GRIGIO, segment.size = 0.3, min.segment.length = 0,
    box.padding = 0.2, seed = 1, xlim = c(2026, 2042)) +
  scale_colour_manual(values = pal_01) +
  scale_x_continuous(limits = c(1960, 2042), breaks = seq(1960, 2020, 10),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(2, 10, 2),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "In Europa la spesa militare risale dopo trent’anni di calo",
       subtitle = "Spesa militare in percentuale del Pil, alcuni paesi europei e Stati Uniti, 1960-2025",
       caption = CAP_SIPRI) +
  theme_linechart(legend.position = "none")

ggsave(file.path(OUT, "01_spesa_militare_pil_1960_2025.png"), p01,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# =============================================================================
# 02 — Italia: spesa Nato in euro 2025 e in % del Pil, 2007-2026
# =============================================================================

COL_BLU_CHIARO <- "#9CC7F4"

d02 <- read_csv(file.path(IN, "serie_italia_2000_2026.csv"), show_col_types = FALSE) |>
  filter(anno >= 2007) |>
  transmute(anno, mld_reali = nato_reale25 / 1000, pct_pil = nato_pct_pil_nominale) |>
  # Quote di Pil 2024-2026 come pubblicate dalla Nato (luglio 2026)
  mutate(pct_pil = case_when(anno == 2024 ~ 1.52, anno == 2025 ~ 2.09,
                             anno == 2026 ~ 2.10, TRUE ~ pct_pil),
         fase = if_else(anno >= 2025, "dopo", "prima"))
write_csv(d02, file.path(OUT, "02_italia_spesa_nato_2007_2026.csv"))

anni_lab02 <- c(2008, 2015, 2024, 2026)

p02a <- ggplot(d02, aes(anno, mld_reali)) +
  geom_col(aes(fill = fase), width = 0.75) +
  geom_text(data = filter(d02, anno %in% anni_lab02),
            aes(label = paste0("€ ", num_it(mld_reali), " mld"), colour = fase),
            angle = 90, hjust = 1.1, vjust = 0.5, size = 3.0, fontface = "bold",
            family = "Source Sans Pro") +
  annotate("text", x = 2025.5, y = 54, label = "Dal 2025 entrano\nnuove voci di spesa",
           hjust = 1, vjust = 1, size = 2.9, colour = COL_GRIGIO_SCURO,
           family = "Source Sans Pro", lineheight = 0.95) +
  scale_fill_manual(values = c(prima = COL_BLU, dopo = COL_BLU_CHIARO)) +
  scale_colour_manual(values = c(prima = "white", dopo = "#06366A")) +
  scale_x_continuous(breaks = c(2007, 2012, 2017, 2022, 2026), expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(10, 50, 10),
                     labels = function(x) paste0("€ ", x, " mld"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Spesa al netto dell’inflazione") +
  theme_linechart(legend.position = "none",
                  plot.title = element_text(family = "Source Sans Pro", face = "bold",
                                            size = 10.5, color = COL_NERO, hjust = 0,
                                            margin = margin(b = 0.3, unit = "cm")))

p02b <- ggplot(d02, aes(anno, pct_pil)) +
  annotate("rect", xmin = 2024.5, xmax = 2026.5, ymin = 0, ymax = 2.5,
           fill = COL_GRIGIO, alpha = 0.13) +
  geom_line(colour = COL_ROSSO, linewidth = 1) +
  geom_point(colour = COL_ROSSO, size = 1.6) +
  geom_text(data = filter(d02, anno %in% anni_lab02),
            aes(label = paste0(num_it(pct_pil, 2), "%"),
                vjust = if_else(anno %in% c(2015, 2024), 1.9, -1.0),
                hjust = if_else(anno == 2024, 0, 0.5)), size = 3.1, fontface = "bold", colour = COL_ROSSO,
            family = "Source Sans Pro") +
  scale_x_continuous(breaks = c(2007, 2012, 2017, 2022, 2026), expand = c(0.03, 0)) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0.5, 2.5, 0.5),
                     labels = function(x) paste0(num_it(x), "%"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Spesa in percentuale del Pil") +
  theme_linechart(plot.title = element_text(family = "Source Sans Pro", face = "bold",
                                            size = 10.5, color = COL_NERO, hjust = 0,
                                            margin = margin(b = 0.3, unit = "cm")))

p02 <- (p02a | plot_spacer() | p02b) +
  plot_layout(widths = c(1, 0.08, 1)) +
  plot_annotation(
    title = "La spesa militare italiana è scesa fino al 2015, poi è risalita",
    subtitle = "Spesa per la difesa secondo i criteri Nato, a prezzi 2025 e in percentuale del Pil, Italia, 2007-2026 (2025 e 2026 stime)",
    caption = CAP_NATO,
    theme = theme_linechart())

ggsave(file.path(OUT, "02_italia_spesa_nato_2007_2026.png"), p02,
       width = 10, height = 6.5, dpi = 220, bg = "white")

# =============================================================================
# 03 — Funzione Difesa: personale, esercizio, investimento (euro 2025)
# =============================================================================

d03 <- read_csv(file.path(IN, "funzione_difesa_reale.csv"), show_col_types = FALSE) |>
  select(anno, Personale = personale_reale25, Esercizio = esercizio_reale25,
         Investimento = investimento_reale25) |>
  pivot_longer(-anno, names_to = "settore", values_to = "mln") |>
  filter(!is.na(mln)) |>
  mutate(mld = mln / 1000, seg = if_else(anno <= 2013, "a", "b"))
write_csv(d03, file.path(OUT, "03_funzione_difesa_settori_2008_2026.csv"))

pal_03 <- c(Personale = COL_GRIGIO_SCURO, Esercizio = COL_ROSSO, Investimento = COL_BLU)
nomi_03 <- c(Personale = "Personale",
             Esercizio = "Esercizio: manutenzione\ne addestramento",
             Investimento = "Investimento: armamenti\ne mezzi nuovi")

lab03_fine <- d03 |> filter(anno == 2026) |> mutate(label = nomi_03[settore])
lab03_val  <- d03 |> filter(anno %in% c(2008, 2026))

p03 <- ggplot(d03, aes(anno, mld, colour = settore, group = interaction(settore, seg))) +
  annotate("rect", xmin = 2013.4, xmax = 2016.6, ymin = 0, ymax = 14,
           fill = COL_GRIGIO, alpha = 0.13) +
  annotate("text", x = 2015, y = 13.6, label = "2014-2016\ndati non\ndisponibili",
           size = 2.8, colour = COL_GRIGIO_SCURO, family = "Source Sans Pro",
           vjust = 1, lineheight = 0.95) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  geom_text(data = lab03_val,
            aes(label = paste0("€ ", num_it(mld), " mld"),
                x = if_else(anno == 2008, anno - 0.3, anno),
                hjust = 1,
                vjust = if_else(anno == 2008, 0.5, -1.0)), size = 3.1, fontface = "bold", family = "Source Sans Pro",
            show.legend = FALSE) +
  geom_text(data = lab03_fine, aes(label = label), hjust = 0, nudge_x = 0.5,
            size = 3.4, fontface = "bold", family = "Source Sans Pro",
            lineheight = 0.95) +
  scale_colour_manual(values = pal_03) +
  scale_x_continuous(limits = c(2005.4, 2033.5), breaks = seq(2008, 2026, 3),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(2, 14, 2),
                     labels = function(x) paste0("€ ", x, " mld"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "La difesa spende sempre di più in armamenti e poco in manutenzione",
       subtitle = "Bilancio del ministero della Difesa, Funzione Difesa, per settore di spesa, a prezzi 2025, Italia, 2008-2026",
       caption = CAP_DIFESA) +
  theme_linechart(legend.position = "none")

ggsave(file.path(OUT, "03_funzione_difesa_settori_2008_2026.png"), p03,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# =============================================================================
# 04 — Treemap della spesa pubblica italiana, stima 2025 (Eurostat Cofog 2024 + conti 2025, vedi stima_cofog_2025.py)
# =============================================================================

d04 <- read_csv(file.path(IN, "stima_cofog_2025.csv"), show_col_types = FALSE) |>
  transmute(gruppo, voce = str_replace(voce, fixed("\\n"), "\n"), mld = mld_2025_stima) |>
  mutate(pct = mld / sum(mld) * 100,
         tipo = case_when(voce == "Difesa: investimenti" ~ "invest",
                          gruppo == "Difesa" ~ "difesa",
                          TRUE ~ "altro"))
write_csv(d04, file.path(OUT, "04_treemap_spesa_pubblica_2025.csv"))

grigi <- c("#C9C9C9", "#BDBDBD", "#D4D4D4", "#B3B3B3", "#C4C4C4", "#D9D9D9",
           "#B8B8B8", "#CECECE", "#C1C1C1", "#D1D1D1", "#BABABA")
d04 <- d04 |>
  mutate(fill = case_when(tipo == "invest" ~ COL_ARANCIO,
                          tipo == "difesa" ~ COL_BLU,
                          TRUE ~ grigi[row_number()]))

lay <- treemapify(d04, area = "mld", subgroup = "gruppo",
                  xlim = c(0, 10), ylim = c(0, 6.6)) |>
  mutate(cx = (xmin + xmax) / 2, cy = (ymin + ymax) / 2,
         w = xmax - xmin, h = ymax - ymin,
         lab = paste0(voce, "\n€ ", fmt_mld(mld), " mld · ",
                      if_else(pct >= 10, num_it(pct, 0), num_it(pct)), "%"))

grandi  <- lay |> filter(tipo == "altro", w > 1.1, h > 0.55)
piccoli <- lay |> filter(tipo == "altro", !(w > 1.1 & h > 0.55))
difesa  <- lay |> filter(tipo != "altro")
gruppo_difesa <- difesa |> summarise(xmin = min(xmin), xmax = max(xmax),
                                     ymin = min(ymin), ymax = max(ymax))

p04 <- ggplot(lay) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            colour = "white", linewidth = 0.6) +
  geom_rect(data = gruppo_difesa,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, colour = COL_NERO, linewidth = 0.5) +
  geom_text(data = grandi, aes(x = xmin + 0.08, y = ymax - 0.08, label = lab,
                               size = case_when(w < 1.4 ~ 2.3, w < 1.8 ~ 2.5, TRUE ~ 3.0)),
            hjust = 0, vjust = 1, colour = COL_NERO,
            family = "Source Sans Pro", fontface = "bold", lineheight = 0.95) +
  geom_text(data = piccoli,
            aes(x = xmin + 0.06, y = ymax - 0.06, label = str_replace(voce, "\n.*", "")),
            hjust = 0, vjust = 1, size = 2.4, colour = COL_NERO,
            family = "Source Sans Pro", fontface = "bold") +
  scale_fill_identity() +
  scale_size_identity() +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(title = "La difesa vale meno di tre euro ogni cento di spesa pubblica",
       subtitle = "Spesa delle amministrazioni pubbliche per funzione, in blu la difesa e in arancione i suoi investimenti, Italia, stima per il 2025",
       caption = "Elaborazione e stima di Lorenzo Ruffino su dati Eurostat") +
  theme_linechart(axis.line = element_blank(), axis.text = element_blank())

# Etichette della difesa fuori dal riquadro, a destra
lab_dif <- difesa |>
  mutate(testo = case_when(
    tipo == "difesa" ~ paste0("Difesa: stipendi\ne spese correnti\n€ ", fmt_mld(mld), " mld · ", num_it(pct), "%"),
    TRUE ~ paste0("Difesa: investimenti\nin armamenti, mezzi\ne infrastrutture\n€ ", fmt_mld(mld), " mld · ", num_it(pct), "%")),
    col = if_else(tipo == "difesa", COL_BLU, COL_ARANCIO))

x_lab <- 10.3
lab_dif <- lab_dif |> mutate(ty = cy)

p04 <- p04 +
  geom_segment(data = lab_dif, aes(x = xmax, y = cy, xend = x_lab - 0.06, yend = ty),
               colour = COL_GRIGIO_SCURO, linewidth = 0.3) +
  geom_text(data = lab_dif, aes(x = x_lab, y = ty, label = testo, colour = col),
            hjust = 0, vjust = 0.5, size = 3.0, fontface = "bold",
            family = "Source Sans Pro", lineheight = 0.95) +
  scale_colour_identity() +
  theme(plot.margin = unit(c(0.4, 4.2, 0.4, 0.4), "cm"))

ggsave(file.path(OUT, "04_treemap_spesa_pubblica_2025.png"), p04,
       width = 10, height = 7, dpi = 220, bg = "white")

# --- 04 bis: stessa mappa con gli aiuti militari all'Ucraina in rosso, nell'angolo in basso a destra della difesa (Kiel Institute, 2025) ---
UCRAINA_MLD <- 0.3   # Kiel Institute, Ukraine Support Tracker, febbraio 2026: aiuti militari stanziati nel 2025
COL_GIALLO_TESTO <- "#B07D00"
blu <- difesa |> filter(tipo == "difesa")
area_tot <- 10 * 6.6
lato <- sqrt(area_tot * UCRAINA_MLD / sum(d04$mld))
sq <- tibble(xmin = blu$xmax - lato, xmax = blu$xmax,
             ymin = blu$ymin, ymax = blu$ymin + lato)
lab_ucr <- tibble(x = x_lab, y = blu$ymin - 0.2,
                  testo = paste0("Aiuti militari\nall’Ucraina nel 2025\n€ ", num_it(UCRAINA_MLD), " mld · ",
                                 num_it(UCRAINA_MLD / sum(d04$mld) * 100, 2), "%"))
p04u <- p04 +
  geom_rect(data = sq, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = COL_ROSSO, colour = NA) +
  geom_segment(data = sq, aes(x = xmax, y = (ymin + ymax) / 2, xend = x_lab - 0.06, yend = lab_ucr$y),
               inherit.aes = FALSE, colour = COL_GRIGIO_SCURO, linewidth = 0.3) +
  geom_text(data = lab_ucr, aes(x = x, y = y, label = testo), inherit.aes = FALSE,
            hjust = 0, vjust = 0.5, size = 3.0, fontface = "bold", colour = COL_ROSSO,
            family = "Source Sans Pro", lineheight = 0.95) +
  labs(subtitle = "Spesa delle amministrazioni pubbliche per funzione, in blu la difesa, in arancione i suoi investimenti\ne in rosso gli aiuti militari all’Ucraina, Italia, stima per il 2025",
       caption = "Elaborazione e stima di Lorenzo Ruffino su dati Eurostat e Kiel Institute")
ggsave(file.path(OUT, "04_treemap_spesa_pubblica_2025_ucraina.png"), p04u,
       width = 10, height = 7.2, dpi = 220, bg = "white")

# =============================================================================
# 05 — Composizione della spesa per la difesa per voce economica, 2024
# =============================================================================

paesi_05 <- c("EU27_2020" = "Unione europea", "IT" = "Italia", "DE" = "Germania",
              "FR" = "Francia", "ES" = "Spagna", "PL" = "Polonia",
              "NL" = "Paesi Bassi", "SE" = "Svezia", "EL" = "Grecia")

d05 <- read_csv(file.path(IN, "cofog_def_all.csv"), show_col_types = FALSE,
                col_types = cols(.default = col_character(), time = col_integer(),
                                 value = col_double())) |>
  filter(unit == "MIO_EUR", cofog99 == "GF02", time == 2024,
         geo %in% names(paesi_05), na_item %in% c("TE", "D1", "P2", "P51G")) |>
  select(geo, na_item, value) |>
  pivot_wider(names_from = na_item, values_from = value) |>
  mutate(Stipendi = D1 / TE * 100, Esercizio = P2 / TE * 100,
         Investimenti = P51G / TE * 100,
         Altro = 100 - Stipendi - Esercizio - Investimenti,
         paese = paesi_05[geo])
write_csv(d05, file.path(OUT, "05_composizione_spesa_difesa_2024.csv"))

ordine_05 <- d05 |> arrange(desc(Esercizio)) |> pull(paese)

voci_05 <- c(Stipendi = "Stipendi",
             Esercizio = "Esercizio",
             Investimenti = "Investimenti",
             Altro = "Altro")
pal_05 <- setNames(c(COL_GRIGIO, COL_ROSSO, COL_BLU, "#D9D9D9"), voci_05)

d05l <- d05 |>
  select(paese, Stipendi, Esercizio, Investimenti, Altro) |>
  pivot_longer(-paese, names_to = "voce", values_to = "pct") |>
  mutate(voce = factor(voci_05[voce], levels = rev(voci_05)),
         paese = factor(paese, levels = ordine_05))

p05 <- ggplot(d05l, aes(pct, paese, fill = voce)) +
  geom_col(width = 0.72) +
  geom_text(data = filter(d05l, pct >= 5),
            aes(label = paste0(round(pct), "%"), group = voce,
                colour = if_else(voce == voci_05["Altro"], "nero", "bianco")),
            position = position_stack(vjust = 0.5), size = 3.1,
            fontface = "bold", family = "Source Sans Pro", show.legend = FALSE) +
  scale_fill_manual(values = pal_05, breaks = unname(voci_05)) +
  scale_colour_manual(values = c(nero = COL_NERO, bianco = "white")) +
  scale_x_continuous(expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "L’Italia spende poco per la manutenzione delle forze armate",
       subtitle = "Spesa pubblica per la difesa divisa per voce economica, in percentuale del totale, alcuni paesi europei, 2024\nL’esercizio comprende manutenzione e addestramento, gli investimenti armamenti e infrastrutture",
       caption = CAP_EUROSTAT) +
  theme_linechart(axis.text.x = element_blank(), axis.line = element_blank(),
                  legend.justification = "left", legend.location = "plot",
                  legend.text = element_text(size = 9.5, color = COL_NERO),
                  axis.text.y = element_text(
                    size = 10, color = COL_NERO, hjust = 1,
                    face = if_else(levels(d05l$paese) == "Italia", "bold", "plain")))

ggsave(file.path(OUT, "05_composizione_spesa_difesa_2024.png"), p05,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# =============================================================================
# 05 bis — Composizione della spesa militare secondo la definizione NATO, 2026
# =============================================================================

d05n <- read_csv(file.path(IN, "nato_categorie_2026.csv"), show_col_types = FALSE) |>
  select(paese, personale, altro, equipaggiamenti, infrastrutture)
write_csv(d05n, file.path(OUT, "05_composizione_spesa_nato_2026.csv"))

ordine_05n <- d05n |> arrange(desc(altro)) |> pull(paese)

voci_05n <- c(personale = "Personale",
              altro = "Operazioni e manutenzione",
              equipaggiamenti = "Armamenti",
              infrastrutture = "Infrastrutture")
pal_05n <- setNames(c(COL_GRIGIO, COL_ROSSO, COL_BLU, "#D9D9D9"), voci_05n)

d05nl <- d05n |>
  pivot_longer(-paese, names_to = "voce", values_to = "pct") |>
  mutate(voce = factor(voci_05n[voce], levels = rev(voci_05n)),
         paese = factor(paese, levels = ordine_05n))

p05n <- ggplot(d05nl, aes(pct, paese, fill = voce)) +
  geom_col(width = 0.72) +
  geom_text(data = filter(d05nl, pct >= 4.5),
            aes(label = paste0(round(pct), "%"), group = voce,
                colour = if_else(voce == voci_05n["infrastrutture"], "nero", "bianco")),
            position = position_stack(vjust = 0.5), size = 3.1,
            fontface = "bold", family = "Source Sans Pro", show.legend = FALSE) +
  scale_fill_manual(values = pal_05n, breaks = unname(voci_05n)) +
  scale_colour_manual(values = c(nero = COL_NERO, bianco = "white")) +
  scale_x_continuous(expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "L’Italia spende poco per la manutenzione delle forze armate",
       subtitle = "Spesa militare secondo la definizione Nato divisa per voce, in percentuale del totale, stime per il 2026\nIl personale comprende le pensioni, operazioni e manutenzione comprendono le altre spese di funzionamento",
       caption = "Elaborazione di Lorenzo Ruffino su dati Nato") +
  theme_linechart(axis.text.x = element_blank(), axis.line = element_blank(),
                  legend.justification = "left", legend.location = "plot",
                  legend.text = element_text(size = 9.5, color = COL_NERO),
                  axis.text.y = element_text(
                    size = 10, color = COL_NERO, hjust = 1,
                    face = if_else(levels(d05nl$paese) == "Italia", "bold", "plain")))

ggsave(file.path(OUT, "05_composizione_spesa_nato_2026.png"), p05n,
       width = 9, height = 5.6, dpi = 220, bg = "white")

# --- Controlli ---------------------------------------------------------------
cat("01 Italia 2015:", d01$pil_pct[d01$paese == "Italia" & d01$anno == 2015],
    " 2025:", d01$pil_pct[d01$paese == "Italia" & d01$anno == 2025], "\n")
cat("02 calo 2008-2015:", round((d02$mld_reali[d02$anno == 2015] / d02$mld_reali[d02$anno == 2008] - 1) * 100, 1), "%\n")
cat("04 quota difesa:", round(sum(d04$pct[d04$gruppo == "Difesa"]), 2), "%\n")
print(d05 |> select(paese, Stipendi, Esercizio, Investimenti, Altro) |> mutate(across(-paese, \(x) round(x, 1))))
