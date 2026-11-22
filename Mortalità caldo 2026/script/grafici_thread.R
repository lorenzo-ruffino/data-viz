# Grafici per il thread sui morti in più del caldo di maggio-giugno 2026.
# Usa le tabelle di 22_eccesso.R e 23_geografia.R (osservati corretti per
# completezza, attesi senza caldo anomalo).
#
# Output: output/thread/01_serie_giornaliera.png, 02_mappa_province.png,
#         03_eta.png, 04_caldo_eccesso_province.png

library(tidyverse)
library(sf)
library(showtext)
library(ggrepel)
library(patchwork)

RADICE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz"
setwd(file.path(RADICE, "Mortalità caldo 2026"))
dir.create("output/thread", showWarnings = FALSE)

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)
sf_use_s2(FALSE)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"
COL_FONDO  <- "#F2F2F2"
COL_VIOLA  <- "#A82DE3"
COL_VERDE  <- "#1B9E77"
CAP <- "Elaborazione di Lorenzo Ruffino su dati Istat e Copernicus"

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
                                  margin = margin(t = 0.5, unit = "cm"))
    ) +
    theme(...)
}

fmt_num <- function(x) format(round(x), big.mark = ".", decimal.mark = ",", trim = TRUE)

# ---- 01 Serie giornaliera ---------------------------------------------------------

gg <- read_csv("output/eccesso_italia_giornaliero.csv", show_col_types = FALSE) |>
  filter(data >= as.Date("2026-05-01"))
ondate <- tibble(inizio = as.Date(c("2026-05-21", "2026-06-17")),
                 fine = as.Date(c("2026-05-29", "2026-06-30")))
fondo <- geom_rect(data = ondate, aes(xmin = inizio - 0.5, xmax = fine + 0.5,
                                      ymin = -Inf, ymax = Inf),
                   inherit.aes = FALSE, fill = COL_FONDO)
asse_x <- scale_x_date(date_breaks = "1 week", date_labels = "%d/%m", expand = c(0, 0),
                       limits = as.Date(c("2026-04-30", "2026-07-01")))

p1a <- ggplot(gg, aes(data)) +
  fondo +
  geom_ribbon(aes(ymin = att_inf, ymax = att_sup), fill = "#CDEBE2") +
  geom_line(aes(y = atteso), color = COL_VERDE, linewidth = 0.7) +
  geom_line(aes(y = oss), color = COL_VIOLA, linewidth = 0.9) +
  annotate("text", x = as.Date("2026-05-02"), y = 1990, label = "Decessi osservati",
           hjust = 0, family = "Source Sans Pro", fontface = "bold", color = COL_VIOLA, size = 3.6) +
  annotate("text", x = as.Date("2026-05-02"), y = 1450,
           label = "Decessi attesi senza caldo anomalo", hjust = 0,
           family = "Source Sans Pro", fontface = "bold", color = COL_VERDE, size = 3.6) +
  annotate("text", x = as.Date("2026-05-25"), y = 2045, label = "21-29 maggio",
           family = "Source Sans Pro", color = "#5A5A5A", size = 3.2) +
  annotate("text", x = as.Date("2026-06-23") + 0.5, y = 2045, label = "17-30 giugno",
           family = "Source Sans Pro", color = "#5A5A5A", size = 3.2) +
  asse_x +
  scale_y_continuous(breaks = seq(1400, 2000, 200), labels = fmt_num) +
  coord_cartesian(ylim = c(1400, 2060), clip = "off") +
  theme_linechart(axis.line.x = element_blank()) +
  labs(title = "Con le ondate di calore è aumentata la mortalità",
       subtitle = paste0("Decessi giornalieri osservati e attesi senza caldo anomalo, Italia, maggio-giugno 2026\n",
                         "Sotto, anomalia della temperatura media rispetto agli stessi giorni del 2011-2025"))

p1b <- ggplot(gg, aes(data, anomalia)) +
  fondo +
  geom_col(aes(fill = anomalia > 0), width = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = COL_NERO) +
  scale_fill_manual(values = c(`TRUE` = COL_ROSSO, `FALSE` = COL_BLU), guide = "none") +
  asse_x +
  scale_y_continuous(breaks = c(-2, 0, 2, 4, 6),
                     labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "°")) +
  theme_linechart(axis.line = element_blank()) +
  labs(caption = paste0(CAP, " (dati 2026 provvisori)"))

p1 <- p1a / p1b + plot_layout(heights = c(3, 1.1))
ggsave("output/thread/01_serie_giornaliera.png", p1, width = 9, height = 6.5, dpi = 220, bg = "white")

# ---- 02 Mappa per provincia, 17-30 giugno ------------------------------------------

comuni <- read_csv("input/derivati/comuni.csv", locale = locale(encoding = "latin1"),
                   col_types = cols(cod_comune = "c"))
ecc_prov <- read_csv("output/eccesso_province.csv", show_col_types = FALSE)

prov_geo <- readRDS(file.path(RADICE, "Temperature Copernicus/input/geo/geo_province_2025.rds")) |>
  st_transform(4326) |>
  mutate(cod_prov = ifelse(COD_UTS > 200, COD_UTS - 200L, COD_UTS)) |>
  filter(cod_prov %in% comuni$cod_prov) |>
  select(cod_prov)
com_geo <- read_sf(file.path(RADICE, "Referendum_Giustizia_2026/utilities/Com01012026_g_WGS84.json")) |>
  st_make_valid()
sardegna <- com_geo |>
  inner_join(comuni |> filter(cod_reg == 20) |> select(cod_comune, cod_prov),
             by = c("PRO_COM_T" = "cod_comune")) |>
  group_by(cod_prov) |>
  summarise(.groups = "drop")
province <- bind_rows(prov_geo, sardegna)
stopifnot(nrow(province) == 110)
regioni <- read_sf(file.path(RADICE, "Temperature Copernicus/input/geo/Reg01012025_g_WGS84.json")) |>
  st_make_valid()

bin_levels <- c("meno di -10%", "da -10% a 0", "da 0 a +10%", "da +10% a +20%",
                "da +20% a +30%", "+30% e oltre")
bin_colours <- c("meno di -10%" = "#5C9CDE", "da -10% a 0" = "#D6E6F7",
                 "da 0 a +10%" = "#FCE4E7", "da +10% a +20%" = "#F6A2AA",
                 "da +20% a +30%" = "#F12938", "+30% e oltre" = "#8E1622")

d2 <- ecc_prov |>
  filter(periodo == "17-30 giugno") |>
  mutate(bin = factor(case_when(
    ecc_perc < -10 ~ bin_levels[1], ecc_perc < 0 ~ bin_levels[2],
    ecc_perc < 10 ~ bin_levels[3], ecc_perc < 20 ~ bin_levels[4],
    ecc_perc < 30 ~ bin_levels[5], TRUE ~ bin_levels[6]), levels = bin_levels))
stopifnot(nrow(d2) == 110)
md <- province |> left_join(d2, by = "cod_prov")

p2 <- ggplot() +
  geom_sf(data = md, aes(fill = bin), color = "white", linewidth = 0.12) +
  geom_sf(data = regioni, fill = NA, color = "#2B2B2B", linewidth = 0.4) +
  scale_fill_manual(values = bin_colours, drop = FALSE, name = NULL) +
  guides(fill = guide_legend(reverse = TRUE, keyheight = unit(0.6, "cm"),
                             keywidth = unit(0.45, "cm"))) +
  coord_sf(ylim = c(36.6, 47.1), expand = FALSE) +
  theme_linechart(axis.line = element_blank(), axis.text = element_blank(),
                  legend.position = c(0.99, 0.92), legend.justification = c(1, 1),
                  legend.text = element_text(size = 10, color = COL_NERO, hjust = 0)) +
  labs(title = "La mortalità è salita soprattutto nel Nord-Ovest",
       subtitle = paste0("Differenza tra decessi osservati e attesi senza caldo anomalo,\n",
                         "per provincia, 17-30 giugno 2026"),
       caption = "Elaborazione di Lorenzo Ruffino su dati Istat (provvisori)")
ggsave("output/thread/02_mappa_province.png", p2, width = 8, height = 9, dpi = 220, bg = "white")

# ---- 03 Età, 17-30 giugno -----------------------------------------------------

es <- read_csv("output/eccesso_eta_sesso.csv", show_col_types = FALSE) |>
  filter(periodo == "17-30 giugno", eta != "tutte", sesso == "entrambi") |>
  mutate(eta = factor(eta, levels = c("0-64", "65-74", "75-84", "85+"),
                      labels = c("Sotto i 65 anni", "65-74 anni", "75-84 anni", "85 anni e più")),
         etichetta = paste0("+", fmt_num(eccesso), "\n(+", round(ecc_perc), "%)"))

p3 <- ggplot(es, aes(eta, eccesso)) +
  geom_col(fill = COL_ROSSO, width = 0.65) +
  geom_text(aes(label = etichetta), vjust = -0.35, family = "Source Sans Pro",
            fontface = "bold", size = 3.8, color = COL_NERO, lineheight = 0.95) +
  scale_y_continuous(limits = c(0, 1100), breaks = seq(200, 1000, 200), expand = c(0, 0),
                     labels = function(x) paste0("+", fmt_num(x))) +
  coord_cartesian(clip = "off") +
  theme_linechart(axis.line.y = element_blank(), axis.text.x = element_text(size = 10)) +
  labs(title = "Quasi nove morti in più su dieci avevano più di 75 anni",
       subtitle = paste0("Differenza tra decessi osservati e attesi senza caldo anomalo, ",
                         "per classe di età, Italia, 17-30 giugno 2026"),
       caption = "Elaborazione di Lorenzo Ruffino su dati Istat (provvisori)")
ggsave("output/thread/03_eta.png", p3, width = 8, height = 6.5, dpi = 220, bg = "white")

# ---- 04 Anomalia di temperatura ed eccesso per provincia, 17-30 giugno --------------

gp <- read_csv("output/geografia_province.csv", show_col_types = FALSE) |>
  filter(periodo == "17-30 giugno") |>
  mutate(ecc = 100 * (oe - 1))
etich <- gp |> filter(provincia %in% c("Torino", "Milano", "Roma", "Napoli", "Palermo",
                                       "Novara", "Genova", "Bari", "Firenze", "Venezia"))

p4 <- ggplot(gp, aes(an_tmean, ecc)) +
  geom_hline(yintercept = 0, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_smooth(aes(weight = atteso), method = "lm", se = FALSE, color = COL_NERO,
              linewidth = 0.7, linetype = "dashed") +
  geom_point(aes(size = atteso), color = COL_ROSSO, alpha = 0.75, stroke = 0) +
  geom_text_repel(data = etich, aes(label = provincia), family = "Source Sans Pro",
                  fontface = "bold", size = 3.4, color = COL_NERO, seed = 1,
                  box.padding = 0.45, min.segment.length = 0, segment.colour = COL_GRIGIO) +
  scale_size_area(max_size = 9, guide = "none") +
  scale_x_continuous(breaks = 1:6, labels = function(x) paste0("+", x, "°")) +
  scale_y_continuous(breaks = seq(-20, 40, 10),
                     labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  theme_linechart(axis.title.x = element_text(size = 10, color = COL_NERO, hjust = 0.5,
                                              margin = margin(t = 0.3, unit = "cm")),
                  axis.title.y = element_text(size = 10, color = COL_NERO, hjust = 0.5,
                                              angle = 90, margin = margin(r = 0.3, unit = "cm"))) +
  labs(title = "Dove ha fatto più caldo sono morte più persone",
       subtitle = paste0("Anomalia della temperatura media e differenza tra decessi osservati e attesi, ",
                         "per provincia, 17-30 giugno 2026\nL'area dei cerchi è proporzionale ai ",
                         "decessi attesi, la retta tratteggiata è la regressione pesata"),
       x = "Anomalia della temperatura media rispetto al 2011-2025",
       y = "Decessi in più rispetto all'atteso",
       caption = CAP)
ggsave("output/thread/04_caldo_eccesso_province.png", p4, width = 9, height = 6.5, dpi = 220, bg = "white")

# ---- Controlli -------------------------------------------------------------------

cat("Eccesso 17-30 giugno (somma giornaliera):",
    fmt_num(sum(gg$eccesso[gg$data >= as.Date("2026-06-17")])), "\n")
print(es |> select(eta, eccesso, ecc_perc))
cat("Quota over 75:", round(100 * sum(es$eccesso[3:4]) / sum(es$eccesso)), "%\n")
cat("Pendenza ponderata (punti % per grado):",
    round(coef(lm(ecc ~ an_tmean, data = gp, weights = atteso))[2], 1), "\n")
