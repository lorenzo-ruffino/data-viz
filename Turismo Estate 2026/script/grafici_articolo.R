# Turismo Estate 2026 — grafici per l'articolo
# Dati: Eurostat tour_occ_nim / tour_occ_arm (scaricati con opensdmx in ../input)
#       Istat 122_54_DF_DCSC_TUR_5 (paese di provenienza, mensile)

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
library(tidyverse)
library(showtext)
library(sf)

setwd("/Users/lorenzoruffino/Documents/Progetti/data-viz/Turismo Estate 2026/script")
dir.create("../output", showWarnings = FALSE)

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

COL_NERO   <- "#1C1C1C"
COL_BLU    <- "#0478EA"
COL_ROSSO  <- "#F12938"
COL_GRIGIO <- "#9A9A9A"

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
CAP_ISTAT    <- "Elaborazione di Lorenzo Ruffino su dati Istat"

fmt_num <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")

# --- Dati -------------------------------------------------------------------

leggi_eurostat <- function(f) {
  read_csv(f, col_types = cols(.default = col_character(), OBS_VALUE = col_double())) |>
    filter(!is.na(OBS_VALUE)) |>
    transmute(geo, c_resid,
              anno = as.integer(substr(TIME_PERIOD, 1, 4)),
              mese = as.integer(substr(TIME_PERIOD, 6, 7)),
              valore = OBS_VALUE)
}

# Somma giugno-agosto solo quando ci sono tutti e tre i mesi
estate <- function(df) {
  df |>
    filter(mese %in% 6:8) |>
    group_by(geo, c_resid, anno) |>
    summarise(n = n(), valore = sum(valore), .groups = "drop") |>
    filter(n == 3) |>
    select(-n)
}

notti        <- leggi_eurostat("../input/eurostat_tour_occ_nim.csv")
arrivi       <- leggi_eurostat("../input/eurostat_tour_occ_arm.csv")
notti_hotel  <- leggi_eurostat("../input/eurostat_tour_occ_nim_hotel.csv")
arrivi_hotel <- leggi_eurostat("../input/eurostat_tour_occ_arm_hotel.csv")

estate_notti        <- estate(notti)
estate_arrivi       <- estate(arrivi)
estate_notti_hotel  <- estate(notti_hotel)
estate_arrivi_hotel <- estate(arrivi_hotel)

# Tabella riassuntiva Italia
italia <- estate_notti |> filter(geo == "IT") |> rename(notti = valore) |>
  left_join(estate_arrivi |> filter(geo == "IT") |> rename(arrivi = valore),
            by = c("geo", "c_resid", "anno")) |>
  left_join(estate_notti_hotel |> filter(geo == "IT") |> rename(notti_hotel = valore),
            by = c("geo", "c_resid", "anno")) |>
  left_join(estate_arrivi_hotel |> filter(geo == "IT") |> rename(arrivi_hotel = valore),
            by = c("geo", "c_resid", "anno")) |>
  mutate(notti_per_arrivo = notti / arrivi) |>
  arrange(c_resid, anno)
write_csv(italia, "../output/italia_estate.csv")

# --- 01 Pernottamenti estivi in Italia, totale ------------------------------

d1 <- italia |>
  filter(c_resid == "TOTAL", anno >= 2000) |>
  mutate(evidenza = anno == 2026)

p1 <- ggplot(d1, aes(x = anno, y = notti / 1e6)) +
  geom_col(aes(fill = evidenza), width = 0.75, show.legend = FALSE) +
  geom_text(aes(label = round(notti / 1e6)), vjust = 1.6, color = "white",
            size = 2.4, family = "Source Sans Pro", fontface = "bold") +
  geom_vline(xintercept = 2024.5, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2024.3, y = 268, label = "Dal 2025 inclusi\ngli alloggi privati",
           hjust = 1, vjust = 1, size = 3, color = "#5A5A5A", family = "Source Sans Pro",
           lineheight = 0.95) +
  scale_fill_manual(values = c("TRUE" = COL_ROSSO, "FALSE" = COL_BLU)) +
  scale_y_continuous(breaks = seq(50, 250, 50), labels = function(x) paste0(x, " mln"),
                     limits = c(0, 270), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  labs(title = "Un'altra estate da record per il turismo in Italia",
       subtitle = "Pernottamenti nelle strutture ricettive tra giugno e agosto, Italia, 2000-2026",
       caption = CAP_EUROSTAT)

ggsave("../output/01_pernottamenti_estate_italia.png", p1,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 02 Italiani contro stranieri, tutte le strutture ---------------------

d2 <- italia |>
  filter(c_resid %in% c("DOM", "FOR"), anno >= 2011) |>
  mutate(serie = if_else(c_resid == "DOM", "Turisti italiani", "Turisti stranieri"))

lab2 <- d2 |> filter(anno == max(anno)) |>
  mutate(etichetta = paste0(serie, ": ", fmt_num(notti / 1e6), " mln"))
val2 <- d2 |> filter(anno == 2011)

p2 <- ggplot(d2, aes(x = anno, y = notti / 1e6, color = serie)) +
  geom_vline(xintercept = 2024.5, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2024.3, y = 158, label = "Dal 2025 inclusi\ngli alloggi privati",
           hjust = 1, vjust = 1, size = 3, color = "#5A5A5A", family = "Source Sans Pro",
           lineheight = 0.95) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  geom_text(data = lab2, aes(label = etichetta), hjust = 0, nudge_x = 0.35,
            vjust = ifelse(lab2$c_resid == "DOM", 1.3, -0.5),
            size = 3.6, fontface = "bold", family = "Source Sans Pro") +
  geom_text(data = val2,
            aes(label = paste0(fmt_num(notti / 1e6), " mln"),
                vjust = ifelse(c_resid == "DOM", -1.2, 1.9)),
            hjust = 0,
            size = 3.2, fontface = "bold", family = "Source Sans Pro", show.legend = FALSE) +
  scale_color_manual(values = c("Turisti italiani" = COL_BLU, "Turisti stranieri" = COL_ROSSO)) +
  scale_y_continuous(breaks = seq(25, 150, 25), labels = function(x) paste0(x, " mln"),
                     limits = c(0, 160), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(2011, 2026, 3), limits = c(2010.6, 2033.6),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "In Italia i turisti stranieri hanno superato quelli italiani",
       subtitle = "Pernottamenti nelle strutture ricettive tra giugno e agosto per residenza dei clienti, Italia, 2011-2026",
       caption = CAP_EUROSTAT)

ggsave("../output/02_italiani_stranieri.png", p2,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- 03 Mappa Europa: variazione estate 2026 su 2025 -----------------------

var_paesi <- estate_notti |>
  filter(c_resid == "TOTAL", anno %in% c(2025, 2026)) |>
  pivot_wider(names_from = anno, values_from = valore, names_prefix = "a") |>
  filter(!is.na(a2025), !is.na(a2026)) |>
  mutate(var = (a2026 / a2025 - 1) * 100)
write_csv(var_paesi, "../output/variazione_paesi_estate_2026.csv")

geo <- load_geo_europa()
geo_dati <- geo |>
  left_join(var_paesi |> filter(!grepl("^EU|^EA", geo)) |> select(CNTR_ID = geo, var),
            by = "CNTR_ID")

bin_levels <- c("meno del -4%", "da -4 a -2%", "da -2 a 0%",
                "da 0 a 2%", "da 2 a 4%", "da 4 a 6%", "6% e oltre")
bin_colours <- c(
  "meno del -4%" = "#A01B26",
  "da -4 a -2%"  = "#F12938",
  "da -2 a 0%"   = "#F9B4B9",
  "da 0 a 2%"    = "#D6E6F7",
  "da 2 a 4%"    = "#7FB0E6",
  "da 4 a 6%"    = "#2C7BD0",
  "6% e oltre"   = "#06366A"
)
bin_scuri <- c("meno del -4%", "da -4 a -2%", "da 4 a 6%", "6% e oltre")

geo_dati <- geo_dati |>
  mutate(bin = factor(case_when(
    is.na(var) ~ NA_character_,
    var < -4 ~ "meno del -4%",
    var < -2 ~ "da -4 a -2%",
    var <  0 ~ "da -2 a 0%",
    var <  2 ~ "da 0 a 2%",
    var <  4 ~ "da 2 a 4%",
    var <  6 ~ "da 4 a 6%",
    TRUE     ~ "6% e oltre"
  ), levels = bin_levels))

adjust_label_xy <- function(cntr, x, y) {
  dx <- case_when(
    cntr == "NO" ~ -150000, cntr == "SE" ~ -100000, cntr == "FI" ~ -120000,
    cntr == "EL" ~  -60000, cntr == "IT" ~  -20000, cntr == "LV" ~  100000,
    cntr == "NL" ~   10000, cntr == "HR" ~   60000, cntr == "ME" ~   12000,
    cntr == "RS" ~   30000, cntr == "MK" ~   45000, cntr == "CY" ~  -30000,
    cntr == "SI" ~   -5000, cntr == "AL" ~  -95000, cntr == "LU" ~   55000,
    TRUE ~ 0)
  dy <- case_when(
    cntr == "NO" ~ -300000, cntr == "FI" ~ -250000, cntr == "SE" ~ -120000,
    cntr == "IE" ~  -80000, cntr == "BE" ~   25000, cntr == "HR" ~   75000,
    cntr == "EL" ~   30000, cntr == "MT" ~   25000, cntr == "SI" ~   10000,
    cntr == "ME" ~    5000, cntr == "AL" ~  -40000, cntr == "RS" ~   20000,
    cntr == "MK" ~  -35000, cntr == "BA" ~   10000, cntr == "LU" ~  -15000,
    TRUE ~ 0)
  list(x = x + dx, y = y + dy)
}

fmt_pct <- function(v) {
  s <- formatC(abs(v), format = "f", digits = 1, decimal.mark = ",")
  segno <- ifelse(round(v, 1) > 0, "+", ifelse(round(v, 1) < 0, "-", ""))
  paste0(segno, s, "%")
}

geo_labels <- geo_dati |>
  filter(!is.na(var), !CNTR_ID %in% c("LI")) |>
  mutate(label_value = fmt_pct(var),
         label_color = if_else(bin %in% bin_scuri & !CNTR_ID %in% c("LU", "AL"), "white", COL_NERO))
coords <- st_coordinates(mainland_centroids(geo_labels))
adj <- adjust_label_xy(geo_labels$CNTR_ID, coords[, "X"], coords[, "Y"])
geo_labels$label_x <- adj$x
geo_labels$label_y <- adj$y

p3 <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
  geom_text(data = st_drop_geometry(geo_labels),
            aes(x = label_x, y = label_y, label = label_value, color = label_color),
            family = "Source Sans Pro", size = 2.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(values = bin_colours, drop = FALSE, na.value = COL_NA_MAPPA,
                    name = NULL, breaks = bin_levels) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.75, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0))) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")], ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.98, 0.72), legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(title = "Gli altri grandi paesi turistici sono fermi, l'Italia cresce",
       subtitle = "Variazione dei pernottamenti nelle strutture ricettive tra giugno e agosto, 2026 rispetto al 2025",
       caption = CAP_EUROSTAT)

ggsave("../output/03_mappa_variazione_europa.png", p3,
       width = 9, height = 9, dpi = 220, bg = "white")

# --- Dati per il testo: da dove arrivano gli stranieri (estate 2025) -------

nomi_paesi <- c(
  DE = "Germania", US = "Stati Uniti", FR = "Francia", NL = "Paesi Bassi",
  UK = "Regno Unito", CH_LI = "Svizzera", AT = "Austria", PL = "Polonia",
  BE = "Belgio", ES = "Spagna", CZ = "Repubblica Ceca", DK = "Danimarca"
)

istat5 <- read_csv("../input/istat_122_54_DF_DCSC_TUR_5.csv",
                   col_types = cols(.default = col_character(), OBS_VALUE = col_double())) |>
  filter(!is.na(OBS_VALUE), TYPE_ACCOMMODATION == "ALL", DATA_TYPE == "NI") |>
  mutate(anno = as.integer(substr(TIME_PERIOD, 1, 4)),
         mese = as.integer(substr(TIME_PERIOD, 6, 7))) |>
  filter(mese %in% 6:8, anno == 2025) |>
  group_by(paese = COUNTRY_RES_GUESTS) |>
  summarise(notti = sum(OBS_VALUE), .groups = "drop")

tot_stranieri <- istat5$notti[istat5$paese == "WRL_X_ITA"]
d4 <- istat5 |>
  filter(paese %in% names(nomi_paesi)) |>
  mutate(nome = nomi_paesi[paese], quota = notti / tot_stranieri * 100) |>
  arrange(notti) |>
  mutate(nome = factor(nome, levels = nome),
         evidenza = paese == "DE")
write_csv(d4, "../output/provenienza_stranieri_estate_2025.csv")

# --- 04 Prezzi delle vacanze e stipendi (2019 = 100) ------------------------
# Eurostat prc_hicp_aind (IPCA Italia) + retribuzioni contrattuali Istat,
# totale economia (copiate da "CCNL in termini reali")

hicp <- read_csv("../input/eurostat_prc_hicp_aind.csv",
                 col_types = cols(.default = col_character(), OBS_VALUE = col_double())) |>
  filter(geo == "IT", coicop %in% c("CP00", "CP096", "CP112")) |>
  transmute(serie = coicop, anno = as.integer(TIME_PERIOD), valore = OBS_VALUE)
stipendi <- read_csv("../input/istat_retribuzioni_contrattuali_totale.csv", show_col_types = FALSE) |>
  transmute(serie = "STIP", anno, valore = retribuzioni_contrattuali_nominali_2005_100)

nomi4 <- c(CP112 = "Prezzi di alberghi e alloggi",
           CP096 = "Prezzi dei pacchetti vacanza",
           CP00  = "Prezzi al consumo",
           STIP  = "Stipendi contrattuali")
col4 <- c(CP112 = COL_ROSSO, CP096 = "#F2A900", CP00 = COL_NERO, STIP = COL_BLU)

d5 <- bind_rows(hicp, stipendi) |>
  filter(anno >= 2019, anno <= 2025) |>
  group_by(serie) |>
  mutate(indice = valore / valore[anno == 2019] * 100) |>
  ungroup()
write_csv(d5, "../output/prezzi_vacanze_stipendi.csv")

lab5 <- d5 |> filter(anno == 2025) |>
  mutate(etichetta = paste0(nomi4[serie], ": ", fmt_num(indice, 0)))

p4 <- ggplot(d5, aes(x = anno, y = indice, color = serie)) +
  geom_hline(yintercept = 100, colour = COL_GRIGIO, linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  ggrepel::geom_text_repel(data = lab5, aes(label = etichetta), hjust = 0, nudge_x = 0.2,
                           direction = "y", size = 3.6, fontface = "bold",
                           family = "Source Sans Pro", segment.colour = NA,
                           box.padding = 0.2, seed = 1) +
  scale_color_manual(values = col4) +
  scale_y_continuous(breaks = seq(90, 140, 10), limits = c(90, 142)) +
  scale_x_continuous(breaks = 2019:2025, limits = c(2019, 2028.6), expand = c(0.01, 0)) +
  coord_cartesian(clip = "off") +
  theme_linechart() +
  theme(legend.position = "none") +
  labs(title = "In Italia le vacanze sono rincarate più degli stipendi",
       subtitle = "Indice dei prezzi al consumo armonizzato e retribuzioni contrattuali nominali, 2019 = 100, Italia, 2019-2025",
       caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat e Istat")

ggsave("../output/04_prezzi_vacanze_stipendi.png", p4,
       width = 9, height = 6.5, dpi = 220, bg = "white")

# --- Sanity check -----------------------------------------------------------

tot <- italia |> filter(c_resid == "TOTAL", anno %in% 2024:2026)
cat("Pernottamenti estivi totali (mln):", fmt_num(tot$notti / 1e6), "\n")
cat("Alberghi 2026 italiani/stranieri (mln):",
    fmt_num(italia$notti_hotel[italia$anno == 2026 & italia$c_resid == "DOM"] / 1e6),
    fmt_num(italia$notti_hotel[italia$anno == 2026 & italia$c_resid == "FOR"] / 1e6), "\n")
cat("Variazione IT / UE:",
    fmt_pct(var_paesi$var[var_paesi$geo == "IT"]), fmt_pct(var_paesi$var[var_paesi$geo == "EU27_2020"]), "\n")
cat("Quota Germania sugli stranieri 2025:", fmt_num(d4$quota[d4$paese == "DE"]), "%\n")
