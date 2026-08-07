# Quota di insegnanti con almeno 50 anni in Europa, ultimo anno disponibile.
# Eseguito da `cd script && Rscript insegnanti_anziani_europa.R`.
#
# Mappa binned con classi costanti di 5 punti e scale_fill_manual, etichetta
# del valore su ogni paese. Fonte: Eurostat educ_uoe_perp01 (insegnanti delle
# scuole primarie e secondarie, ISCED 1-3, fascia di eta Y_GE50 su TOTAL).

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(eurostat)
  library(tidyverse)
  library(showtext)
  library(sf)
})

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

# --- Dati: quota di insegnanti (ISCED 1-3) con almeno 50 anni ---------------

geo <- load_geo_europa()                       # 38 paesi, EPSG:3035

raw <- get_eurostat("educ_uoe_perp01", time_format = "num",
                    cache = FALSE, update_cache = TRUE)

base <- raw %>% filter(unit == "NR", isced11 %in% c("ED1", "ED2", "ED3"),
                       sex == "T")

# Ultimo anno con copertura ampia della fascia 50+ (almeno 25 paesi).
ultimo_anno <- base %>%
  filter(age == "Y_GE50", nchar(geo) == 2) %>%
  count(TIME_PERIOD) %>%
  filter(n >= 25) %>%
  pull(TIME_PERIOD) %>% max()

tot <- base %>% filter(age == "TOTAL", TIME_PERIOD == ultimo_anno) %>%
  group_by(geo) %>% summarise(tot = sum(values, na.rm = TRUE), .groups = "drop")
g50 <- base %>% filter(age == "Y_GE50", TIME_PERIOD == ultimo_anno) %>%
  group_by(geo) %>% summarise(g50 = sum(values, na.rm = TRUE), .groups = "drop")

paesi <- inner_join(tot, g50, by = "geo") %>%
  filter(g50 > 0, geo %in% paesi_europa_mappa) %>%   # g50==0 = fascia non riportata
  mutate(anno = ultimo_anno, valore = g50 / tot * 100) %>%
  select(CNTR_ID = geo, anno, valore)

# Media UE27 pubblicata da Eurostat (aggregato ponderato).
eu_row <- base %>% filter(geo == "EU27_2020", TIME_PERIOD == ultimo_anno)
media_ue <- sum(eu_row$values[eu_row$age == "Y_GE50"], na.rm = TRUE) /
            sum(eu_row$values[eu_row$age == "TOTAL"], na.rm = TRUE) * 100

cat("Ultimo anno:", ultimo_anno, "\n")
cat("Media UE27 (ponderata, EU27_2020):", round(media_ue, 1), "\n")
cat("Italia:", round(paesi$valore[paesi$CNTR_ID == "IT"], 1), "\n")
cat("Range:", paste(round(range(paesi$valore), 1), collapse = " - "), "\n")
cat("Paesi senza dato:",
    paste(setdiff(paesi_europa_mappa, paesi$CNTR_ID), collapse = ", "), "\n")

geo_dati <- geo %>% left_join(paesi, by = "CNTR_ID")

# Esporta dato pulito
write_csv(
  st_drop_geometry(geo_dati) %>%
    select(CNTR_ID, paese = NAME_ENGL, anno, quota_insegnanti_50plus = valore) %>%
    filter(!is.na(quota_insegnanti_50plus)) %>%
    arrange(desc(quota_insegnanti_50plus)),
  "../output/insegnanti_anziani_europa.csv"
)

# --- Binning discreto (classi costanti di 5 punti) -------------------------

bin_levels <- c("< 25", "da 25 a 30", "da 30 a 35", "da 35 a 40",
                "da 40 a 45", "da 45 a 50", "da 50 a 55", "≥ 55")
# Rampa blu monocromatica a 8 passi, dal chiaro (valori bassi) allo scuro.
bin_colours <- setNames(
  colorRampPalette(c("#EAF1FA", "#A1C6EE", "#5C9CDE", "#0E5BAD", "#06366A"))(8),
  bin_levels
)
bin_scuri <- c("da 40 a 45", "da 45 a 50",
               "da 50 a 55", "≥ 55")             # → etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <  25  ~ "< 25",
    valore <  30  ~ "da 25 a 30",
    valore <  35  ~ "da 30 a 35",
    valore <  40  ~ "da 35 a 40",
    valore <  45  ~ "da 40 a 45",
    valore <  50  ~ "da 45 a 50",
    valore <  55  ~ "da 50 a 55",
    TRUE          ~ "≥ 55"
  ), levels = bin_levels))

# --- Etichetta valore per paese (percentuale intera) -----------------------

fmt_val <- function(v) formatC(v, format = "f", digits = 0, decimal.mark = ",")

# Micro-stati senza dato o con label che collide: nessuna etichetta.
skip_label <- c("LI")

# Aggiustamenti manuali del centroide (metri EPSG:3035). dx>0 = est, dy>0 = nord.
adjust_label_xy <- function(cntr, x, y) {
  dx <- case_when(
    cntr == "NO" ~ -150000,
    cntr == "SE" ~ -100000,
    cntr == "FI" ~ -120000,
    cntr == "EL" ~  -60000,
    cntr == "IT" ~  -20000,
    cntr == "LV" ~  100000,
    cntr == "NL" ~   10000,
    cntr == "HR" ~  -55000,
    cntr == "ME" ~   12000,
    cntr == "RS" ~   30000,
    cntr == "CY" ~  -30000,
    TRUE         ~  0
  )
  dy <- case_when(
    cntr == "NO" ~ -300000,
    cntr == "FI" ~ -250000,
    cntr == "SE" ~ -120000,
    cntr == "IE" ~  -80000,
    cntr == "BE" ~   25000,
    cntr == "HR" ~   70000,
    cntr == "EL" ~   30000,
    cntr == "MT" ~   25000,
    cntr == "SI" ~   35000,
    cntr == "NL" ~   45000,
    cntr == "ME" ~  -30000,
    cntr == "AL" ~  -25000,
    cntr == "RS" ~   20000,
    TRUE         ~  0
  )
  list(x = x + dx, y = y + dy)
}

geo_labels <- geo_dati %>%
  filter(!is.na(valore), !CNTR_ID %in% skip_label) %>%
  mutate(
    label_value = fmt_val(valore),
    label_color = case_when(
      CNTR_ID == "MT"    ~ "#1C1C1C",
      bin %in% bin_scuri ~ "white",
      TRUE               ~ "#1C1C1C"
    )
  )

centroidi <- mainland_centroids(geo_labels)
coords    <- st_coordinates(centroidi)
geo_labels$label_x <- coords[, "X"]
geo_labels$label_y <- coords[, "Y"]
adj <- adjust_label_xy(geo_labels$CNTR_ID, geo_labels$label_x, geo_labels$label_y)
geo_labels$label_x <- adj$x
geo_labels$label_y <- adj$y

# --- Mappa ------------------------------------------------------------------

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
  geom_text(data = st_drop_geometry(geo_labels),
            aes(x = label_x, y = label_y,
                label = label_value, color = label_color),
            family = "Source Sans Pro", size = 2.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(
    values = bin_colours,
    drop = FALSE,
    na.value = COL_NA_MAPPA,
    name = NULL,
    breaks = bin_levels        # nasconde NA dalla legenda senza toglierlo dal plot
  ) +
  guides(fill = guide_legend(
    reverse = TRUE,             # valori alti (blu scuro) in cima, bassi in fondo
    keyheight = unit(0.7, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")],
           ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.86),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "In Italia oltre metà degli insegnanti ha almeno 50 anni",
    subtitle = paste0(
      "Quota di insegnanti delle scuole primarie e secondarie con almeno ",
      "50 anni, paesi europei, ", ultimo_anno),
    caption = CAP_EUROSTAT
  )

ggsave("../output/insegnanti_anziani_europa.png",
       plot = p, width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/insegnanti_anziani_europa.png\n")
