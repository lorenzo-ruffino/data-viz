# Quota di persone che non hanno mai usato internet, paesi europei, ultimo anno.
# Eseguito da `cd script && Rscript uso_internet_europa.R`.
#
# Mappa binned con bin discreti e scale_fill_manual, etichetta del valore su
# ogni paese. Fonte: Eurostat isoc_r_iuse_i (indic_is I_IUX, unit PC_IND).

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

# --- Dati: persone che non hanno mai usato internet, ultimo anno -----------

geo <- load_geo_europa()                       # 38 paesi, EPSG:3035

net <- get_eurostat("isoc_r_iuse_i", time_format = "num") %>%
  filter(unit == "PC_IND", indic_is == "I_IUX", nchar(geo) == 2)

ultimo_anno <- max(net$TIME_PERIOD, na.rm = TRUE)

net_paesi <- net %>%
  filter(TIME_PERIOD == ultimo_anno, geo %in% paesi_europa_mappa) %>%
  select(CNTR_ID = geo, anno = TIME_PERIOD, valore = values)

# Eurostat non pubblica l'aggregato EU27 per questo indicatore regionale:
# media semplice dei 27 paesi UE.
media_ue <- net %>%
  filter(TIME_PERIOD == ultimo_anno,
         geo %in% c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI",
                    "FR","HR","HU","IE","IT","LT","LU","LV","MT","NL","PL",
                    "PT","RO","SE","SI","SK")) %>%
  summarise(m = mean(values, na.rm = TRUE)) %>% pull(m)

cat("Ultimo anno:", ultimo_anno, "\n")
cat("Media UE27 (semplice):", round(media_ue, 1), "\n")
cat("Italia:", net_paesi$valore[net_paesi$CNTR_ID == "IT"], "\n")
cat("Range:", paste(round(range(net_paesi$valore), 1), collapse = " - "), "\n")
cat("Paesi senza dato:",
    paste(setdiff(paesi_europa_mappa, net_paesi$CNTR_ID), collapse = ", "), "\n")

geo_dati <- geo %>% left_join(net_paesi, by = "CNTR_ID")

# Esporta dato pulito
write_csv(
  st_drop_geometry(geo_dati) %>%
    select(CNTR_ID, paese = NAME_ENGL, anno, mai_usato_internet = valore) %>%
    filter(!is.na(mai_usato_internet)) %>%
    arrange(desc(mai_usato_internet)),
  "../output/uso_internet_europa.csv"
)

# --- Binning discreto (classi costanti di 1 punto) -------------------------

bin_levels <- c("≤ 2%", "da 2 a 3%", "da 3 a 4%", "da 4 a 5%", "da 5 a 6%",
                "da 6 a 7%", "da 7 a 8%", "≥ 8%")
# Palette Viridis discreta: valori bassi chiari (giallo), valori alti scuri
# (viola) → più alta la quota, più scuro il paese.
bin_colours <- setNames(
  viridisLite::viridis(length(bin_levels), direction = -1),
  bin_levels
)
# Etichetta bianca sui bin scuri, nera sui chiari (in base alla luminanza).
.lum <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  as.numeric(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}
bin_scuri <- bin_levels[.lum(bin_colours) < 0.5]   # → etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <= 2   ~ "≤ 2%",
    valore <= 3   ~ "da 2 a 3%",
    valore <= 4   ~ "da 3 a 4%",
    valore <= 5   ~ "da 4 a 5%",
    valore <= 6   ~ "da 5 a 6%",
    valore <= 7   ~ "da 6 a 7%",
    valore <= 8   ~ "da 7 a 8%",
    TRUE          ~ "≥ 8%"
  ), levels = bin_levels))

# --- Etichetta valore per paese --------------------------------------------

# Etichette con un decimale, separatore italiano.
fmt_pct <- function(v) paste0(formatC(round(v, 1), format = "f", digits = 1,
                                       decimal.mark = ","), "%")

# Aggiustamenti manuali del centroide (metri EPSG:3035) per i paesi grandi
# o allungati. dx>0 = est, dy>0 = nord.
adjust_label_xy <- function(cntr, x, y) {
  dx <- case_when(
    cntr == "NO" ~ -150000,
    cntr == "SE" ~ -100000,
    cntr == "EL" ~ -100000,
    cntr == "IT" ~  -20000,
    cntr == "LV" ~  100000,
    cntr == "NL" ~   10000,
    TRUE         ~  0
  )
  dy <- case_when(
    cntr == "NO" ~ -250000,
    cntr == "FI" ~ -250000,
    cntr == "IE" ~  -80000,
    cntr == "BE" ~   30000,
    cntr == "HR" ~   60000,
    cntr == "EL" ~   20000,
    cntr == "MT" ~   25000,
    TRUE         ~  0
  )
  list(x = x + dx, y = y + dy)
}

geo_labels <- geo_dati %>%
  filter(!is.na(valore)) %>%
  mutate(
    label_value = fmt_pct(valore),
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

# Layer dummy fuori canvas con tutti i livelli: genera la legenda completa
# (geom_sf salterebbe i bin senza paesi, es. "da 7 a 8%").
dummy_legenda <- data.frame(bin = factor(bin_levels, levels = bin_levels))

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "white", linewidth = 0.25,
          show.legend = FALSE) +
  geom_rect(data = dummy_legenda, aes(fill = bin),
            xmin = -1e7, xmax = -1e7 + 1, ymin = -1e7, ymax = -1e7 + 1,
            inherit.aes = FALSE, show.legend = TRUE) +
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
    reverse = TRUE,             # valori alti (viola scuro) in cima, bassi in fondo
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
    title = "In Italia 6 persone su 100 non hanno mai usato internet",
    subtitle = paste0(
      "Quota di persone che non hanno mai usato internet, paesi europei, ",
      ultimo_anno),
    caption = CAP_EUROSTAT
  )

ggsave("../output/uso_internet_europa.png",
       plot = p, width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/uso_internet_europa.png\n")
