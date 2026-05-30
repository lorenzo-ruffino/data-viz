# Quota di persone 25-34 anni con istruzione terziaria in Europa, ultimo anno.
# Eseguito da `cd script && Rscript quota_laureati_europa.R`.
#
# Mappa binned con classi discrete a larghezza costante e scale_fill_manual,
# etichetta del valore su ogni paese. Fonte: Eurostat edat_lfse_03 2025.

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

# --- Dati: istruzione terziaria (ISCED 5-8) tra i 25-34 anni ---------------

geo <- load_geo_europa()                       # 38 paesi, EPSG:3035

edu <- get_eurostat("edat_lfse_03", time_format = "num")

edu_f <- edu %>%
  filter(isced11 == "ED5-8", sex == "T", age == "Y25-34", unit == "PC")

ultimo_anno <- max(edu_f$TIME_PERIOD, na.rm = TRUE)

edu_paesi <- edu_f %>%
  filter(TIME_PERIOD == ultimo_anno, geo %in% paesi_europa_mappa) %>%
  select(CNTR_ID = geo, anno = TIME_PERIOD, valore = values)

# Aggregato UE27 ufficiale (ponderato) per il tweet
media_ue <- edu_f %>%
  filter(TIME_PERIOD == ultimo_anno, geo == "EU27_2020") %>%
  pull(values)

cat("Ultimo anno:", ultimo_anno, "\n")
cat("Media UE27:", media_ue, "\n")
cat("Italia:", edu_paesi$valore[edu_paesi$CNTR_ID == "IT"], "\n")
cat("Range:", paste(range(edu_paesi$valore), collapse = " - "), "\n")
cat("Paesi senza dato:",
    paste(setdiff(paesi_europa_mappa, edu_paesi$CNTR_ID), collapse = ", "), "\n")

geo_dati <- geo %>% left_join(edu_paesi, by = "CNTR_ID")

# Esporta dato pulito
write_csv(
  st_drop_geometry(geo_dati) %>%
    select(CNTR_ID, paese = NAME_ENGL, anno, quota_laureati = valore) %>%
    filter(!is.na(quota_laureati)) %>%
    arrange(desc(quota_laureati)),
  "../output/quota_laureati_europa.csv"
)

# --- Binning discreto (classi costanti di 5 punti) -------------------------

bin_levels <- c("< 30%", "da 30 a 35%", "da 35 a 40%", "da 40 a 45%",
                "da 45 a 50%", "da 50 a 55%", "da 55 a 60%", "≥ 60%")
# Rampa blu monocromatica a 8 passi, dal chiaro (valori bassi) allo scuro.
bin_colours <- setNames(
  colorRampPalette(c("#EAF1FA", "#A1C6EE", "#5C9CDE", "#0E5BAD", "#06366A"))(8),
  bin_levels
)
bin_scuri <- c("da 45 a 50%", "da 50 a 55%",
               "da 55 a 60%", "≥ 60%")          # → etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <  30  ~ "< 30%",
    valore <  35  ~ "da 30 a 35%",
    valore <  40  ~ "da 35 a 40%",
    valore <  45  ~ "da 40 a 45%",
    valore <  50  ~ "da 45 a 50%",
    valore <  55  ~ "da 50 a 55%",
    valore <  60  ~ "da 55 a 60%",
    TRUE          ~ "≥ 60%"
  ), levels = bin_levels))

# --- Etichetta valore per paese --------------------------------------------

# Etichette arrotondate all'intero.
fmt_pct <- function(v) paste0(round(v), "%")

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
    cntr == "MT" ~   25000,   # Malta: poco sopra il centroide reale, in mare sotto la Sicilia
    TRUE         ~  0
  )
  list(x = x + dx, y = y + dy)
}

geo_labels <- geo_dati %>%
  filter(!is.na(valore)) %>%
  mutate(
    label_value = fmt_pct(valore),
    # Bianco sui bin scuri, nero sugli altri. Malta: poligono invisibile
    # alla scala europea, label su mare bianco → forza il nero.
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
    title = "L'Italia è penultima nell'Unione Europea per laureati",
    subtitle = paste0(
      "Quota di persone tra 25 e 34 anni con una laurea, paesi europei, ",
      ultimo_anno),
    caption = CAP_EUROSTAT
  )

ggsave("../output/quota_laureati_europa.png",
       plot = p, width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/quota_laureati_europa.png\n")
