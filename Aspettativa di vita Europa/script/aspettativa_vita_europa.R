# Aspettativa di vita alla nascita in Europa, ultimo anno disponibile.
# Eseguito da `cd script && Rscript aspettativa_vita_europa.R`.
#
# Mappa binned con bin discreti (classi costanti di 1 anno) e scale_fill_manual,
# etichetta del valore su ogni paese. Fonte: Eurostat demo_r_mlifexp.

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

# --- Dati: speranza di vita alla nascita, ultimo anno ----------------------

geo <- load_geo_europa()                       # 38 paesi, EPSG:3035

demo <- get_eurostat("demo_r_mlifexp", time_format = "num",
                     cache = FALSE, update_cache = TRUE)

demo_f <- demo %>%
  filter(nchar(geo) == 2, sex == "T", unit == "YR", age == "Y_LT1")

ultimo_anno <- max(demo_f$TIME_PERIOD, na.rm = TRUE)

paesi <- demo_f %>%
  filter(TIME_PERIOD == ultimo_anno, geo %in% paesi_europa_mappa) %>%
  select(CNTR_ID = geo, anno = TIME_PERIOD, valore = values)

# Eurostat non pubblica l'aggregato EU27 per questo dataset: media semplice
# dei 27 paesi dell'Unione (per il tweet).
paesi_ue27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR",
                "HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO",
                "SE","SI","SK")
media_ue <- paesi %>% filter(CNTR_ID %in% paesi_ue27) %>% pull(valore) %>% mean()

cat("Ultimo anno:", ultimo_anno, "\n")
cat("Media semplice UE27:", round(media_ue, 1), "\n")
cat("Italia:", paesi$valore[paesi$CNTR_ID == "IT"], "\n")
cat("Range:", paste(range(paesi$valore), collapse = " - "), "\n")
cat("Paesi senza dato:",
    paste(setdiff(paesi_europa_mappa, paesi$CNTR_ID), collapse = ", "), "\n")

geo_dati <- geo %>% left_join(paesi, by = "CNTR_ID")

# Esporta dato pulito
write_csv(
  st_drop_geometry(geo_dati) %>%
    select(CNTR_ID, paese = NAME_ENGL, anno, aspettativa_vita = valore) %>%
    filter(!is.na(aspettativa_vita)) %>%
    arrange(desc(aspettativa_vita)),
  "../output/aspettativa_vita_europa.csv"
)

# --- Binning discreto (classi costanti di 1 anno) --------------------------

bin_levels <- c("< 77", "da 77 a 78", "da 78 a 79", "da 79 a 80",
                "da 80 a 81", "da 81 a 82", "da 82 a 83", "da 83 a 84", "≥ 84")
# Rampa blu monocromatica a 9 passi, dal chiaro (valori bassi) allo scuro.
bin_colours <- setNames(
  colorRampPalette(c("#EAF1FA", "#A1C6EE", "#5C9CDE", "#0E5BAD", "#06366A"))(9),
  bin_levels
)
bin_scuri <- c("da 81 a 82", "da 82 a 83",
               "da 83 a 84", "≥ 84")             # → etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <  77  ~ "< 77",
    valore <  78  ~ "da 77 a 78",
    valore <  79  ~ "da 78 a 79",
    valore <  80  ~ "da 79 a 80",
    valore <  81  ~ "da 80 a 81",
    valore <  82  ~ "da 81 a 82",
    valore <  83  ~ "da 82 a 83",
    valore <  84  ~ "da 83 a 84",
    TRUE          ~ "≥ 84"
  ), levels = bin_levels))

# --- Etichetta valore per paese --------------------------------------------

# Anni con un decimale, separatore italiano.
fmt_val <- function(v) formatC(v, format = "f", digits = 1, decimal.mark = ",")

# Liechtenstein: micro-stato incastrato tra CH e AT, la label collide con i
# vicini → mostrato colorato ma senza etichetta del valore.
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
    # Bianco sui bin scuri, nero sugli altri. Malta: poligono invisibile alla
    # scala europea, label su mare bianco → forza il nero.
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
    title = "In Italia l'aspettativa di vita è tra le più alte",
    subtitle = paste0(
      "Speranza di vita alla nascita in anni, paesi europei, ", ultimo_anno),
    caption = CAP_EUROSTAT
  )

ggsave("../output/aspettativa_vita_europa.png",
       plot = p, width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/aspettativa_vita_europa.png\n")
