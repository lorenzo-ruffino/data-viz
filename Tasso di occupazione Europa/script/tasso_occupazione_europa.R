# Tasso di occupazione 20-64 anni in Europa, ultimo anno disponibile.
# Eseguito da `cd script && Rscript tasso_occupazione_europa.R`.
#
# Mappa binned con bin discreti e scale_fill_manual, etichetta del valore su
# ogni paese. Fonte: Eurostat 2025.

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

# --- Dati: tasso di occupazione 20-64 anni, ultimo anno --------------------

geo <- load_geo_europa()                       # 38 paesi, EPSG:3035

emp <- get_eurostat("lfsi_emp_a", time_format = "num")

emp_f <- emp %>%
  filter(indic_em == "EMP_LFS", sex == "T", age == "Y20-64", unit == "PC_POP")

ultimo_anno <- max(emp_f$TIME_PERIOD, na.rm = TRUE)

emp_paesi <- emp_f %>%
  filter(TIME_PERIOD == ultimo_anno, geo %in% paesi_europa_mappa) %>%
  select(CNTR_ID = geo, anno = TIME_PERIOD, valore = values)

# Aggregato UE27 ufficiale (ponderato) per il tweet
media_ue <- emp_f %>%
  filter(TIME_PERIOD == ultimo_anno, geo == "EU27_2020") %>%
  pull(values)

cat("Ultimo anno:", ultimo_anno, "\n")
cat("Media UE27:", media_ue, "\n")
cat("Italia:", emp_paesi$valore[emp_paesi$CNTR_ID == "IT"], "\n")
cat("Range:", paste(range(emp_paesi$valore), collapse = " - "), "\n")
cat("Paesi senza dato:",
    paste(setdiff(paesi_europa_mappa, emp_paesi$CNTR_ID), collapse = ", "), "\n")

geo_dati <- geo %>% left_join(emp_paesi, by = "CNTR_ID")

# Esporta dato pulito
write_csv(
  st_drop_geometry(geo_dati) %>%
    select(CNTR_ID, paese = NAME_ENGL, anno, tasso_occupazione = valore) %>%
    filter(!is.na(tasso_occupazione)) %>%
    arrange(desc(tasso_occupazione)),
  "../output/tasso_occupazione_europa.csv"
)

# --- Binning discreto (classi costanti di 2 punti) -------------------------

bin_levels <- c("< 70%", "da 70 a 72%", "da 72 a 74%", "da 74 a 76%",
                "da 76 a 78%", "da 78 a 80%", "da 80 a 82%", "≥ 82%")
# Rampa blu monocromatica a 8 passi, dal chiaro (valori bassi) allo scuro.
bin_colours <- setNames(
  colorRampPalette(c("#EAF1FA", "#A1C6EE", "#5C9CDE", "#0E5BAD", "#06366A"))(8),
  bin_levels
)
bin_scuri <- c("da 76 a 78%", "da 78 a 80%",
               "da 80 a 82%", "≥ 82%")          # → etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <  70  ~ "< 70%",
    valore <  72  ~ "da 70 a 72%",
    valore <  74  ~ "da 72 a 74%",
    valore <  76  ~ "da 74 a 76%",
    valore <  78  ~ "da 76 a 78%",
    valore <  80  ~ "da 78 a 80%",
    valore <  82  ~ "da 80 a 82%",
    TRUE          ~ "≥ 82%"
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
    title = "L'Italia ha il tasso di occupazione più basso dell'Ue",
    subtitle = paste0(
      "Quota di occupati sulla popolazione tra 20 e 64 anni, paesi europei, ",
      ultimo_anno),
    caption = CAP_EUROSTAT
  )

ggsave("../output/tasso_occupazione_europa.png",
       plot = p, width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/tasso_occupazione_europa.png\n")
