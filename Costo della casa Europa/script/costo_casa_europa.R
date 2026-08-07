# Quota di popolazione con costi dell'abitazione sopra il 40% del reddito
# disponibile ("housing cost overburden rate"), ultimo anno.
# Eseguito da `cd script && Rscript costo_casa_europa.R`.
#
# Mappa binned con bin discreti a larghezza costante (2 punti) e
# scale_fill_manual, etichetta del valore su ogni paese.
# Fonte: Eurostat tespm140.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(eurostat)
  library(tidyverse)
  library(showtext)
  library(sf)
})

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

# --- Dati -------------------------------------------------------------------

geo <- load_geo_europa()                       # 38 paesi, EPSG:3035

costo <- get_eurostat("tespm140", time_format = "num",
                      cache = FALSE, update_cache = TRUE)

costo_f <- costo %>%
  filter(freq == "A", unit == "PC", rskpovth == "TOTAL",
         age == "TOTAL", sex == "T")

ultimo_anno <- max(costo_f$TIME_PERIOD, na.rm = TRUE)

paesi <- costo_f %>%
  filter(TIME_PERIOD == ultimo_anno, geo %in% paesi_europa_mappa) %>%
  select(CNTR_ID = geo, anno = TIME_PERIOD, valore = values)

# Aggregato UE27 ufficiale (ponderato) per il tweet.
media_ue <- costo_f %>%
  filter(TIME_PERIOD == ultimo_anno, geo == "EU27_2020") %>%
  pull(values)

cat("Ultimo anno:", ultimo_anno, "\n")
cat("Media UE27 (ponderata):", media_ue, "\n")
cat("Italia:", paesi$valore[paesi$CNTR_ID == "IT"], "\n")
cat("Range:", paste(range(paesi$valore), collapse = " - "), "\n")
cat("Paesi senza dato:",
    paste(setdiff(paesi_europa_mappa, paesi$CNTR_ID), collapse = ", "), "\n")

geo_dati <- geo %>% left_join(paesi, by = "CNTR_ID")

# Esporta dato pulito
write_csv(
  st_drop_geometry(geo_dati) %>%
    select(CNTR_ID, paese = NAME_ENGL, anno, quota_sovraccarico = valore) %>%
    filter(!is.na(quota_sovraccarico)) %>%
    arrange(desc(quota_sovraccarico)),
  "../output/costo_casa_europa.csv"
)

# --- Binning discreto (classi costanti di 2 punti) --------------------------

bin_levels <- c("< 4%", "da 4 a 6%", "da 6 a 8%", "da 8 a 10%",
                "da 10 a 12%", "da 12 a 14%", "≥ 14%")
# Rampa rossa monocromatica a 7 passi, dal chiaro (valori bassi) allo scuro.
bin_colours <- setNames(
  colorRampPalette(c("#FCE4E7", "#F6A2AA", "#F12938", "#A02530", "#5A1018"))(7),
  bin_levels
)
bin_scuri <- c("da 8 a 10%", "da 10 a 12%",
               "da 12 a 14%", "≥ 14%")          # → etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <  4   ~ "< 4%",
    valore <  6   ~ "da 4 a 6%",
    valore <  8   ~ "da 6 a 8%",
    valore < 10   ~ "da 8 a 10%",
    valore < 12   ~ "da 10 a 12%",
    valore < 14   ~ "da 12 a 14%",
    TRUE          ~ "≥ 14%"
  ), levels = bin_levels))

# --- Etichetta valore per paese --------------------------------------------

# Un decimale con virgola, senza ",0" sui valori interi.
# formatC e non format(): quest'ultimo aggiunge padding leading.
fmt_pct <- function(v) ifelse(
  v %% 1 == 0,
  paste0(as.integer(v), "%"),
  paste0(formatC(v, format = "f", digits = 1, decimal.mark = ","), "%")
)

# Aggiustamenti manuali del centroide (metri EPSG:3035). dx>0 = est, dy>0 = nord.
# DK: "23,4%" è più larga dello Jutland, quindi sborda sul mare e in bianco
# sparisce → spostata sul Mare del Nord a ovest del paese, in nero.
# EL: il centroide cade nell'Egeo e la label sborda sul mare → portata sulla
# Grecia continentale settentrionale, dove il poligono è largo.
adjust_label_xy <- function(cntr, x, y) {
  dx <- case_when(
    cntr == "NO" ~ -150000,
    cntr == "SE" ~ -100000,
    cntr == "FI" ~ -120000,
    cntr == "DK" ~ -190000,
    cntr == "IT" ~  -20000,
    cntr == "LV" ~  100000,
    cntr == "NL" ~   10000,
    cntr == "LU" ~  -75000,
    cntr == "AT" ~  -20000,
    cntr == "SI" ~  -50000,
    cntr == "EL" ~   85000,
    cntr == "HR" ~  -10000,
    cntr == "RS" ~   30000,
    cntr == "CY" ~  -30000,
    TRUE         ~  0
  )
  dy <- case_when(
    cntr == "NO" ~ -300000,
    cntr == "FI" ~ -250000,
    cntr == "SE" ~ -120000,
    cntr == "IE" ~  -80000,
    cntr == "DK" ~   60000,
    cntr == "BE" ~   25000,
    cntr == "LU" ~  -30000,
    cntr == "AT" ~    5000,
    cntr == "HR" ~   10000,
    cntr == "EL" ~  220000,
    cntr == "MT" ~   25000,
    cntr == "SI" ~  -20000,
    cntr == "NL" ~   45000,
    cntr == "RS" ~   20000,
    TRUE         ~  0
  )
  list(x = x + dx, y = y + dy)
}

geo_labels <- geo_dati %>%
  filter(!is.na(valore)) %>%
  mutate(
    label_value = fmt_pct(valore),
    # Bianco sui bin scuri, nero sugli altri. Malta e Lussemburgo: poligono
    # quasi invisibile alla scala europea, label su sfondo chiaro → nero.
    # Danimarca: label spostata sul mare, quindi nero anche se il bin è scuro.
    label_color = case_when(
      CNTR_ID %in% c("MT", "LU", "DK") ~ "#1C1C1C",
      bin %in% bin_scuri               ~ "white",
      TRUE                             ~ "#1C1C1C"
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
    reverse = TRUE,             # valori alti (rosso scuro) in cima, bassi in fondo
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
    title = "Quanti spendono per la casa oltre il 40% del reddito",
    subtitle = paste0(
      "Quota di popolazione, nei costi rientrano affitto o interessi sul mutuo, ",
      "utenze,\nmanutenzione e imposte, paesi europei, ", ultimo_anno),
    caption = CAP_EUROSTAT
  )

ggsave("../output/costo_casa_europa.png",
       plot = p, width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/costo_casa_europa.png\n")
