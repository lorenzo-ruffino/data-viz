# Sovraffollamento delle carceri in Europa, ultimo anno disponibile.
# Eseguito da `cd script && Rscript sovraffollamento_carceri_europa.R`.
#
# Tasso di affollamento = detenuti / capienza ufficiale * 100. Mappa binned con
# palette divergente centrata sul 100% (sotto = capienza non saturata, sopra =
# sovraffollamento), etichetta del valore su ogni paese.
# Fonte: Eurostat crim_pris_age (detenuti) e crim_pris_cap (capienza ufficiale).

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

# --- Dati -------------------------------------------------------------------

geo <- load_geo_europa()                       # 38 paesi, EPSG:3035

# Detenuti totali (sesso e classi d'età aggregati), valori assoluti.
age <- get_eurostat("crim_pris_age", time_format = "num",
                    cache = FALSE, update_cache = TRUE)
det <- age %>%
  filter(sex == "T", age == "TOTAL", unit == "NR") %>%
  group_by(geo) %>%
  filter(TIME_PERIOD == max(TIME_PERIOD)) %>%
  ungroup() %>%
  select(CNTR_ID = geo, anno_det = TIME_PERIOD, detenuti = values)

# Capienza ufficiale delle carceri, valori assoluti.
cap <- get_eurostat("crim_pris_cap", time_format = "num",
                    cache = FALSE, update_cache = TRUE)
off <- cap %>%
  filter(indic_cr == "PRIS_OFF_CAP", unit == "NR") %>%
  group_by(geo) %>%
  filter(TIME_PERIOD == max(TIME_PERIOD)) %>%
  ungroup() %>%
  select(CNTR_ID = geo, anno_cap = TIME_PERIOD, capienza = values)

paesi <- inner_join(det, off, by = "CNTR_ID") %>%
  filter(CNTR_ID %in% paesi_europa_mappa) %>%
  mutate(valore = detenuti / capienza * 100)

ultimo_anno <- max(paesi$anno_det, na.rm = TRUE)

# Eurostat non pubblica un aggregato UE per questo indicatore: media semplice
# dei 27 paesi dell'Unione (per il tweet).
paesi_ue27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR",
                "HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO",
                "SE","SI","SK")
media_ue <- paesi %>% filter(CNTR_ID %in% paesi_ue27) %>% pull(valore) %>% mean()

cat("Ultimo anno detenuti:", ultimo_anno, "\n")
cat("Anni capienza:", paste(sort(unique(paesi$anno_cap)), collapse = ","), "\n")
cat("Media semplice UE27:", round(media_ue, 1), "\n")
cat("Italia:", round(paesi$valore[paesi$CNTR_ID == "IT"], 1), "\n")
cat("Range:", paste(round(range(paesi$valore), 1), collapse = " - "), "\n")
print(paesi %>% select(CNTR_ID, valore) %>% arrange(desc(valore)) %>%
      mutate(valore = round(valore, 1)) %>% as.data.frame())
cat("Paesi senza dato:",
    paste(setdiff(paesi_europa_mappa, paesi$CNTR_ID), collapse = ", "), "\n")

geo_dati <- geo %>% left_join(paesi, by = "CNTR_ID")

# Esporta dato pulito
write_csv(
  st_drop_geometry(geo_dati) %>%
    select(CNTR_ID, paese = NAME_ENGL, anno_detenuti = anno_det,
           anno_capienza = anno_cap, detenuti, capienza,
           tasso_affollamento = valore) %>%
    filter(!is.na(tasso_affollamento)) %>%
    arrange(desc(tasso_affollamento)),
  "../output/sovraffollamento_carceri_europa.csv"
)

# --- Binning discreto, divergente attorno al 100% --------------------------

# 8 classi simmetriche attorno al 100%: 4 blu (sotto la capienza) e 4 rosse
# (sovraffollamento). Palette divergente RdBu a 8 passi.
bin_levels <- c("≤ 70%", "da 70% a 80%", "da 80% a 90%", "da 90% a 100%",
                "da 100% a 110%", "da 110% a 120%", "da 120% a 130%", "≥ 130%")
bin_colours <- c(
  "≤ 70%"           = "#2166AC",
  "da 70% a 80%"    = "#4393C3",
  "da 80% a 90%"    = "#92C5DE",
  "da 90% a 100%"   = "#D1E5F0",
  "da 100% a 110%"  = "#FDDBC7",
  "da 110% a 120%"  = "#F4A582",
  "da 120% a 130%"  = "#D6604D",
  "≥ 130%"          = "#B2182B"
)
bin_scuri <- c("≤ 70%", "da 70% a 80%", "da 120% a 130%", "≥ 130%")  # etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <= 70  ~ "≤ 70%",
    valore <  80  ~ "da 70% a 80%",
    valore <  90  ~ "da 80% a 90%",
    valore < 100  ~ "da 90% a 100%",
    valore < 110  ~ "da 100% a 110%",
    valore < 120  ~ "da 110% a 120%",
    valore < 130  ~ "da 120% a 130%",
    TRUE          ~ "≥ 130%"
  ), levels = bin_levels))

# --- Etichetta valore per paese --------------------------------------------

fmt_val <- function(v) paste0(formatC(v, format = "f", digits = 0,
                                      decimal.mark = ","), "%")

# Liechtenstein: micro-stato, valore statisticamente rumoroso (77 detenuti) e
# label che collide con i vicini → colorato ma senza etichetta.
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
    cntr == "HR" ~  -35000,
    cntr == "SI" ~  -75000,
    cntr == "RS" ~   55000,
    cntr == "BA" ~  -45000,
    cntr == "ME" ~  -55000,
    cntr == "MK" ~   45000,
    cntr == "AL" ~  -35000,
    cntr == "XK" ~   40000,
    TRUE         ~  0
  )
  dy <- case_when(
    cntr == "NO" ~ -300000,
    cntr == "FI" ~ -250000,
    cntr == "SE" ~ -120000,
    cntr == "IE" ~  -80000,
    cntr == "BE" ~   25000,
    cntr == "HR" ~   55000,
    cntr == "EL" ~   30000,
    cntr == "MT" ~   25000,
    cntr == "SI" ~   60000,
    cntr == "NL" ~   45000,
    cntr == "RS" ~   25000,
    cntr == "BA" ~  -10000,
    cntr == "ME" ~  -45000,
    cntr == "MK" ~  -20000,
    cntr == "AL" ~  -15000,
    cntr == "XK" ~   10000,
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
    reverse = TRUE,             # valori alti (rosso) in cima, bassi in fondo
    keyheight = unit(0.7, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(xlim = bbox_europa[c("xmin", "xmax")],
           ylim = bbox_europa[c("ymin", "ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(1.0, 0.78),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "Le carceri italiane sono tra le più sovraffollate d'Europa",
    subtitle = paste0(
      "Detenuti ogni 100 posti regolamentari nelle carceri, paesi europei, ",
      ultimo_anno, ".\nSopra il 100% i detenuti superano la capienza ufficiale."),
    caption = CAP_EUROSTAT
  )

ggsave("../output/sovraffollamento_carceri_europa.png",
       plot = p, width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/sovraffollamento_carceri_europa.png\n")
