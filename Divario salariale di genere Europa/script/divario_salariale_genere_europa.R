# Divario salariale di genere non corretto in Europa, ultimo anno disponibile.
# Eseguito da `cd script && Rscript divario_salariale_genere_europa.R`.
#
# Pay gap (Eurostat sdg_05_20, % tra retribuzione oraria lorda media di uomini
# e donne, NACE Rev. 2 sezioni B-S escluso O), mappa binned con 7 classi
# discrete a larghezza costante e scale_fill_manual, etichetta del valore su
# ogni paese.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(eurostat)
  library(tidyverse)
  library(showtext)
  library(sf)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

# --- Dati: divario salariale di genere non corretto ------------------------

geo <- load_geo_europa()                          # 38 paesi, EPSG:3035

gpg <- get_eurostat("sdg_05_20", time_format = "num")

gpg_f <- gpg %>%
  filter(nace_r2 == "B-S_X_O", unit == "PC")

ultimo_anno <- max(gpg_f$TIME_PERIOD, na.rm = TRUE)

# Aggregato UE27 ufficiale (ponderato) per il tweet
media_ue <- gpg_f %>%
  filter(TIME_PERIOD == ultimo_anno, geo == "EU27_2020") %>%
  pull(values)

gpg_paesi <- gpg_f %>%
  filter(TIME_PERIOD == ultimo_anno, geo %in% paesi_europa_mappa) %>%
  select(CNTR_ID = geo, anno = TIME_PERIOD, valore = values)

cat("Ultimo anno:", ultimo_anno, "\n")
cat("Media UE27:", media_ue, "\n")
cat("Italia:", gpg_paesi$valore[gpg_paesi$CNTR_ID == "IT"], "\n")
cat("Range:", paste(range(gpg_paesi$valore), collapse = " - "), "\n")
cat("Paesi senza dato:",
    paste(setdiff(paesi_europa_mappa, gpg_paesi$CNTR_ID), collapse = ", "), "\n")

geo_dati <- geo %>% left_join(gpg_paesi, by = "CNTR_ID")

write_csv(
  st_drop_geometry(geo_dati) %>%
    select(CNTR_ID, paese = NAME_ENGL, anno, divario_pct = valore) %>%
    filter(!is.na(divario_pct)) %>%
    arrange(desc(divario_pct)),
  "../output/divario_salariale_genere_europa.csv"
)

# --- Binning discreto (7 classi da 3 punti) --------------------------------

bin_levels <- c("< 3%", "da 3 a 6%", "da 6 a 9%", "da 9 a 12%",
                "da 12 a 15%", "da 15 a 18%", "≥ 18%")
# Rampa rossa monocromatica a 7 passi: il divario è un indicatore "negativo",
# rosso scuro segnala valori alti.
bin_colours <- setNames(
  colorRampPalette(c("#FCE6E0", "#F5B0A0", "#E97A60", "#C9351B", "#7A1A0A"))(7),
  bin_levels
)
bin_scuri <- c("da 12 a 15%", "da 15 a 18%", "≥ 18%")   # → etichetta bianca

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <  3   ~ "< 3%",
    valore <  6   ~ "da 3 a 6%",
    valore <  9   ~ "da 6 a 9%",
    valore < 12   ~ "da 9 a 12%",
    valore < 15   ~ "da 12 a 15%",
    valore < 18   ~ "da 15 a 18%",
    TRUE          ~ "≥ 18%"
  ), levels = bin_levels))

# --- Etichetta valore per paese --------------------------------------------

fmt_val <- function(v) formatC(v, format = "f", digits = 1, decimal.mark = ",")

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
    breaks = bin_levels
  ) +
  guides(fill = guide_legend(
    reverse = TRUE,
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
    title = "L'Italia è tra i paesi con il divario salariale di genere più basso",
    subtitle = paste0(
      "Differenza percentuale tra retribuzione oraria lorda media di uomini ",
      "e donne,\npaesi europei, ", ultimo_anno, ". Media Ue 27 ",
      formatC(media_ue, format = "f", digits = 1, decimal.mark = ","), "%"
    ),
    caption = CAP_EUROSTAT
  )

ggsave("../output/divario_salariale_genere_europa.png",
       plot = p, width = 9, height = 9, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/divario_salariale_genere_europa.png\n")
