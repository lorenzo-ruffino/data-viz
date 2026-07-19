# script/obesita_mondo.R — eseguito da `cd script && Rscript obesita_mondo.R`
#
# Mappa mondiale della prevalenza di obesità tra gli adulti (BMI >= 30), 2024.
# Fonte: NCD Risk Factor Collaboration (NCD-RisC), stime 1980-2024 pubblicate su
# Nature (2026), scaricate da ncdrisc.org (input/ncdrisc_2026_bmi_country.csv).
# Prevalenza standardizzata per età; media semplice di uomini e donne.
# Geometrie: GISCO gisco_get_countries(resolution = "20"), salvate una tantum
# in input/geo_mondo_gisco.geojson.

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(janitor)
  library(showtext)
})

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 220)   # allineato al dpi di ggsave: font alle dimensioni nominali del tema

CAP_NCDRISC <- "Elaborazione di Lorenzo Ruffino su dati NCD-RisC"

input_dir  <- "../input"
output_dir <- "../output"

# 1) Dati -----------------------------------------------------------------------

dati <- read_csv(file.path(input_dir, "ncdrisc_2026_bmi_country.csv"),
                 show_col_types = FALSE) %>%
  clean_names() %>%
  filter(year == 2024) %>%
  select(iso, paese = country_region_world, sex,
         obesita = prevalence_of_bmi_30_kg_m2_obesity) %>%
  pivot_wider(names_from = sex, values_from = obesita) %>%
  transmute(iso, paese,
            uomini = Men * 100,
            donne  = Women * 100,
            media  = (Men + Women) / 2 * 100)

write_csv(dati %>% arrange(desc(media)) %>% mutate(across(where(is.numeric), ~round(.x, 1))),
          file.path(output_dir, "obesita_mondo.csv"))

cat("Paesi:", nrow(dati), "| Mondo range:", round(min(dati$media), 1), "-",
    round(max(dati$media), 1), "| Italia:", round(dati$media[dati$iso == "ITA"], 1), "\n")

# 2) Classi discrete -------------------------------------------------------------

bin_levels <- c("< 5%", "5-10%", "10-15%", "15-20%", "20-25%", "25-30%", "30-40%", "≥ 40%")

dati <- dati %>%
  mutate(bin = factor(case_when(
    media < 5  ~ bin_levels[1],
    media < 10 ~ bin_levels[2],
    media < 15 ~ bin_levels[3],
    media < 20 ~ bin_levels[4],
    media < 25 ~ bin_levels[5],
    media < 30 ~ bin_levels[6],
    media < 40 ~ bin_levels[7],
    TRUE       ~ bin_levels[8]
  ), levels = bin_levels))

bin_colours <- setNames(
  viridisLite::viridis(length(bin_levels), option = "A", direction = -1),
  bin_levels)

# 3) Geometrie -------------------------------------------------------------------

geo <- read_sf(file.path(input_dir, "geo_mondo_gisco.geojson")) %>%
  filter(!coalesce(NAME_ENGL == "Antarctica", FALSE))

# GISCO non compila l'ISO3 di Taiwan: senza fix resterebbe grigio
geo$ISO3_CODE[coalesce(geo$NAME_ENGL == "Taiwan", FALSE)] <- "TWN"

# crop a ovest: elimina il Pacifico vuoto oltre l'Alaska; si perdono solo
# micro-isole invisibili a questa scala (Samoa, Tonga, Samoa Americane, Niue,
# Tokelau), che restano comunque nel CSV esportato
sf_use_s2(FALSE)   # crop planare: le geometrie GISCO non sono valide sulla sfera s2
geo <- suppressWarnings(
  geo %>% st_make_valid() %>%
    st_crop(xmin = -169, ymin = -90, xmax = 180, ymax = 90))

# elimina le isole del Pacifico centrale (Hawaii, Polinesia francese, Pitcairn):
# in proiezione spingerebbero il bordo ovest oltre l'Alaska, lasciando una
# colonna d'oceano vuota a sinistra; il rettangolo non tocca i continenti,
# quindi nessun bordo di taglio visibile
box_pacifico <- st_as_sfc(st_bbox(c(xmin = -170, ymin = -60, xmax = -128, ymax = 35),
                                  crs = st_crs(geo)))
geo <- suppressWarnings(st_difference(geo, box_pacifico))
geo <- geo[!st_is_empty(geo), ]
sf_use_s2(TRUE)

geo_dati <- geo %>%
  left_join(dati, by = c("ISO3_CODE" = "iso")) %>%
  st_transform("+proj=eqearth")

na_paesi <- geo_dati %>% st_drop_geometry() %>% filter(is.na(media)) %>% pull(NAME_ENGL)
cat("Geometrie senza dato:", length(na_paesi), "\n",
    paste(head(na_paesi, 15), collapse = ", "), "...\n")

# 4) Mappa -----------------------------------------------------------------------

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "white", linewidth = 0.12) +
  scale_fill_manual(values = bin_colours,
                    na.value = COL_NA_MAPPA,
                    breaks = bin_levels,
                    guide = guide_legend(nrow = 1, byrow = TRUE,
                                         label.position = "bottom")) +
  coord_sf(expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.53, 0.02),      # dentro la mappa, sotto l'Africa
        legend.justification = c(0.5, 0),
        legend.direction = "horizontal",
        legend.key.width = unit(0.95, "cm"),
        legend.key.height = unit(0.28, "cm"),
        legend.spacing.x = unit(0.08, "cm"),
        legend.text = element_text(size = 8.5, color = "#1C1C1C", hjust = 0.5),
        legend.margin = margin(0, 0, 0, 0)) +
  labs(
    title = "Nel mondo più di un adulto su sette è obeso",
    subtitle = "Quota di adulti obesi (indice di massa corporea ≥ 30), standardizzata per età, media di uomini e donne, 2024.",
    caption = CAP_NCDRISC
  )

ggsave(file.path(output_dir, "obesita_mondo.png"),
       plot = p, width = 10.2, height = 6.2, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/obesita_mondo.png\n")
