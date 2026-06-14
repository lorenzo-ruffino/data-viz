# script/occupazione_manifatturiera_europa.R
# Mappa NUTS-2: quota occupati nell'industria (B-E) sul totale

library(eurostat)
library(tidyverse)
library(showtext)
library(sf)
library(giscoR)
library(rnaturalearth)

source_dir <- ".."
input_dir  <- file.path(source_dir, "input")
output_dir <- file.path(source_dir, "output")

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")

# --- Font --------------------------------------------------------------------
font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

# --- Dati Eurostat -----------------------------------------------------------
df <- get_eurostat("lfst_r_lfe2en2", time_format = "num")

# 2025, occupati totali (entrambi i sessi, 15+ anni)
share <- df %>%
  filter(sex == "T", age == "Y_GE15", TIME_PERIOD == 2025,
         nace_r2 %in% c("B-E", "TOTAL")) %>%
  select(geo, nace_r2, values)

# Portugal e Netherlands: remap NUTS 2024 → NUTS 2021 (matching giscoR geometry)
n24_mapping <- c(
  "PT19" = "PT16",  "PT1A" = "PT17",  "PT1B" = "PT17",
  "PT1C" = "PT18",  "PT1D" = "PT16",
  "NL35" = "NL31",  "NL36" = "NL33"
)

share <- share %>%
  mutate(geo = ifelse(geo %in% names(n24_mapping), n24_mapping[geo], geo)) %>%
  group_by(geo, nace_r2) %>%
  summarise(values = sum(values, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = nace_r2, values_from = values) %>%
  mutate(share = `B-E` / TOTAL * 100) %>%
  filter(nchar(geo) == 4) %>%
  select(NUTS_ID = geo, share)

write_csv(share, file.path(output_dir, "occupazione_manifatturiera_europa.csv"))

# --- Geometrie NUTS-2 --------------------------------------------------------
nuts2_geo <- gisco_get_nuts(nuts_level = 2, year = "2021", epsg = 3035)

paesi <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR",
           "HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO",
           "SE","SI","SK","IS","LI","NO","CH","UK",
            "AL","BA","ME","MK","RS",
            "BY","RU")
nuts2_geo <- nuts2_geo %>% filter(CNTR_CODE %in% paesi)

country_borders <- load_geo_europa()

# Paesi extra-NUTS (visibili come sfondo grigio, no dati)
extra_geo <- gisco_get_countries(epsg = 3035) %>%
  filter(CNTR_ID == "BA") %>%
  bind_rows(
    ne_countries(scale = 50, returnclass = "sf") %>%
      filter(name == "Kosovo") %>%
      st_transform(3035) %>%
      select(geometry)
  )

geo_dati <- nuts2_geo %>% left_join(share, by = "NUTS_ID")

# --- Binning (10 bin, larghezza costante 3 pp, primo e ultimo aperti) ---------
bin_levels <- c(
  "Meno del 6%","6\u20139%","9\u201312%","12\u201315%","15\u201318%",
  "18\u201321%","21\u201324%","24\u201327%","27\u201330%","30% e pi\u00f9"
)

# Palette Mako (reversed: high share = dark)
mako_cols <- rev(viridisLite::mako(10))
names(mako_cols) <- bin_levels

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(share)  ~ NA_character_,
    share < 6     ~ "Meno del 6%",
    share < 9     ~ "6\u20139%",
    share < 12    ~ "9\u201312%",
    share < 15    ~ "12\u201315%",
    share < 18    ~ "15\u201318%",
    share < 21    ~ "18\u201321%",
    share < 24    ~ "21\u201324%",
    share < 27    ~ "24\u201327%",
    share < 30    ~ "27\u201330%",
    TRUE          ~ "30% e pi\u00f9"
  ), levels = bin_levels))

# --- Mappa -------------------------------------------------------------------
p <- ggplot() +
  geom_sf(data = extra_geo, fill = COL_NA_MAPPA, color = "white", linewidth = 0.3) +
  geom_sf(data = geo_dati, aes(fill = bin), color = NA, linewidth = 0) +
  geom_sf(data = country_borders, fill = NA, color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = mako_cols,
    drop = FALSE,
    na.value = COL_NA_MAPPA,
    name = NULL,
    breaks = bin_levels
  ) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.48, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(xlim = bbox_europa[c("xmin","xmax")],
           ylim = bbox_europa[c("ymin","ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.98, 0.70),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "Dove l'industria pesa di più sull'occupazione",
    subtitle = paste0(
      "Quota % di occupati nell'industria in senso stretto (NACE B-E, esclusa costruzioni)\n",
      "sul totale dell'occupazione, per regione NUTS-2, 2025"
    ),
    caption = "Elaborazione di Lorenzo Ruffino su dati Eurostat"
  )

ggsave(file.path(output_dir, "occupazione_manifatturiera_europa.png"),
       p, width = 10, height = 9.5, dpi = 220, bg = "white")
