source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
library(eurostat); library(tidyverse); library(showtext); library(sf); library(scales)

font_add_google("Source Sans 3", "Source Sans Pro"); showtext_auto(); showtext_opts(dpi = 220)
base <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Vacanze scolastiche Europa"

dati <- read_csv(file.path(base, "input/quota_estate.csv"), show_col_types = FALSE) %>%
  transmute(CNTR_ID, valore = quota_estate)

geo <- load_geo_europa()
geo_dati <- geo %>% left_join(dati, by = "CNTR_ID")

# bin a larghezza costante (5 punti), estremi aperti
bin_levels <- c("meno del 50%","50-55%","55-60%","60-65%","65-70%","70-75%","75% e oltre")
bin_colours <- c("meno del 50%"="#FDE3E6","50-55%"="#F9C0C7","55-60%"="#F49AA6",
                 "60-65%"="#EE7080","65-70%"="#E03E50","70-75%"="#BC2230","75% e oltre"="#7E1019")
bin_scuri <- c("65-70%","70-75%","75% e oltre")

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore < 50   ~ "meno del 50%",
    valore < 55   ~ "50-55%",
    valore < 60   ~ "55-60%",
    valore < 65   ~ "60-65%",
    valore < 70   ~ "65-70%",
    valore < 75   ~ "70-75%",
    TRUE          ~ "75% e oltre"
  ), levels = bin_levels))

# offset manuali (metri EPSG:3035): dx>0 verso est, dy>0 verso nord
adjust_label_xy <- function(cntr, x, y) {
  dx <- dplyr::case_when(
    cntr == "NO" ~ -150000, cntr == "SE" ~ -100000, cntr == "FI" ~ -120000,
    cntr == "EL" ~  -60000, cntr == "IT" ~  -20000, cntr == "LV" ~  100000,
    cntr == "NL" ~   10000, cntr == "HR" ~  -55000, cntr == "ME" ~   12000,
    cntr == "RS" ~   30000, cntr == "CY" ~  -30000, cntr == "MK" ~   20000,
    TRUE ~ 0)
  dy <- dplyr::case_when(
    cntr == "NO" ~ -300000, cntr == "FI" ~ -250000, cntr == "SE" ~ -120000,
    cntr == "IE" ~  -80000, cntr == "BE" ~   25000, cntr == "HR" ~   70000,
    cntr == "EL" ~   30000, cntr == "MT" ~   25000, cntr == "SI" ~   35000,
    cntr == "ME" ~  -30000, cntr == "AL" ~  -25000, cntr == "RS" ~   20000,
    cntr == "MK" ~  -20000, cntr == "BA" ~   10000,
    TRUE ~ 0)
  list(x = x + dx, y = y + dy)
}

geo_lab <- geo_dati %>% filter(!is.na(valore)) %>%
  mutate(label = paste0(round(valore), "%"),
         label_color = ifelse(bin %in% bin_scuri, "white", "#1C1C1C"))
cen <- mainland_centroids(geo_lab); co <- sf::st_coordinates(cen)
geo_lab$lx <- co[,"X"]; geo_lab$ly <- co[,"Y"]
adj <- adjust_label_xy(geo_lab$CNTR_ID, geo_lab$lx, geo_lab$ly)
geo_lab$lx <- adj$x; geo_lab$ly <- adj$y

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "#9CA3AF", linewidth = 0.25) +
  geom_text(data = sf::st_drop_geometry(geo_lab),
            aes(x = lx, y = ly, label = label, color = label_color),
            family = "Source Sans Pro", size = 2.7, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(values = bin_colours, drop = FALSE, na.value = COL_NA_MAPPA,
                    name = NULL, breaks = bin_levels) +
  guides(fill = guide_legend(reverse = TRUE,
           keyheight = unit(0.7, "cm"), keywidth = unit(0.55, "cm"),
           label.theme = element_text(family = "Source Sans Pro", size = 11, color = "#1C1C1C", hjust = 0))) +
  coord_sf(xlim = bbox_europa[c("xmin","xmax")], ylim = bbox_europa[c("ymin","ymax")],
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.74), legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm"),
        plot.title = element_text(family = "Source Sans Pro", size = 18, face = "bold", color = "#1C1C1C",
                                  margin = margin(b = 0.1, unit = "cm")),
        plot.subtitle = element_text(family = "Source Sans Pro", size = 12, color = "#1C1C1C", lineheight = 1.3,
                                     margin = margin(b = 0.2, t = 0.1, unit = "cm")),
        plot.caption = element_text(family = "Source Sans Pro", size = 11, color = "#5A5A5A", hjust = 1,
                                    margin = margin(t = 0.3, unit = "cm")),
        plot.title.position = "plot") +
  labs(title = "Quanto pesa l'estate sulle vacanze scolastiche",
       subtitle = "Giorni di vacanza estiva in percentuale di tutti i giorni di vacanza, scuole secondarie, 2023/2024\nDove i calendari variano per regione o land, il dato è indicativo",
       caption = "Elaborazione di Lorenzo Ruffino su dati Eurydice")

ggsave(file.path(base, "output/mappa_quota_estate.png"), p,
       width = 9, height = 9, dpi = 220, bg = "white")
cat("OK\n")
