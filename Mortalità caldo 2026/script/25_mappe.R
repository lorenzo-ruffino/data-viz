# Mappe dell'eccesso relativo di mortalità per provincia, 21-29 maggio e
# 17-30 giugno 2026 (da 22_eccesso.R, osservati corretti per completezza).
#
# Confini: province Istat 2025 (Temperature Copernicus/input/geo), con i codici
# delle città metropolitane (2xx) riportati a quelli di provincia; la Sardegna è
# ricostruita dalle otto nuove unità dei dati di mortalità (112-119) unendo i
# confini comunali al 1/1/2026. Il punto nero indica le province in cui
# l'intervallo di previsione al 95% dell'eccesso non comprende lo zero.
#
# Output: output/mappa_eccesso_17_30_giugno.png, output/mappa_eccesso_21_29_maggio.png

library(tidyverse)
library(sf)
library(showtext)

RADICE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz"
setwd(file.path(RADICE, "Mortalità caldo 2026"))

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)
sf_use_s2(FALSE)

comuni <- read_csv("input/derivati/comuni.csv", locale = locale(encoding = "latin1"),
                   col_types = cols(cod_comune = "c"))
ecc <- read_csv("output/eccesso_province.csv", show_col_types = FALSE)

# ---- Geometrie a 110 province -------------------------------------------------

prov_geo <- readRDS(file.path(RADICE, "Temperature Copernicus/input/geo/geo_province_2025.rds")) |>
  st_transform(4326) |>
  mutate(cod_prov = ifelse(COD_UTS > 200, COD_UTS - 200L, COD_UTS)) |>
  filter(cod_prov %in% comuni$cod_prov) |>
  select(cod_prov)

com_geo <- read_sf(file.path(RADICE, "Referendum_Giustizia_2026/utilities/Com01012026_g_WGS84.json")) |>
  st_make_valid()
sardegna <- com_geo |>
  inner_join(comuni |> filter(cod_reg == 20) |> select(cod_comune, cod_prov),
             by = c("PRO_COM_T" = "cod_comune")) |>
  group_by(cod_prov) |>
  summarise(.groups = "drop")

province <- bind_rows(prov_geo, sardegna)
stopifnot(nrow(province) == 110, setequal(province$cod_prov, unique(comuni$cod_prov)))

regioni <- read_sf(file.path(RADICE, "Temperature Copernicus/input/geo/Reg01012025_g_WGS84.json")) |>
  st_make_valid()

# ---- Bin (larghezza costante 10 punti) -----------------------------------------

bin_levels <- c("meno di -10%", "da -10% a 0", "da 0 a +10%", "da +10% a +20%",
                "da +20% a +30%", "+30% e oltre")
bin_colours <- c("meno di -10%" = "#4F7FB8", "da -10% a 0" = "#BCD3EA",
                 "da 0 a +10%" = "#FCE4E7", "da +10% a +20%" = "#F49BA5",
                 "da +20% a +30%" = "#F12938", "+30% e oltre" = "#8E1622")

theme_mappa <- theme_minimal() +
  theme(
    text = element_text(family = "Source Sans Pro"),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
    panel.grid = element_blank(), panel.background = element_blank(),
    plot.background = element_blank(), legend.background = element_blank(),
    legend.key = element_blank(), legend.title = element_blank(),
    legend.position = c(0.99, 0.90), legend.justification = c(1, 1),
    legend.text = element_text(size = 11, color = "#1C1C1C", hjust = 0),
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 17.5, color = "#1C1C1C", hjust = 0,
                              margin = margin(b = 0.1, unit = "cm")),
    plot.subtitle = element_text(size = 11, color = "#1C1C1C", hjust = 0, lineheight = 1.35,
                                 margin = margin(b = 0.25, t = 0.1, unit = "cm")),
    plot.caption = element_text(size = 11, color = "#1C1C1C", hjust = 1,
                                margin = margin(t = 0.4, unit = "cm")))

mappa <- function(per, titolo, file) {
  d <- ecc |> filter(periodo == per) |>
    mutate(bin = factor(case_when(
      ecc_perc < -10 ~ bin_levels[1], ecc_perc < 0 ~ bin_levels[2],
      ecc_perc < 10 ~ bin_levels[3], ecc_perc < 20 ~ bin_levels[4],
      ecc_perc < 30 ~ bin_levels[5], TRUE ~ bin_levels[6]), levels = bin_levels),
      signif = ecc_inf > 0)
  stopifnot(nrow(d) == 110)
  print(table(d$bin))
  md <- province |> left_join(d, by = "cod_prov")
  punti <- suppressWarnings(md |> filter(signif) |> st_point_on_surface())

  p <- ggplot() +
    geom_sf(data = md, aes(fill = bin), color = "white", linewidth = 0.12) +
    geom_sf(data = regioni, fill = NA, color = "#2B2B2B", linewidth = 0.45) +
    geom_sf(data = punti, color = "#1C1C1C", size = 0.9) +
    scale_fill_manual(values = bin_colours, drop = FALSE, name = NULL) +
    guides(fill = guide_legend(
      reverse = TRUE, keyheight = unit(0.62, "cm"), keywidth = unit(0.45, "cm"),
      label.theme = element_text(family = "Source Sans Pro", size = 11,
                                 color = "#1C1C1C", hjust = 0))) +
    coord_sf(ylim = c(36.6, 47.1), expand = FALSE) +
    theme_mappa +
    labs(title = titolo,
         subtitle = paste0("Scarto tra decessi osservati e attesi senza caldo anomalo,\n",
                           "per provincia, ", per, " 2026; il punto indica uno\n",
                           "scarto statisticamente significativo (intervallo al 95%)"),
         caption = "Elaborazione di Lorenzo Ruffino su dati Istat (provvisori)")
  ggsave(file, p, width = 8, height = 9.3, units = "in", dpi = 220, bg = "white")
  cat("Salvata", file, "\n")
}

mappa("17-30 giugno", "I morti in più dell'ondata di fine giugno",
      "output/mappa_eccesso_17_30_giugno.png")
mappa("21-29 maggio", "I morti in più del caldo di fine maggio",
      "output/mappa_eccesso_21_29_maggio.png")
