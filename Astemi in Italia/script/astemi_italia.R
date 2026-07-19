# Astemi (persone che non consumano alcol) per regione, 18 anni e piu, 2025.
# Eseguito da `cd script && Rscript astemi_italia.R`.
#
# Astemi = 11_ALC_NFUORI (persone che non consumano vino, birra ne alcolici
# fuori pasto). Il dato regionale Istat e' solo per 11+; la quota 18+ e' STIMATA
# applicando al regionale 11+ il rapporto nazionale astemi(18+)/astemi(11+),
# ricavato dal dato nazionale per classe d'eta (dataflow _223, THV e HSC).
# Fonti in input/: istat_alcol.csv (regionale 11+), istat_astemi_eta_nazionale.csv.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(showtext)
  library(sf)
})

# --- Tema -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

# --- Fattore di riproporzione 18+ dal nazionale per eta ---------------------

naz <- read_csv("../input/istat_astemi_eta_nazionale.csv", show_col_types = FALSE)
nv <- function(a, m) naz$OBS_VALUE[naz$AGE == a & naz$MEASURE == m][1]
popn <- function(a) nv(a, "THV") / nv(a, "HSC") * 100        # popolazione (migliaia)

astemi11_naz <- nv("Y_GE11", "HSC")
ast18_cnt <- nv("Y_GE11", "THV") - nv("Y11-13", "THV") - nv("Y14-17", "THV")
pop18     <- popn("Y_GE11") - popn("Y11-13") - popn("Y14-17")
astemi18_naz <- ast18_cnt / pop18 * 100
k <- astemi18_naz / astemi11_naz

cat(sprintf("Nazionale astemi 11+: %.1f  | 18+: %.1f  | k = %.4f\n",
            astemi11_naz, astemi18_naz, k))

# --- Dati regionali: astemi 11+ -> stima 18+ -------------------------------

grezzi <- read_csv("../input/istat_alcol.csv", show_col_types = FALSE) %>%
  filter(MEASURE == "HSC", DATA_TYPE == "11_ALC_NFUORI")
ultimo_anno <- max(grezzi$TIME_PERIOD, na.rm = TRUE)

reg <- grezzi %>%
  filter(TIME_PERIOD == ultimo_anno) %>%
  transmute(REF_AREA, astemi_11 = OBS_VALUE, valore = OBS_VALUE * k)  # stima 18+

valore_italia <- reg$valore[reg$REF_AREA == "IT"]

cw <- c(
  ITC1 = "ITC1", ITC2 = "ITC2", ITC3 = "ITC3", ITC4 = "ITC4",
  ITD1 = "ITH1", ITD2 = "ITH2", ITD3 = "ITH3", ITD4 = "ITH4", ITD5 = "ITH5",
  ITE1 = "ITI1", ITE2 = "ITI2", ITE3 = "ITI3", ITE4 = "ITI4",
  ITF1 = "ITF1", ITF2 = "ITF2", ITF3 = "ITF3", ITF4 = "ITF4",
  ITF5 = "ITF5", ITF6 = "ITF6", ITG1 = "ITG1", ITG2 = "ITG2"
)
regioni <- reg %>% filter(REF_AREA %in% names(cw)) %>% mutate(NUTS_ID = cw[REF_AREA])

geo <- load_geo_italia_regioni() %>% st_transform(3035)
geo_dati <- geo %>% left_join(regioni, by = "NUTS_ID")

cat("Italia (stima 18+):", round(valore_italia, 1), "\n")
cat("Classifica (stima 18+):\n")
print(geo_dati %>% st_drop_geometry() %>% select(NAME_LATN, valore) %>%
        arrange(desc(valore)) %>% mutate(valore = round(valore, 1)), n = 21)

write_csv(
  geo_dati %>% st_drop_geometry() %>%
    transmute(nuts_id = NUTS_ID, regione = NAME_LATN, anno = ultimo_anno,
              astemi_11plus = round(astemi_11, 1),
              astemi_18plus_stima = round(valore, 1)) %>%
    arrange(desc(astemi_18plus_stima)),
  "../output/astemi_italia.csv"
)

# --- Binning discreto (classi costanti di 2 punti) --------------------------

bin_levels <- c("meno di 26", "da 26 a 28", "da 28 a 30",
                "da 30 a 32", "da 32 a 34", "34 e oltre")
# Rampa blu monocromatica a 6 passi, dal chiaro (basso) allo scuro (alto).
bin_colours <- setNames(
  colorRampPalette(c("#EAF1FA", "#A1C6EE", "#5C9CDE", "#0E5BAD", "#06366A"))(6),
  bin_levels
)
bin_scuri <- c("da 30 a 32", "da 32 a 34", "34 e oltre")   # -> etichetta bianca
# Etichette legenda con il simbolo di percentuale.
bin_labels <- c("meno di 26%", "da 26% a 28%", "da 28% a 30%",
                "da 30% a 32%", "da 32% a 34%", "34% e oltre")

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(valore) ~ NA_character_,
    valore <  26  ~ "meno di 26",
    valore <  28  ~ "da 26 a 28",
    valore <  30  ~ "da 28 a 30",
    valore <  32  ~ "da 30 a 32",
    valore <  34  ~ "da 32 a 34",
    TRUE          ~ "34 e oltre"
  ), levels = bin_levels))

# --- Etichetta valore per regione (percentuale intera) ----------------------

fmt_val <- function(v) formatC(v, format = "f", digits = 0, decimal.mark = ",")

pts <- suppressWarnings(st_point_on_surface(st_geometry(geo_dati)))
coords <- st_coordinates(pts)
geo_dati$lx <- coords[, "X"]; geo_dati$ly <- coords[, "Y"]

nudge <- function(id, x, y) {
  dx <- case_when(id == "ITC2" ~   5000,
                  id == "ITH2" ~  -8000, id == "ITF2" ~   8000, TRUE ~ 0)
  dy <- case_when(id == "ITC2" ~   4000, id == "ITH1" ~  10000,
                  id == "ITH2" ~ -12000,
                  id == "ITF3" ~  12000, id == "ITF6" ~  16000,
                  id == "ITF5" ~ -10000, TRUE ~ 0)
  list(x = x + dx, y = y + dy)
}
adj <- nudge(geo_dati$NUTS_ID, geo_dati$lx, geo_dati$ly)
geo_dati$lx <- adj$x; geo_dati$ly <- adj$y

# Liguria: il centroide cade in mare (forma a mezzaluna). Posiziono l'etichetta
# nel punto centrale a terra, nella curva attorno a Genova.
geo_dati$lx[geo_dati$NUTS_ID == "ITC3"] <- 4222862
geo_dati$ly[geo_dati$NUTS_ID == "ITC3"] <- 2374000

geo_dati <- geo_dati %>%
  mutate(label_color = case_when(
    NUTS_ID == "ITC3"  ~ "#1C1C1C",      # Liguria: label nel mare, sempre scura
    bin %in% bin_scuri ~ "white",
    TRUE               ~ "#1C1C1C"
  ))

# --- Mappa ------------------------------------------------------------------

p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "white", linewidth = 0.3) +
  geom_text(aes(x = lx, y = ly, label = fmt_val(valore), color = label_color),
            family = "Source Sans Pro", size = 2.7, fontface = "bold") +
  scale_color_identity() +
  scale_fill_manual(values = bin_colours, drop = FALSE, labels = bin_labels,
                    na.value = COL_NA_MAPPA, name = NULL, breaks = bin_levels) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.7, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(xlim = c(4055456, 5049071), ylim = c(1505000, 2665352),
           crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.94, 0.90),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm"),
        plot.caption = element_text(size = 9, color = "#1C1C1C", hjust = 1,
                                    margin = margin(t = 0.15, unit = "cm"))) +
  labs(
    title = "Al Sud ci sono più astemi che al Nord",
    subtitle = paste0(
      "Quota stimata di persone di 18 anni e più che non consumano alcol, ",
      "per regione, ", ultimo_anno),
    caption = "Elaborazione di Lorenzo Ruffino su dati Istat"
  )

ggsave("../output/astemi_italia.png",
       plot = p, width = 7.5, height = 8.7, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/astemi_italia.png\n")
