# Tasso di fecondità totale (numero medio di figli per donna) per provincia
# italiana, ultimo anno disponibile (2025).
# Eseguito da `cd script && Rscript fecondita_province.R`.
#
# Mappa choropleth provinciale (NUTS-3) con scala monocromatica rossa a classi
# discrete (rosso scuro = fecondità bassa): bin da 0,10, classi aperte agli
# estremi.
# Fonte: Istat, Indicatori di fecondità, dataflow 25_326_DF_DCIS_FECONDITA1_5
# (tasso di fecondità totale per anno di evento e cittadinanza della madre),
# cittadinanza totale, 2015-2025, scaricato via MCP istat in input/.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(giscoR)
  library(showtext)
  library(sf)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)

CAP_ISTAT <- "Elaborazione di Lorenzo Ruffino su dati Istat"

fmt_num <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, decimal.mark = ",")
}

# --- Dati -------------------------------------------------------------------

raw <- read_tsv("../input/istat_25_326_DF_DCIS_FECONDITA1_5.tsv",
                show_col_types = FALSE)

anno_label <- max(raw$TIME_PERIOD, na.rm = TRUE)

italia_val  <- raw %>% filter(REF_AREA == "IT", TIME_PERIOD == anno_label) %>% pull(OBS_VALUE)
italia_prec <- raw %>% filter(REF_AREA == "IT", TIME_PERIOD == anno_label - 1) %>% pull(OBS_VALUE)

# Crosswalk codici Istat ITTER107 → codici NUTS 2021 delle geometrie GISCO
# (stesso schema di "Soddisfazione per la vita per provincia").
nuts_da_istat <- function(code) {
  special <- c(ITC45 = "ITC4C", IT108 = "ITC4D",
               ITF41 = "ITF46", ITF42 = "ITF47", IT110 = "ITF48",
               ITG25 = "ITG2D", ITG26 = "ITG2E", ITG27 = "ITG2F",
               ITG28 = "ITG2G", IT111 = "ITG2H", IT109 = "ITI35")
  out <- ifelse(code %in% names(special), unname(special[code]), code)
  out <- ifelse(startsWith(out, "ITD"), sub("^ITD", "ITH", out), out)
  out <- ifelse(startsWith(out, "ITE"), sub("^ITE", "ITI", out), out)
  out
}

# Province: codici a 5 caratteri (le regioni ne hanno 4, le ripartizioni 3).
dati_prov <- raw %>%
  filter(nchar(REF_AREA) == 5, TIME_PERIOD == anno_label) %>%
  transmute(REF_AREA, NUTS_ID = nuts_da_istat(REF_AREA), valore = OBS_VALUE)

stopifnot(nrow(dati_prov) == 107)

# --- Geometrie province (NUTS-3, GISCO 2021) -------------------------------

geo <- gisco_get_nuts(country = "IT", nuts_level = 3,
                      resolution = "03", year = "2021") %>%
  st_transform(3035) %>%
  select(NUTS_ID, NAME_LATN)

geo_dati <- geo %>% left_join(dati_prov, by = "NUTS_ID")

mancanti <- geo_dati %>% filter(is.na(valore)) %>% pull(NAME_LATN)
if (length(mancanti) > 0) stop("Province senza dato: ", paste(mancanti, collapse = ", "))

cat("Anno:", anno_label, "\n")
cat("Italia:", italia_val, "(anno prima:", italia_prec, ")\n")
cat("Range province:", paste(range(geo_dati$valore), collapse = " - "), "\n")

write_csv(
  st_drop_geometry(geo_dati) %>%
    select(codice_istat = REF_AREA, NUTS_ID, provincia = NAME_LATN,
           figli_per_donna = valore) %>%
    arrange(desc(figli_per_donna)),
  "../output/fecondita_province.csv"
)

# --- Binning monocromatico --------------------------------------------------

bin_levels <- c("meno di 0,90", "da 0,90 a 1,00", "da 1,00 a 1,10",
                "da 1,10 a 1,20", "da 1,20 a 1,30", "1,30 e oltre")
bin_colours <- setNames(
  c("#6E0D1A", "#B8202B", "#E2533C", "#F68E6E", "#FCC5B0", "#FEEDE7"),
  bin_levels
)

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    valore < 0.90 ~ bin_levels[1],
    valore < 1.00 ~ bin_levels[2],
    valore < 1.10 ~ bin_levels[3],
    valore < 1.20 ~ bin_levels[4],
    valore < 1.30 ~ bin_levels[5],
    TRUE          ~ bin_levels[6]
  ), levels = bin_levels))

print(table(geo_dati$bin))

# --- Contorni regionali (dissolvenza province → NUTS-2) --------------------

geo_reg <- geo_dati %>%
  mutate(reg = substr(NUTS_ID, 1, 4)) %>%
  group_by(reg) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# --- Mappa ------------------------------------------------------------------

# Tela 8,5 in: corpi del tema riscalati di ~1,06 rispetto agli 8 in nominali.
p <- ggplot(geo_dati) +
  geom_sf(aes(fill = bin), color = "white", linewidth = 0.15) +
  geom_sf(data = geo_reg, fill = NA, color = "#1C1C1C", linewidth = 0.5) +
  scale_fill_manual(
    values = bin_colours,
    drop = FALSE,
    na.value = COL_NA_MAPPA,
    name = NULL,
    breaks = bin_levels
  ) +
  guides(fill = guide_legend(
    reverse = TRUE,             # valori alti (chiari) in cima
    keyheight = unit(0.55, "cm"), keywidth = unit(0.45, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9.5,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.95),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm"),
        plot.title = element_text(family = "Source Sans Pro SemiBold",
                                  size = 15, color = "#1C1C1C", hjust = 0,
                                  margin = margin(b = 0.1, unit = "cm")),
        plot.subtitle = element_text(size = 9.5, color = "#1C1C1C", hjust = 0,
                                     lineheight = 1.35,
                                     margin = margin(b = 0.25, t = 0.1, unit = "cm")),
        plot.caption = element_text(size = 9.5, color = "#1C1C1C", hjust = 1,
                                    margin = margin(t = 0.5, unit = "cm"))) +
  labs(
    title = "In Sardegna meno di un figlio per donna",
    subtitle = paste0(
      "Tasso di fecondità totale (numero medio di figli per donna),\n",
      "province italiane, ", anno_label, ". Italia ", fmt_num(italia_val),
      " (", fmt_num(italia_prec), " nel ", anno_label - 1, ")"
    ),
    caption = CAP_ISTAT
  )

ggsave("../output/fecondita_province.png",
       plot = p, width = 8.5, height = 9.5, units = "in", dpi = 220, bg = "white")

cat("Mappa salvata in ../output/fecondita_province.png\n")
