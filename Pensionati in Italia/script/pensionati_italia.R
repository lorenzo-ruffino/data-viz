# Pensionati ogni 100 abitanti per provincia, 2022.
# Numero pensionati: Istat SDMX 46_812 (input/istat_46_812_pensionati.csv).
# Popolazione al 1° gennaio 2022: Istat SDMX 22_289
# (input/istat_22_289_popolazione.csv).
# Eseguito da `cd script && Rscript pensionati_italia.R`.

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

output_dir <- file.path("..", "output")
input_dir  <- file.path("..", "input")

# --- Pensionati per provincia (2022) --------------------------------------

pens_raw <- read_csv(file.path(input_dir, "istat_46_812_pensionati.csv"),
                     show_col_types = FALSE)

pensionati <- pens_raw %>%
  filter(DATA_TYPE == "P_RSNU", nchar(REF_AREA) == 5) %>%
  select(cod_istat = REF_AREA, pensionati = OBS_VALUE)

italia_pens <- pens_raw %>%
  filter(DATA_TYPE == "P_RSNU", REF_AREA == "IT") %>%
  pull(OBS_VALUE)

# --- Popolazione per provincia al 1° gennaio 2022 -------------------------

pop_raw <- read_csv(file.path(input_dir, "istat_22_289_popolazione.csv"),
                    show_col_types = FALSE)

popolazione <- pop_raw %>%
  filter(TIME_PERIOD == "2022-01-01", SEX == "9", AGE == "TOTAL",
         nchar(REF_AREA) == 5) %>%
  select(cod_istat = REF_AREA, popolazione = OBS_VALUE)

italia_pop <- pop_raw %>%
  filter(TIME_PERIOD == "2022-01-01", SEX == "9", AGE == "TOTAL",
         REF_AREA == "IT") %>%
  pull(OBS_VALUE)

media_ita <- round(italia_pens / italia_pop * 100, 1)
cat("Italia 2022:", italia_pens, "/", italia_pop, "=", media_ita, "%\n")

# --- Quota per provincia ---------------------------------------------------

dati_prov <- pensionati %>%
  inner_join(popolazione, by = "cod_istat") %>%
  mutate(quota = round(pensionati / popolazione * 100, 1))

cat("Province con dato:", nrow(dati_prov), "\n")
cat("Range province:", min(dati_prov$quota), "-", max(dati_prov$quota), "\n")

# --- Crosswalk codici ITTER107 -> NUTS-3 GISCO ----------------------------
# I codici territoriali Istat (CL_ITTER107) restano sulla numerazione
# NUTS2006/2010: divergono da GISCO per Milano/Monza, Foggia/Bari/BAT,
# le province sarde, Fermo e per le lettere di Nord-est (ITD->ITH) e
# Centro (ITE->ITI).

cross_speciali <- c(
  "ITC45" = "ITC4C",  # Milano
  "IT108" = "ITC4D",  # Monza e della Brianza
  "IT109" = "ITI35",  # Fermo
  "ITF41" = "ITF46",  # Foggia
  "ITF42" = "ITF47",  # Bari
  "IT110" = "ITF48",  # Barletta-Andria-Trani
  "ITG25" = "ITG2D",  # Sassari
  "ITG26" = "ITG2E",  # Nuoro
  "ITG27" = "ITG2F",  # Cagliari
  "ITG28" = "ITG2G",  # Oristano
  "IT111" = "ITG2H"   # Sud Sardegna
)

dati_prov <- dati_prov %>%
  mutate(
    NUTS_ID = case_when(
      cod_istat %in% names(cross_speciali) ~ cross_speciali[cod_istat],
      substr(cod_istat, 1, 3) == "ITD" ~ sub("^ITD", "ITH", cod_istat),
      substr(cod_istat, 1, 3) == "ITE" ~ sub("^ITE", "ITI", cod_istat),
      TRUE ~ cod_istat
    )
  )

# --- Geometrie NUTS-3 (GISCO 2024) ----------------------------------------

geo <- gisco_get_nuts(country = "IT", nuts_level = 3,
                      resolution = "03", year = "2024") %>%
  st_transform(3035) %>%
  select(NUTS_ID, NAME_LATN)

geo_dati <- geo %>% left_join(dati_prov, by = "NUTS_ID")

mancanti <- geo_dati %>% filter(is.na(quota)) %>% pull(NAME_LATN)
if (length(mancanti) > 0) {
  cat("Province GISCO senza dato:", paste(mancanti, collapse = ", "), "\n")
}
orfani <- setdiff(dati_prov$NUTS_ID, geo$NUTS_ID)
if (length(orfani) > 0) {
  cat("Codici dati senza geometria:", paste(orfani, collapse = ", "), "\n")
}

# --- Binning (2 punti percentuali costanti) -------------------------------

bin_levels <- c("Meno del 22%", "22–24%", "24–26%", "26–28%",
                "28–30%", "30–32%", "32–34%", "34% e oltre")

bin_colours <- c(
  "Meno del 22%" = "#EDF3FC",
  "22–24%"  = "#CDE0F6",
  "24–26%"  = "#9ABFEA",
  "26–28%"  = "#5C9CDE",
  "28–30%"  = "#2279C3",
  "30–32%"  = "#0E5BAD",
  "32–34%"  = "#083C7A",
  "34% e oltre"  = "#041C3B"
)

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(quota)                 ~ NA_character_,
    quota <  22                  ~ "Meno del 22%",
    quota >= 22 & quota < 24     ~ "22–24%",
    quota >= 24 & quota < 26     ~ "24–26%",
    quota >= 26 & quota < 28     ~ "26–28%",
    quota >= 28 & quota < 30     ~ "28–30%",
    quota >= 30 & quota < 32     ~ "30–32%",
    quota >= 32 & quota < 34     ~ "32–34%",
    TRUE                         ~ "34% e oltre"
  ), levels = bin_levels))

# --- Contorni regionali ---------------------------------------------------

geo_reg <- geo_dati %>%
  mutate(reg = substr(NUTS_ID, 1, 4)) %>%
  group_by(reg) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# --- Mappa ----------------------------------------------------------------

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
    reverse = TRUE,
    keyheight = unit(0.65, "cm"), keywidth = unit(0.5, "cm"),
    label.theme = element_text(family = "Source Sans Pro", size = 9.5,
                               color = "#1C1C1C", hjust = 0)
  )) +
  coord_sf(crs = 3035, expand = FALSE) +
  theme_map() +
  theme(legend.position = c(0.99, 0.95),
        legend.justification = c(1, 1),
        legend.spacing.y = unit(0, "cm")) +
  labs(
    title = "Quanti pensionati ci sono in Italia",
    subtitle = paste0(
      "Pensionati ogni 100 abitanti per provincia di residenza, 2022. ",
      "Italia ", formatC(media_ita, format = "f", digits = 1,
                         decimal.mark = ","), "%"
    ),
    caption = "Elaborazione di Lorenzo Ruffino su dati Istat"
  )

ggsave(file.path(output_dir, "pensionati_italia.png"),
       p, width = 8.5, height = 9.5, dpi = 220, bg = "white")

cat("Mappa salvata.\n")

# --- CSV -------------------------------------------------------------------

export <- geo_dati %>%
  st_drop_geometry() %>%
  filter(!is.na(quota)) %>%
  arrange(desc(quota)) %>%
  select(provincia = NAME_LATN, NUTS_ID, pensionati, popolazione, quota)

write_csv(export, file.path(output_dir, "pensionati_italia.csv"))

cat("\nTop 5:\n")
print(head(export, 5))
cat("\nBottom 5:\n")
print(tail(export, 5))
