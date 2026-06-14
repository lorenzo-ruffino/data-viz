# Quota di stranieri residenti per provincia italiana, 1° gennaio 2025.
# Dati comunali ISTAT (Dati_cittadinanza_2025.csv) aggregati a provincia via
# crosswalk LAU → NUTS-3 (GISCO 2021) con match per nome comune.
# Eseguito da `cd script && Rscript stranieri_residenti_italia.R`.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(giscoR)
  library(showtext)
  library(sf)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto()
showtext_opts(dpi = 300)

output_dir <- file.path("..", "output")
input_dir  <- file.path("..", "input")

# --- Dati comunali 2025 --------------------------------------------------

raw <- read_csv2(
  "/Users/lorenzoruffino/Downloads/Dati_cittadinanza_2025.csv",
  show_col_types = FALSE
)

# Totale e stranieri per comune (solo codici a 6 cifre = comuni, non aggregati
# provinciali a 3 cifre che duplicherebbero il conteggio)
comuni <- raw %>%
  filter(nchar(`Codice Istat`) == 6) %>%
  mutate(
    is_italiano = `Stato di cittadinanza` == "Italia",
    cittadinanza = if_else(is_italiano, "italiani", "stranieri")
  ) %>%
  group_by(`Codice Istat`, Denominazione, cittadinanza) %>%
  summarise(n = sum(Totale, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = cittadinanza, values_from = n, values_fill = 0) %>%
  mutate(
    tot        = italiani + stranieri,
    quota_for  = stranieri / tot * 100,
    quota_for  = round(quota_for, 1)
  )

# --- Crosswalk LAU → NUTS-3 ---------------------------------------------

# Carica o crea il crosswalk
rds_path <- file.path(input_dir, "lau_nuts3_crosswalk.rds")
if (file.exists(rds_path)) {
  crosswalk <- readRDS(rds_path)
} else {
  lau   <- gisco_get_lau(year = "2021", country = "IT")
  nuts3 <- gisco_get_nuts(country = "IT", nuts_level = 3, year = "2021",
                          resolution = "20")
  lau_ctr <- st_centroid(lau)
  joined  <- st_join(lau_ctr, nuts3[, c("NUTS_ID", "NAME_LATN")], join = st_within)
  crosswalk <- joined %>%
    st_drop_geometry() %>%
    select(LAU_NAME, NUTS_ID, prov_name = NAME_LATN)
  saveRDS(crosswalk, rds_path)
}

# Match comuni per nome
comuni_map <- comuni %>%
  inner_join(crosswalk, by = c("Denominazione" = "LAU_NAME"),
             relationship = "many-to-many")

cat("Comuni matched:", n_distinct(comuni_map$`Codice Istat`),
    "/", n_distinct(comuni$`Codice Istat`), "\n")

# --- Aggregazione provinciale (NUTS-3) -----------------------------------

dati_prov <- comuni_map %>%
  group_by(NUTS_ID, prov_name) %>%
  summarise(
    italiani  = sum(italiani, na.rm = TRUE),
    stranieri = sum(stranieri, na.rm = TRUE),
    tot       = sum(tot, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    quota_for = stranieri / tot * 100,
    quota_for = round(quota_for, 1)
  )

# Media nazionale
italia_tot  <- sum(dati_prov$tot)
italia_for  <- sum(dati_prov$stranieri)
media_ita   <- round(italia_for / italia_tot * 100, 1)

cat("Italia 2025:", italia_for, "/", italia_tot, "=", media_ita, "%\n")
cat("Range province:", min(dati_prov$quota_for), "-", max(dati_prov$quota_for), "\n")

# --- Geometrie NUTS-3 (GISCO 2021) ---------------------------------------

geo <- gisco_get_nuts(country = "IT", nuts_level = 3,
                      resolution = "03", year = "2021") %>%
  st_transform(3035) %>%
  select(NUTS_ID, NAME_LATN)

geo_dati <- geo %>% left_join(dati_prov, by = "NUTS_ID")

mancanti <- geo_dati %>% filter(is.na(quota_for)) %>% pull(NAME_LATN)
if (length(mancanti) > 0) {
  cat("Province senza dato:", paste(mancanti, collapse = ", "), "\n")
}

# --- Binning (2 punti percentuali costanti) -------------------------------

bin_levels <- c("2\u20134%", "4\u20136%", "6\u20138%", "8\u201310%",
                "10\u201312%", "12\u201314%", "14\u201316%", "16% e oltre")

bin_colours <- c(
  "2\u20134%"     = "#EDF3FC",
  "4\u20136%"     = "#CDE0F6",
  "6\u20138%"     = "#9ABFEA",
  "8\u201310%"    = "#5C9CDE",
  "10\u201312%"   = "#2279C3",
  "12\u201314%"   = "#0E5BAD",
  "14\u201316%"   = "#083C7A",
  "16% e oltre"   = "#041C3B"
)

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(quota_for)                   ~ NA_character_,
    quota_for <  4                     ~ "2\u20134%",
    quota_for >= 4  & quota_for <  6   ~ "4\u20136%",
    quota_for >= 6  & quota_for <  8   ~ "6\u20138%",
    quota_for >= 8  & quota_for <  10  ~ "8\u201310%",
    quota_for >= 10 & quota_for <  12  ~ "10\u201312%",
    quota_for >= 12 & quota_for <  14  ~ "12\u201314%",
    quota_for >= 14 & quota_for <  16  ~ "14\u201316%",
    TRUE                               ~ "16% e oltre"
  ), levels = bin_levels))

# --- Contorni regionali --------------------------------------------------

geo_reg <- geo_dati %>%
  mutate(reg = substr(NUTS_ID, 1, 4)) %>%
  group_by(reg) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# --- Mappa ---------------------------------------------------------------

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
    title = "Quanti sono gli stranieri in Italia",
    subtitle = paste0(
      "Quota di stranieri residenti per provincia, 1\u00B0 gennaio 2025. ",
      "Italia ", formatC(media_ita, format = "f", digits = 1,
                         decimal.mark = ","), "%"
    ),
    caption = "Elaborazione di Lorenzo Ruffino su dati Istat"
  )

ggsave(file.path(output_dir, "stranieri_residenti_italia.png"),
       p, width = 8.5, height = 9.5, dpi = 220, bg = "white")

cat("Mappa salvata.\n")

# --- CSV ----------------------------------------------------------------

write_csv(
  dati_prov %>%
    arrange(desc(quota_for)) %>%
    select(provincia = prov_name, NUTS_ID, italiani, stranieri,
           tot, quota_for),
  file.path(output_dir, "stranieri_residenti_italia.csv")
)
