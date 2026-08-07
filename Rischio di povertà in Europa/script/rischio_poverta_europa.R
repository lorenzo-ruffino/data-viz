source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(eurostat)
  library(tidyverse)
  library(sf)
  library(giscoR)
  library(showtext)
})

font_add_google("Source Sans 3", "Source Sans Pro")
showtext_auto(); showtext_opts(dpi = 300)
CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

# --- Dati -------------------------------------------------------------------
dati_raw <- get_eurostat("ilc_li41", time_format = "num",
                          cache = FALSE, update_cache = TRUE)

# Dati NUTS-2 (4 caratteri) + gestione codici aggregati
dati_nuts2 <- dati_raw %>%
  filter(freq == "A", unit == "PC", TIME_PERIOD == 2025, nchar(geo) == 4)

# Splitta FI19_20 in due righe per FI19 e FI20
fi_agg <- dati_raw %>%
  filter(freq == "A", unit == "PC", TIME_PERIOD == 2025, geo == "FI19_20")
if (nrow(fi_agg) > 0) {
  dati_nuts2 <- bind_rows(
    dati_nuts2,
    fi_agg %>% mutate(geo = "FI19"),
    fi_agg %>% mutate(geo = "FI20")
  )
}

# Dati nazionali per paesi piccoli senza NUTS-2
dati_naz <- dati_raw %>%
  filter(freq == "A", unit == "PC", TIME_PERIOD == 2025, nchar(geo) == 2) %>%
  select(CNTR_ID = geo, naz_value = values)

small_nat_to_nuts2 <- c(CY = "CY00", EE = "EE00", LV = "LV00", LU = "LU00", MT = "MT00")
dati_small <- dati_naz %>%
  filter(CNTR_ID %in% names(small_nat_to_nuts2)) %>%
  mutate(NUTS_ID = small_nat_to_nuts2[CNTR_ID]) %>%
  select(NUTS_ID, values = naz_value)

it_nat <- dati_naz %>% filter(CNTR_ID == "IT")

cat("Italia nazionale:", round(it_nat$naz_value, 1), "%\n")
cat("N righe NUTS-2:", nrow(dati_nuts2), "\n")
cat("N paesi piccoli:", nrow(dati_small), "\n")
cat("Range NUTS-2:", min(dati_nuts2$values, na.rm = TRUE),
    "-", max(dati_nuts2$values, na.rm = TRUE), "\n")

# --- Geometrie --------------------------------------------------------------
# Sfondo: 38 paesi europei (EU27 + EFTA + UK + Balcani, TR già escluso)
sfondo <- load_geo_europa()

# NUTS-2: GISCO 2024, escludi Turchia
nuts2 <- gisco_get_nuts(year = "2024", nuts_level = 2, resolution = "10") %>%
  filter(CNTR_CODE != "TR") %>%
  select(NUTS_ID) %>%
  st_transform(3035)

# Unisci dati
dati_join <- bind_rows(
  dati_nuts2 %>% select(NUTS_ID = geo, values),
  dati_small
)

geo_dati <- nuts2 %>%
  left_join(dati_join, by = "NUTS_ID")

# Conta NA per diagnostica
n_na <- sum(is.na(geo_dati$values))
cat("Regioni senza dato (grigie):", n_na, "\n")
if (n_na > 0) {
  na_regions <- geo_dati %>% filter(is.na(values)) %>% pull(NUTS_ID)
  cat("  Codici:", paste(na_regions, collapse = ", "), "\n")
}

# --- Binning ----------------------------------------------------------------
bin_levels <- c(
  "Meno del 5%",
  "5\u201310%",
  "10\u201315%",
  "15\u201320%",
  "20\u201325%",
  "25\u201330%",
  "30\u201335%",
  "35% e oltre"
)

bin_colours <- setNames(
  viridisLite::viridis(length(bin_levels), option = "A", direction = -1),
  bin_levels
)

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(values)           ~ NA_character_,
    values < 5              ~ bin_levels[1],
    values >= 5  & values < 10 ~ bin_levels[2],
    values >= 10 & values < 15 ~ bin_levels[3],
    values >= 15 & values < 20 ~ bin_levels[4],
    values >= 20 & values < 25 ~ bin_levels[5],
    values >= 25 & values < 30 ~ bin_levels[6],
    values >= 30 & values < 35 ~ bin_levels[7],
    TRUE                    ~ bin_levels[8]
  ), levels = bin_levels))

# --- Grafico -----------------------------------------------------------------
dummy_legenda <- data.frame(bin = factor(bin_levels, levels = bin_levels))

p <- ggplot() +
  geom_sf(data = sfondo, fill = COL_NA_MAPPA, color = "white",
          linewidth = 0.15, show.legend = FALSE) +
  geom_sf(data = geo_dati, aes(fill = bin), color = NA, linewidth = 0,
          show.legend = FALSE) +
  geom_sf(data = sfondo, fill = NA, color = "#1C1C1C", linewidth = 0.28) +
  geom_rect(data = dummy_legenda, aes(fill = bin),
            xmin = -1e7, xmax = -1e7 + 1, ymin = -1e7, ymax = -1e7 + 1,
            inherit.aes = FALSE, show.legend = TRUE) +
  scale_fill_manual(
    values = bin_colours,
    drop = FALSE,
    na.value = COL_NA_MAPPA,
    name = NULL,
    breaks = bin_levels
  ) +
  guides(fill = guide_legend(
    reverse = TRUE,
    keyheight = unit(0.68, "cm"), keywidth = unit(0.45, "cm"),
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
    title = "Il Mezzogiorno è l'area più a rischio di povertà in Europa",
    subtitle = paste0(
      "Quota di popolazione a rischio di povertà per regione, 2025. ",
      "Misura la percentuale di\npersone con un reddito disponibile ",
      "equivalente inferiore al 60% della mediana nazionale"
    ),
    caption = CAP_EUROSTAT
  )

ggsave("../output/rischio_poverta_europa.png",
       p, width = 9, height = 9, dpi = 220, bg = "white")

dati_out <- dati_join %>%
  filter(!is.na(values)) %>%
  arrange(desc(values))
write_csv(dati_out, "../output/rischio_poverta_europa.csv")
cat("\nMappa e CSV salvati.\n")
