# Quota di occupati in agricoltura, silvicoltura e pesca (NACE A) sul totale
# degli occupati per regione NUTS-2, 2023 (ultimo anno completo).
# Fonte: Eurostat nama_10r_3empers (conti regionali, occupati in migliaia).
# Eseguito da `cd script && Rscript agricoltura_europa.R`.

source("/Users/lorenzoruffino/Documents/Progetti/data-viz/utilities/R/mappe.R")
suppressPackageStartupMessages({
  library(eurostat)
  library(tidyverse)
  library(sf)
  library(giscoR)
  library(showtext)
})

font_add_google("Source Sans 3", "Source Sans Pro")
font_add_google("Source Sans 3", "Source Sans Pro SemiBold", regular.wt = 600)
showtext_auto()
showtext_opts(dpi = 300)
CAP_EUROSTAT <- "Elaborazione di Lorenzo Ruffino su dati Eurostat"

ANNO <- 2023

# --- Dati -------------------------------------------------------------------

dati_raw <- get_eurostat("nama_10r_3empers", time_format = "num",
                         cache = FALSE, update_cache = TRUE)

base <- dati_raw %>%
  filter(wstatus == "EMP", unit == "THS",
         nace_r2 %in% c("A", "TOTAL"),
         TIME_PERIOD == ANNO, !is.na(values))

# NUTS-2 (4 caratteri), esclusi i territori extra-regio (ITZZ, FRZZ, ...)
share_nuts2 <- base %>%
  filter(nchar(geo) == 4, substr(geo, 3, 4) != "ZZ") %>%
  select(geo, nace_r2, values) %>%
  pivot_wider(names_from = nace_r2, values_from = values) %>%
  filter(!is.na(A), !is.na(TOTAL), TOTAL > 0) %>%
  mutate(share = A / TOTAL * 100)

cat("Regioni NUTS-2 con quota:", nrow(share_nuts2), "\n")
cat("Range:", round(min(share_nuts2$share), 1), "-",
    round(max(share_nuts2$share), 1), "\n")
agg <- unique(base$geo[grepl("_", base$geo)])
if (length(agg) > 0) cat("Codici aggregati da gestire:", paste(agg, collapse = ", "), "\n")

# --- Quote nazionali (per tweet e diagnostica) ------------------------------

share_naz <- base %>%
  filter(nchar(geo) == 2) %>%
  select(geo, nace_r2, values) %>%
  pivot_wider(names_from = nace_r2, values_from = values) %>%
  filter(!is.na(A), !is.na(TOTAL)) %>%
  mutate(share = round(A / TOTAL * 100, 1)) %>%
  arrange(desc(share))

eu27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
          "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK")
eu27_pres <- share_naz %>% filter(geo %in% eu27)
media_eu <- sum(eu27_pres$A) / sum(eu27_pres$TOTAL) * 100
cat("Paesi EU27 con dato nazionale:", nrow(eu27_pres), "su 27\n")
cat("Media EU27 ponderata:", round(media_eu, 1), "%\n\n")
print(as.data.frame(share_naz %>% select(geo, share)))

# --- Geometrie --------------------------------------------------------------

sfondo <- load_geo_europa()

nuts2 <- gisco_get_nuts(year = "2024", nuts_level = 2, resolution = "10") %>%
  filter(CNTR_CODE != "TR") %>%
  select(NUTS_ID) %>%
  st_transform(3035)

geo_dati <- nuts2 %>%
  left_join(share_nuts2 %>% select(NUTS_ID = geo, share), by = "NUTS_ID")

na_regions <- geo_dati %>% filter(is.na(share)) %>% pull(NUTS_ID)
cat("\nRegioni senza dato (grigie):", length(na_regions), "\n")
cat("  Codici:", paste(na_regions, collapse = ", "), "\n")
orfani <- setdiff(share_nuts2$geo, nuts2$NUTS_ID)
if (length(orfani) > 0) cat("Codici dati senza geometria:", paste(orfani, collapse = ", "), "\n")

# --- Binning (3 punti percentuali costanti) ---------------------------------

bin_levels <- c("Meno del 3%", "3–6%", "6–9%", "9–12%",
                "12–15%", "15–18%", "18–21%", "21% e oltre")

bin_colours <- setNames(
  viridisLite::viridis(length(bin_levels), option = "A", direction = -1),
  bin_levels
)

geo_dati <- geo_dati %>%
  mutate(bin = factor(case_when(
    is.na(share)               ~ NA_character_,
    share <  3                 ~ bin_levels[1],
    share >= 3  & share < 6    ~ bin_levels[2],
    share >= 6  & share < 9    ~ bin_levels[3],
    share >= 9  & share < 12   ~ bin_levels[4],
    share >= 12 & share < 15   ~ bin_levels[5],
    share >= 15 & share < 18   ~ bin_levels[6],
    share >= 18 & share < 21   ~ bin_levels[7],
    TRUE                       ~ bin_levels[8]
  ), levels = bin_levels))

# --- Grafico ----------------------------------------------------------------

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
    title = "L'agricoltura dà ancora lavoro nell'Europa orientale",
    subtitle = paste0(
      "Quota di occupati in agricoltura, silvicoltura e pesca sul totale ",
      "degli occupati\nper regione, ", ANNO, ". In grigio i paesi senza dato"
    ),
    caption = CAP_EUROSTAT
  )

ggsave("../output/agricoltura_europa.png",
       p, width = 9, height = 9, dpi = 220, bg = "white")

cat("\nMappa salvata.\n")

# --- CSV --------------------------------------------------------------------

export <- share_nuts2 %>%
  mutate(share = round(share, 2)) %>%
  arrange(desc(share)) %>%
  select(NUTS_ID = geo, occupati_agricoltura_migliaia = A,
         occupati_totale_migliaia = TOTAL, quota = share)

write_csv(export, "../output/agricoltura_europa.csv")

cat("\nTop 5:\n")
print(head(export, 5))
