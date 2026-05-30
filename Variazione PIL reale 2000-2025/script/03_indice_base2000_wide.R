library(eurostat)
library(dplyr)
library(tidyr)
library(readr)

# nama_10_gdp — PIL ai prezzi di mercato (B1GQ), volumi concatenati indice 2015=100 (CLV_I15)
# Obiettivo: indice del PIL reale ribasato al 2000 = 100, formato wide (una riga per anno)
geos <- c(
  IT = "Italia",
  ES = "Spagna",
  FR = "Francia",
  DE = "Germania",
  EU27_2020 = "Unione europea"
)

dat <- get_eurostat(
  "nama_10_gdp",
  filters = list(
    na_item = "B1GQ",     # PIL ai prezzi di mercato
    unit    = "CLV_I15",  # volumi concatenati, indice 2015 = 100
    geo     = names(geos)
  ),
  time_format = "num"
)

time_col <- if ("time" %in% names(dat)) "time" else "TIME_PERIOD"

dat <- dat %>%
  rename(anno = !!time_col, geo_code = geo) %>%
  filter(anno >= 2000) %>%
  select(geo_code, anno, value = values) %>%
  group_by(geo_code) %>%
  arrange(anno) %>%
  mutate(indice = value / value[anno == 2000] * 100) %>%  # ribasa a 2000 = 100
  ungroup()

wide <- dat %>%
  mutate(paese = geos[geo_code]) %>%
  select(anno, paese, indice) %>%
  mutate(indice = round(indice, 2)) %>%
  pivot_wider(names_from = paese, values_from = indice) %>%
  arrange(anno) %>%
  select(anno, Italia, Spagna, Francia, Germania, `Unione europea`)  # ordine richiesto

out_path <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Variazione PIL reale 2000-2025/output/indice_pil_reale_base2000_wide.csv"
write_csv(wide, out_path)

cat("Salvato:", out_path, "\n")
cat("Anni:", min(wide$anno), "-", max(wide$anno), "(", nrow(wide), "righe )\n\n")
print(wide, n = Inf)
