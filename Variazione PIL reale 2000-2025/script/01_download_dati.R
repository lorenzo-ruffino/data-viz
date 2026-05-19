library(eurostat)
library(dplyr)
library(tidyr)
library(readr)

# nama_10_gdp: GDP and main components — chain linked volumes (2010), index 2010=100
# Usiamo CLV_I10 (chain linked volumes, index) per confrontare le variazioni reali
dat <- get_eurostat(
  "nama_10_gdp",
  filters = list(
    na_item = "B1GQ",         # PIL ai prezzi di mercato
    unit    = "CLV_I15"       # chain linked volumes, index 2015 = 100
  ),
  time_format = "num"
)

cat("Colonne disponibili:\n")
print(names(dat))

time_col <- if ("time" %in% names(dat)) "time" else "TIME_PERIOD"

dat <- dat %>%
  rename(year = !!time_col) %>%
  filter(year %in% c(2000, 2025)) %>%
  select(geo, year, value = values) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "y") %>%
  mutate(var_pct = (y2025 / y2000 - 1) * 100) %>%
  arrange(desc(var_pct))

write_csv(dat, "/Users/lorenzoruffino/Documents/Progetti/data-viz/Variazione PIL reale 2000-2025/input/pil_reale_2000_2025.csv")

print(dat, n = 50)
