library(dplyr)
library(tidyr)
library(readr)

# Fonte: Our World in Data / Penn World Table 10.x
# https://ourworldindata.org/grapher/total-factor-productivity.csv?country=ESP~GBR~USA~FRA~ITA~DEU
# Variabile: rtfpna = Total Factor Productivity a prezzi nazionali costanti (indice, 2017 = 1).
# NB: i livelli di rtfpna NON sono confrontabili tra paesi; ribasando ciascun paese al
# proprio valore 1960 = 100 si ottiene la traiettoria di crescita della PTF di ogni paese.

geos <- c(
  ESP = "Spagna",
  GBR = "Regno Unito",
  USA = "Stati Uniti",
  FRA = "Francia",
  ITA = "Italia",
  DEU = "Germania"
)

dat <- read_csv(
  "/Users/lorenzoruffino/Documents/Progetti/data-viz/Produttività totale dei fattori/input/tfp_owid_rtfpna.csv",
  show_col_types = FALSE
)

wide <- dat %>%
  filter(year >= 1960) %>%
  group_by(code) %>%
  arrange(year) %>%
  mutate(indice = rtfpna / rtfpna[year == 1960] * 100) %>%  # ribasa a 1960 = 100
  ungroup() %>%
  mutate(paese = geos[code]) %>%
  select(anno = year, paese, indice) %>%
  mutate(indice = round(indice, 2)) %>%
  pivot_wider(names_from = paese, values_from = indice) %>%
  arrange(anno) %>%
  select(anno, Spagna, `Regno Unito`, `Stati Uniti`, Francia, Italia, Germania)  # ordine URL

out_path <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Produttività totale dei fattori/output/tfp_indice_base1960_wide.csv"
write_csv(wide, out_path)

cat("Salvato:", out_path, "\n")
cat("Anni:", min(wide$anno), "-", max(wide$anno), "(", nrow(wide), "righe )\n")
cat("Celle mancanti (NA):", sum(is.na(wide)), "\n\n")
print(wide, n = Inf)
