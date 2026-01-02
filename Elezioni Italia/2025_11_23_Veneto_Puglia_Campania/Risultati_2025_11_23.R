library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

setwd("~/Documents/Progetti/data-viz/Elezioni Italia/2025_11_23_Veneto_Puglia_Campania")

pulisci_testo = function(x) {
  x %>%
    toupper() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_replace_all("[^A-Z0-9 ]", "") %>%
    str_squish()
}

to_proper = function(x) {
  str_to_title(tolower(x))
}

risultati_2020 = read_excel("2020/Regionali_AltreRegioni_Scrutini.xlsx") %>%
  mutate(
    COMUNE = case_when(
      COMUNE == "ALANO DI PIAVE" ~ "SETTEVILLE",
      COMUNE == "QUERO VAS" ~ "SETTEVILLE",
      COMUNE == "CARCERI" ~ "SANTA CATERINA D'ESTE",
      COMUNE == "VIGHIZZOLO D'ESTE" ~ "SANTA CATERINA D'ESTE",
      COMUNE == "GAMBUGLIANO" ~ "SOVIZZO",
      TRUE ~ COMUNE
    ),
    coalizione = case_when(
      COGNOME %in% c("ZAIA", "CALDORO", "FITTO") ~ "CDX",
      COGNOME %in% c("LORENZONI", "CAPPELLETTI", "DE LUCA", "CIARAMBINO", "EMILIANO", "LARICCHIA") ~ "CSX/M5S",
      TRUE ~ "Altri"
    )
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, coalizione, VOTICAND) %>%
  distinct() %>%
  group_by(COMUNE, coalizione, REGIONE, PROVINCIA) %>%
  summarise(VOTI = sum(VOTICAND, na.rm = TRUE), .groups = "drop") %>%
  group_by(COMUNE, REGIONE, PROVINCIA) %>%
  mutate(
    perc_2020 = round(VOTI / sum(VOTI) * 100, 2),
    voti_2020 = VOTI
  ) %>%
  ungroup() %>%
  mutate(comune = pulisci_testo(COMUNE)) %>%
  select(comune, REGIONE, PROVINCIA, coalizione, perc_2020, voti_2020)

risultati_2025 = fread("Veneto_Campania_Puglia_Risultati_Presidente.csv") %>%
  select(Regione, Provincia, Comune, Cognome, Voti) %>%
  distinct() %>%
  mutate(
    coalizione = case_when(
      Cognome %in% c("LOBUONO", "STEFANI", "CIRIELLI") ~ "CDX",
      Cognome %in% c("DECARO", "MANILDO", "FICO") ~ "CSX/M5S",
      TRUE ~ "Altri"
    )
  ) %>%
  group_by(Regione, Provincia, Comune, coalizione) %>%
  summarise(VOTI = sum(Voti, na.rm = TRUE), .groups = "drop") %>%
  group_by(Regione, Provincia, Comune) %>%
  mutate(
    perc_2025 = round(VOTI / sum(VOTI) * 100, 2),
    voti_2025 = VOTI
  ) %>%
  ungroup() %>%
  mutate(comune = pulisci_testo(Comune)) %>%
  select(comune, Regione, Provincia, coalizione, perc_2025, voti_2025)

puglia = read_csv("Mappe/puglia_centroidi.csv", show_col_types = FALSE) %>%
  mutate(
    comune = pulisci_testo(COMUNE),
    regione = "PUGLIA"
  ) %>%
  select(comune, regione, x = X, y = Y)

campania = read_csv("Mappe/campania_centroidi.csv", show_col_types = FALSE) %>%
  mutate(
    comune = pulisci_testo(COMUNE),
    regione = "CAMPANIA"
  ) %>%
  select(comune, regione, x = X, y = Y)

veneto = read_csv("Mappe/veneto_centroidi.csv", show_col_types = FALSE) %>%
  mutate(
    comune = pulisci_testo(COMUNE),
    regione = "VENETO"
  ) %>%
  select(comune, regione, x = X, y = Y)

centroidi = bind_rows(puglia, campania, veneto)

dati_2020_wide = risultati_2020 %>%
  filter(coalizione %in% c("CDX", "CSX/M5S")) %>%
  select(comune, REGIONE, coalizione, perc_2020, voti_2020) %>%
  pivot_wider(
    names_from = coalizione,
    values_from = c(perc_2020, voti_2020),
    values_fill = 0
  )

dati_2025_wide = risultati_2025 %>%
  filter(coalizione %in% c("CDX", "CSX/M5S")) %>%
  select(comune, Regione, Provincia, coalizione, perc_2025, voti_2025) %>%
  pivot_wider(
    names_from = coalizione,
    values_from = c(perc_2025, voti_2025),
    values_fill = 0
  )

dati_completi = dati_2025_wide %>%
  left_join(dati_2020_wide, by = "comune") %>%
  left_join(centroidi, by = c("comune", "Regione" = "regione")) %>%
  mutate(
    margine_voti = abs(`voti_2025_CDX` - `voti_2025_CSX/M5S`),
    vincitore = case_when(
      `voti_2025_CDX` > `voti_2025_CSX/M5S` ~ "CDX",
      `voti_2025_CDX` < `voti_2025_CSX/M5S` ~ "CSX/M5S",
      TRUE ~ "Pareggio"
    ),
    comune_proper = to_proper(comune),
    var_perc = case_when(
      Regione == "VENETO" ~ round(perc_2025_CDX - perc_2020_CDX, 2),
      TRUE ~ round(`perc_2025_CSX/M5S` - `perc_2020_CSX/M5S`, 2)
    ),
    var_perc_neg = -var_perc
  ) %>%
  select(
    comune = comune_proper,
    provincia = Provincia,
    regione = Regione,
    `CSX/M5S_perc_2025` = `perc_2025_CSX/M5S`,
    `CSX/M5S_voti_2025` = `voti_2025_CSX/M5S`,
    `CSX/M5S_voti_2020` = `voti_2020_CSX/M5S`,
    `CSX/M5S_perc_2020` = `perc_2020_CSX/M5S`,
    CDX_perc_2025 = perc_2025_CDX,
    CDX_voti_2025 = voti_2025_CDX,
    CDX_voti_2020 = voti_2020_CDX,
    CDX_perc_2020 = perc_2020_CDX,
    margine_voti,
    vincitore,
    x,
    y,
    var_perc,
    var_perc_neg
  )

dati_completi %>%
  filter(regione == "PUGLIA") %>%
  write_csv("Mappe/puglia_risultati_2025.csv")

dati_completi %>%
  filter(regione == "CAMPANIA") %>%
  write_csv("Mappe/campania_risultati_2025.csv")

dati_completi %>%
  filter(regione == "VENETO") %>%
  write_csv("Mappe/veneto_risultati_2025.csv")

provincia_2025 = risultati_2025 %>%
  group_by(Regione, Provincia, coalizione) %>%
  summarise(
    voti_2025 = sum(voti_2025, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Regione, Provincia) %>%
  mutate(
    perc_2025 = round(voti_2025 / sum(voti_2025) * 100, 2)
  ) %>%
  ungroup()

provincia_2020 = risultati_2020 %>%
  group_by(REGIONE, PROVINCIA, coalizione) %>%
  summarise(
    voti_2020 = sum(voti_2020, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(REGIONE, PROVINCIA) %>%
  mutate(
    perc_2020 = round(voti_2020 / sum(voti_2020) * 100, 2)
  ) %>%
  ungroup()

provincia_completo = provincia_2025 %>%
  left_join(
    provincia_2020,
    by = c("Regione" = "REGIONE", "Provincia" = "PROVINCIA", "coalizione")
  ) %>%
  mutate(
    var_perc = round(perc_2025 - perc_2020, 2),
    var_voti = voti_2025 - voti_2020
  ) %>%
  arrange(Regione, Provincia, coalizione)

print(provincia_completo)