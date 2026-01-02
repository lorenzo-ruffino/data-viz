library(readxl)
library(readr)
library(tidyverse)
library(stringi)

setwd("~/Documents/Progetti/data-viz/Elezioni Italia/2025_11_23_Campania_Regionali")

data = read_excel("~/Documents/Progetti/data-viz/Elezioni Italia/2025_11_23_Veneto_Regionali/Regionali_AltreRegioni_Scrutini.xlsx")

coordinate = read_csv("campania_centroidi.csv")

data_turnout = data %>%
  filter(REGIONE == "CAMPANIA") %>%
  ) %>%
  group_by(COMUNE) %>%
  summarise(
    ELETTORI = first(ELETTORI_TOT),
    VOTANTI = first(VOTANTI_TOT),
    BIANCHE = first(SKBIANCHE),
    .groups = "drop"
  ) %>%
  mutate(
    VOTI_ESPRESSI = VOTANTI - BIANCHE,
    AFFLUENZA = round(VOTANTI / ELETTORI * 100, 2)
  )

data_grouped = data %>%
  filter(REGIONE == "CAMPANIA") %>%
  mutate(R = case_when(
    COGNOME == "CALDORO" ~ "CDX",
    COGNOME %in% c("DE LUCA", "CIARAMBINO") ~ "CSX/M5S",
    TRUE ~ "Altri"
  )) %>%
  select(COMUNE, PROVINCIA, R, VOTICAND)%>%
  distinct() %>%
  group_by(COMUNE, R, PROVINCIA) %>%
  summarise(VOTI = sum(VOTICAND, na.rm = TRUE), .groups = "drop") %>%
  group_by(COMUNE) %>%
  mutate(PCT = round(VOTI / sum(VOTI) * 100, 2)) %>%
  ungroup()

data_pivot = data_grouped %>%
  pivot_wider(names_from = R, values_from = c(VOTI, PCT), values_fill = 0)

coordinate_clean = coordinate %>%
  mutate(
    COMUNE_CLEAN = gsub("[^A-Z0-9]", "", stri_trans_general(toupper(COMUNE), "Latin-ASCII"))
  ) %>%
  select(COMUNE_CLEAN, X, Y, COD_PROV, COD_REG)

risultati = data_pivot %>%
  left_join(data_turnout, by = "COMUNE") %>%
  mutate(
    VINCITORE = case_when(
      VOTI_CDX > `VOTI_CSX/M5S` ~ "CDX",
      `VOTI_CSX/M5S` > VOTI_CDX ~ "CSX/M5S",
      TRUE ~ "Pareggio"
    ),
    DIFF_VOTI = abs(VOTI_CDX - `VOTI_CSX/M5S`),
    COMUNE_CLEAN = gsub("[^A-Z0-9]", "", stri_trans_general(toupper(COMUNE), "Latin-ASCII"))
  ) %>%
  left_join(coordinate_clean, by = "COMUNE_CLEAN")%>%
  select(-COMUNE_CLEAN)

write.csv(risultati, "risultati_campania_regionali.csv", row.names = FALSE)





risultati %>%
  group_by(PROVINCIA) %>%
  summarise(
    ELETTORI = sum(ELETTORI, na.rm = TRUE),
    VOTANTI = sum(VOTANTI, na.rm = TRUE),
    VOTI_ESPRESSI = sum(VOTI_ESPRESSI, na.rm = TRUE),
    VOTI_CDX = sum(VOTI_CDX, na.rm = TRUE),
    `VOTI_CSX/M5S` = sum(`VOTI_CSX/M5S`, na.rm = TRUE),
    VOTI_Altri = sum(VOTI_Altri, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    AFFLUENZA = VOTANTI / ELETTORI,
    PCT_CDX = VOTI_CDX / VOTI_ESPRESSI ,
    `PCT_CSX/M5S` = `VOTI_CSX/M5S` / VOTI_ESPRESSI
  )