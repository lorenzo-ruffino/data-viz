library(istat)
library(tidyverse)

search_istatdata("migrazioni interne", lang = "ita")

dati = get_istatdata(agencyId = "IT1",
                     dataset_id = "28_185_DF_DCIS_MIGRAZIONI_1",
                     version = "1.0",
                     start = NULL,
                     end = NULL,
                     recent = FALSE,
                     csv = FALSE,
                     xlsx = FALSE)



data = dati %>%
  filter(CITIZENSHIP == "TOTAL"
         & SEX != 9
         & AGE == "TOTAL")%>%
  filter(
    !REF_AREA %in% c("IT", "ITC", "ITD", "ITE", "ITF", "ITG", "ITD1", "ITD2", "ITCD", "ITFG")
  )%>%
  filter(
    !TERRITORY_NEXT_RESID %in% c("IT", "ITC", "ITD", "ITE", "ITF", "ITG", "ITD1", "ITD2", "ITCD", "ITFG")
  )%>%
  mutate(
    partenza = case_when(
      # Country level
      REF_AREA == "IT" ~ "Italia",
      
      # Macroregions
      REF_AREA == "ITC" ~ "Nord-Ovest",
      REF_AREA == "ITD" ~ "Nord-Est",
      REF_AREA == "ITE" ~ "Centro",
      REF_AREA == "ITF" ~ "Sud",
      REF_AREA == "ITG" ~ "Isole",
      
      # Northwest regions
      REF_AREA == "ITC1" ~ "Piemonte",
      REF_AREA == "ITC2" ~ "Valle d'Aosta",
      REF_AREA == "ITC3" ~ "Liguria",
      REF_AREA == "ITC4" ~ "Lombardia",
      REF_AREA == "ITCD" ~ "Provincia autonoma di Trento e Bolzano",
      
      # Northeast regions
      REF_AREA == "ITD1" ~ "Bolzano/Bozen",
      REF_AREA == "ITD2" ~ "Trento",  
      REF_AREA == "ITD3" ~ "Veneto",
      REF_AREA == "ITD4" ~ "Friuli-Venezia Giulia",
      REF_AREA == "ITD5" ~ "Emilia-Romagna",
      REF_AREA == "ITDA" ~ "Trentino-Alto Adige",
      
      # Central regions
      REF_AREA == "ITE1" ~ "Toscana",
      REF_AREA == "ITE2" ~ "Umbria",
      REF_AREA == "ITE3" ~ "Marche",
      REF_AREA == "ITE4" ~ "Lazio",
      
      # Southern regions
      REF_AREA == "ITF1" ~ "Abruzzo",
      REF_AREA == "ITF2" ~ "Molise",
      REF_AREA == "ITF3" ~ "Campania",
      REF_AREA == "ITF4" ~ "Puglia",
      REF_AREA == "ITF5" ~ "Basilicata",
      REF_AREA == "ITF6" ~ "Calabria",
      REF_AREA == "ITFG" ~ "Abruzzo e Molise",
      
      # Island regions
      REF_AREA == "ITG1" ~ "Sicilia",
      REF_AREA == "ITG2" ~ "Sardegna",
      
      TRUE ~ "Unknown"  # Default case for any unmatched codes
    ),
    
    partenza_macro = case_when(
      REF_AREA == "IT" ~ "Italia",
      REF_AREA %in% c("ITC", "ITC1", "ITC2", "ITC3", "ITC4", "ITCD") ~ "Nord-Ovest",
      REF_AREA %in% c("ITD", "ITD1", "ITD2", "ITD3", "ITD4", "ITD5", "ITDA") ~ "Nord-Est",
      REF_AREA %in% c("ITE", "ITE1", "ITE2", "ITE3", "ITE4") ~ "Centro",
      REF_AREA %in% c("ITF", "ITF1", "ITF2", "ITF3", "ITF4", "ITF5", "ITF6", "ITFG") ~ "Mezzogiorno",
      REF_AREA %in% c("ITG", "ITG1", "ITG2") ~ "Mezzogiorno",
      TRUE ~ "Unknown"
    )
  )%>%
  mutate(
    arrivo = case_when(
      # Country level
      TERRITORY_NEXT_RESID == "IT" ~ "Italia",
      
      # Macroregions
      TERRITORY_NEXT_RESID == "ITC" ~ "Nord-Ovest",
      TERRITORY_NEXT_RESID == "ITD" ~ "Nord-Est",
      TERRITORY_NEXT_RESID == "ITE" ~ "Centro",
      TERRITORY_NEXT_RESID == "ITF" ~ "Sud",
      TERRITORY_NEXT_RESID == "ITG" ~ "Isole",
      
      # Northwest regions
      TERRITORY_NEXT_RESID == "ITC1" ~ "Piemonte",
      TERRITORY_NEXT_RESID == "ITC2" ~ "Valle d'Aosta",
      TERRITORY_NEXT_RESID == "ITC3" ~ "Liguria",
      TERRITORY_NEXT_RESID == "ITC4" ~ "Lombardia",
      TERRITORY_NEXT_RESID == "ITCD" ~ "Provincia autonoma di Trento e Bolzano",
      
      # Northeast regions
      TERRITORY_NEXT_RESID == "ITD1" ~ "Bolzano/Bozen",
      TERRITORY_NEXT_RESID == "ITD2" ~ "Trento",  
      TERRITORY_NEXT_RESID == "ITD3" ~ "Veneto",
      TERRITORY_NEXT_RESID == "ITD4" ~ "Friuli-Venezia Giulia",
      TERRITORY_NEXT_RESID == "ITD5" ~ "Emilia-Romagna",
      TERRITORY_NEXT_RESID == "ITDA" ~ "Trentino-Alto Adige",
      
      # Central regions
      TERRITORY_NEXT_RESID == "ITE1" ~ "Toscana",
      TERRITORY_NEXT_RESID == "ITE2" ~ "Umbria",
      TERRITORY_NEXT_RESID == "ITE3" ~ "Marche",
      TERRITORY_NEXT_RESID == "ITE4" ~ "Lazio",
      
      # Southern regions
      TERRITORY_NEXT_RESID == "ITF1" ~ "Abruzzo",
      TERRITORY_NEXT_RESID == "ITF2" ~ "Molise",
      TERRITORY_NEXT_RESID == "ITF3" ~ "Campania",
      TERRITORY_NEXT_RESID == "ITF4" ~ "Puglia",
      TERRITORY_NEXT_RESID == "ITF5" ~ "Basilicata",
      TERRITORY_NEXT_RESID == "ITF6" ~ "Calabria",
      TERRITORY_NEXT_RESID == "ITFG" ~ "Abruzzo e Molise",
      
      # Island regions
      TERRITORY_NEXT_RESID == "ITG1" ~ "Sicilia",
      TERRITORY_NEXT_RESID == "ITG2" ~ "Sardegna",
      
      TRUE ~ "Unknown"  
    ),
    
    arrivo_macro = case_when(
      TERRITORY_NEXT_RESID == "IT" ~ "Italia",
      TERRITORY_NEXT_RESID %in% c("ITC", "ITC1", "ITC2", "ITC3", "ITC4", "ITCD") ~ "Nord-Ovest",
      TERRITORY_NEXT_RESID %in% c("ITD", "ITD1", "ITD2", "ITD3", "ITD4", "ITD5", "ITDA") ~ "Nord-Est",
      TERRITORY_NEXT_RESID %in% c("ITE", "ITE1", "ITE2", "ITE3", "ITE4") ~ "Centro",
      TERRITORY_NEXT_RESID %in% c("ITF", "ITF1", "ITF2", "ITF3", "ITF4", "ITF5", "ITF6", "ITFG") ~ "Mezzogiorno",
      TERRITORY_NEXT_RESID %in% c("ITG", "ITG1", "ITG2") ~ "Mezzogiorno",
      TRUE ~ "Unknown"
    ))%>%
  group_by(partenza, partenza_macro, arrivo, arrivo_macro, SEX, obsTime)%>%
  summarise(value = sum(obsValue, na.rm = T))%>%
  filter(partenza != arrivo)


macro_regioni = data %>%
  group_by(partenza_macro, arrivo_macro)%>%
  summarise(value = sum(value, na.rm = T))%>%
  filter(partenza_macro != arrivo_macro)

write.csv(all, file="macro_regioni_mov.csv", row.names  = T)


regioni = data %>%
  group_by(partenza, arrivo)%>%
  summarise(value = sum(value, na.rm = T))%>%
  filter(partenza != arrivo)

write.csv(regioni, file="regioni_mov.csv", row.names  = T)


data %>%
  group_by(partenza, arrivo)%>%
  summarise(value = sum(value, na.rm = T))%>%
  filter(partenza != arrivo)%>%
  group_by(partenza)%>%
  arrange(desc(value))%>%
  slice(1)%>%
  arrange(arrivo)

regioni = data %>%
  group_by(partenza, arrivo)%>%
  summarise(value = sum(value, na.rm = T))%>%
  filter(partenza != arrivo) %>%
  spread(partenza, value)

write.csv(regioni, file="regioni_wide.csv", row.names  = T)


partenze = data %>%
  filter(partenza != arrivo)%>%
  group_by(partenza)%>%
  summarise(partenze = -sum(value, na.rm = T))

arrivi = data %>%
  filter(partenza != arrivo)%>%
  group_by(arrivo)%>%
  summarise(arrivi = sum(value, na.rm = T))

totale = inner_join(partenze, arrivi, by=c("partenza"="arrivo"))%>%
  mutate(diff = partenze + arrivi)%>%
  arrange(desc(diff))


write.csv(totale, file="regioni_diff.csv", row.names  = T)




tempo = data %>%
  filter(partenza != arrivo)%>%
  group_by(obsTime, SEX)%>%
  summarise(value = sum(value, na.rm = T))%>%
  spread(SEX, value)%>%
  mutate(tot = `1`+ `2`)


write.csv(tempo, file="regioni_tempo.csv", row.names  = T)
