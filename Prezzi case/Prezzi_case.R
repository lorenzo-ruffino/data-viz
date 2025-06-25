library(eurostat)
library(tidyverse)
library(data.table)
library(istat)
library(jsonlite)
library(httr)

setwd("~/Documenti/data-viz/Prezzi case")

check_access_to_data()

# Prezzi case Italia da Eurostat

affitti = get_eurostat("prc_hicp_aind")%>%
  filter(unit == "INX_A_AVG"
         & coicop == "CP041"
         & geo == "IT"
         & TIME_PERIOD >= as.Date("2010-01-01"))%>%
  select(TIME_PERIOD, values)%>%
  rename(rent = values)


acquisti = get_eurostat("prc_hpi_a")%>%
  filter(unit == "I15_A_AVG"
         #& purchase == "TOTAL"
         & geo == "IT"
         & TIME_PERIOD >= as.Date("2010-01-01"))%>%
  select(TIME_PERIOD, purchase, values)%>%
  spread(purchase, values)

reddito = fread("reddito_imponibile.csv")

data = left_join(affitti, acquisti, by=c('TIME_PERIOD'))%>%
  mutate(Anno = lubridate::year(TIME_PERIOD))%>%
  left_join(reddito, by=c("Anno"))%>%
  mutate_at(vars(-TIME_PERIOD), ~ . / first(.) * 100)%>%
  rename(affitto = rent,
         acquisto = TOTAL,
         acquisto_esistenti = DW_EXST,
         acquisto_nuove = DW_NEW,
         reddito_nominale = Nominale,
         reddito_reale = Reale)%>%
  select(-Anno)


write.csv(data, file="eurostat_indici_prezzi_case.csv")


# Prezzi case per macro-are da Istat

search_istatdata("abitazioni", lang = "ita")

dati_istat = get_istatdata(agencyId = "IT1",
                           dataset_id = "143_497",
                           version = "1.0",
                           start = NULL,
                           end = NULL,
                           recent = FALSE,
                           csv = FALSE,
                           xlsx = FALSE)

dati_macro = dati_istat %>%
  filter(REF_AREA %in% c("ITC", "ITD", "ITE", "ITFG")
         & PURCHASES_DWELLINGS == 'ALL'
         & FREQ == 'A'
         & MEASURE == 4) %>%
  mutate(REF_AREA = recode(REF_AREA,
                           "ITC" = "Nord-Ovest",
                           "ITD" = "Nord-Est",
                           "ITE" = "Centro",
                           "ITFG" = "Sud e Isole"))%>%
  select(obsTime, REF_AREA, obsValue)%>%
  group_by(REF_AREA)%>%
  arrange(obsTime)%>%
  mutate(obsValue = 100*(obsValue / first(obsValue)))%>%
  spread(REF_AREA, obsValue)


write.csv(dati_macro, file="istat_indici_prezzi_case_macroarea.csv")


# Prezzi case da Immobiliare.it

regioni <- c("abruzzo", "basilicata", "calabria", "campania", "emilia-romagna", 
             "friuli-venezia-giulia", "lazio", "liguria", "lombardia", "marche", 
             "molise", "piemonte", "puglia", "sardegna", "sicilia", "toscana", 
             "trentino-alto-adige", "umbria", "valle-d-aosta", "veneto")

url_vendita <- "https://www.immobiliare.it/api-next/city-guide/price-chart/1/?__lang=it&path=%2Fmercato-immobiliare%2F"
url_affitto <- "https://www.immobiliare.it/api-next/city-guide/price-chart/2/?__lang=it&path=%2Fmercato-immobiliare%2F"

estrai_dati <- function(regione, tipo_url) {
  url <- paste0(tipo_url, regione, "%2F")
  
  response <- GET(url)
  if (status_code(response) == 200) {
    json_data <- fromJSON(content(response, "text"))
    
    data.frame(
      regione = gsub("-", " ", regione),
      data = json_data$labels,
      prezzo = as.numeric(json_data$values)
    )
  }
}

dati_vendita <- do.call(rbind, lapply(regioni, estrai_dati, url_vendita))
dati_affitto <- do.call(rbind, lapply(regioni, estrai_dati, url_affitto))

dati_vendita$tipo <- "vendita"
dati_affitto$tipo <- "affitto"

dati_completi <- rbind(dati_vendita, dati_affitto)%>%
  mutate(data = as.Date(data),
         anno = year(data))%>%
  group_by(anno, regione, tipo)%>%
  summarise(prezzo = mean(prezzo, na.rm=T))%>%
  spread(tipo, prezzo)%>%
  mutate(rapporto = vendita / affitto)

regioni_vendita = dati_completi %>%
  group_by(regione) %>%
  mutate(
    vendita_indice = vendita / first(vendita) * 100,
    affitto_indice = affitto / first(affitto) * 100,
    rapporto_indice = rapporto / first(rapporto) * 100
  ) %>%
  ungroup()%>%
  select(anno, vendita_indice, regione)%>%
  spread(regione, vendita_indice)

regioni_affitto = dati_completi %>%
  group_by(regione) %>%
  mutate(
    vendita_indice = vendita / first(vendita) * 100,
    affitto_indice = affitto / first(affitto) * 100,
    rapporto_indice = rapporto / first(rapporto) * 100
  ) %>%
  ungroup()%>%
  select(anno, affitto_indice, regione)%>%
  spread(regione, affitto_indice)


write.csv(regioni_vendita, "dati_regioni_vendita.csv", row.names = FALSE)
write.csv(regioni_affitto, "dati_regioni_affitto.csv", row.names = FALSE)