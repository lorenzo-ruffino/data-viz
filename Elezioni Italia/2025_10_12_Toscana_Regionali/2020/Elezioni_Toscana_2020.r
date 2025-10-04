library(rvest)
library(httr)
library(tidyverse)

# Funzione per estrarre link comuni con nome provincia
estrai_link_comuni_con_provincia <- function(url_provincia) {
  
  response <- GET(url_provincia, config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  html_parsed <- content(response, as = "parsed", encoding = "UTF-8")
  
  # Estrai il nome della provincia
  provincia <- html_parsed %>%
    html_element("h2") %>%
    html_text() %>%
    str_replace("Risultati Provincia di ", "") %>%
    str_replace("Risultati Circoscrizione ", "") %>%
    str_trim()
  
  # Estrai i link
  links <- html_parsed %>%
    html_elements("a") %>%
    keep(~ html_text(.x) == "DETTAGLIO") %>%
    html_attr("href")
  
  base_url <- "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/"
  
  tibble(
    provincia = provincia,
    url = paste0(base_url, links)
  )
}

# Lista completa URL province
url_province <- c(
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09051000.html",  # Arezzo
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09053000.html",  # Grosseto
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09049000.html",  # Livorno
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09045000.html",  # Massa-Carrara
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09050000.html",  # Pisa
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09047000.html",  # Pistoia
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09100000.html",  # Prato
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09052000.html",  # Siena
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09048100.html",  # Firenze 1
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09048200.html",  # Firenze 2
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09048300.html",  # Firenze 3
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09048400.html",  # Firenze 4
  "https://elezioni2020.regione.toscana.it/Elezioni2020/www/Risultati/09046000.html"   # Lucca
)

# Estrai tutti i link con provincia
tutti_link_con_provincia <- map_df(url_province, estrai_link_comuni_con_provincia)

# Salva
write_csv(tutti_link_con_provincia, "link_comuni_con_provincia.csv")








library(rvest)
library(httr)
library(tidyverse)

# Funzione per estrarre dati da un singolo comune
estrai_dati_comune <- function(url) {
  
  # Scarica il contenuto
  response <- GET(url, config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  html_parsed <- content(response, as = "parsed", encoding = "UTF-8")
  
  # Estrai il nome del comune
  comune <- html_parsed %>%
    html_element("h2") %>%
    html_text() %>%
    str_replace("Risultati Comune di ", "") %>%
    str_trim()
  
  # Estrai la tabella
  tabella <- html_parsed %>%
    html_table() %>%
    .[[1]]
  
  # Rinomina le colonne
  colnames(tabella) <- c("candidato_presidente", "voti_presidente", "perc_presidente", 
                         "lista", "voti_lista", "perc_lista", "link")
  
  # Filtra solo i candidati e aggiungi il comune
  candidati <- tabella %>%
    filter(candidato_presidente != "" & 
             !candidato_presidente %in% c("VOTI VALIDI", "CONTESTATE NON ASSEGNATE", "BIANCHE", "NULLE")) %>%
    select(candidato = candidato_presidente, 
           voti = voti_presidente,
           percentuale = perc_presidente) %>%
    mutate(
      comune = comune,
      voti_numeric = as.numeric(gsub("\\.", "", as.character(voti))),
      perc_numeric = as.numeric(gsub(",", ".", percentuale))
    ) %>%
    select(comune, candidato, voti, voti_numeric, percentuale, perc_numeric)
  
  return(candidati)
}

# Funzione con gestione errori e delay
estrai_dati_comune_safe <- function(url, provincia) {
  Sys.sleep(1)  # Aspetta 1 secondo tra richieste
  
  tryCatch({
    dati <- estrai_dati_comune(url)
    dati$provincia <- provincia
    return(dati)
  }, error = function(e) {
    message(paste("Errore con URL:", url, "- Errore:", e$message))
    return(NULL)
  })
}

# Applica a tutti i comuni
tutti_dati <- map2_df(
  tutti_link_con_provincia$url, 
  tutti_link_con_provincia$provincia,
  estrai_dati_comune_safe,
  .progress = TRUE
)

# Salva i risultati
write_csv(tutti_dati, "risultati_elezioni_toscana_2020.csv")

print(paste("Comuni unici:", n_distinct(tutti_dati$comune)))



risultati_comuni <- tutti_dati %>%
  select(comune, provincia, candidato, voti_numeric) %>%
  pivot_wider(names_from = candidato, values_from = voti_numeric, values_fill = 0) %>%
  # Calcola totali
  mutate(
    # Somma CSX + M5S
    voti_csx_m5s = `Giani Eugenio` + `Galletti Irene`,
    voti_cdx = `Ceccardi Susanna`,
    
    # Determina vincitore
    vincitore = if_else(voti_csx_m5s > voti_cdx, "CSX/M5S", "CDX"),
    
    # Calcola differenza in valore assoluto
    differenza = abs(voti_csx_m5s - voti_cdx),
    
    # Calcola totale voti nel comune (somma tutti i candidati)
    totale_voti = rowSums(across(c(`Giani Eugenio`, `Ceccardi Susanna`, `Galletti Irene`, 
                                   `Fattori Tommaso`, starts_with("Catello"), 
                                   starts_with("Barzanti"), starts_with("Vigni"))), na.rm = TRUE),
    
    # Calcola percentuali
    perc_csx_m5s = round((voti_csx_m5s / totale_voti) * 100, 2),
    perc_cdx = round((voti_cdx / totale_voti) * 100, 2)
  ) %>%
  select(provincia, comune, 
         voti_csx_m5s, voti_cdx, 
         vincitore, differenza,
         perc_csx_m5s, perc_cdx,
         totale_voti)


write_csv(risultati_comuni, "risultati_comuni_toscana_2020.csv")
