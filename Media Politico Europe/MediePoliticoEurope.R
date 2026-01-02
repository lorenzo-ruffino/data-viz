library(jsonlite)
library(tidyverse)

# Funzione per scaricare e pulire i dati
get_polls <- function(country_code, start_date = "2021-01-01", min_threshold = 5) {
  
  url <- paste0("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/", 
                country_code, "-parliament")
  
  # Scarica e processa i dati
  polls <- fromJSON(url)$trends$kalmanSmooth %>%
    mutate(date = as.Date(date)) %>%
    bind_cols(.$parties) %>%
    select(-parties) %>%
    filter(date >= start_date) %>%
    # Rimuovi partiti con _anno
    select(-matches("_\\d{4}$"))
  
  # Trova l'ultima riga per filtrare partiti sotto soglia
  ultima_riga <- tail(polls, 1)
  
  # Identifica colonne da mantenere
  cols_to_keep <- c("date")
  for(col in names(polls)) {
    if(col != "date" && is.numeric(polls[[col]])) {
      ultimo_val <- ultima_riga[[col]]
      if(!is.na(ultimo_val) && ultimo_val > min_threshold) {
        cols_to_keep <- c(cols_to_keep, col)
      }
    }
  }
  
  polls %>% select(all_of(cols_to_keep))
}

# Scarica tutti i paesi
polls_es <- get_polls("ES")
polls_de <- get_polls("DE")
polls_uk <- get_polls("GB")
polls_fr <- get_polls("FR")
polls_it <- get_polls("IT")


write_csv(polls_es, "polls_es.csv")
write_csv(polls_de, "polls_de.csv")
write_csv(polls_uk, "polls_uk.csv")
write_csv(polls_fr, "polls_fr.csv")
write_csv(polls_it, "polls_it.csv")

# Verifica
cat("Spagna:", paste(setdiff(names(polls_es), "date"), collapse=", "), "\n")
cat("Germania:", paste(setdiff(names(polls_de), "date"), collapse=", "), "\n")
cat("UK:", paste(setdiff(names(polls_uk), "date"), collapse=", "), "\n")
cat("Francia:", paste(setdiff(names(polls_fr), "date"), collapse=", "), "\n")
cat("Italia:", paste(setdiff(names(polls_it), "date"), collapse=", "), "\n")