library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)

setwd("~/Documenti/data-viz/Imposte e redditi tra 2001 e 2023")

# Union tutti i dati


files <- list.files(path = "Input_Comune", pattern = "^Redditi", full.names = TRUE)

lista_dati <- list()

for (file in files) {
  df <- fread(file)
  
  lista_dati[[length(lista_dati) + 1]] <- df
}




data = bind_rows(lista_dati) %>%
  clean_names() %>%
  group_by(anno_di_imposta) %>%
  summarise(reddito_imponibile_frequenza = sum(reddito_imponibile_frequenza, na.rm = TRUE),
            reddito_imponibile_ammontare_in_euro = sum(reddito_imponibile_ammontare_in_euro, na.rm = TRUE),
            reddito_imponibile_ammontare = sum(reddito_imponibile_ammontare, na.rm = TRUE),
            imposta_netta_frequenza = sum(imposta_netta_frequenza, na.rm = TRUE),
            imposta_netta_ammontare_in_euro = sum(imposta_netta_ammontare_in_euro, na.rm = TRUE),
            imposta_netta_ammontare = sum(imposta_netta_ammontare, na.rm = TRUE)) %>%
  mutate(
    # Usa coalesce che prende il primo valore non NA
    reddito_freq = reddito_imponibile_frequenza,
    reddito_valore = dplyr::coalesce(
      # Se la somma di in_euro è 0 ma ci sono valori in ammontare, usa ammontare
      if_else(reddito_imponibile_ammontare_in_euro == 0 & reddito_imponibile_ammontare > 0, 
              reddito_imponibile_ammontare, reddito_imponibile_ammontare_in_euro),
      # Se in_euro è NA o 0, usa ammontare
      reddito_imponibile_ammontare
    ),
    imposta_freq = imposta_netta_frequenza,
    imposta_valore = dplyr::coalesce(
      # Se la somma di in_euro è 0 ma ci sono valori in ammontare, usa ammontare
      if_else(imposta_netta_ammontare_in_euro == 0 & imposta_netta_ammontare > 0, 
              imposta_netta_ammontare, imposta_netta_ammontare_in_euro),
      # Se in_euro è NA o 0, usa ammontare
      imposta_netta_ammontare
    ),
    reddito_medio = reddito_valore / reddito_freq,
    imposta_media = imposta_valore / imposta_freq,
    imposta_su_reddito = 100 * (imposta_media / reddito_medio),
    imposta_totale = imposta_valore / 1e09) %>%
  select(anno_di_imposta, reddito_medio, imposta_media, imposta_su_reddito, imposta_totale)


## Classi reddito



files <- list.files(path = "Input/Classi_Reddito",full.names = TRUE)

lista_dati <- list()

for (file in files) {
  df <- fread(file,  skip = 9, colClasses = "character")
  df$nome_file <- basename(file)
  
  lista_dati[[length(lista_dati) + 1]] <- df
}



classi = bind_rows(lista_dati) %>%
  clean_names()%>%
  select(nome_file, classi_di_reddito_complessivo_in_euro, imposta_lorda_frequenza, imposta_lorda_ammontare)%>%
  mutate(across(3:ncol(.), ~ as.numeric(gsub("\\.", "", .))))%>%
  group_by(nome_file, classi_di_reddito_complessivo_in_euro)%>%
  summarise(imposta_lorda_frequenza = sum(imposta_lorda_frequenza, na.rm = T),
            imposta_lorda_ammontare = sum(imposta_lorda_ammontare, na.rm = T))%>%
  filter(classi_di_reddito_complessivo_in_euro != 'TOTALE')%>%
  mutate(anno = as.numeric(str_extract(nome_file, "\\d{4}")))%>%
  mutate(
    limite_inferiore = case_when(
      classi_di_reddito_complessivo_in_euro == "zero" ~ 0,
      classi_di_reddito_complessivo_in_euro == "minore di -1.000" ~ -Inf,
      str_detect(classi_di_reddito_complessivo_in_euro, "^oltre") ~ 
        as.numeric(gsub("\\.", "", str_extract(classi_di_reddito_complessivo_in_euro, "\\d+\\.?\\d*"))),
      str_detect(classi_di_reddito_complessivo_in_euro, "^da -") ~ 
        -as.numeric(gsub("\\.", "", str_extract(classi_di_reddito_complessivo_in_euro, "\\d+\\.?\\d*"))),
      str_detect(classi_di_reddito_complessivo_in_euro, "^da") ~ 
        as.numeric(gsub("\\.", "", str_extract(classi_di_reddito_complessivo_in_euro, "\\d+\\.?\\d*"))),
      TRUE ~ NA_real_
    ),
    
    limite_superiore = case_when(
      classi_di_reddito_complessivo_in_euro == "zero" ~ 0,
      classi_di_reddito_complessivo_in_euro == "minore di -1.000" ~ -1000,
      str_detect(classi_di_reddito_complessivo_in_euro, "^oltre") ~ Inf,
      TRUE ~ as.numeric(gsub("\\.", "", str_extract(classi_di_reddito_complessivo_in_euro, "a (-?\\d+\\.?\\d*)", group = 1)))
    )
  )


# Funzione per estrarre limiti
estrai_limiti <- function(classe) {
  # Casi speciali
  if (classe == "zero") return(c(0, 0))
  if (classe == "minore di -1.000") return(c(-Inf, -1000))
  if (grepl("^oltre", classe)) {
    valore <- as.numeric(gsub("\\.", "", gsub("oltre ", "", classe)))
    return(c(valore, Inf))
  }
  
  # Rimuovi "da " e dividi su " a "
  senza_da <- gsub("^da ", "", classe)
  parti <- strsplit(senza_da, " a ")[[1]]
  
  # Estrai e converti i valori
  inf <- parti[1]
  sup <- parti[2]
  
  # Gestisci numeri negativi e rimuovi punti
  if (grepl("^-", inf)) {
    inf_val <- -as.numeric(gsub("\\.", "", gsub("^-", "", inf)))
  } else {
    inf_val <- as.numeric(gsub("\\.", "", inf))
  }
  
  if (grepl("^-", sup)) {
    sup_val <- -as.numeric(gsub("\\.", "", gsub("^-", "", sup)))
  } else {
    sup_val <- as.numeric(gsub("\\.", "", sup))
  }
  
  return(c(inf_val, sup_val))
}

# Applica la funzione e crea le nuove colonne
limiti <- t(sapply(classi$classi_di_reddito_complessivo_in_euro, estrai_limiti))
classi$limite_inferiore <- limiti[,1]
classi$limite_superiore <- limiti[,2]


all = classi %>%
  ungroup()%>%
  select(classi_di_reddito_complessivo_in_euro, limite_inferiore, limite_superiore)%>%
  distinct()


prova = fread("/home/lorenzoruffino/Documenti/data-viz/Imposte e redditi tra 2001 e 2023/Input/Classi_Reddito/PFDIP2010tab_02_02_.csv",  skip = 9, colClasses = "character")%>%
  clean_names()