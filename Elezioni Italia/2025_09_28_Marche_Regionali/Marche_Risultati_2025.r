library(jsonlite)
library(purrr)
library(dplyr)
library(tidyr)
library(rlang)

setwd("~/Documents/data-viz/Elezioni Italia/2025_09_28_Marche_Regionali")

### Risultati

estrai_risultati <- function(url) {
  data <- fromJSON(url, simplifyDataFrame = TRUE)
  
  # id ente
  ente_id <- as.integer(str_match(url, "_(\\d+)\\.json$")[,2])
  
  # anagrafica
  anagr <- as_tibble(data$anagrRaggrup) %>%
    select(nraggrupmacro, nome, cognome_cand, nome_breve)
  
  # voti per candidato (chiave = nraggrupmacro)
  arr <- data$votiRaggrup$arrVotiRaggrup %||% list()
  if (length(arr) == 0) return(tibble())
  
  voti <- enframe(arr, name = "nraggrupmacro", value = "obj") %>%
    mutate(
      nraggrupmacro = as.integer(nraggrupmacro),
      voti              = map_int(obj, ~ .x$voti %||% NA_integer_),
      voti_nona         = map_int(obj, ~ .x$voti_nona %||% NA_integer_),
      voti_solosindpres = map_int(obj, ~ .x$voti_solosindpres %||% NA_integer_)
    ) %>%
    select(-obj)
  
  # uniscoi
  res <- voti %>%
    left_join(anagr, by = "nraggrupmacro") %>%
    mutate(
      ente_id     = ente_id,
      tot_sezioni = as.integer(data$votiRaggrup$tot_sezio),
      iscritti    = as.integer(data$votiRaggrup$iscri_totali),
      votanti     = as.integer(data$votiRaggrup$tvotanti),
      valide      = as.integer(data$votiRaggrup$tvotival),
      bianche     = as.integer(data$votiRaggrup$tbianche),
      nulle       = as.integer(data$votiRaggrup$tnulle),
      timestamp   = data$timestamp
    ) %>%
    relocate(ente_id)
  
  res
}

# ciclo su tutti i file
urls <- sprintf("https://dati.elezioni.marche.it/static_json/raggrup_0_%d.json", 1:228)

df_risultati <- map_dfr(urls, ~possibly(estrai_risultati, otherwise = tibble())(.x))

enti = read_csv("marche_affluenza_2025.csv")%>%
  select(idente, nome)%>%
  rename(ente_id = idente,
         comune = nome)%>%
  distinct()

df_risultati_enti = left_join(df_risultati, enti, by ="ente_id")


write.csv(df_risultati_enti, file="marche_risultati_2025.csv", row.names = F)

df_risultati_wide = df_risultati_enti %>%
  group_by(ente_id, comune, nome_breve, votanti)%>%
  summarise(voti = sum(voti, na.rm = T))%>%
  spread(nome_breve, voti )%>%
  mutate(Acquaroli = `Francesco Acquaroli` / votanti,
         Ricci = `Matteo Ricci` / votanti,
         colore = ifelse(Acquaroli >= Ricci, 'Acquaroli', 'Ricci'),
         differenza = abs(`Francesco Acquaroli` - `Matteo Ricci`))%>%
  select(ente_id, comune, votanti, Acquaroli, `Francesco Acquaroli`, Ricci, `Matteo Ricci`, colore, differenza )


write.csv(df_risultati_wide, file="marche_risultati_vantaggio_2025.csv", row.names = F)
  

