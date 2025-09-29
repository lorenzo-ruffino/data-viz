library(jsonlite)
library(purrr)
library(dplyr)
library(tidyr)
library(rlang)


setwd("~/Documents/data-viz/Elezioni Italia/2020")

### Affluenza

estrai_affluenze <- function(url, provincia_id) {
  data <- fromJSON(url, simplifyDataFrame = TRUE)
  
  # anagrafica
  enti <- as_tibble(data$enti) %>%
    mutate(provincia_id = provincia_id)
  
  # affluenze
  affluenze_df <- map_dfr(names(data$affluenze), function(id) {
    affluenze <- data$affluenze[[id]]$votantiAffluenze
    
    if (is.null(affluenze) || length(affluenze) == 0) {
      return(tibble())
    }
    
    map_dfr(names(affluenze), function(orario) {
      riga <- affluenze[[orario]]
      if (is.null(riga)) return(tibble())
      
      tibble(
        idente = as.integer(id),
        orario = as.integer(orario),
        tot_sezio    = riga$tot_sezio %||% NA,
        iscri_maschi = riga$iscri_maschi %||% NA,
        iscri_femm   = riga$iscri_femm %||% NA,
        iscri_totale = riga$iscri_totale %||% NA,
        tvmaschi     = riga$tvmaschi %||% NA,
        tvfemm       = riga$tvfemm %||% NA,
        tvotanti     = riga$tvotanti %||% NA
      )
    })
  })
  
  affluenze_df %>%
    left_join(enti, by = "idente")
}

urls <- paste0("https://dati2020.elezioni.marche.it/static_json/afflu_", 1:5, ".json")

df_tutte_2020 <- map2_dfr(urls, 1:5, estrai_affluenze)


write.csv(df_tutte_2020, file="2025_09_28_Marche_Regionali/2020/marche_affluenza_2020.csv", row.names = F)


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
urls <- sprintf("https://dati2020.elezioni.marche.it/static_json/raggrup_0_%d.json", 1:228)

df_risultati_2020 <- map_dfr(urls, ~possibly(estrai_risultati, otherwise = tibble())(.x))


write.csv(df_risultati_2020, file="2025_09_28_Marche_Regionali/2020/marche_risultati_2020.csv", row.names = F)

