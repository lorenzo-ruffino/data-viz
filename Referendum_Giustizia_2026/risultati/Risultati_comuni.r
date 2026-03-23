# Estrazione risultati Referendum Giustizia 2026 per comune
# Fonte: https://eleapi.interno.gov.it
# Output:
#   risultati/risultati_comuni.csv              → dati grezzi per comune (tutte le schede)
#   risultati/risultati_province.csv            → aggregato per provincia
#   risultati/risultati_regioni.csv             → aggregato per regione
#   risultati/risultati_nazionale.csv           → aggregato nazionale
#   risultati/provvisori/risultati_comuni_grafico.csv → comuni + coord geografiche (per grafici)

library(httr)
library(jsonlite)
library(dplyr)
library(data.table)
library(parallel)
library(stringr)
library(sf)
sf_use_s2(FALSE)

# ==============================================================================
# CONFIGURAZIONE
# ==============================================================================

DATE          <- "20260322"
TIPO_ELEZIONE <- "09"
N_WORKERS     <- 4L
MAX_RETRY     <- 3L

PROJ_DIR   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum_Giustizia_2026"
OUTPUT_DIR <- file.path(PROJ_DIR, "risultati")

CODICI_CSV <- file.path(PROJ_DIR, "utilities", "codici_comuni_15-03-2026.csv")
GEOJSON    <- file.path(PROJ_DIR, "utilities", "Com01012025_g_WGS84(1).json")

SCHEDA <- "01"

headers <- c(
  'accept'            = 'application/json, text/plain, */*',
  'accept-language'   = 'en-US,en;q=0.6',
  'dnt'               = '1',
  'origin'            = 'https://elezioni.interno.gov.it',
  'priority'          = 'u=1, i',
  'referer'           = 'https://elezioni.interno.gov.it/',
  'sec-ch-ua-mobile'  = '?0',
  'sec-ch-ua-platform'= '"macOS"',
  'sec-fetch-dest'    = 'empty',
  'sec-fetch-mode'    = 'cors',
  'sec-fetch-site'    = 'same-site',
  'sec-gpc'           = '1'
)

# ==============================================================================
# 1. LISTA COMUNI CON CODICI
# ==============================================================================

cat("Scarico lista enti...\n")
url_enti  <- paste0('https://eleapi.interno.gov.it/siel/PX/getentiFI/DE/', DATE, '/TE/', TIPO_ELEZIONE)
data_enti <- fromJSON(rawToChar(GET(url_enti, add_headers(.headers = headers))$content))$enti

regione_mapping  <- setNames(
  data_enti$desc[data_enti$tipo == "RE"],
  substr(data_enti$cod[data_enti$tipo == "RE"], 1, 2)
)
provincia_mapping <- setNames(
  data_enti$desc[data_enti$tipo == "PR"],
  substr(data_enti$cod[data_enti$tipo == "PR"], 3, 5)
)

df_comuni <- data_enti[data_enti$tipo == "CM", ] %>%
  mutate(
    cod_regione    = substr(cod, 1, 2),
    cod_provincia  = substr(cod, 3, 5),
    cod_comune     = substr(cod, 6, 9),
    desc_regione   = regione_mapping[cod_regione],
    desc_provincia = provincia_mapping[cod_provincia]
  )

cat("Comuni trovati:", nrow(df_comuni), "\n")

# ==============================================================================
# 2. FUNZIONE FETCH
# ==============================================================================

fetch_comune_scheda <- function(row_list) {
  library(httr)
  library(jsonlite)

  headers <- c(
    'accept'            = 'application/json, text/plain, */*',
    'accept-language'   = 'en-US,en;q=0.6',
    'dnt'               = '1',
    'origin'            = 'https://elezioni.interno.gov.it',
    'priority'          = 'u=1, i',
    'referer'           = 'https://elezioni.interno.gov.it/',
    'sec-ch-ua-mobile'  = '?0',
    'sec-ch-ua-platform'= '"macOS"',
    'sec-fetch-dest'    = 'empty',
    'sec-fetch-mode'    = 'cors',
    'sec-fetch-site'    = 'same-site',
    'sec-gpc'           = '1'
  )

  url <- paste0(
    'https://eleapi.interno.gov.it/siel/PX/scrutiniFI/DE/',
    row_list$DATE, '/TE/', row_list$TIPO_ELEZIONE,
    '/SK/', row_list$SCHEDA,
    '/RE/', row_list$cod_regione,
    '/PR/', row_list$cod_provincia,
    '/CM/', row_list$cod_comune
  )

  base_row <- data.frame(
    cod_regione    = row_list$cod_regione,
    desc_regione   = row_list$desc_regione,
    cod_provincia  = row_list$cod_provincia,
    desc_provincia = row_list$desc_provincia,
    cod_comune     = row_list$cod_comune,
    desc_comune    = row_list$desc,
    tipo_comune    = NA_character_,
    ele_t          = NA_integer_,
    sz_tot         = NA_integer_,
    sz_perv        = NA_integer_,
    vot_t          = NA_integer_,
    sk_bianche     = NA_integer_,
    sk_nulle       = NA_integer_,
    sk_contestate  = NA_integer_,
    voti_si        = NA_integer_,
    voti_no        = NA_integer_,
    dt_agg         = NA_character_,
    stato          = "no_data",
    stringsAsFactors = FALSE
  )

  for (attempt in 1:row_list$MAX_RETRY) {
    Sys.sleep(0.1 * attempt)

    result <- tryCatch({
      r    <- GET(url, add_headers(.headers = headers), timeout(15))
      body <- rawToChar(r$content)

      if (grepl("^<", trimws(body))) {
        if (attempt < row_list$MAX_RETRY) return(NULL)
        base_row$stato <- "rate_limited"
        return(list(base_row))
      }

      data <- fromJSON(body)

      if (!is.null(data$Error)) {
        base_row$stato <- "dati_non_trovati"
        return(list(base_row))
      }

      if (status_code(r) == 200 && !is.null(data$scheda) && nrow(data$scheda) > 0) {
        info <- data$int
        sch  <- data$scheda[1, ]  # una riga per quesito

        list(data.frame(
          cod_regione    = row_list$cod_regione,
          desc_regione   = row_list$desc_regione,
          cod_provincia  = row_list$cod_provincia,
          desc_provincia = row_list$desc_provincia,
          cod_comune     = row_list$cod_comune,
          desc_comune    = row_list$desc,
          tipo_comune    = if (!is.null(info$tipo_tras)) info$tipo_tras else NA_character_,
          ele_t          = info$ele_t,
          sz_tot         = info$sz_tot,
          sz_perv        = sch$sz_perv,
          vot_t          = sch$vot_t,
          sk_bianche     = sch$sk_bianche,
          sk_nulle       = sch$sk_nulle,
          sk_contestate  = sch$sk_contestate,
          voti_si        = sch$voti_si,
          voti_no        = sch$voti_no,
          dt_agg         = as.character(sch$dt_agg),
          stato          = "ok",
          stringsAsFactors = FALSE
        ))
      } else {
        base_row$stato <- "no_data"
        list(base_row)
      }
    }, error = function(e) {
      if (attempt < row_list$MAX_RETRY) return(NULL)
      base_row$stato <- "errore"
      list(base_row)
    })

    if (!is.null(result)) return(result)
  }

  base_row$stato <- "max_retry"
  list(base_row)
}

# ==============================================================================
# 3. FETCH PARALLELO
# ==============================================================================

comuni_list <- lapply(1:nrow(df_comuni), function(i) {
  c(as.list(df_comuni[i, ]), list(
    DATE          = DATE,
    TIPO_ELEZIONE = TIPO_ELEZIONE,
    SCHEDA        = SCHEDA,
    MAX_RETRY     = MAX_RETRY
  ))
})

cat("Avvio fetch parallelo:", length(comuni_list), "comuni con", N_WORKERS, "worker...\n")
t0 <- proc.time()

cl <- makeCluster(N_WORKERS)
risultati_raw <- parLapply(cl, comuni_list, fetch_comune_scheda)
stopCluster(cl)

elapsed <- round((proc.time() - t0)["elapsed"])
cat("Completato in", elapsed, "secondi\n")

risultati_comuni <- do.call(rbind, unlist(risultati_raw, recursive = FALSE))
rownames(risultati_comuni) <- NULL

cat("\nStato fetch:\n")
print(table(risultati_comuni$stato))
cat("Comuni totali:", nrow(risultati_comuni), "\n")

# ==============================================================================
# 4. AGGREGATI (province, regioni, nazionale)
# ==============================================================================

agg_risultati <- function(df, ...) {
  df %>%
    filter(stato == "ok") %>%
    group_by(...) %>%
    summarise(
      ele_t         = sum(ele_t,         na.rm = TRUE),
      sz_tot        = sum(sz_tot,        na.rm = TRUE),
      sz_perv       = sum(sz_perv,       na.rm = TRUE),
      vot_t         = sum(vot_t,         na.rm = TRUE),
      sk_bianche    = sum(sk_bianche,    na.rm = TRUE),
      sk_nulle      = sum(sk_nulle,      na.rm = TRUE),
      sk_contestate = sum(sk_contestate, na.rm = TRUE),
      voti_si       = sum(voti_si,       na.rm = TRUE),
      voti_no       = sum(voti_no,       na.rm = TRUE),
      n_comuni      = n(),
      .groups = "drop"
    ) %>%
    mutate(
      perc_vot = vot_t / ele_t,
      perc_si  = voti_si / (voti_si + voti_no),
      perc_no  = voti_no / (voti_si + voti_no),
      perc_sz  = sz_perv / sz_tot
    )
}

risultati_province  <- agg_risultati(risultati_comuni, cod_regione, desc_regione, cod_provincia, desc_provincia)
risultati_regioni   <- agg_risultati(risultati_comuni, cod_regione, desc_regione)
risultati_nazionale <- agg_risultati(risultati_comuni)

# ==============================================================================
# 5. SALVATAGGIO CSV BASE
# ==============================================================================

fwrite(risultati_comuni,    file.path(OUTPUT_DIR, "risultati_comuni.csv"))
fwrite(risultati_province,  file.path(OUTPUT_DIR, "risultati_province.csv"))
fwrite(risultati_regioni,   file.path(OUTPUT_DIR, "risultati_regioni.csv"))
fwrite(risultati_nazionale, file.path(OUTPUT_DIR, "risultati_nazionale.csv"))
cat("\nCSV base salvati in", OUTPUT_DIR, "\n")

# ==============================================================================
# 6. VERSIONE GRAFICO: join con coordinate geografiche
# ==============================================================================

# Solo comuni con dati ok
risultati_ok <- risultati_comuni %>%
  filter(stato == "ok") %>%
  mutate(
    cod_join   = paste0(sprintf("%03d", as.integer(cod_provincia)),
                        sprintf("%04d", as.integer(cod_comune))),
    diff_si_no = voti_si - voti_no,
    vincitore  = ifelse(voti_si > voti_no, "SI", "NO"),
    perc_si    = voti_si / (voti_si + voti_no) * 100,
    perc_no    = voti_no / (voti_si + voti_no) * 100,
    perc_vot   = vot_t / ele_t * 100
  )

# Codici ISTAT (ponte tra codici API e ISTAT)
  codici <- fread(CODICI_CSV, sep = ";", encoding = "Latin-1", na.strings = character(0)) %>%
    rename(cod_elettorale = `CODICE ELETTORALE`, cod_istat = `CODICE ISTAT`) %>%
    mutate(
      cod_istat = gsub('="([^"]*)"', "\\1", cod_istat),
      cod_join  = str_sub(as.character(cod_elettorale), -7, -1)
    ) %>%
    select(cod_join, cod_istat)

  # Centroidi dal GeoJSON
    geo <- st_read(GEOJSON, quiet = TRUE)
    centroids <- st_centroid(geo) %>%
      mutate(
        lon       = st_coordinates(.)[, 1],
        lat       = st_coordinates(.)[, 2],
        nome_norm = toupper(trimws(COMUNE))
      ) %>%
      st_drop_geometry() %>%
      select(cod_istat = PRO_COM_T, nome_norm, lon, lat, COD_REG)

    cat("Comuni nel GeoJSON:", nrow(centroids), "\n")

    # Join principale via cod_istat
    df_grafico <- risultati_ok %>%
      left_join(codici, by = "cod_join") %>%
      left_join(centroids %>% select(cod_istat, lon, lat), by = "cod_istat")

    # Fallback Sardegna: join per nome normalizzato
    sard_centroids <- centroids %>%
      filter(COD_REG == 20) %>%
      select(nome_norm, lon_s = lon, lat_s = lat)

    df_grafico <- df_grafico %>%
      mutate(nome_norm = toupper(trimws(desc_comune))) %>%
      left_join(sard_centroids, by = "nome_norm") %>%
      mutate(
        lat = ifelse(is.na(lat), lat_s, lat),
        lon = ifelse(is.na(lon), lon_s, lon)
      ) %>%
      select(-nome_norm, -lon_s, -lat_s)



# Colonne finali versione grafico
df_grafico <- df_grafico %>%
  select(
    cod_regione, desc_regione, cod_provincia, desc_provincia,
    cod_comune, desc_comune, cod_istat,
    ele_t, sz_tot, sz_perv, vot_t, perc_vot,
    sk_bianche, sk_nulle,
    voti_si, voti_no, perc_si, perc_no,
    diff_si_no, vincitore,
    lat, lon
  )

fwrite(df_grafico, file.path(OUTPUT_DIR, "risultati_comuni_grafico.csv"))
cat("Salvato: risultati_comuni_grafico.csv\n")
