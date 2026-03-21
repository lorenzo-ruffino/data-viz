# Estrazione risultati Referendum Giustizia 2026 per comune
# Fonte: https://eleapi.interno.gov.it
# Output: risultati SI/NO per comune, aggregati per provincia e regione

library(httr)
library(jsonlite)
library(dplyr)
library(data.table)
library(parallel)

DATE          <- "20260322"
TIPO_ELEZIONE <- "09"
SCHEDA        <- "01"
N_WORKERS     <- 4L
MAX_RETRY     <- 3L

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
# 2. FETCH PARALLELO CON RETRY
# ==============================================================================

fetch_comune <- function(row_list) {
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

  # Riga base con NA (inclusa sempre, anche senza dati)
  base_row <- data.frame(
    cod_regione    = row_list$cod_regione,
    desc_regione   = row_list$desc_regione,
    cod_provincia  = row_list$cod_provincia,
    desc_provincia = row_list$desc_provincia,
    cod_comune     = row_list$cod_comune,
    desc_comune    = row_list$desc,
    tipo_comune    = NA_character_,
    ele_t          = NA_integer_,
    ele_m          = NA_integer_,
    ele_f          = NA_integer_,
    sz_tot         = NA_integer_,
    sz_perv        = NA_integer_,
    vot_t          = NA_integer_,
    vot_m          = NA_integer_,
    vot_f          = NA_integer_,
    perc_vot       = NA_real_,
    sk_bianche     = NA_integer_,
    sk_nulle       = NA_integer_,
    sk_contestate  = NA_integer_,
    voti_si        = NA_integer_,
    voti_no        = NA_integer_,
    perc_si        = NA_real_,
    perc_no        = NA_real_,
    dt_agg         = NA_character_,
    stato          = "no_data",
    stringsAsFactors = FALSE
  )

  for (attempt in 1:row_list$MAX_RETRY) {
    Sys.sleep(0.1 * attempt)  # backoff crescente: 0.1, 0.2, 0.3

    result <- tryCatch({
      r    <- GET(url, add_headers(.headers = headers), timeout(15))
      body <- rawToChar(r$content)

      # Rileva rate limiting (risposta HTML)
      if (grepl("^<", trimws(body))) {
        if (attempt < row_list$MAX_RETRY) next
        base_row$stato <- "rate_limited"
        return(list(base_row))
      }

      data <- fromJSON(body)

      # Dati non trovati (normale se il comune non ha ancora sezioni)
      if (!is.null(data$Error)) {
        base_row$stato <- "dati_non_trovati"
        return(list(base_row))
      }

      if (status_code(r) == 200 && !is.null(data$scheda) && nrow(data$scheda) > 0) {
        info <- data$int
        sch  <- data$scheda
        lapply(1:nrow(sch), function(i) {
          data.frame(
            cod_regione    = row_list$cod_regione,
            desc_regione   = row_list$desc_regione,
            cod_provincia  = row_list$cod_provincia,
            desc_provincia = row_list$desc_provincia,
            cod_comune     = row_list$cod_comune,
            desc_comune    = row_list$desc,
            tipo_comune    = if (!is.null(info$tipo_tras)) info$tipo_tras else NA_character_,
            ele_t          = info$ele_t,
            ele_m          = info$ele_m,
            ele_f          = info$ele_f,
            sz_tot         = info$sz_tot,
            sz_perv        = sch$sz_perv[i],
            vot_t          = sch$vot_t[i],
            vot_m          = sch$vot_m[i],
            vot_f          = sch$vot_f[i],
            perc_vot       = as.numeric(sch$perc_vot[i]),
            sk_bianche     = sch$sk_bianche[i],
            sk_nulle       = sch$sk_nulle[i],
            sk_contestate  = sch$sk_contestate[i],
            voti_si        = sch$voti_si[i],
            voti_no        = sch$voti_no[i],
            perc_si        = as.numeric(sch$perc_si[i]),
            perc_no        = as.numeric(sch$perc_no[i]),
            dt_agg         = as.character(sch$dt_agg[i]),
            stato          = "ok",
            stringsAsFactors = FALSE
          )
        })
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

comuni_list <- lapply(1:nrow(df_comuni), function(i) {
  c(as.list(df_comuni[i, ]), list(DATE = DATE, TIPO_ELEZIONE = TIPO_ELEZIONE,
                                   SCHEDA = SCHEDA, MAX_RETRY = MAX_RETRY))
})

cat("Avvio fetch parallelo con", N_WORKERS, "worker...\n")
t0 <- proc.time()

risultati_raw <- mclapply(comuni_list, fetch_comune, mc.cores = N_WORKERS)

elapsed <- round((proc.time() - t0)["elapsed"])
cat("Completato in", elapsed, "secondi\n")

risultati_comuni <- do.call(rbind, unlist(risultati_raw, recursive = FALSE))
rownames(risultati_comuni) <- NULL

cat("\nStato fetch:\n")
print(table(risultati_comuni$stato))
cat("Comuni totali:", nrow(risultati_comuni), "\n")

# ==============================================================================
# 3. AGGREGATI (solo comuni con dati)
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
# 4. SALVATAGGIO CSV
# ==============================================================================

fwrite(risultati_comuni,    "risultati_comuni.csv")
fwrite(risultati_province,  "risultati_province.csv")
fwrite(risultati_regioni,   "risultati_regioni.csv")
fwrite(risultati_nazionale, "risultati_nazionale.csv")
cat("CSV salvati in risultati/\n")
