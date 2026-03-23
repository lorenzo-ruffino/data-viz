# Estrazione risultati Referendum Giustizia 2026 per sezione
# Fonte: https://eleapi.interno.gov.it
# Dipende da Risultati.r per la lista comuni e sz_tot (sempre rieseguito)

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

OUTPUT_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum_Giustizia_2026/risultati"

# ==============================================================================
# 1. LISTA COMUNI CON sz_tot (da CSV già estratto)
# ==============================================================================

risultati_comuni <- fread(file.path(OUTPUT_DIR, "risultati_comuni.csv"))

comuni_base <- risultati_comuni %>%
  select(cod_regione, desc_regione, cod_provincia, desc_provincia,
         cod_comune, desc_comune, ele_t, sz_tot) %>%
  distinct() %>%
  filter(!is.na(sz_tot), sz_tot > 0)

# Espandi: una riga per sezione
df_sezioni <- do.call(rbind, lapply(1:nrow(comuni_base), function(i) {
  row <- comuni_base[i, ]
  n   <- as.integer(row$sz_tot)
  data.frame(
    cod_regione    = row$cod_regione,
    desc_regione   = row$desc_regione,
    cod_provincia  = row$cod_provincia,
    desc_provincia = row$desc_provincia,
    cod_comune     = row$cod_comune,
    desc_comune    = row$desc_comune,
    ele_t_comune   = row$ele_t,
    sz_tot         = n,
    cod_sezione    = sprintf("%04d", 1:n),
    stringsAsFactors = FALSE
  )
}))

cat("Sezioni totali da fetchare:", nrow(df_sezioni), "\n")

# ==============================================================================
# 2. FETCH PARALLELO CON RETRY
# ==============================================================================

fetch_sezione <- function(row_list) {
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
    '/PR/', sprintf("%03d", as.integer(row_list$cod_provincia)),
    '/CM/', sprintf("%04d", as.integer(row_list$cod_comune)),
    '/SZ/', row_list$cod_sezione
  )

  base_row <- data.frame(
    cod_regione    = row_list$cod_regione,
    desc_regione   = row_list$desc_regione,
    cod_provincia  = row_list$cod_provincia,
    desc_provincia = row_list$desc_provincia,
    cod_comune     = row_list$cod_comune,
    desc_comune    = row_list$desc_comune,
    ele_t_comune   = row_list$ele_t_comune,
    cod_sezione    = row_list$cod_sezione,
    ele_t          = NA_integer_,
    ele_m          = NA_integer_,
    ele_f          = NA_integer_,
    osp            = NA_character_,
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
    Sys.sleep(0.1 * attempt)

    result <- tryCatch({
      r    <- GET(url, add_headers(.headers = headers), timeout(15))
      body <- rawToChar(r$content)

      if (grepl("^<", trimws(body))) {
        if (attempt < row_list$MAX_RETRY) return(NULL)
        base_row$stato <- "rate_limited"
        return(base_row)
      }

      data <- fromJSON(body)

      if (!is.null(data$Error)) {
        base_row$stato <- "dati_non_trovati"
        return(base_row)
      }

      if (!is.null(data$scheda) && nrow(data$scheda) > 0) {
        info <- data$int
        sch  <- data$scheda[1, ]
        data.frame(
          cod_regione    = row_list$cod_regione,
          desc_regione   = row_list$desc_regione,
          cod_provincia  = row_list$cod_provincia,
          desc_provincia = row_list$desc_provincia,
          cod_comune     = row_list$cod_comune,
          desc_comune    = row_list$desc_comune,
          ele_t_comune   = row_list$ele_t_comune,
          cod_sezione    = row_list$cod_sezione,
          ele_t          = info$ele_t,
          ele_m          = info$ele_m,
          ele_f          = info$ele_f,
          osp            = if (!is.null(info$osp)) info$osp else NA_character_,
          sz_perv        = sch$sz_perv,
          vot_t          = sch$vot_t,
          vot_m          = sch$vot_m,
          vot_f          = sch$vot_f,
          perc_vot       = as.numeric(gsub(",", ".", sch$perc_vot)),
          sk_bianche     = sch$sk_bianche,
          sk_nulle       = sch$sk_nulle,
          sk_contestate  = sch$sk_contestate,
          voti_si        = sch$voti_si,
          voti_no        = sch$voti_no,
          perc_si        = as.numeric(gsub(",", ".", sch$perc_si)),
          perc_no        = as.numeric(gsub(",", ".", sch$perc_no)),
          dt_agg         = as.character(sch$dt_agg),
          stato          = "ok",
          stringsAsFactors = FALSE
        )
      } else {
        base_row$stato <- "no_data"
        base_row
      }
    }, error = function(e) {
      if (attempt < row_list$MAX_RETRY) return(NULL)
      base_row$stato <- "errore"
      base_row
    })

    if (!is.null(result)) return(result)
  }

  base_row$stato <- "max_retry"
  base_row
}

sezioni_input <- lapply(1:nrow(df_sezioni), function(i) {
  c(as.list(df_sezioni[i, ]),
    list(DATE = DATE, TIPO_ELEZIONE = TIPO_ELEZIONE,
         SCHEDA = SCHEDA, MAX_RETRY = MAX_RETRY))
})

cat("Avvio fetch parallelo con", N_WORKERS, "worker...\n")
t0 <- proc.time()

cl <- makeCluster(N_WORKERS)
risultati_raw <- parLapply(cl, sezioni_input, fetch_sezione)
stopCluster(cl)

elapsed <- round((proc.time() - t0)["elapsed"])
cat("Completato in", elapsed, "secondi\n")

risultati_sezioni <- do.call(rbind, risultati_raw)
rownames(risultati_sezioni) <- NULL

cat("\nStato fetch:\n")
print(table(risultati_sezioni$stato))
cat("Sezioni totali:", nrow(risultati_sezioni), "\n")

# ==============================================================================
# 3. SALVATAGGIO
# ==============================================================================

fwrite(risultati_sezioni, file.path(OUTPUT_DIR, "risultati_sezioni.csv"))
cat("CSV salvato: risultati_sezioni.csv\n")
