# Estrazione risultati Referendum Giustizia 2026 - Voto estero
# Fonte: https://eleapi.interno.gov.it
# Livelli: totale estero, ripartizione (ER), paese (NA)

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

BASE_SCRUTINI <- paste0('https://eleapi.interno.gov.it/siel/PX/scrutiniFE/DE/',
                        DATE, '/TE/', TIPO_ELEZIONE, '/SK/', SCHEDA)

# ==============================================================================
# 1. LISTA ENTI ESTERI
# ==============================================================================

url_enti  <- paste0('https://eleapi.interno.gov.it/siel/PX/getentiFE/DE/', DATE, '/TE/', TIPO_ELEZIONE)
data_enti <- fromJSON(rawToChar(GET(url_enti, add_headers(.headers = headers))$content))$enti

df_er <- data_enti %>% filter(tipo == "ER") %>%
  mutate(cod_rip = sprintf("%02d", as.integer(substr(cod, 1, 1))),
         desc_rip = desc)

df_na <- data_enti %>% filter(tipo == "NA") %>%
  mutate(cod_rip  = sprintf("%02d", as.integer(substr(cod, 1, 1))),
         cod_naz  = substr(cod, 2, 4),
         desc_naz = desc) %>%
  left_join(df_er %>% select(cod_rip, desc_rip), by = "cod_rip")

cat("Ripartizioni:", nrow(df_er), "| Paesi:", nrow(df_na), "\n")

# ==============================================================================
# 2. HELPER: parsing scheda
# ==============================================================================

parse_scheda <- function(data) {
  if (is.null(data$scheda) || nrow(data$scheda) == 0) return(NULL)
  sch <- data$scheda[1, ]
  list(
    sz_perv       = if (!is.null(sch)) sch$sz_perv    else NA_integer_,
    vot_t         = if (!is.null(sch)) sch$vot_t      else NA_integer_,
    perc_vot      = as.numeric(gsub(",", ".", sch$perc_vot)),
    sk_bianche    = if (!is.null(sch)) sch$sk_bianche else NA_integer_,
    sk_nulle      = if (!is.null(sch)) sch$sk_nulle   else NA_integer_,
    sk_contestate = if (!is.null(sch)) sch$sk_contestate else NA_integer_,
    voti_si       = if (!is.null(sch)) sch$voti_si    else NA_integer_,
    voti_no       = if (!is.null(sch)) sch$voti_no    else NA_integer_,
    perc_si       = as.numeric(gsub(",", ".", sch$perc_si)),
    perc_no       = as.numeric(gsub(",", ".", sch$perc_no)),
    dt_agg        = as.character(sch$dt_agg)
  )
}

# ==============================================================================
# 3. TOTALE ESTERO
# ==============================================================================

r <- GET(BASE_SCRUTINI, add_headers(.headers = headers))
d <- fromJSON(rawToChar(r$content))
sch <- parse_scheda(d)

risultati_totale <- data.frame(
  livello       = "ESTERO",
  desc          = "TOTALE ESTERO",
  ele_t         = d$int$ele_t,
  sz_tot        = d$int$sz_tot,
  sz_perv       = if (!is.null(sch)) sch$sz_perv    else NA_integer_,
  vot_t         = if (!is.null(sch)) sch$vot_t      else NA_integer_,
  perc_vot      = if (!is.null(sch)) sch$perc_vot   else NA_real_,
  sk_bianche    = if (!is.null(sch)) sch$sk_bianche else NA_integer_,
  sk_nulle      = if (!is.null(sch)) sch$sk_nulle   else NA_integer_,
  sk_contestate = if (!is.null(sch)) sch$sk_contestate else NA_integer_,
  voti_si       = if (!is.null(sch)) sch$voti_si    else NA_integer_,
  voti_no       = if (!is.null(sch)) sch$voti_no    else NA_integer_,
  perc_si       = if (!is.null(sch)) sch$perc_si    else NA_real_,
  perc_no       = if (!is.null(sch)) sch$perc_no    else NA_real_,
  dt_agg        = if (!is.null(sch)) sch$dt_agg     else NA_character_,
  stringsAsFactors = FALSE
)
cat("Totale estero: ele_t =", risultati_totale$ele_t, "| sz_tot =", risultati_totale$sz_tot, "\n")

# ==============================================================================
# 4. PER RIPARTIZIONE
# ==============================================================================

risultati_ripartizioni <- lapply(1:nrow(df_er), function(i) {
  row <- df_er[i, ]
  r <- GET(paste0(BASE_SCRUTINI, '/ER/', row$cod_rip),
           add_headers(.headers = headers))
  d <- fromJSON(rawToChar(r$content))
  sch <- parse_scheda(d)
  Sys.sleep(0.2)
  data.frame(
    cod_rip       = row$cod_rip,
    desc_rip      = row$desc_rip,
    ele_t         = d$int$ele_t,
    sz_tot        = d$int$sz_tot,
    sz_perv       = if (!is.null(sch)) sch$sz_perv    else NA_integer_,
    vot_t         = if (!is.null(sch)) sch$vot_t      else NA_integer_,
    perc_vot      = if (!is.null(sch)) sch$perc_vot   else NA_real_,
    sk_bianche    = if (!is.null(sch)) sch$sk_bianche else NA_integer_,
    sk_nulle      = if (!is.null(sch)) sch$sk_nulle   else NA_integer_,
    sk_contestate = if (!is.null(sch)) sch$sk_contestate else NA_integer_,
    voti_si       = if (!is.null(sch)) sch$voti_si    else NA_integer_,
    voti_no       = if (!is.null(sch)) sch$voti_no    else NA_integer_,
    perc_si       = if (!is.null(sch)) sch$perc_si    else NA_real_,
    perc_no       = if (!is.null(sch)) sch$perc_no    else NA_real_,
    dt_agg        = if (!is.null(sch)) sch$dt_agg     else NA_character_,
    stringsAsFactors = FALSE
  )
})

risultati_ripartizioni <- do.call(rbind, risultati_ripartizioni)
cat("Ripartizioni estratte:", nrow(risultati_ripartizioni), "\n")

# ==============================================================================
# 5. PER PAESE (parallelo)
# ==============================================================================

fetch_paese <- function(row_list) {
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

  url <- paste0(row_list$BASE_SCRUTINI,
                '/ER/', row_list$cod_rip,
                '/NA/', row_list$cod_naz)

  base_row <- data.frame(
    cod_rip       = row_list$cod_rip,
    desc_rip      = row_list$desc_rip,
    cod_naz       = row_list$cod_naz,
    desc_naz      = row_list$desc_naz,
    ele_t         = NA_integer_,
    sz_tot        = NA_integer_,
    sz_perv       = NA_integer_,
    vot_t         = NA_integer_,
    perc_vot      = NA_real_,
    sk_bianche    = NA_integer_,
    sk_nulle      = NA_integer_,
    sk_contestate = NA_integer_,
    voti_si       = NA_integer_,
    voti_no       = NA_integer_,
    perc_si       = NA_real_,
    perc_no       = NA_real_,
    dt_agg        = NA_character_,
    stato         = "no_data",
    stringsAsFactors = FALSE
  )

  for (attempt in 1:row_list$MAX_RETRY) {
    Sys.sleep(0.1 * attempt)
    result <- tryCatch({
      r    <- GET(url, add_headers(.headers = headers), timeout(15))
      body <- rawToChar(r$content)

      if (grepl("^<", trimws(body))) {
        if (attempt < row_list$MAX_RETRY) return(NULL)
        base_row$stato <- "rate_limited"; return(base_row)
      }

      d <- fromJSON(body)
      if (!is.null(d$Error)) {
        base_row$stato <- "dati_non_trovati"; return(base_row)
      }

      sch <- if (!is.null(d$scheda) && nrow(d$scheda) > 0) d$scheda[1, ] else NULL
      data.frame(
        cod_rip       = row_list$cod_rip,
        desc_rip      = row_list$desc_rip,
        cod_naz       = row_list$cod_naz,
        desc_naz      = row_list$desc_naz,
        ele_t         = d$int$ele_t,
        sz_tot        = d$int$sz_tot,
        sz_perv       = if (!is.null(sch)) sch$sz_perv    else NA_integer_,
        vot_t         = if (!is.null(sch)) sch$vot_t      else NA_integer_,
        perc_vot      = if (!is.null(sch)) as.numeric(gsub(",", ".", sch$perc_vot)) else NA_real_,
        sk_bianche    = if (!is.null(sch)) sch$sk_bianche else NA_integer_,
        sk_nulle      = if (!is.null(sch)) sch$sk_nulle   else NA_integer_,
        sk_contestate = if (!is.null(sch)) sch$sk_contestate else NA_integer_,
        voti_si       = if (!is.null(sch)) sch$voti_si    else NA_integer_,
        voti_no       = if (!is.null(sch)) sch$voti_no    else NA_integer_,
        perc_si       = if (!is.null(sch)) as.numeric(gsub(",", ".", sch$perc_si)) else NA_real_,
        perc_no       = if (!is.null(sch)) as.numeric(gsub(",", ".", sch$perc_no)) else NA_real_,
        dt_agg        = if (!is.null(sch)) as.character(sch$dt_agg) else NA_character_,
        stato         = "ok",
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      if (attempt < row_list$MAX_RETRY) return(NULL)
      base_row$stato <- "errore"; base_row
    })
    if (!is.null(result)) return(result)
  }
  base_row$stato <- "max_retry"; base_row
}

paesi_input <- lapply(1:nrow(df_na), function(i) {
  c(as.list(df_na[i, ]), list(BASE_SCRUTINI = BASE_SCRUTINI, MAX_RETRY = MAX_RETRY))
})

cat("Avvio fetch paesi con", N_WORKERS, "worker...\n")
t0 <- proc.time()
risultati_raw <- mclapply(paesi_input, fetch_paese, mc.cores = N_WORKERS)
elapsed <- round((proc.time() - t0)["elapsed"])
cat("Completato in", elapsed, "secondi\n")

risultati_paesi <- do.call(rbind, risultati_raw)
rownames(risultati_paesi) <- NULL

cat("\nStato fetch:\n")
print(table(risultati_paesi$stato))

# ==============================================================================
# 6. SALVATAGGIO
# ==============================================================================

fwrite(risultati_paesi,        file.path(OUTPUT_DIR, "risultati_estero.csv"))
fwrite(risultati_ripartizioni, file.path(OUTPUT_DIR, "risultati_estero_ripartizioni.csv"))
cat("CSV salvati: risultati_estero.csv (", nrow(risultati_paesi), "paesi) |",
    "risultati_estero_ripartizioni.csv (", nrow(risultati_ripartizioni), "ripartizioni)\n")
