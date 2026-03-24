# Estrazione risultati per sezione — script unificato
#
# Fasi:
#   1. FETCH INIZIALE  — scarica tutte le sezioni 1:sz_tot per ogni comune
#   2. RETRY           — riprova le sezioni rimaste in errore (stato "non_ok")
#   3. DISCOVERY       — per i comuni con sezioni mancanti (numerazione non sequenziale)
#                        scannerizza 1:UPPER escludendo quelle già ok
#   4. PULIZIA         — rimuove righe di sezioni inesistenti nei comuni noti,
#                        rinomina stati residui in "non_disponibile"
#
# Input richiesto: risultati_comuni.csv (già estratto da Risultati_comuni.r)
# Output:          risultati_sezioni.csv
#
# Riuso per elezioni future: aggiorna DATE, TIPO_ELEZIONE, SCHEDA nella sezione
# CONFIGURAZIONE qui sotto. Il resto dello script è generico.

library(httr)
library(jsonlite)
library(dplyr)
library(data.table)
library(parallel)

# ==============================================================================
# CONFIGURAZIONE
# ==============================================================================

DATE          <- "20260322"
TIPO_ELEZIONE <- "09"
SCHEDA        <- "01"
N_WORKERS     <- 4L
MAX_RETRY     <- 5L     # tentativi per sezione
SLEEP_BASE    <- 0.1    # secondi di sleep tra tentativi (moltiplicato per attempt)
UPPER         <- 2500L  # range massimo discovery per comuni con numerazione non sequenziale

OUTPUT_DIR <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum_Giustizia_2026/risultati"
CSV_PATH   <- file.path(OUTPUT_DIR, "risultati_sezioni.csv")

# ==============================================================================
# FUNZIONE FETCH — comune a tutte le fasi
# ==============================================================================

fetch_sezione <- function(row_list) {
  library(httr)
  library(jsonlite)

  headers <- c(
    'accept'            = 'application/json, text/plain, */*',
    'accept-language'   = 'en-US,en;q=0.6',
    'dnt'               = '1',
    'origin'            = 'https://elezioni.interno.gov.it',
    'referer'           = 'https://elezioni.interno.gov.it/',
    'sec-fetch-dest'    = 'empty',
    'sec-fetch-mode'    = 'cors',
    'sec-fetch-site'    = 'same-site'
  )

  url <- paste0(
    'https://eleapi.interno.gov.it/siel/PX/scrutiniFI/DE/',
    row_list$DATE, '/TE/', row_list$TIPO_ELEZIONE,
    '/SK/', row_list$SCHEDA,
    '/PR/', sprintf("%03d", as.integer(row_list$cod_provincia)),
    '/CM/', sprintf("%04d", as.integer(row_list$cod_comune)),
    '/SZ/', sprintf("%04d", as.integer(row_list$cod_sezione))
  )

  base_row <- data.frame(
    cod_regione    = row_list$cod_regione,
    desc_regione   = row_list$desc_regione,
    cod_provincia  = row_list$cod_provincia,
    desc_provincia = row_list$desc_provincia,
    cod_comune     = row_list$cod_comune,
    desc_comune    = row_list$desc_comune,
    ele_t_comune   = row_list$ele_t_comune,
    cod_sezione    = as.integer(row_list$cod_sezione),
    ele_t          = NA_integer_, ele_m = NA_integer_, ele_f = NA_integer_,
    osp            = NA_character_,
    sz_perv        = NA_integer_, vot_t = NA_integer_,
    vot_m          = NA_integer_, vot_f = NA_integer_,
    perc_vot       = NA_real_,
    sk_bianche     = NA_integer_, sk_nulle = NA_integer_, sk_contestate = NA_integer_,
    voti_si        = NA_integer_, voti_no = NA_integer_,
    perc_si        = NA_real_,    perc_no = NA_real_,
    dt_agg         = NA_character_,
    stato          = "no_data",
    stringsAsFactors = FALSE
  )

  result <- base_row

  for (attempt in 1:row_list$MAX_RETRY) {
    Sys.sleep(row_list$SLEEP_BASE * attempt)

    esito <- tryCatch({
      r    <- GET(url, add_headers(.headers = headers), timeout(20))
      body <- rawToChar(r$content)

      if (grepl("^<", trimws(body))) {
        list(tipo = "retry")  # rate limited: HTML invece di JSON
      } else if (grepl("scheda:}", body) || grepl("\"scheda\":}", body)) {
        list(tipo = "done", row = { base_row$stato <- "dati_non_trovati"; base_row })
      } else {
        data <- fromJSON(body)
        if (!is.null(data$Error)) {
          list(tipo = "done", row = { base_row$stato <- "dati_non_trovati"; base_row })
        } else if (!is.null(data$scheda) && nrow(data$scheda) > 0) {
          info <- data$int
          sch  <- data$scheda[1, ]
          list(tipo = "done", row = data.frame(
            cod_regione    = row_list$cod_regione,
            desc_regione   = row_list$desc_regione,
            cod_provincia  = row_list$cod_provincia,
            desc_provincia = row_list$desc_provincia,
            cod_comune     = row_list$cod_comune,
            desc_comune    = row_list$desc_comune,
            ele_t_comune   = row_list$ele_t_comune,
            cod_sezione    = as.integer(row_list$cod_sezione),
            ele_t          = info$ele_t, ele_m = info$ele_m, ele_f = info$ele_f,
            osp            = if (!is.null(info$osp)) info$osp else NA_character_,
            sz_perv        = sch$sz_perv, vot_t = sch$vot_t,
            vot_m          = sch$vot_m,  vot_f = sch$vot_f,
            perc_vot       = as.numeric(gsub(",", ".", sch$perc_vot)),
            sk_bianche     = sch$sk_bianche, sk_nulle = sch$sk_nulle,
            sk_contestate  = sch$sk_contestate,
            voti_si        = sch$voti_si, voti_no = sch$voti_no,
            perc_si        = as.numeric(gsub(",", ".", sch$perc_si)),
            perc_no        = as.numeric(gsub(",", ".", sch$perc_no)),
            dt_agg         = as.character(sch$dt_agg),
            stato          = "ok",
            stringsAsFactors = FALSE
          ))
        } else {
          list(tipo = "done", row = { base_row$stato <- "dati_non_trovati"; base_row })
        }
      }
    }, error = function(e) list(tipo = "retry"))

    if (esito$tipo == "done") {
      result <- esito$row
      break
    }
  }

  if (result$stato == "no_data") result$stato <- "max_retry"
  result
}

run_parallel <- function(input_list, n_workers) {
  cl  <- makeCluster(n_workers)
  out <- parLapply(cl, input_list, fetch_sezione)
  stopCluster(cl)
  out
}

# ==============================================================================
# FASE 1 — FETCH INIZIALE (1:sz_tot per ogni comune)
# ==============================================================================

cat("=== FASE 1: fetch iniziale ===\n")

risultati_comuni <- fread(file.path(OUTPUT_DIR, "risultati_comuni.csv"))

comuni_base <- risultati_comuni %>%
  select(cod_regione, desc_regione, cod_provincia, desc_provincia,
         cod_comune, desc_comune, ele_t, sz_tot) %>%
  distinct() %>%
  filter(!is.na(sz_tot), sz_tot > 0)

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
    cod_sezione    = 1:n,
    stringsAsFactors = FALSE
  )
}))

cat("Sezioni da fetchare:", nrow(df_sezioni), "\n")

sezioni_input <- lapply(1:nrow(df_sezioni), function(i) {
  c(as.list(df_sezioni[i, ]),
    list(DATE = DATE, TIPO_ELEZIONE = TIPO_ELEZIONE, SCHEDA = SCHEDA,
         MAX_RETRY = MAX_RETRY, SLEEP_BASE = SLEEP_BASE))
})

t0 <- proc.time()
cat("Avvio con", N_WORKERS, "worker...\n")
risultati_raw <- run_parallel(sezioni_input, N_WORKERS)
elapsed <- round((proc.time() - t0)["elapsed"])
cat("Completato in", elapsed, "secondi\n")

sz <- setDT(do.call(rbind, risultati_raw))
rownames(sz) <- NULL
cat("Stato fase 1:\n"); print(table(sz$stato))

fwrite(sz, CSV_PATH)
cat("CSV salvato:", CSV_PATH, "\n\n")

# ==============================================================================
# FASE 2 — RETRY sezioni in errore (max_retry)
# ==============================================================================

cat("=== FASE 2: retry errori ===\n")

da_rifare <- sz[stato == "max_retry"]
cat("Sezioni da rifare:", nrow(da_rifare), "\n")

if (nrow(da_rifare) > 0) {
  retry_input <- lapply(1:nrow(da_rifare), function(i) {
    c(as.list(da_rifare[i, ]),
      list(DATE = DATE, TIPO_ELEZIONE = TIPO_ELEZIONE, SCHEDA = SCHEDA,
           MAX_RETRY = MAX_RETRY, SLEEP_BASE = 0.3))  # sleep più lungo
  })

  t0 <- proc.time()
  retry_raw <- run_parallel(retry_input, N_WORKERS)
  elapsed <- round((proc.time() - t0)["elapsed"])
  cat("Completato in", elapsed, "secondi\n")

  retry_dt <- setDT(do.call(rbind, retry_raw))
  cat("Stato retry:\n"); print(table(retry_dt$stato))

  # Sostituisce le righe max_retry con i nuovi risultati
  sz <- rbind(sz[stato != "max_retry"], retry_dt, fill = TRUE)
  setorder(sz, cod_regione, cod_provincia, cod_comune, cod_sezione)
  cat("Stato dopo retry:\n"); print(table(sz$stato))
  fwrite(sz, CSV_PATH)
} else {
  cat("Nessuna sezione da rifare.\n")
}
cat("\n")

# ==============================================================================
# FASE 3 — DISCOVERY sezioni a numerazione non sequenziale
# Comuni con n_ok > 0 MA n_ok < sz_tot: mancano sezioni, scan 1:UPPER
# ==============================================================================

cat("=== FASE 3: discovery sezioni mancanti ===\n")

problematici <- sz[, .(
  n_ok  = sum(stato == "ok"),
  n_err = sum(stato != "ok"),
  sz_tot = .N
), by = .(desc_regione, desc_comune, cod_provincia, cod_comune)
][n_ok > 0 & n_ok < sz_tot][order(-n_err)]

cat("Comuni con sezioni mancanti:", nrow(problematici), "\n")

if (nrow(problematici) > 0) {
  print(problematici[, .(desc_regione, desc_comune, sz_tot, n_ok, n_err)])
  cat("\n")

  sz_aggiornato <- copy(sz)

  for (i in 1:nrow(problematici)) {
    com <- problematici[i]
    cat(sprintf("[%d/%d] %s — sz_tot=%d, ok=%d, mancanti=%d\n",
                i, nrow(problematici), com$desc_comune,
                com$sz_tot, com$n_ok, com$n_err))

    gia_ok    <- sz[cod_provincia == com$cod_provincia &
                    cod_comune    == com$cod_comune &
                    stato         == "ok", cod_sezione]
    da_scan   <- setdiff(1:UPPER, gia_ok)
    info_com  <- sz[cod_provincia == com$cod_provincia & cod_comune == com$cod_comune][1]

    disc_input <- lapply(da_scan, function(n) list(
      cod_regione    = info_com$cod_regione,
      desc_regione   = info_com$desc_regione,
      cod_provincia  = com$cod_provincia,
      desc_provincia = info_com$desc_provincia,
      cod_comune     = com$cod_comune,
      desc_comune    = com$desc_comune,
      ele_t_comune   = info_com$ele_t_comune,
      cod_sezione    = n,
      DATE = DATE, TIPO_ELEZIONE = TIPO_ELEZIONE, SCHEDA = SCHEDA,
      MAX_RETRY = MAX_RETRY, SLEEP_BASE = SLEEP_BASE
    ))

    t0 <- proc.time()
    disc_raw <- run_parallel(disc_input, N_WORKERS)
    elapsed  <- round((proc.time() - t0)["elapsed"])

    nuove    <- do.call(rbind, disc_raw)
    nuove_ok <- nuove[nuove$stato == "ok", ]

    tot_ok <- com$n_ok + nrow(nuove_ok)
    cat(sprintf("  %ds — nuove ok: %d | totale: %d/%d\n",
                elapsed, nrow(nuove_ok), tot_ok, com$sz_tot))

    # Rimuove righe non-ok del comune, aggiunge nuove ok
    sz_aggiornato <- sz_aggiornato[!(cod_provincia == com$cod_provincia &
                                     cod_comune    == com$cod_comune &
                                     stato         != "ok")]
    if (nrow(nuove_ok) > 0) {
      sz_aggiornato <- rbind(sz_aggiornato, setDT(nuove_ok), fill = TRUE)
    }
  }

  setDT(sz_aggiornato)
  setorder(sz_aggiornato, cod_regione, cod_provincia, cod_comune, cod_sezione)
  sz <- sz_aggiornato
  cat("Stato dopo discovery:\n"); print(table(sz$stato))
  fwrite(sz, CSV_PATH)
}
cat("\n")

# ==============================================================================
# FASE 4 — PULIZIA FINALE
# Rimuove righe dati_non_trovati nei comuni dove altre sezioni sono ok
# (sezioni inesistenti emerse dalla discovery), rinomina stati residui
# ==============================================================================

cat("=== FASE 4: pulizia finale ===\n")

# Comuni dove abbiamo sezioni ok: le dati_non_trovati sono sezioni inesistenti
comuni_con_ok <- sz[stato == "ok", unique(paste(cod_provincia, cod_comune))]
sz[, chiave := paste(cod_provincia, cod_comune)]

righe_prima <- nrow(sz)
sz <- sz[!(chiave %in% comuni_con_ok & stato == "dati_non_trovati")]
sz[, chiave := NULL]
cat("Righe rimosse (sezioni inesistenti):", righe_prima - nrow(sz), "\n")

# Stato residuo → non_disponibile
sz[stato %in% c("max_retry", "dati_non_trovati"), stato := "non_disponibile"]

setorder(sz, cod_regione, cod_provincia, cod_comune, cod_sezione)
cat("Stato finale:\n"); print(table(sz$stato))
cat("Totale sezioni:", nrow(sz), "\n")

fwrite(sz, CSV_PATH)
cat("CSV finale salvato:", CSV_PATH, "\n")
