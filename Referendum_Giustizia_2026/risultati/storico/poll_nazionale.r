# Polling risultati nazionali referendum ogni 5 minuti
# Salva ogni rilevazione con timestamp in risultati/storico/

library(httr)
library(jsonlite)
library(data.table)

DATE          <- "20260322"
TIPO_ELEZIONE <- "09"
SCHEDA        <- "01"
INTERVALLO    <- 5 * 60  # secondi
OUTPUT_DIR    <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Referendum_Giustizia_2026/risultati/storico"

dir.create(OUTPUT_DIR, showWarnings = FALSE)

headers <- c(
  'accept'          = 'application/json, text/plain, */*',
  'origin'          = 'https://elezioni.interno.gov.it',
  'referer'         = 'https://elezioni.interno.gov.it/',
  'dnt'             = '1',
  'sec-fetch-dest'  = 'empty',
  'sec-fetch-mode'  = 'cors',
  'sec-fetch-site'  = 'same-site'
)

url <- paste0(
  'https://eleapi.interno.gov.it/siel/PX/scrutiniFI/DE/', DATE,
  '/TE/', TIPO_ELEZIONE, '/SK/', SCHEDA
)

fetch_nazionale <- function() {
  r <- tryCatch(
    GET(url, add_headers(.headers = headers), timeout(15)),
    error = function(e) NULL
  )
  if (is.null(r) || r$status_code != 200) return(NULL)

  body <- fromJSON(rawToChar(r$content))
  int  <- body$int
  sch  <- body$scheda[body$scheda$cod == as.integer(SCHEDA), ]

  data.frame(
    timestamp     = format(Sys.time(), "%Y%m%d_%H%M%S"),
    dt_agg        = as.character(int$dt_ele),
    ele_t         = int$ele_t,
    sz_tot        = int$sz_tot,
    sz_perv       = sch$sz_perv,
    vot_t         = sch$vot_t,
    perc_vot      = as.numeric(sch$perc_vot),
    sk_bianche    = sch$sk_bianche,
    sk_nulle      = sch$sk_nulle,
    voti_si       = sch$voti_si,
    voti_no       = sch$voti_no,
    perc_si       = sch$voti_si / (sch$voti_si + sch$voti_no) * 100,
    perc_no       = sch$voti_no / (sch$voti_si + sch$voti_no) * 100,
    stringsAsFactors = FALSE
  )
}

cat("Avvio polling ogni", INTERVALLO / 60, "minuti. Ctrl+C per fermare.\n")
cat("Output:", OUTPUT_DIR, "\n\n")

repeat {
  ts  <- format(Sys.time(), "%Y%m%d_%H%M%S")
  row <- fetch_nazionale()

  if (!is.null(row)) {
    nome_file <- file.path(OUTPUT_DIR, paste0("nazionale_", ts, ".csv"))
    fwrite(row, nome_file)
    cat(ts, "| sezioni:", row$sz_perv, "/", row$sz_tot,
        "| vot:", paste0(round(row$perc_vot, 2), "%"),
        "| SI:", paste0(round(row$perc_si, 2), "%"),
        "| NO:", paste0(round(row$perc_no, 2), "%"), "\n")
  } else {
    cat(ts, "| errore fetch\n")
  }

  Sys.sleep(INTERVALLO)
}
