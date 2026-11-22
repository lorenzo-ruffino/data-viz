# ============================================================
# Costo delle case - Come si va al lavoro (ISTAT, Aspetti della vita
# quotidiana), per tipo di comune e regione
# ============================================================
# Fonte: ISTAT 83_63_DF_DCCV_AVQ_PERSONE_170 "Spostamenti per andare a
# lavoro - regioni e tipo di comune" (indagine Aspetti della vita quotidiana)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

base_dir  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Costo delle case"
input_dir <- file.path(base_dir, "input")

# L'API SDMX dell'ISTAT a volte risponde lentamente: scarico in un file
# temporaneo con timeout lungo e qualche tentativo prima di arrendermi.
options(timeout = 600)
scarica <- function(url, tentativi = 4) {
  dest <- tempfile()
  for (i in seq_len(tentativi)) {
    ok <- tryCatch({ download.file(url, dest, quiet = TRUE, mode = "wb"); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (ok && file.size(dest) > 0) return(dest)
    Sys.sleep(5 * i)
  }
  stop("Download non riuscito: ", url)
}

# ---- ISTAT, Aspetti della vita quotidiana: come si va al lavoro ----
# Occupati di 15 anni e più che escono di casa abitualmente per andare al
# lavoro, per mezzo usato e tempo impiegato (per 100 persone con le stesse
# caratteristiche), per regione e tipo di comune. Nella convenzione ISTAT le
# quote per mezzo sono riferite a chi usa mezzi di trasporto (più mezzi
# possibili), le quote per tempo a tutti gli occupati che escono di casa.
avq <- read_csv(scarica(paste0("https://esploradati.istat.it/SDMXWS/rest/data/",
                       "83_63_DF_DCCV_AVQ_PERSONE_170/A....9.Y_GE15../ALL/",
                       "?startPeriod=2015&format=csv")),
                col_types = cols(.default = "c")) %>%
  filter(MEASURE == "HSC") %>%
  transmute(
    area = REF_AREA,
    area_nome = recode(REF_AREA, IT = "Italia", "2" = "centro area metropolitana",
                       "3" = "periferia area metropolitana",
                       "6" = "comuni fino a 2.000 ab.",
                       "7" = "comuni 2.001-10.000 ab.",
                       "8" = "comuni 10.001-50.000 ab.",
                       "9" = "comuni oltre 50.000 ab.",
                       ITC1 = "Piemonte", ITC4 = "Lombardia", ITE4 = "Lazio",
                       ITF3 = "Campania", ITD5 = "Emilia-Romagna",
                       .default = REF_AREA),
    voce = recode(DATA_TYPE,
                  "15_EMPL_MOVT_15" = "tempo fino a 15 minuti",
                  "15_EMPL_MOVT_31" = "tempo 31 minuti e più",
                  "15_EMPMOV_FOOT" = "a piedi",
                  "15_EMPMOV_MEANS" = "usano mezzi di trasporto",
                  "15_EMPMOV_TRAIN" = "treno", "15_EMPMOV_BUS" = "tram, bus",
                  "15_EMPMOV_METRO" = "metropolitana",
                  "15_EMPMOV_COACH" = "pullman, corriera",
                  "15_EMPMOV_BUSC" = "pullman aziendale",
                  "15_EMPMOV_PCAR" = "auto (conducente)",
                  "15_EMPMOV_PPASS" = "auto (passeggero)",
                  "15_EMPMOV_MOTOR" = "moto, motorino",
                  "15_EMPMOV_BICYC" = "bicicletta"),
    anno = as.integer(TIME_PERIOD), valore = as.numeric(OBS_VALUE)) %>%
  arrange(area, voce, anno)
write_csv(avq, file.path(input_dir, "trasporti_istat_avq_spostamenti_lavoro.csv"))
