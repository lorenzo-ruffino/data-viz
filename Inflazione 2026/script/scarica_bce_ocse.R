# Scarica i dati BCE (salari negoziati, inflazione percepita della Consumer
# Expectations Survey) e OCSE (salari medi annui a prezzi costanti)

library(tidyverse)

dir_in <- "Inflazione 2026/input"

ecb <- function(chiave, inizio) {
  url <- paste0("https://data-api.ecb.europa.eu/service/data/", chiave,
                "?format=csvdata&startPeriod=", inizio)
  read_csv(url, col_types = cols(.default = col_character())) |>
    mutate(OBS_VALUE = as.numeric(OBS_VALUE))
}

# Salari negoziati area euro, variazione annua (BCE)
ecb("STS/Q.I9.N.INWR.000000.3.ANR", "1992") |>
  select(trimestre = TIME_PERIOD, var_annua = OBS_VALUE) |>
  write_csv(file.path(dir_in, "bce_salari_negoziati_area_euro.csv"))

# Wage tracker BCE (contratti negoziati, include proiezioni dei contratti già
# firmati): INWS = una tantum spalmate (headline), INWR = una tantum non
# spalmate, INWX = senza una tantum, COVR = copertura dei dipendenti
ecb("EWT/A+Q.U2.N.WT.INWS+INWR+INWX+COVR._T.4F0.", "2019") |>
  transmute(freq = FREQ, serie = EWT_CONCEPT, periodo = TIME_PERIOD,
            valore = OBS_VALUE) |>
  write_csv(file.path(dir_in, "bce_wage_tracker.csv"))

# Consumer Expectations Survey: inflazione percepita negli ultimi 12 mesi
# (C1020) e attesa nei prossimi 12 mesi (C1120), mediana e media ponderate
paesi_ces <- "IT+DE+FR+ES+NL+BE+AT+IE+PT+GR+FI+Z18"
ces <- ecb(paste0("CES/M.", paesi_ces, ".ALL.T.C1020+C1120.NUM_VAR.WM+WA"), "2020")
ces |>
  transmute(paese = REF_AREA,
            variabile = recode(CES_VARIABLE, C1020 = "percepita_12m_passati",
                               C1120 = "attesa_12m_futuri"),
            statistica = recode(CES_DENOM, WM = "mediana", WA = "media"),
            mese = TIME_PERIOD, valore = OBS_VALUE) |>
  arrange(paese, variabile, statistica, mese) |>
  write_csv(file.path(dir_in, "bce_ces_inflazione_percepita.csv"))

# OCSE: salari medi annui per dipendente equivalente a tempo pieno,
# prezzi costanti in valuta nazionale
oecd <- read_csv(paste0("https://sdmx.oecd.org/public/rest/data/",
                        "OECD.ELS.SAE,DSD_EARNINGS@AV_AN_WAGE,/all",
                        "?startPeriod=1990&format=csvfilewithlabels"),
                 col_types = cols(.default = col_character()))
oecd |>
  filter(PRICE_BASE == "Q", UNIT_MEASURE != "USD_PPP") |>
  transmute(paese = REF_AREA, nome = `Reference area`, valuta = UNIT_MEASURE,
            anno = as.integer(TIME_PERIOD), salario_reale = as.numeric(OBS_VALUE),
            anno_base = BASE_PER) |>
  arrange(paese, anno) |>
  write_csv(file.path(dir_in, "ocse_salari_medi_reali.csv"))

# Commissione europea (DG ECFIN), indagine sui consumatori: stime quantitative
# dell'inflazione percepita (Q51, ultimi 12 mesi) e attesa (Q61, prossimi 12
# mesi). Pubblicate solo per UE e area euro, trimestrali
zip_ecfin <- tempfile(fileext = ".zip")
download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/",
                     "documents/series/nace2_ecfin_2609/consumer_inflation_nace2.zip"),
              zip_ecfin, mode = "wb")
xlsx <- unzip(zip_ecfin, files = "consumer_inflation_quantitative_estimates.xlsx",
              exdir = tempdir())
map_dfr(c("EU_Q51", "EA_Q51", "EU_Q61", "EA_Q61"), function(foglio) {
  readxl::read_excel(xlsx, sheet = foglio, skip = 1,
                     col_names = c("trimestre", "media", "primo_quartile",
                                   "mediana", "terzo_quartile")) |>
    mutate(area = substr(foglio, 1, 2),
           domanda = if_else(str_ends(foglio, "Q51"), "percepita_12m_passati",
                             "attesa_12m_futuri"))
}) |>
  filter(!is.na(trimestre)) |>
  relocate(area, domanda) |>
  write_csv(file.path(dir_in, "ecfin_inflazione_percepita_quantitativa.csv"))
