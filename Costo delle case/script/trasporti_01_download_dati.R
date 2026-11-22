# ============================================================
# Costo delle case - Trasporti, pendolarismo e lavoro da remoto
# Scarica e pulisce i dati Eurostat e ISTAT usati nella sezione
# "vivere fuori e venire in città a lavorare".
# ============================================================
# Fonti:
#   Eurostat road_eqs_carhab  autovetture per 1.000 abitanti (paesi)
#   Eurostat urb_ctran        Urban Audit, trasporti nelle città
#   Eurostat tran_hv_psmod    ripartizione modale del trasporto passeggeri
#   Eurostat lfso_19plwk28    tempo di pendolarismo (modulo ad hoc LFS 2019)
#   Eurostat lfsa_ehomp       occupati che lavorano da casa
#   ISTAT 609_1_DF_DCCV_URBANENV_11/12/13  Dati ambientali nelle città:
#         mobilità sostenibile, parco veicolare, trasporto pubblico locale
# ============================================================

suppressPackageStartupMessages({
  library(eurostat)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(xml2)
})

base_dir  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Costo delle case"
input_dir <- file.path(base_dir, "input")
dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)

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

paesi_ue <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR",
              "HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO",
              "SE","SI","SK","EU27_2020")

# ---- 1. Autovetture per 1.000 abitanti, paesi europei ----
auto <- get_eurostat("road_eqs_carhab", time_format = "num") %>%
  filter(TIME_PERIOD >= 2015) %>%
  select(geo, anno = TIME_PERIOD, auto_per_1000 = values) %>%
  arrange(geo, anno)
write_csv(auto, file.path(input_dir, "trasporti_auto_per_1000_paesi.csv"))

ultimo_anno <- max(auto$anno[auto$geo == "IT" & !is.na(auto$auto_per_1000)])
auto %>%
  filter(anno == ultimo_anno, geo %in% paesi_ue) %>%
  arrange(desc(auto_per_1000)) %>%
  print(n = 30)

# ---- 2. Urban Audit: città europee a confronto ----
citta <- c(IT001C = "Roma", IT002C = "Milano (greater city)",
           IT003C = "Napoli (greater city)", IT004C = "Torino",
           IT005C = "Palermo", IT006C = "Genova",
           FR001C = "Parigi (greater city)", DE001C = "Berlino",
           DE003C = "Monaco di Baviera", ES001C = "Madrid (greater city)",
           ES002C = "Barcellona (greater city)", UK001C = "Londra",
           AT001C = "Vienna", PL001C = "Varsavia")
indicatori_ua <- c(
  TT1057I = "auto immatricolate per 1.000 abitanti",
  TT1003V = "quota spostamenti casa-lavoro in auto (%)",
  TT1010V = "quota spostamenti casa-lavoro con mezzi pubblici (%)",
  TT1007V = "quota spostamenti casa-lavoro in bicicletta (%)",
  TT1008V = "quota spostamenti casa-lavoro a piedi (%)",
  TT1019V = "durata media del tragitto casa-lavoro (minuti)",
  TT1064V = "pendolari in entrata nella città",
  TT1065V = "pendolari in uscita dalla città",
  TT1079V = "rete ciclabile (km)"
)
ua <- get_eurostat("urb_ctran", time_format = "num") %>%
  filter(cities %in% names(citta), indic_ur %in% names(indicatori_ua),
         !is.na(values)) %>%
  mutate(citta = citta[cities], indicatore = indicatori_ua[indic_ur]) %>%
  select(codice = cities, citta, codice_ind = indic_ur, indicatore,
         anno = TIME_PERIOD, valore = values) %>%
  arrange(codice_ind, codice, anno)
write_csv(ua, file.path(input_dir, "trasporti_urban_audit_citta.csv"))

# ---- 3. Ripartizione modale (quota dell'auto sui passeggeri-km) ----
modale <- get_eurostat("tran_hv_psmod", time_format = "num") %>%
  filter(geo %in% paesi_ue, TIME_PERIOD >= 2010) %>%
  select(geo, anno = TIME_PERIOD, modo = vehicle, quota = values) %>%
  mutate(modo = recode(modo, CAR = "auto", BUS_TOT = "autobus e pullman",
                       TRN = "treno")) %>%
  arrange(geo, anno, modo)
write_csv(modale, file.path(input_dir, "trasporti_ripartizione_modale_ue.csv"))

# ---- 4. Tempo di pendolarismo (LFS 2019) ----
# Quote calcolate sugli occupati 15-74 che si spostano (esclusi 0 minuti
# e non risposte), per grado di urbanizzazione.
pend_raw <- get_eurostat("lfso_19plwk28", time_format = "num") %>%
  filter(geo %in% c(paesi_ue, "EU27_2020"), sex == "T", age == "Y15-74",
         isced11 == "TOTAL")
pend <- pend_raw %>%
  filter(unit == "THS_PER",
         duration %in% c("MN_GE1", "MN_GE30", "MN_GE60", "MN45-59")) %>%
  select(geo, deg_urb, duration, values) %>%
  pivot_wider(names_from = duration, values_from = values) %>%
  mutate(quota_30_e_piu = round(100 * MN_GE30 / MN_GE1, 1),
         quota_45_e_piu = round(100 * (`MN45-59` + MN_GE60) / MN_GE1, 1),
         quota_60_e_piu = round(100 * MN_GE60 / MN_GE1, 1)) %>%
  left_join(pend_raw %>% filter(unit == "MN", duration == "MN_GE1") %>%
              select(geo, deg_urb, minuti_medi = values),
            by = c("geo", "deg_urb")) %>%
  mutate(urbanizzazione = recode(deg_urb, TOTAL = "totale", DEG1 = "città",
                                 DEG2 = "piccole città e sobborghi",
                                 DEG3 = "aree rurali")) %>%
  filter(deg_urb != "NRP") %>%
  select(geo, urbanizzazione, pendolari_migliaia = MN_GE1, minuti_medi,
         quota_30_e_piu, quota_45_e_piu, quota_60_e_piu) %>%
  arrange(urbanizzazione, desc(quota_30_e_piu))
write_csv(pend, file.path(input_dir, "trasporti_tempo_pendolarismo_ue_2019.csv"))

# ---- 5. Lavoro da casa, serie Italia vs UE ----
remoto <- get_eurostat("lfsa_ehomp", time_format = "num") %>%
  filter(sex == "T", age == "Y15-64", wstatus == "EMP",
         geo %in% c(paesi_ue, "EU27_2020"), TIME_PERIOD >= 2015) %>%
  select(geo, anno = TIME_PERIOD, frequenza = frequenc, quota = values) %>%
  mutate(frequenza = recode(frequenza, USU = "abitualmente",
                            SMT = "qualche volta", NVR = "mai")) %>%
  arrange(geo, anno, frequenza)
write_csv(remoto, file.path(input_dir, "trasporti_lavoro_da_casa_ue.csv"))

# ---- 6. ISTAT, Dati ambientali nelle città (capoluoghi metropolitani) ----
capoluoghi <- c("001272" = "Torino", "010025" = "Genova", "015146" = "Milano",
                "027042" = "Venezia", "037006" = "Bologna",
                "048017" = "Firenze", "058091" = "Roma", "063049" = "Napoli",
                "072006" = "Bari", "080063" = "Reggio di Calabria",
                "082053" = "Palermo", "083048" = "Messina",
                "087015" = "Catania", "092009" = "Cagliari",
                "IT_C_M" = "Capoluoghi metropolitani",
                "IT_C_P" = "Capoluoghi di provincia")

# Etichette dei codici DATA_TYPE dalla codelist SDMX (se l'endpoint non
# risponde le descrizioni restano vuote: i codici bastano per i calcoli)
etichette <- tryCatch({
  cl <- read_xml(scarica("https://esploradati.istat.it/SDMXWS/rest/codelist/IT1/CL_TIPO_DATO_AMB"))
  ns <- xml_ns(cl)
  codici <- xml_find_all(cl, "//structure:Code", ns)
  tibble(
    DATA_TYPE = xml_attr(codici, "id"),
    descrizione = vapply(codici, function(n) {
      x <- xml_find_first(n, "common:Name[@xml:lang='it']", ns)
      if (is.na(x)) NA_character_ else xml_text(x)
    }, character(1))
  )
}, error = function(e) tibble(DATA_TYPE = character(), descrizione = character()))

scarica_istat <- function(flusso) {
  url <- paste0("https://esploradati.istat.it/SDMXWS/rest/data/", flusso,
                "/A.", paste(names(capoluoghi), collapse = "+"),
                "./ALL/?startPeriod=2015&format=csv")
  read_csv(scarica(url), col_types = cols(.default = "c")) %>%
    transmute(flusso = flusso, REF_AREA, DATA_TYPE,
              anno = as.integer(TIME_PERIOD), valore = as.numeric(OBS_VALUE))
}
istat <- bind_rows(lapply(paste0("609_1_DF_DCCV_URBANENV_", 11:13), scarica_istat)) %>%
  mutate(sezione = recode(flusso,
                          "609_1_DF_DCCV_URBANENV_11" = "mobilità sostenibile",
                          "609_1_DF_DCCV_URBANENV_12" = "parco veicolare",
                          "609_1_DF_DCCV_URBANENV_13" = "trasporto pubblico locale"),
         comune = capoluoghi[REF_AREA]) %>%
  left_join(etichette, by = "DATA_TYPE") %>%
  select(sezione, codice_comune = REF_AREA, comune, codice = DATA_TYPE,
         descrizione, anno, valore) %>%
  arrange(sezione, codice, comune, anno)
write_csv(istat, file.path(input_dir, "trasporti_istat_mobilita_urbana.csv"))

# ---- 7. Tabella di sintesi per i capoluoghi (ultimo anno, 2024) ----
# Popolazione residente al 1° gennaio 2025 (ISTAT, dataflow 22_389), usata
# per esprimere rete ciclabile e reti su ferro per abitante.
comuni <- setdiff(names(capoluoghi), c("IT_C_M", "IT_C_P"))
pop <- read_csv(scarica(paste0("https://esploradati.istat.it/SDMXWS/rest/data/22_389/A.",
                       paste(comuni, collapse = "+"),
                       ".JAN.9.TOTAL.99/ALL/?format=csv")),
                col_types = cols(.default = "c")) %>%
  filter(TIME_PERIOD == "2025") %>%
  transmute(comune = capoluoghi[REF_AREA], pop_1gen2025 = as.numeric(OBS_VALUE))
sel <- c(CPASCAR1000 = "auto_per_1000_ab", TOFTKM = "posti_km_per_ab",
         TOTDEPC = "passeggeri_tpl_per_ab", EXTMETRKM = "km_metro",
         EXTTRAMKM = "km_tram", SCHETRBKMH = "velocita_bus_kmh",
         SCHETRTKMH = "velocita_tram_kmh", SCHETRMKMH = "velocita_metro_kmh",
         BILANESKM = "km_piste_ciclabili", BILANES100KM = "piste_km_per_100kmq",
         BIKSH10000 = "bici_sharing_per_10000_ab",
         SCOOTSH10000 = "monopattini_sharing_per_10000_ab",
         CARSHA10000 = "auto_sharing_per_10000_ab",
         MICMOB = "veicoli_micromob_sharing", MICMOB10000 = "micromob_per_10000_ab")
sintesi <- istat %>%
  filter(codice %in% names(sel), anno == 2024) %>%
  mutate(var = sel[codice]) %>%
  select(comune, var, valore) %>%
  pivot_wider(names_from = var, values_from = valore) %>%
  left_join(pop, by = "comune") %>%
  mutate(piste_km_per_10000_ab = round(km_piste_ciclabili / pop_1gen2025 * 1e4, 2),
         metro_tram_km_per_100000_ab = round((km_metro + km_tram) / pop_1gen2025 * 1e5, 1)) %>%
  arrange(desc(posti_km_per_ab))
write_csv(sintesi, file.path(input_dir, "trasporti_capoluoghi_sintesi_2024.csv"))
print(as.data.frame(sintesi))
