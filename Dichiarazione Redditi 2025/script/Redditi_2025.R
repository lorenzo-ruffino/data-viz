# ============================================================
# Dichiarazione Redditi 2025 (anno d'imposta 2024)
# Confronto con Dichiarazione 2024 (anno d'imposta 2023)
# Variazioni REALI: deflazione del valore precedente con
# inflazione 2023 -> 2024 = 1.008 (FOI senza tabacchi, ISTAT)
# ============================================================

library(tidyverse)
library(data.table)
library(janitor)

INFLAZIONE <- 1.008

# Determina la cartella output (..../Dichiarazione Redditi 2025/output)
script_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("--file=", "", args[grep("--file=", args)])
  if (length(file_arg) > 0) {
    normalizePath(dirname(file_arg))
  } else if (requireNamespace("rstudioapi", quietly = TRUE) &&
             rstudioapi::isAvailable()) {
    dirname(rstudioapi::getActiveDocumentContext()$path)
  } else {
    getwd()
  }
}, error = function(e) getwd())

output_dir <- normalizePath(file.path(script_dir, "..", "output"), mustWork = FALSE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

url_reg_2025      <- "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/REG_calcolo_irpef_2025.csv?d=1615465800"
url_reg_2024      <- "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/REG_calcolo_irpef_2024.csv?d=1615465800"
url_tipo_2025     <- "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/REG_tipo_reddito_2025.csv?d=1615465800"
url_tipo_2024     <- "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/REG_tipo_reddito_2024.csv?d=1615465800"
url_sesso_2025    <- "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/sesso_calcolo_irpef_2025.csv?d=1615465800"
url_sesso_2024    <- "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/sesso_calcolo_irpef_2024.csv?d=1615465800"
url_eta_2025      <- "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/cla_anno_calcolo_irpef_2025.csv?d=1615465800"
url_eta_2024      <- "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/cla_anno_calcolo_irpef_2024.csv?d=1615465800"

leggi_reg <- function(url) {
  fread(url) %>%
    mutate(across(3:ncol(.), ~ as.numeric(gsub("\\.", "", .)))) %>%
    clean_names()
}

leggi_tipo <- function(url) {
  fread(url) %>%
    mutate(across(3:ncol(.), ~ as.numeric(gsub("\\.", "", .)))) %>%
    clean_names()
}

leggi_sesso <- function(url) {
  as.data.frame(fread(url)) %>%
    mutate(across(3:ncol(.), ~ as.numeric(gsub("\\.", "", .)))) %>%
    clean_names()
}

leggi_eta <- function(url) {
  as.data.frame(fread(url)) %>%
    mutate(across(3:ncol(.), ~ as.numeric(gsub("\\.", "", .)))) %>%
    clean_names()
}

aggiungi_variazione <- function(df) {
  df %>%
    mutate(
      reddito_medio_2024_reale = reddito_medio_2024 * INFLAZIONE,
      variazione_reale_pct = ((reddito_medio_2025 / reddito_medio_2024_reale) - 1) * 100
    ) %>%
    select(-reddito_medio_2024_reale)
}

# ------------------------------------------------------------
# 1) COMPLESSIVI
# ------------------------------------------------------------

calc_complessivo <- function(url) {
  leggi_reg(url) %>%
    summarise(reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = TRUE),
              contribuenti = sum(reddito_imponibile_frequenza, na.rm = TRUE)) %>%
    mutate(reddito_medio = reddito / contribuenti) %>%
    pull(reddito_medio)
}

complessivo <- tibble(
  categoria = "Totale",
  reddito_medio_2025 = calc_complessivo(url_reg_2025),
  reddito_medio_2024 = calc_complessivo(url_reg_2024)
) %>%
  aggiungi_variazione()

write.csv(complessivo, file.path(output_dir, "complessivo.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 2) PER TIPOLOGIA (dipendenti / pensionati / autonomi)
# ------------------------------------------------------------

calc_tipologia <- function(url) {
  leggi_tipo(url) %>%
    summarise(across(3:ncol(.), ~ sum(., na.rm = TRUE))) %>%
    summarise(
      Dipendenti = reddito_da_lavoro_dipendente_e_assimilati_ammontare_in_euro /
                   reddito_da_lavoro_dipendente_e_assimilati_frequenza,
      Pensionati = reddito_da_pensione_ammontare_in_euro /
                   reddito_da_pensione_frequenza,
      Autonomi   = reddito_da_lavoro_autonomo_ammontare_in_euro /
                   reddito_da_lavoro_autonomo_frequenza
    ) %>%
    pivot_longer(everything(), names_to = "categoria", values_to = "reddito_medio")
}

tipologia <- calc_tipologia(url_tipo_2025) %>%
  rename(reddito_medio_2025 = reddito_medio) %>%
  inner_join(
    calc_tipologia(url_tipo_2024) %>% rename(reddito_medio_2024 = reddito_medio),
    by = "categoria"
  ) %>%
  aggiungi_variazione()

write.csv(tipologia, file.path(output_dir, "tipologia.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 3) PER GENERE
# ------------------------------------------------------------

calc_genere <- function(url) {
  leggi_sesso(url) %>%
    group_by(sesso) %>%
    summarise(reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = TRUE),
              contribuenti = sum(reddito_imponibile_frequenza, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(reddito_medio = reddito / contribuenti) %>%
    select(categoria = sesso, reddito_medio)
}

genere <- calc_genere(url_sesso_2025) %>%
  rename(reddito_medio_2025 = reddito_medio) %>%
  inner_join(
    calc_genere(url_sesso_2024) %>% rename(reddito_medio_2024 = reddito_medio),
    by = "categoria"
  ) %>%
  aggiungi_variazione()

write.csv(genere, file.path(output_dir, "genere.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 4) PER ETA'
# ------------------------------------------------------------

calc_eta <- function(url) {
  leggi_eta(url) %>%
    mutate(classi_di_eta = ifelse(classi_di_eta == "0 - 14", "15 - 24", classi_di_eta)) %>%
    group_by(classi_di_eta) %>%
    summarise(reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = TRUE),
              contribuenti = sum(reddito_imponibile_frequenza, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(reddito_medio = reddito / contribuenti) %>%
    select(categoria = classi_di_eta, reddito_medio)
}

eta <- calc_eta(url_eta_2025) %>%
  rename(reddito_medio_2025 = reddito_medio) %>%
  inner_join(
    calc_eta(url_eta_2024) %>% rename(reddito_medio_2024 = reddito_medio),
    by = "categoria"
  ) %>%
  aggiungi_variazione()

write.csv(eta, file.path(output_dir, "eta.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 5) PER MACROAREA
# ------------------------------------------------------------

assegna_macroarea <- function(df) {
  df %>%
    mutate(macro_area = case_when(
      toupper(regione) %in% c("LOMBARDIA", "PIEMONTE", "VALLE D'AOSTA", "LIGURIA") ~ "NORD-OVEST",
      toupper(regione) %in% c("VENETO","TRENTINO ALTO ADIGE", "EMILIA ROMAGNA",
                              "F.V. GIULIA", "FRIULI VENEZIA GIULIA",
                              "TRENTINO ALTO ADIGE(P.A.BOLZANO)",
                              "TRENTINO ALTO ADIGE(P.A.TRENTO)") ~ "NORD-EST",
      toupper(regione) %in% c("TOSCANA", "LAZIO", "MARCHE", "UMBRIA") ~ "CENTRO",
      toupper(regione) %in% c("SARDEGNA", "ABRUZZO", "BASILICATA", "SICILIA",
                              "PUGLIA", "MOLISE", "CALABRIA", "CAMPANIA") ~ "SUD",
      TRUE ~ toupper(regione)
    ))
}

calc_macroarea <- function(url) {
  leggi_reg(url) %>%
    assegna_macroarea() %>%
    group_by(macro_area) %>%
    summarise(reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = TRUE),
              contribuenti = sum(reddito_imponibile_frequenza, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(reddito_medio = reddito / contribuenti) %>%
    select(categoria = macro_area, reddito_medio)
}

macroarea <- calc_macroarea(url_reg_2025) %>%
  rename(reddito_medio_2025 = reddito_medio) %>%
  inner_join(
    calc_macroarea(url_reg_2024) %>% rename(reddito_medio_2024 = reddito_medio),
    by = "categoria"
  ) %>%
  aggiungi_variazione()

write.csv(macroarea, file.path(output_dir, "macroarea.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 6) PER REGIONE
# ------------------------------------------------------------

calc_regione <- function(url) {
  leggi_reg(url) %>%
    group_by(regione) %>%
    summarise(reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = TRUE),
              contribuenti = sum(reddito_imponibile_frequenza, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(reddito_medio = reddito / contribuenti) %>%
    select(categoria = regione, reddito_medio)
}

regione <- calc_regione(url_reg_2025) %>%
  rename(reddito_medio_2025 = reddito_medio) %>%
  inner_join(
    calc_regione(url_reg_2024) %>% rename(reddito_medio_2024 = reddito_medio),
    by = "categoria"
  ) %>%
  aggiungi_variazione()

write.csv(regione, file.path(output_dir, "regione.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 7) PER FASCIA DI REDDITO (classificazione esistente)
# ------------------------------------------------------------

mappa_fasce <- function(df) {
  df %>%
    mutate(categoria = case_when(
      classi_di_reddito_complessivo_in_euro == 'minore di -1.000' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da -1.000 a 0' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'zero' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 0 a 1.000' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 1.000 a 1.500' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 1.500 a 2.000' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 2.000 a 2.500' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 2.500 a 3.000' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 3.000 a 3.500' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 3.500 a 4.000' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 4.000 a 5.000' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 5.000 a 6.000' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 6.000 a 7.500' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 7.500 a 10.000' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 10.000 a 12.000' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 12.000 a 15.000' ~ '0-15.000',
      classi_di_reddito_complessivo_in_euro == 'da 15.000 a 20.000' ~ '15-26.000',
      classi_di_reddito_complessivo_in_euro == 'da 20.000 a 26.000' ~ '15-26.000',
      classi_di_reddito_complessivo_in_euro == 'da 26.000 a 29.000' ~ '26-35.000',
      classi_di_reddito_complessivo_in_euro == 'da 29.000 a 35.000' ~ '26-35.000',
      classi_di_reddito_complessivo_in_euro == 'da 35.000 a 40.000' ~ '35-55.000',
      classi_di_reddito_complessivo_in_euro == 'da 40.000 a 50.000' ~ '35-55.000',
      classi_di_reddito_complessivo_in_euro == 'da 50.000 a 55.000' ~ '35-55.000',
      classi_di_reddito_complessivo_in_euro == 'da 55.000 a 60.000' ~ '55-75.000',
      classi_di_reddito_complessivo_in_euro == 'da 60.000 a 70.000' ~ '55-75.000',
      classi_di_reddito_complessivo_in_euro == 'da 70.000 a 75.000' ~ '55-75.000',
      classi_di_reddito_complessivo_in_euro == 'da 75.000 a 80.000' ~ '75-120.000',
      classi_di_reddito_complessivo_in_euro == 'da 80.000 a 90.000' ~ '75-120.000',
      classi_di_reddito_complessivo_in_euro == 'da 90.000 a 100.000' ~ '75-120.000',
      classi_di_reddito_complessivo_in_euro == 'da 100.000 a 120.000' ~ '>120.000',
      classi_di_reddito_complessivo_in_euro == 'da 120.000 a 150.000' ~ '>120.000',
      classi_di_reddito_complessivo_in_euro == 'da 150.000 a 200.000' ~ '>120.000',
      classi_di_reddito_complessivo_in_euro == 'da 200.000 a 300.000' ~ '>120.000',
      classi_di_reddito_complessivo_in_euro == 'oltre 300.000' ~ '>120.000'
    ))
}

calc_fascia <- function(url) {
  leggi_reg(url) %>%
    mappa_fasce() %>%
    group_by(categoria) %>%
    summarise(reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = TRUE),
              contribuenti = sum(reddito_imponibile_frequenza, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(reddito_medio = reddito / contribuenti) %>%
    select(categoria, reddito_medio)
}

ordine_fasce <- c("0-15.000", "15-26.000", "26-35.000", "35-55.000",
                  "55-75.000", "75-120.000", ">120.000")

fascia <- calc_fascia(url_reg_2025) %>%
  rename(reddito_medio_2025 = reddito_medio) %>%
  inner_join(
    calc_fascia(url_reg_2024) %>% rename(reddito_medio_2024 = reddito_medio),
    by = "categoria"
  ) %>%
  aggiungi_variazione() %>%
  mutate(categoria = factor(categoria, levels = ordine_fasce)) %>%
  arrange(categoria)

write.csv(fascia, file.path(output_dir, "fascia_reddito.csv"), row.names = FALSE)

# Distribuzione: numero contribuenti, % contribuenti, % IRPEF, imposta media
distribuzione_fascia <- leggi_reg(url_reg_2025) %>%
  mappa_fasce() %>%
  group_by(categoria) %>%
  summarise(
    contribuenti = sum(reddito_imponibile_frequenza, na.rm = TRUE),
    reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = TRUE),
    imposta = sum(imposta_netta_ammontare_in_euro, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_contribuenti = round(100 * contribuenti / sum(contribuenti), 2),
    pct_irpef = round(100 * imposta / sum(imposta), 2),
    aliquota_effettiva = round(100 * imposta / reddito, 2),
    imposta_media = round(imposta / contribuenti, 0),
    reddito_medio = round(reddito / contribuenti, 0),
    categoria = factor(categoria, levels = ordine_fasce)
  ) %>%
  arrange(categoria) %>%
  select(categoria, contribuenti, pct_contribuenti, pct_irpef,
         reddito_medio, imposta_media, aliquota_effettiva)

write.csv(distribuzione_fascia, file.path(output_dir, "distribuzione_fascia.csv"), row.names = FALSE)

# Distribuzione dettagliata per fasce di 10 mila (compatibile con grafico storico)
ordine_fasce_dettaglio <- c("0-10 mila","10-20 mila","20-30 mila","30-40 mila",
                            "40-50 mila","50-60 mila","60-70 mila","70-80 mila",
                            "80-90 mila","90-100 mila","100-110 mila",">120 mila")

distribuzione_dettaglio <- leggi_reg(url_reg_2025) %>%
  mutate(categoria = case_when(
    classi_di_reddito_complessivo_in_euro %in% c('minore di -1.000','da -1.000 a 0','zero','da 0 a 1.000','da 1.000 a 1.500','da 1.500 a 2.000','da 2.000 a 2.500','da 2.500 a 3.000','da 3.000 a 3.500','da 3.500 a 4.000','da 4.000 a 5.000','da 5.000 a 6.000','da 6.000 a 7.500','da 7.500 a 10.000') ~ '0-10 mila',
    classi_di_reddito_complessivo_in_euro %in% c('da 10.000 a 12.000','da 12.000 a 15.000','da 15.000 a 20.000') ~ '10-20 mila',
    classi_di_reddito_complessivo_in_euro %in% c('da 20.000 a 26.000','da 26.000 a 29.000') ~ '20-30 mila',
    classi_di_reddito_complessivo_in_euro %in% c('da 29.000 a 35.000','da 35.000 a 40.000') ~ '30-40 mila',
    classi_di_reddito_complessivo_in_euro == 'da 40.000 a 50.000' ~ '40-50 mila',
    classi_di_reddito_complessivo_in_euro %in% c('da 50.000 a 55.000','da 55.000 a 60.000') ~ '50-60 mila',
    classi_di_reddito_complessivo_in_euro == 'da 60.000 a 70.000' ~ '60-70 mila',
    classi_di_reddito_complessivo_in_euro %in% c('da 70.000 a 75.000','da 75.000 a 80.000') ~ '70-80 mila',
    classi_di_reddito_complessivo_in_euro == 'da 80.000 a 90.000' ~ '80-90 mila',
    classi_di_reddito_complessivo_in_euro == 'da 90.000 a 100.000' ~ '90-100 mila',
    classi_di_reddito_complessivo_in_euro == 'da 100.000 a 120.000' ~ '100-110 mila',
    classi_di_reddito_complessivo_in_euro %in% c('da 120.000 a 150.000','da 150.000 a 200.000','da 200.000 a 300.000','oltre 300.000') ~ '>120 mila'
  )) %>%
  group_by(categoria) %>%
  summarise(
    contribuenti = sum(reddito_imponibile_frequenza, na.rm = TRUE),
    reddito = sum(reddito_imponibile_ammontare_in_euro, na.rm = TRUE),
    imposta = sum(imposta_netta_ammontare_in_euro, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_contribuenti = round(100 * contribuenti / sum(contribuenti), 2),
    pct_irpef = round(100 * imposta / sum(imposta), 2),
    peso_imposta_su_reddito = round(100 * imposta / reddito, 2),
    imposta_media = round(imposta / contribuenti, 0),
    reddito_medio = round(reddito / contribuenti, 0),
    categoria = factor(categoria, levels = ordine_fasce_dettaglio)
  ) %>%
  arrange(categoria) %>%
  select(categoria, contribuenti, pct_contribuenti, pct_irpef,
         reddito_medio, imposta_media, peso_imposta_su_reddito)

write.csv(distribuzione_dettaglio, file.path(output_dir, "distribuzione_fascia_dettaglio.csv"), row.names = FALSE)

# Totali aggregati (numero contribuenti, redditi, imposte)
totali <- leggi_reg(url_reg_2025) %>%
  summarise(
    numero_contribuenti = sum(numero_contribuenti, na.rm = TRUE),
    reddito_complessivo = sum(reddito_complessivo_ammontare_in_euro, na.rm = TRUE),
    contribuenti_imponibile = sum(reddito_imponibile_frequenza, na.rm = TRUE),
    reddito_imponibile = sum(reddito_imponibile_ammontare_in_euro, na.rm = TRUE),
    imposta_lorda = sum(imposta_lorda_ammontare_in_euro, na.rm = TRUE),
    imposta_netta = sum(imposta_netta_ammontare_in_euro, na.rm = TRUE),
    irpef_a_credito = sum(irpef_a_credito_ammontare_in_euro, na.rm = TRUE),
    irpef_a_debito = sum(irpef_a_debito_ammontare_in_euro, na.rm = TRUE)
  )

write.csv(totali, file.path(output_dir, "totali.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 8) SERIE STORICA NOMINALE / REALE (base 2024)
# ------------------------------------------------------------
# Serie storica fino al 2023 (base reale = 2023). Aggiungiamo il
# 2024 e ribasiamo i valori reali al 2024 moltiplicando per 1.014.

serie_storica <- tibble(
  anno = 2001:2023,
  nominale = c(15249.61, 15491.73, 15164.38, 15422.89, 16263.42, 17139.71,
               18345.10, 18761.78, 18903.47, 19105.24, 19177.84, 19309.74,
               19572.65, 19720.12, 20016.53, 20213.73, 20354.55, 20800.08,
               20992.59, 20851.52, 21776.59, 22806.31, 23950.00),
  reale_base_2023 = c(23125.66022, 22934.89949, 21911.31546, 21850.72143,
                      22655.38953, 23409.02593, 24631.30705, 24402.38363,
                      24404.81770, 24288.05219, 23739.38355, 23202.29522,
                      23254.44839, 23385.94682, 23759.62111, 24017.71523,
                      23921.63450, 24181.87557, 24286.74886, 24194.28567,
                      24806.92162, 24041.82058, 23950.00000)
) %>%
  mutate(reale = reale_base_2023 * INFLAZIONE) %>%
  select(anno, nominale, reale) %>%
  bind_rows(tibble(
    anno = 2024,
    nominale = round(complessivo$reddito_medio_2025, 2),
    reale = round(complessivo$reddito_medio_2025, 2)
  )) %>%
  mutate(reale = round(reale, 2))

write.csv(serie_storica, file.path(output_dir, "serie_storica.csv"), row.names = FALSE)
