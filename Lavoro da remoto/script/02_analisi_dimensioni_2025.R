# ============================================================
# Lavoro da remoto - Profilo strutturale degli occupati italiani
# Media dei 4 trimestri 2025
# ============================================================
# Fonte: microdati RCFL 2025 Q1-Q4
# Variabile: QC52 ("ha lavorato da casa? compreso telelavoro e
#                   smart work")
# Peso: COEF_CCP / 10
# Universo: occupati (COND3 == 1) con risposta valida (1,2,3,4)
#
# Definizioni:
#   qualunque = ha lavorato da casa almeno qualche volta
#               (QC52 in 1, 2, 4)
#   frequente = ha lavorato da casa per la maggior parte del
#               tempo o in modo esclusivo (QC52 in 1, 4)
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

base_dir   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Lavoro da remoto"
input_dir  <- file.path(base_dir, "input")
output_dir <- file.path(base_dir, "output")
dir.create(file.path(output_dir, "2025_dimensioni"), showWarnings = FALSE, recursive = TRUE)
out_2025 <- file.path(output_dir, "2025_dimensioni")

# ---- 1. Lettura 4 trimestri 2025 ----
files_2025 <- c(
  "RCFL_Microdati_2025_Primo_trimestre.txt",
  "RCFL_Microdati_2025_Secondo_trimestre.txt",
  "RCFL_Microdati_2025_Terzo_trimestre.txt",
  "RCFL_Microdati_2025_Quarto_trimestre.txt"
)

needed_vars <- c(
  "COND3", "COEF_CCP", "QC52",
  "SESSO", "CLETAS", "RIP5", "CITTAD",
  "HATLEV3MOD",
  "DIPAUT", "POSPRO",
  "CAT12", "PROF1",
  "DETIND", "PIEPAR"
)

read_q <- function(fname) {
  cat("  ", fname, "\n")
  read_delim(file.path(input_dir, fname),
             delim = "\t",
             col_select = all_of(needed_vars),
             col_types = cols(.default = "c"),
             show_col_types = FALSE,
             progress = FALSE)
}

cat("Lettura 4 trimestri 2025...\n")
raw <- map_df(files_2025, read_q, .id = "trimestre")

# ---- 2. Pulizia e filtro occupati con risposta valida ----
dati <- raw %>%
  mutate(
    peso  = as.numeric(str_trim(COEF_CCP)) / 10,
    cond3 = suppressWarnings(as.integer(str_trim(COND3))),
    qc52  = suppressWarnings(as.integer(str_trim(QC52))),
    sesso = suppressWarnings(as.integer(str_trim(SESSO))),
    cletas = suppressWarnings(as.integer(str_trim(CLETAS))),
    rip5  = suppressWarnings(as.integer(str_trim(RIP5))),
    cittad = suppressWarnings(as.integer(str_trim(CITTAD))),
    hatlev3 = suppressWarnings(as.integer(str_trim(HATLEV3MOD))),
    dipaut = suppressWarnings(as.integer(str_trim(DIPAUT))),
    pospro = suppressWarnings(as.integer(str_trim(POSPRO))),
    cat12 = suppressWarnings(as.integer(str_trim(CAT12))),
    prof1 = suppressWarnings(as.integer(str_trim(PROF1))),
    detind = suppressWarnings(as.integer(str_trim(DETIND))),
    piepar = suppressWarnings(as.integer(str_trim(PIEPAR)))
  ) %>%
  filter(cond3 == 1L, !is.na(peso), peso > 0, qc52 %in% c(1L, 2L, 3L, 4L))

cat(sprintf("\nOsservazioni occupati con QC52 valida: %d\n", nrow(dati)))
cat(sprintf("Stima occupati media 2025 (migliaia): %.0f\n",
            sum(dati$peso) / 4 / 1000))

# ---- 3. Funzione di analisi per dimensione ----
analizza <- function(df, var_name, labels = NULL, dim_label = var_name,
                     ord_by = "perc_qualsiasi", drop_zero = TRUE) {
  out <- df %>%
    filter(!is.na(.data[[var_name]])) %>%
    group_by(modalita = .data[[var_name]]) %>%
    summarise(
      base_persone   = sum(peso),
      n_obs          = n(),
      qualsiasi_peso = sum(peso[qc52 %in% c(1L, 2L, 4L)]),
      frequente_peso = sum(peso[qc52 %in% c(1L, 4L)]),
      .groups = "drop"
    ) %>%
    mutate(
      perc_qualsiasi = qualsiasi_peso / base_persone * 100,
      perc_frequente = frequente_peso / base_persone * 100,
      base_migliaia_media = round(base_persone / 4 / 1000, 0),
      perc_quota_occupati = base_persone / sum(base_persone) * 100
    )

  if (!is.null(labels)) {
    out$etichetta <- labels[as.character(out$modalita)]
    out <- out %>% select(modalita, etichetta, everything())
  }

  out <- out %>%
    arrange(desc(.data[[ord_by]]))

  out %>%
    mutate(
      perc_qualsiasi = round(perc_qualsiasi, 1),
      perc_frequente = round(perc_frequente, 1),
      perc_quota_occupati = round(perc_quota_occupati, 1)
    )
}

# ---- 4. Etichette ----
lab_sesso <- c("1"="Uomini", "2"="Donne")
lab_rip5  <- c("1"="Nord-Ovest", "2"="Nord-Est", "3"="Centro",
               "4"="Sud", "5"="Isole")
lab_cittad <- c("1"="Italiana", "2"="Straniera UE", "3"="Straniera extra-UE")
lab_hatlev3 <- c("1"="Fino a sec. inferiore (ISCED 0-2)",
                 "2"="Sec. superiore (ISCED 3-4)",
                 "3"="Terziario (ISCED 5-8)")
lab_dipaut <- c("1"="Dipendente", "2"="Collaboratore", "3"="Autonomo")
lab_detind <- c("1"="Tempo determinato", "2"="Tempo indeterminato")
lab_piepar <- c("1"="Tempo pieno", "2"="Tempo parziale")
lab_pospro <- c("1"="Dirigente", "2"="Quadro", "3"="Impiegato",
                "4"="Operaio", "7"="Imprenditore",
                "8"="Libero professionista", "9"="Lavoratore in proprio",
                "11"="Coadiuvante familiare",
                "12"="Co.co.co.", "13"="Prestaz. d'opera occasionale")
lab_cat12 <- c(
  "1"  = "Agricoltura, silvicoltura, pesca",
  "2"  = "Industria in senso stretto",
  "3"  = "Costruzioni",
  "4"  = "Commercio",
  "5"  = "Alberghi e ristoranti",
  "6"  = "Trasporto e magazzinaggio",
  "7"  = "Info e comunicazione",
  "8"  = "Finanza e assicurazioni",
  "9"  = "Immobiliari e servizi alle imprese",
  "10" = "P.A. e difesa",
  "11" = "Istruzione, sanita', servizi sociali",
  "12" = "Altri servizi collettivi e personali"
)
lab_prof1 <- c(
  "1" = "Legislatori, dirigenti, imprenditori",
  "2" = "Professioni intellettuali e scientifiche",
  "3" = "Professioni tecniche",
  "4" = "Impiegati esecutivi d'ufficio",
  "5" = "Qualificate nel commercio e servizi",
  "6" = "Artigiani, operai specializzati, agric.",
  "7" = "Conduttori impianti, macchinari, veicoli",
  "8" = "Professioni non qualificate",
  "9" = "Forze armate"
)
lab_cletas <- c(
  "5"="15-19","6"="20-24","7"="25-29","8"="30-34","9"="35-39",
  "10"="40-44","11"="45-49","12"="50-54","13"="55-59","14"="60-64",
  "15"="65-69","16"="70-74","17"="75+"
)

# Età aggregata (5 fasce)
dati <- dati %>%
  mutate(eta_5fasce = case_when(
    cletas %in% 5:6   ~ 1L,  # 15-24
    cletas %in% 7:8   ~ 2L,  # 25-34
    cletas %in% 9:10  ~ 3L,  # 35-44
    cletas %in% 11:12 ~ 4L,  # 45-54
    cletas %in% 13:14 ~ 5L,  # 55-64
    cletas >= 15      ~ 6L,  # 65+
    TRUE ~ NA_integer_
  ))
lab_eta5 <- c("1"="15-24", "2"="25-34", "3"="35-44",
              "4"="45-54", "5"="55-64", "6"="65+")

# Macro-area
dati <- dati %>%
  mutate(macro = case_when(
    rip5 %in% 1:2 ~ 1L,   # Nord
    rip5 == 3     ~ 2L,   # Centro
    rip5 %in% 4:5 ~ 3L,   # Mezzogiorno
    TRUE ~ NA_integer_
  ))
lab_macro <- c("1"="Nord", "2"="Centro", "3"="Mezzogiorno")

# ---- 5. TOTALE ITALIA ----
totale <- dati %>%
  summarise(
    n_obs = n(),
    base_persone = sum(peso),
    qualsiasi = sum(peso[qc52 %in% c(1L, 2L, 4L)]) / sum(peso) * 100,
    frequente = sum(peso[qc52 %in% c(1L, 4L)]) / sum(peso) * 100,
    qc52_1_pcent_tempo  = sum(peso[qc52 == 1L]) / sum(peso) * 100,
    qc52_2_qualche      = sum(peso[qc52 == 2L]) / sum(peso) * 100,
    qc52_3_no           = sum(peso[qc52 == 3L]) / sum(peso) * 100,
    qc52_4_esclusivo    = sum(peso[qc52 == 4L]) / sum(peso) * 100
  )

cat("\n========================================================\n")
cat("TOTALE ITALIA - Media 2025\n")
cat("========================================================\n")
cat(sprintf("  Occupati base (migliaia, media 4 trim): %.0f\n",
            totale$base_persone / 4 / 1000))
cat(sprintf("  %% almeno qualche volta da casa:  %.2f%%\n", totale$qualsiasi))
cat(sprintf("  %% per >= meta' del tempo o escl.: %.2f%%\n", totale$frequente))
cat("\n  Composizione QC52:\n")
cat(sprintf("    1 (>= meta' tempo):  %.2f%%\n", totale$qc52_1_pcent_tempo))
cat(sprintf("    2 (qualche volta):   %.2f%%\n", totale$qc52_2_qualche))
cat(sprintf("    3 (mai):             %.2f%%\n", totale$qc52_3_no))
cat(sprintf("    4 (esclusivo):       %.2f%%\n", totale$qc52_4_esclusivo))

write_csv(totale, file.path(out_2025, "00_totale_italia.csv"))

# ---- 6. Analisi per dimensione ----
dimensioni <- list(
  list(var = "sesso",     lab = lab_sesso,   nome = "Genere",                      ord = "perc_qualsiasi"),
  list(var = "eta_5fasce", lab = lab_eta5,   nome = "Eta'",                        ord = "modalita"),
  list(var = "macro",     lab = lab_macro,   nome = "Macroarea",                   ord = "perc_qualsiasi"),
  list(var = "rip5",      lab = lab_rip5,    nome = "Ripartizione",                ord = "perc_qualsiasi"),
  list(var = "cittad",    lab = lab_cittad,  nome = "Cittadinanza",                ord = "perc_qualsiasi"),
  list(var = "hatlev3",   lab = lab_hatlev3, nome = "Titolo di studio",            ord = "perc_qualsiasi"),
  list(var = "cat12",     lab = lab_cat12,   nome = "Settore (ATECO 12)",          ord = "perc_qualsiasi"),
  list(var = "prof1",     lab = lab_prof1,   nome = "Professione (ISCO 1-digit)",  ord = "perc_qualsiasi"),
  list(var = "dipaut",    lab = lab_dipaut,  nome = "Pos. nella professione",      ord = "perc_qualsiasi"),
  list(var = "pospro",    lab = lab_pospro,  nome = "Pos. professione (dettaglio)", ord = "perc_qualsiasi"),
  list(var = "detind",    lab = lab_detind,  nome = "Tipo contratto (solo dip.)",  ord = "perc_qualsiasi"),
  list(var = "piepar",    lab = lab_piepar,  nome = "Tempo pieno/parziale",        ord = "perc_qualsiasi")
)

risultati <- list()
for (d in dimensioni) {
  res <- analizza(dati, d$var, d$lab, dim_label = d$nome, ord_by = d$ord)
  if (d$ord == "modalita") res <- res %>% arrange(modalita)
  risultati[[d$nome]] <- res

  cat(sprintf("\n--- %s ---\n", d$nome))
  print(as.data.frame(res %>% select(any_of(c("modalita","etichetta",
                                              "base_migliaia_media",
                                              "perc_quota_occupati",
                                              "perc_qualsiasi",
                                              "perc_frequente")))),
        row.names = FALSE)

  fname <- sprintf("%02d_%s.csv",
                   which(sapply(dimensioni, function(x) x$nome) == d$nome),
                   gsub("[^a-zA-Z0-9]+", "_", tolower(d$nome)))
  write_csv(res, file.path(out_2025, fname))
}

# ---- 7. Cross: settore x posizione (dip vs aut) ----
cat("\n========================================================\n")
cat("CROSS: settore (CAT5) x dipendenti vs autonomi\n")
cat("========================================================\n")
dati5 <- dati %>%
  mutate(cat5 = case_when(
    cat12 == 1 ~ 1L,
    cat12 == 2 ~ 2L,
    cat12 == 3 ~ 3L,
    cat12 == 4 ~ 4L,
    cat12 %in% 5:12 ~ 5L,
    TRUE ~ NA_integer_
  ),
  dip_aut = if_else(dipaut == 1L, "Dipendenti", "Autonomi/Coll."))

lab_cat5 <- c("1"="Agricoltura","2"="Industria","3"="Costruzioni",
              "4"="Commercio","5"="Altri servizi")

cross_set_pos <- dati5 %>%
  filter(!is.na(cat5), !is.na(dip_aut)) %>%
  group_by(cat5, dip_aut) %>%
  summarise(
    base_migliaia = sum(peso) / 4 / 1000,
    perc_qualsiasi = sum(peso[qc52 %in% c(1L,2L,4L)]) / sum(peso) * 100,
    perc_frequente = sum(peso[qc52 %in% c(1L,4L)]) / sum(peso) * 100,
    .groups = "drop"
  ) %>%
  mutate(settore = lab_cat5[as.character(cat5)],
         perc_qualsiasi = round(perc_qualsiasi, 1),
         perc_frequente = round(perc_frequente, 1),
         base_migliaia  = round(base_migliaia, 0)) %>%
  select(settore, dip_aut, base_migliaia, perc_qualsiasi, perc_frequente) %>%
  arrange(settore, dip_aut)

print(as.data.frame(cross_set_pos), row.names = FALSE)
write_csv(cross_set_pos, file.path(out_2025, "20_cross_settore_x_posizione.csv"))

# ---- 8. Cross: titolo di studio x settore ----
cat("\n========================================================\n")
cat("CROSS: titolo di studio x macro-settore servizi\n")
cat("========================================================\n")
cross_tit_set <- dati %>%
  filter(!is.na(hatlev3), !is.na(cat12)) %>%
  group_by(hatlev3, cat12) %>%
  summarise(
    base_migliaia = sum(peso) / 4 / 1000,
    perc_qualsiasi = sum(peso[qc52 %in% c(1L,2L,4L)]) / sum(peso) * 100,
    .groups = "drop"
  ) %>%
  mutate(titolo = lab_hatlev3[as.character(hatlev3)],
         settore = lab_cat12[as.character(cat12)],
         perc_qualsiasi = round(perc_qualsiasi, 1),
         base_migliaia  = round(base_migliaia, 0)) %>%
  select(titolo, settore, base_migliaia, perc_qualsiasi) %>%
  arrange(titolo, desc(perc_qualsiasi))
write_csv(cross_tit_set, file.path(out_2025, "21_cross_titolo_x_settore.csv"))

# ---- 9. Solo terziario per settore ----
cat("\n--- Sottocampione: solo laureati per settore (top 8) ---\n")
laureati <- dati %>%
  filter(hatlev3 == 3L, !is.na(cat12)) %>%
  group_by(cat12) %>%
  summarise(
    base_migliaia = sum(peso) / 4 / 1000,
    perc_qualsiasi = sum(peso[qc52 %in% c(1L,2L,4L)]) / sum(peso) * 100,
    perc_frequente = sum(peso[qc52 %in% c(1L,4L)]) / sum(peso) * 100,
    .groups = "drop"
  ) %>%
  mutate(settore = lab_cat12[as.character(cat12)],
         perc_qualsiasi = round(perc_qualsiasi, 1),
         perc_frequente = round(perc_frequente, 1),
         base_migliaia = round(base_migliaia, 0)) %>%
  arrange(desc(perc_qualsiasi))
print(as.data.frame(laureati %>% select(settore, base_migliaia, perc_qualsiasi, perc_frequente) %>% head(12)),
      row.names = FALSE)
write_csv(laureati, file.path(out_2025, "22_laureati_per_settore.csv"))

cat(sprintf("\n\nCSV salvati in: %s/\n", out_2025))
