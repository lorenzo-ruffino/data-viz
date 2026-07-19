# ============================================================
# Lavoro da remoto - Approfondimenti
#  (1) Eta' in fasce decennali con correzione per composizione
#      settore x posizione (standardizzazione diretta)
#  (2) Confronto settoriale 2021 (picco) vs 2025 (oggi)
# ============================================================
# Fonte: microdati RCFL 2021 Q1-Q4 e 2025 Q1-Q4
# Variabile: QC52 (lavoro da casa, incluso smart work)
# Pesi: COEF_CCP/10  (eccetto 2021 Q1 dove il file e' gia' in unita')
# Universo: occupati con risposta valida (1,2,3,4)
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

base_dir   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Lavoro da remoto"
input_dir  <- file.path(base_dir, "input")
output_dir <- file.path(base_dir, "output")
out_dir    <- file.path(output_dir, "2025_dimensioni")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

needed_vars <- c("COND3", "COEF_CCP", "QC52", "SESSO", "CLETAS",
                 "RIP5", "DIPAUT", "POSPRO", "CAT12", "CAT5",
                 "PROF1", "HATLEV3MOD")

# ---- Reader ----
read_clean <- function(fname, weight_divisor) {
  cat("  ", fname, " (peso/", weight_divisor, ")\n", sep = "")
  read_delim(file.path(input_dir, fname),
             delim = "\t",
             col_select = all_of(needed_vars),
             col_types = cols(.default = "c"),
             show_col_types = FALSE,
             progress = FALSE) %>%
    mutate(
      peso  = as.numeric(str_trim(COEF_CCP)) / weight_divisor,
      cond3 = suppressWarnings(as.integer(str_trim(COND3))),
      qc52  = suppressWarnings(as.integer(str_trim(QC52))),
      sesso = suppressWarnings(as.integer(str_trim(SESSO))),
      cletas = suppressWarnings(as.integer(str_trim(CLETAS))),
      rip5  = suppressWarnings(as.integer(str_trim(RIP5))),
      dipaut = suppressWarnings(as.integer(str_trim(DIPAUT))),
      pospro = suppressWarnings(as.integer(str_trim(POSPRO))),
      cat12 = suppressWarnings(as.integer(str_trim(CAT12))),
      cat5  = suppressWarnings(as.integer(str_trim(CAT5))),
      prof1 = suppressWarnings(as.integer(str_trim(PROF1))),
      hatlev3 = suppressWarnings(as.integer(str_trim(HATLEV3MOD)))
    ) %>%
    filter(cond3 == 1L, !is.na(peso), peso > 0, qc52 %in% c(1L, 2L, 3L, 4L))
}

# ---- Eta' in fasce decennali ----
add_eta_decennale <- function(df) {
  df %>%
    mutate(eta = case_when(
      cletas == 5            ~ 1L,  # <20
      cletas %in% 6:7        ~ 2L,  # 20-29
      cletas %in% 8:9        ~ 3L,  # 30-39
      cletas %in% 10:11      ~ 4L,  # 40-49
      cletas %in% 12:13      ~ 5L,  # 50-59
      cletas >= 14           ~ 6L,  # 60+
      TRUE ~ NA_integer_
    ))
}
lab_eta <- c("1"="<20", "2"="20-29", "3"="30-39",
             "4"="40-49", "5"="50-59", "6"="60+")

# ---- Lettura 2025 ----
cat("\nLettura 2025 ...\n")
files_2025 <- c("RCFL_Microdati_2025_Primo_trimestre.txt",
                "RCFL_Microdati_2025_Secondo_trimestre.txt",
                "RCFL_Microdati_2025_Terzo_trimestre.txt",
                "RCFL_Microdati_2025_Quarto_trimestre.txt")
d25 <- map_df(files_2025, read_clean, weight_divisor = 10) %>%
  add_eta_decennale()

cat(sprintf("  Osservazioni 2025: %d  -  Occupati medi: %.0f mila\n",
            nrow(d25), sum(d25$peso) / 4 / 1000))

# ---- Lettura 2021 (per confronto settoriale) ----
cat("\nLettura 2021 ...\n")
d21q1 <- read_clean("RCFL_Microdati_2021_Primo_trimestre.txt",   weight_divisor = 1)
d21q2 <- read_clean("RCFL_Microdati_2021_Secondo_trimestre.txt", weight_divisor = 10)
d21q3 <- read_clean("RCFL_Microdati_2021_Terzo_trimestre.txt",   weight_divisor = 10)
d21q4 <- read_clean("RCFL_Microdati_2021_Quarto_trimestre.txt",  weight_divisor = 10)
d21 <- bind_rows(d21q1, d21q2, d21q3, d21q4) %>% add_eta_decennale()
cat(sprintf("  Osservazioni 2021: %d  -  Occupati medi: %.0f mila\n",
            nrow(d21), sum(d21$peso) / 4 / 1000))

# ============================================================
# PARTE A - Eta' decennale grezza 2025
# ============================================================
cat("\n========================================================\n")
cat("A. ETA' 2025 (fasce decennali) - dato grezzo\n")
cat("========================================================\n")

eta_grezza <- d25 %>%
  filter(!is.na(eta)) %>%
  group_by(eta) %>%
  summarise(
    n_obs               = n(),
    base_migliaia       = round(sum(peso) / 4 / 1000, 0),
    quota_occupati      = round(sum(peso) / sum(d25$peso) * 100, 1),
    perc_qualsiasi      = round(sum(peso[qc52 %in% c(1L,2L,4L)]) / sum(peso) * 100, 1),
    perc_frequente      = round(sum(peso[qc52 %in% c(1L,4L)])    / sum(peso) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(fascia = lab_eta[as.character(eta)]) %>%
  arrange(eta)

print(as.data.frame(eta_grezza %>% select(fascia, base_migliaia, quota_occupati,
                                          perc_qualsiasi, perc_frequente)),
      row.names = FALSE)
write_csv(eta_grezza, file.path(out_dir, "30_eta_decennale_grezza_2025.csv"))

# ============================================================
# PARTE B - Eta' x posizione (dipendenti vs autonomi/collab)
# ============================================================
cat("\n========================================================\n")
cat("B. ETA' x POSIZIONE (dip vs aut) - 2025\n")
cat("========================================================\n")

eta_pos <- d25 %>%
  mutate(pos = case_when(
    dipaut == 1L      ~ "Dipendenti",
    dipaut %in% 2:3   ~ "Autonomi/Collab.",
    TRUE              ~ NA_character_
  )) %>%
  filter(!is.na(eta), !is.na(pos)) %>%
  group_by(eta, pos) %>%
  summarise(
    base_migliaia  = round(sum(peso) / 4 / 1000, 0),
    perc_qualsiasi = round(sum(peso[qc52 %in% c(1L,2L,4L)]) / sum(peso) * 100, 1),
    perc_frequente = round(sum(peso[qc52 %in% c(1L,4L)])    / sum(peso) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(fascia = lab_eta[as.character(eta)]) %>%
  select(fascia, pos, base_migliaia, perc_qualsiasi, perc_frequente) %>%
  arrange(fascia, pos)

print(as.data.frame(eta_pos), row.names = FALSE)
write_csv(eta_pos, file.path(out_dir, "31_eta_x_posizione_2025.csv"))

# ============================================================
# PARTE C - Sottocampioni: solo dipendenti del terziario
#                          e solo "colletti bianchi" dipendenti
# ============================================================
cat("\n========================================================\n")
cat("C. ETA' su sottocampioni piu' omogenei (no effetto composizione)\n")
cat("========================================================\n")

# C1: dipendenti del terziario (CAT5 == 5)
d25_terziario_dip <- d25 %>% filter(dipaut == 1L, cat5 == 5L)
eta_terziario_dip <- d25_terziario_dip %>%
  filter(!is.na(eta)) %>%
  group_by(eta) %>%
  summarise(
    base_migliaia = round(sum(peso) / 4 / 1000, 0),
    perc_qualsiasi = round(sum(peso[qc52 %in% c(1L,2L,4L)]) / sum(peso) * 100, 1),
    perc_frequente = round(sum(peso[qc52 %in% c(1L,4L)])    / sum(peso) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(fascia = lab_eta[as.character(eta)]) %>% arrange(eta)

cat("\n-- C1. Solo dipendenti del terziario (Altri servizi, CAT5=5) --\n")
print(as.data.frame(eta_terziario_dip %>% select(fascia, base_migliaia,
                                                  perc_qualsiasi, perc_frequente)),
      row.names = FALSE)
write_csv(eta_terziario_dip, file.path(out_dir, "32_eta_dipendenti_terziario_2025.csv"))

# C2: dipendenti "colletti bianchi": Dirigenti, Quadri, Impiegati
d25_white <- d25 %>% filter(dipaut == 1L, pospro %in% c(1L, 2L, 3L))
eta_white <- d25_white %>%
  filter(!is.na(eta)) %>%
  group_by(eta) %>%
  summarise(
    base_migliaia = round(sum(peso) / 4 / 1000, 0),
    perc_qualsiasi = round(sum(peso[qc52 %in% c(1L,2L,4L)]) / sum(peso) * 100, 1),
    perc_frequente = round(sum(peso[qc52 %in% c(1L,4L)])    / sum(peso) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(fascia = lab_eta[as.character(eta)]) %>% arrange(eta)

cat("\n-- C2. Solo dipendenti dirigenti/quadri/impiegati --\n")
print(as.data.frame(eta_white %>% select(fascia, base_migliaia,
                                         perc_qualsiasi, perc_frequente)),
      row.names = FALSE)
write_csv(eta_white, file.path(out_dir, "33_eta_dipendenti_colletti_bianchi_2025.csv"))

# ============================================================
# PARTE D - Standardizzazione diretta per (CAT5 x dipaut binary)
# ============================================================
cat("\n========================================================\n")
cat("D. ETA' standardizzata per settore (CAT5) x posizione (dip/aut)\n")
cat("========================================================\n")
# Metodo: per ogni fascia di eta', calcoliamo il tasso che si
# osserverebbe se la fascia avesse la stessa distribuzione di
# settore (CAT5) x posizione (Dipendente vs Aut/Coll.) della
# popolazione totale degli occupati. Le celle non osservate
# vengono escluse e i pesi rinormalizzati sulle celle osservate.

d25_std <- d25 %>%
  filter(!is.na(eta), !is.na(cat5), !is.na(dipaut)) %>%
  mutate(dip_bin = if_else(dipaut == 1L, "Dip", "Aut"))

# Distribuzione totale per cella
joint_tot <- d25_std %>%
  group_by(cat5, dip_bin) %>%
  summarise(peso_cell_tot = sum(peso), .groups = "drop") %>%
  mutate(w_tot = peso_cell_tot / sum(peso_cell_tot))

# Tassi per (eta, cat5, dip_bin)
rates_cell <- d25_std %>%
  group_by(eta, cat5, dip_bin) %>%
  summarise(
    peso_cell = sum(peso),
    n         = n(),
    perc_qualsiasi = sum(peso[qc52 %in% c(1L,2L,4L)]) / sum(peso) * 100,
    perc_frequente = sum(peso[qc52 %in% c(1L,4L)])    / sum(peso) * 100,
    .groups = "drop"
  )

eta_std <- rates_cell %>%
  inner_join(joint_tot %>% select(cat5, dip_bin, w_tot),
             by = c("cat5", "dip_bin")) %>%
  group_by(eta) %>%
  summarise(
    cells_used         = n(),
    w_observed         = sum(w_tot),
    perc_qualsiasi_std = round(sum(perc_qualsiasi * w_tot) / sum(w_tot), 1),
    perc_frequente_std = round(sum(perc_frequente * w_tot) / sum(w_tot), 1),
    .groups = "drop"
  ) %>%
  mutate(fascia = lab_eta[as.character(eta)]) %>%
  arrange(eta)

# Confronto grezzo vs standardizzato
confronto_eta <- eta_grezza %>%
  select(eta, fascia, perc_qualsiasi_grezzo = perc_qualsiasi,
         perc_frequente_grezzo = perc_frequente) %>%
  left_join(eta_std %>% select(eta, perc_qualsiasi_std, perc_frequente_std,
                               cells_used, w_observed),
            by = "eta") %>%
  mutate(
    diff_qualsiasi_pp = round(perc_qualsiasi_std - perc_qualsiasi_grezzo, 1),
    diff_frequente_pp = round(perc_frequente_std - perc_frequente_grezzo, 1)
  )

print(as.data.frame(confronto_eta %>%
                      select(fascia,
                             perc_qualsiasi_grezzo, perc_qualsiasi_std,
                             diff_qualsiasi_pp,
                             perc_frequente_grezzo, perc_frequente_std,
                             diff_frequente_pp,
                             cells_used, w_observed)),
      row.names = FALSE)
write_csv(confronto_eta, file.path(out_dir, "34_eta_grezza_vs_standardizzata_2025.csv"))

# ============================================================
# PARTE E - Confronto settoriale 2021 vs 2025
# ============================================================
cat("\n========================================================\n")
cat("E. CONFRONTO SETTORIALE 2021 vs 2025 (CAT12)\n")
cat("========================================================\n")
lab_cat12 <- c(
  "1"  = "Agricoltura, silvicoltura, pesca",
  "2"  = "Industria in senso stretto",
  "3"  = "Costruzioni",
  "4"  = "Commercio",
  "5"  = "Alberghi e ristoranti",
  "6"  = "Trasporto e magazzinaggio",
  "7"  = "Informazione e comunicazione",
  "8"  = "Finanza e assicurazioni",
  "9"  = "Immobiliari e servizi alle imprese",
  "10" = "P.A. e difesa",
  "11" = "Istruzione, sanita', servizi sociali",
  "12" = "Altri servizi collettivi e personali"
)

settore_anno <- function(df, anno_lab) {
  df %>%
    filter(!is.na(cat12)) %>%
    group_by(cat12) %>%
    summarise(
      base_migliaia  = sum(peso) / 4 / 1000,
      perc_qualsiasi = sum(peso[qc52 %in% c(1L,2L,4L)]) / sum(peso) * 100,
      perc_frequente = sum(peso[qc52 %in% c(1L,4L)])    / sum(peso) * 100,
      .groups = "drop"
    ) %>%
    mutate(anno = anno_lab)
}

set_2021 <- settore_anno(d21, "2021")
set_2025 <- settore_anno(d25, "2025")

confronto_settore <- set_2021 %>%
  rename(qualsiasi_2021 = perc_qualsiasi,
         frequente_2021 = perc_frequente,
         base_2021      = base_migliaia) %>%
  select(cat12, base_2021, qualsiasi_2021, frequente_2021) %>%
  full_join(
    set_2025 %>%
      rename(qualsiasi_2025 = perc_qualsiasi,
             frequente_2025 = perc_frequente,
             base_2025      = base_migliaia) %>%
      select(cat12, base_2025, qualsiasi_2025, frequente_2025),
    by = "cat12"
  ) %>%
  mutate(
    settore = lab_cat12[as.character(cat12)],
    var_qualsiasi_pp = qualsiasi_2025 - qualsiasi_2021,
    var_frequente_pp = frequente_2025 - frequente_2021,
    qualsiasi_2021 = round(qualsiasi_2021, 1),
    qualsiasi_2025 = round(qualsiasi_2025, 1),
    frequente_2021 = round(frequente_2021, 1),
    frequente_2025 = round(frequente_2025, 1),
    var_qualsiasi_pp = round(var_qualsiasi_pp, 1),
    var_frequente_pp = round(var_frequente_pp, 1),
    base_2021 = round(base_2021, 0),
    base_2025 = round(base_2025, 0)
  ) %>%
  arrange(desc(qualsiasi_2021)) %>%
  select(settore, cat12, base_2021, base_2025,
         qualsiasi_2021, qualsiasi_2025, var_qualsiasi_pp,
         frequente_2021, frequente_2025, var_frequente_pp)

print(as.data.frame(confronto_settore %>%
                      select(settore, qualsiasi_2021, qualsiasi_2025,
                             var_qualsiasi_pp,
                             frequente_2021, frequente_2025,
                             var_frequente_pp)),
      row.names = FALSE)
write_csv(confronto_settore, file.path(out_dir, "40_settori_2021_vs_2025.csv"))

# Totale Italia 2021 vs 2025
tot_anno <- function(df) {
  df %>% summarise(
    qualsiasi = sum(peso[qc52 %in% c(1L,2L,4L)]) / sum(peso) * 100,
    frequente = sum(peso[qc52 %in% c(1L,4L)])    / sum(peso) * 100
  )
}
t21 <- tot_anno(d21)
t25 <- tot_anno(d25)
cat(sprintf("\nTotale Italia 2021 (media 4 trim): qualsiasi %.1f%%, frequente %.1f%%\n",
            t21$qualsiasi, t21$frequente))
cat(sprintf("Totale Italia 2025 (media 4 trim): qualsiasi %.1f%%, frequente %.1f%%\n",
            t25$qualsiasi, t25$frequente))
cat(sprintf("Variazione qualsiasi: %+.1f pp\n", t25$qualsiasi - t21$qualsiasi))
cat(sprintf("Variazione frequente: %+.1f pp\n", t25$frequente - t21$frequente))

cat(sprintf("\nCSV salvati in: %s/\n", out_dir))
