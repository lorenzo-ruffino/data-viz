# ============================================================
# Mismatch istruzione-lavoro - VERTICALE (sovraistruzione)
# Fotografia 2025 (media 4 trimestri RCFL)
# ============================================================
# Definizione (standard Eurostat/ISTAT):
#   sovraistruito = occupato con titolo TERZIARIO (ISCED 5-8)
#   che lavora in una professione a bassa/media qualifica
#   (ISCO 4-8: PROF1 in 4,5,6,7,8).
#   Base = laureati occupati in ISCO 1-8 (escluse Forze armate=9).
#   tasso = sovraistruiti / base * 100
#
# Variabili RCFL:
#   COND3==1        occupati
#   HATLEV3MOD==3   titolo terziario (ISCED 5,6,7,8)
#   PROF1           professione ISCO 1-digit (1..9)
#   HATFIELD_D      area disciplinare (campo di studio), 001-014
#   RIP5, SESSO, CLETAS, CITTAD
#   COEF_CCP        peso (1 decimale virtuale -> /10)
# ============================================================

library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)

micro_dir  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Lavoro da remoto/input"
base_dir   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Mismatch istruzione e lavoro"
out_dir    <- file.path(base_dir, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

files_2025 <- file.path(micro_dir, c(
  "RCFL_Microdati_2025_Primo_trimestre.txt",
  "RCFL_Microdati_2025_Secondo_trimestre.txt",
  "RCFL_Microdati_2025_Terzo_trimestre.txt",
  "RCFL_Microdati_2025_Quarto_trimestre.txt"
))

needed <- c("COND3","COEF_CCP","PROF1","HATLEV3MOD","HATFIELD_D",
            "RIP5","SESSO","CLETAS","CITTAD")

read_q <- function(f) {
  cat("  ", basename(f), "\n")
  read_delim(f, delim = "\t", col_select = all_of(needed),
             col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
}

cat("Lettura 4 trimestri 2025...\n")
raw <- map_df(files_2025, read_q)

dati <- raw %>%
  mutate(
    peso    = as.numeric(str_trim(COEF_CCP)) / 10,
    cond3   = as.integer(str_trim(COND3)),
    prof1   = as.integer(str_trim(PROF1)),
    hatlev3 = as.integer(str_trim(HATLEV3MOD)),
    field   = str_trim(HATFIELD_D),
    rip5    = as.integer(str_trim(RIP5)),
    sesso   = as.integer(str_trim(SESSO)),
    cletas  = as.integer(str_trim(CLETAS)),
    cittad  = as.integer(str_trim(CITTAD))
  ) %>%
  filter(cond3 == 1L, !is.na(peso), peso > 0, !is.na(prof1))

# Universo per la sovraistruzione: LAUREATI occupati in ISCO 1-8 (escl. Forze armate=9)
lau <- dati %>%
  filter(hatlev3 == 3L, prof1 %in% 1:8) %>%
  mutate(sovra = prof1 %in% 4:8)

cat(sprintf("\nLaureati occupati (ISCO 1-8), oss.: %d ; stima media (migliaia): %.0f\n",
            nrow(lau), sum(lau$peso)/4/1000))

# ---- Etichette ----
lab_field <- c("001"="Programmi generici","002"="Insegnamento","003"="Arte e design",
  "004"="Letterario-umanistico-linguistico","005"="Scienze sociali e comunicazione",
  "006"="Economico","007"="Giuridico","008"="Scientifico","009"="Informatica/ICT",
  "010"="Ingegneria industriale/informazione","011"="Architettura/Ing. civile",
  "012"="Agrario-forestale-veterinario","013"="Medico-sanitario-farmaceutico","014"="Servizi")
lab_rip5  <- c("1"="Nord-Ovest","2"="Nord-Est","3"="Centro","4"="Sud","5"="Isole")
lab_sesso <- c("1"="Uomini","2"="Donne")
lab_citt  <- c("1"="Italiana","2"="Straniera UE","3"="Straniera extra-UE")

tasso <- function(df) {
  df %>% summarise(
    base_migliaia = round(sum(peso)/4/1000, 0),
    n_obs         = n(),
    sovra_migliaia= round(sum(peso[sovra])/4/1000, 0),
    tasso_sovra   = round(sum(peso[sovra])/sum(peso)*100, 1),
    .groups = "drop")
}

# ---- 0. Totale Italia ----
tot <- tasso(lau)
cat("\n===== TOTALE ITALIA 2025 =====\n"); print(as.data.frame(tot), row.names = FALSE)
write_csv(tot, file.path(out_dir, "v00_totale_italia_2025.csv"))

# ---- 1. Per campo di studio ----
by_field <- lau %>% filter(field %in% names(lab_field)) %>%
  group_by(field) %>% tasso() %>%
  mutate(campo = lab_field[field]) %>%
  select(field, campo, everything()) %>% arrange(desc(tasso_sovra))
cat("\n===== PER CAMPO DI STUDIO =====\n"); print(as.data.frame(by_field), row.names = FALSE)
write_csv(by_field, file.path(out_dir, "v01_per_campo_studio_2025.csv"))

# ---- 2. Per macroarea/ripartizione ----
by_rip <- lau %>% group_by(rip5) %>% tasso() %>%
  mutate(area = lab_rip5[as.character(rip5)]) %>%
  select(rip5, area, everything()) %>% arrange(rip5)
by_macro <- lau %>%
  mutate(macro = case_when(rip5 %in% 1:2 ~ "Nord", rip5==3 ~ "Centro", rip5 %in% 4:5 ~ "Mezzogiorno")) %>%
  group_by(macro) %>% tasso()
cat("\n===== PER RIPARTIZIONE =====\n"); print(as.data.frame(by_rip), row.names = FALSE)
cat("\n===== PER MACROAREA =====\n"); print(as.data.frame(by_macro), row.names = FALSE)
write_csv(by_rip,   file.path(out_dir, "v02_per_ripartizione_2025.csv"))
write_csv(by_macro, file.path(out_dir, "v03_per_macroarea_2025.csv"))

# ---- 3. Per sesso, eta, cittadinanza ----
by_sex <- lau %>% group_by(sesso) %>% tasso() %>% mutate(sesso_lab=lab_sesso[as.character(sesso)])
lau <- lau %>% mutate(eta = case_when(
  cletas %in% 5:7 ~ "15-29", cletas %in% 8:9 ~ "30-39", cletas %in% 10:11 ~ "40-49",
  cletas %in% 12:13 ~ "50-59", cletas >= 14 ~ "60+"))
by_eta <- lau %>% filter(!is.na(eta)) %>% group_by(eta) %>% tasso()
by_citt <- lau %>% group_by(cittad) %>% tasso() %>% mutate(citt_lab=lab_citt[as.character(cittad)])
cat("\n===== PER SESSO =====\n");        print(as.data.frame(by_sex),  row.names = FALSE)
cat("\n===== PER ETA' =====\n");          print(as.data.frame(by_eta),  row.names = FALSE)
cat("\n===== PER CITTADINANZA =====\n");  print(as.data.frame(by_citt), row.names = FALSE)
write_csv(by_sex,  file.path(out_dir, "v04_per_sesso_2025.csv"))
write_csv(by_eta,  file.path(out_dir, "v05_per_eta_2025.csv"))
write_csv(by_citt, file.path(out_dir, "v06_per_cittadinanza_2025.csv"))

# ---- 4. Cross: campo x macroarea ----
cross_fm <- lau %>%
  mutate(macro = case_when(rip5 %in% 1:2 ~ "Nord", rip5==3 ~ "Centro", rip5 %in% 4:5 ~ "Mezzogiorno")) %>%
  filter(field %in% names(lab_field)) %>%
  group_by(field, macro) %>% tasso() %>%
  mutate(campo = lab_field[field]) %>%
  select(field, campo, macro, tasso_sovra, base_migliaia, n_obs) %>%
  arrange(field, macro)
write_csv(cross_fm, file.path(out_dir, "v07_cross_campo_macroarea_2025.csv"))

cat(sprintf("\nOutput salvati in %s\n", out_dir))
