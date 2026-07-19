# ============================================================
# Lavoro da remoto - Serie storica trimestrale RCFL 2015-2025
# ============================================================
# Due serie distinte per via del break questionario nel 2021
# (riforma LFS reg. UE 2019/1700):
#
#   2015 Q1 -> 2020 Q4 : variabile C48
#     "Per accordo col datore di lavoro, nelle 4 settimane ...
#      ha effettuato a casa ore di lavoro retribuite o
#      recuperabili?"
#     001 = Si', 2+ volte/sett ; 002 = Si', <2 volte/sett
#     003 = No                 ; 997 = Non sa
#     Peso: COEFMI  (1 decimale virtuale -> /10)
#
#   2021 Q1 -> 2025 Q4 : variabile QC52
#     "... ha lavorato da casa? (compreso telelavoro e smart work)"
#     1 = Si', almeno la meta' del tempo
#     2 = Si', qualche volta (< meta')
#     3 = No, mai
#     4 = Si', luogo esclusivo
#     997 = Non sa
#     Peso: COEF_CCP (1 decimale virtuale -> /10)
#
# Base di calcolo: occupati (COND3 == 1) con risposta valida
# (esclude 997 "Non sa" dal denominatore).
#
# Definizioni:
#   qualunque = quota occupati che hanno lavorato da casa almeno
#               qualche volta nelle ultime 4 settimane
#               C48  -> (001 + 002)
#               QC52 -> (1 + 2 + 4)
#   frequente = quota occupati che lavorano da casa in modo
#               intensivo
#               C48  -> 001        (2+ volte a settimana)
#               QC52 -> (1 + 4)    (almeno meta' del tempo, o esclusivo)
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

base_dir   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Lavoro da remoto"
input_dir  <- file.path(base_dir, "input")
output_dir <- file.path(base_dir, "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Indice file ----
files_df <- tibble(filename = list.files(input_dir, pattern = "\\.txt$")) %>%
  mutate(
    anno = as.integer(str_extract(filename, "20\\d{2}")),
    trim_label = str_extract(filename, "(Primo|Secondo|Terzo|Quarto)"),
    trim = case_when(
      trim_label == "Primo"   ~ 1L,
      trim_label == "Secondo" ~ 2L,
      trim_label == "Terzo"   ~ 3L,
      trim_label == "Quarto"  ~ 4L
    ),
    era         = if_else(anno <= 2020, "C48", "QC52"),
    weight_col  = if_else(anno <= 2020, "COEFMI", "COEF_CCP"),
    resp_col    = if_else(anno <= 2020, "C48",    "QC52"),
    # Nota: il leggimi 2021Q1 dichiara "1 decimale virtuale" ma
    # empiricamente i pesi in quel trimestre sono gia' in unita'
    # (somma raw = ~21.8M occupati, coerente con comunicato Istat).
    # Tutti gli altri trimestri 2015-2025 hanno 1 decimale virtuale.
    weight_divisor = if_else(anno == 2021 & trim == 1, 1, 10),
    path        = file.path(input_dir, filename)
  ) %>%
  arrange(anno, trim)

cat(sprintf("File trovati: %d (da %dQ%d a %dQ%d)\n",
            nrow(files_df),
            min(files_df$anno), files_df$trim[1],
            max(files_df$anno), tail(files_df$trim, 1)))

# ---- 2. Funzione di lettura per file ----
process_file <- function(path, era, weight_col, resp_col, weight_divisor, anno, trim) {
  cat(sprintf("  [%dQ%d %s, peso/%d] %s\n", anno, trim, era, weight_divisor, basename(path)))

  df <- read_delim(
    path,
    delim       = "\t",
    col_select  = all_of(c("COND3", weight_col, resp_col)),
    col_types   = cols(.default = "c"),
    show_col_types = FALSE,
    progress    = FALSE
  )

  df %>%
    mutate(
      peso     = as.numeric(str_trim(.data[[weight_col]])) / weight_divisor,
      cond3    = suppressWarnings(as.integer(str_trim(COND3))),
      resp_raw = str_trim(.data[[resp_col]]),
      resp_raw = if_else(resp_raw %in% c("", ".", "NA"), NA_character_, resp_raw),
      # Rimuovi zeri iniziali (C48 = "001","002"... ; QC52 = "1","2"...)
      resp_int = suppressWarnings(as.integer(resp_raw))
    ) %>%
    filter(cond3 == 1L, !is.na(peso), peso > 0) %>%
    group_by(resp_int) %>%
    summarise(stima = sum(peso, na.rm = TRUE),
              n_obs = n(),
              .groups = "drop")
}

# ---- 3. Loop su tutti i trimestri ----
cat("\nLettura microdati...\n")
results <- files_df %>%
  mutate(stats = pmap(list(path, era, weight_col, resp_col, weight_divisor, anno, trim),
                     process_file)) %>%
  select(anno, trim, era, stats) %>%
  unnest(stats)

# ---- 4. Costruzione serie storica ----
# Base = occupati con risposta in {1..3} per C48 o {1..4} per QC52
# (esclude 997 "Non sa" e NA = non applicabile per condizione)

serie <- results %>%
  filter(resp_int %in% c(1, 2, 3, 4)) %>%
  group_by(anno, trim, era) %>%
  summarise(
    occupati_base   = sum(stima),
    n_obs_base      = sum(n_obs),
    stima_1         = sum(stima[resp_int == 1]),
    stima_2         = sum(stima[resp_int == 2]),
    stima_3         = sum(stima[resp_int == 3]),
    stima_4         = sum(stima[resp_int == 4], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    qualsiasi_lavoro_casa = if_else(era == "C48",
                                    stima_1 + stima_2,
                                    stima_1 + stima_2 + stima_4),
    frequente_lavoro_casa = if_else(era == "C48",
                                    stima_1,
                                    stima_1 + stima_4),
    perc_qualsiasi = round(qualsiasi_lavoro_casa / occupati_base * 100, 2),
    perc_frequente = round(frequente_lavoro_casa / occupati_base * 100, 2),
    trimestre      = sprintf("%d Q%d", anno, trim)
  ) %>%
  arrange(anno, trim)

# ---- 5. Verifica pesi: stima totale occupati (vs comunicati ISTAT) ----
verifica <- serie %>%
  mutate(occupati_migliaia = round(occupati_base / 1000)) %>%
  select(trimestre, era, n_obs_base, occupati_migliaia)

cat("\n========================================================\n")
cat("VERIFICA PESI - Stima occupati (migliaia, base = risposte valide al quesito)\n")
cat("========================================================\n")
print(as.data.frame(verifica), row.names = FALSE)

# ---- 6. Serie storica per pubblicazione ----
cat("\n========================================================\n")
cat("SERIE STORICA - % occupati che lavorano da casa\n")
cat("========================================================\n")

out_print <- serie %>%
  select(trimestre, era,
         perc_qualsiasi, perc_frequente,
         occupati_migliaia = occupati_base) %>%
  mutate(occupati_migliaia = round(occupati_migliaia / 1000))

print(as.data.frame(out_print), row.names = FALSE)

# Output con NA per la serie "altra" (per facilitare il grafico
# con due linee distinte e gap visibile sul break)
serie_grafico <- serie %>%
  mutate(
    perc_qualsiasi_c48  = if_else(era == "C48",  perc_qualsiasi, NA_real_),
    perc_qualsiasi_qc52 = if_else(era == "QC52", perc_qualsiasi, NA_real_),
    perc_frequente_c48  = if_else(era == "C48",  perc_frequente, NA_real_),
    perc_frequente_qc52 = if_else(era == "QC52", perc_frequente, NA_real_)
  ) %>%
  select(anno, trim, trimestre, era,
         perc_qualsiasi_c48, perc_qualsiasi_qc52,
         perc_frequente_c48, perc_frequente_qc52,
         occupati_base_migliaia = occupati_base, n_obs_base)

# ---- 7. Salvataggio CSV ----
write_csv(serie,         file.path(output_dir, "lavoro_da_remoto_serie_lungo.csv"))
write_csv(serie_grafico, file.path(output_dir, "lavoro_da_remoto_serie_grafico.csv"))

cat(sprintf("\nCSV salvati in: %s\n", output_dir))
cat("  - lavoro_da_remoto_serie_lungo.csv   (formato lungo, una riga per trimestre)\n")
cat("  - lavoro_da_remoto_serie_grafico.csv (due colonne distinte C48/QC52 per gap visibile)\n")
