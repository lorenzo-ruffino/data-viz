# ============================================================
# Lavoro da remoto - Confronto europeo 2025 (Eurostat lfsa_ehomp)
# ============================================================
# Variabile Eurostat: lfsa_ehomp
#   "Employed persons working from home as a percentage of the
#    total employment, by sex, age and professional status"
#
# Dimensioni:
#   sex      = T (totale)
#   age      = Y15-64 (e Y_GE15 come check)
#   wstatus  = EMP (occupati totali)
#   frequenc = USU (di solito), SMT (qualche volta), NVR (mai)
#
# Definizione "usually" Eurostat ~ analogo "frequente" RCFL.
# Definizione "sometimes" Eurostat ~ qualche volta nelle ultime
# 4 settimane (analogo a parte "non frequente" del RCFL QC52).
# any_remote = USU + SMT.
#
# Anno scelto: 2025 (ultimo disponibile al 2026-05).
# ============================================================

library(eurostat)
library(dplyr)
library(tidyr)
library(readr)

base_dir   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Lavoro da remoto"
input_dir  <- file.path(base_dir, "input")
output_dir <- file.path(base_dir, "output", "2025_dimensioni")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Download ----
cat("Download Eurostat lfsa_ehomp...\n")
raw <- get_eurostat("lfsa_ehomp", time_format = "num")

# ---- 2. Filtro 2025, totale, 15-64, occupati ----
d25 <- raw %>%
  filter(
    sex         == "T",
    age         == "Y15-64",
    wstatus     == "EMP",
    TIME_PERIOD == 2025,
    !is.na(values)
  ) %>%
  select(geo, frequenc, values)

# ---- 3. Formato largo ----
wide_15_64 <- d25 %>%
  pivot_wider(names_from = frequenc, values_from = values) %>%
  mutate(
    age_band   = "15-64",
    usually    = USU,
    sometimes  = SMT,
    never      = NVR,
    any_remote = round(USU + SMT, 1)
  ) %>%
  select(geo, age_band, usually, sometimes, never, any_remote) %>%
  arrange(desc(any_remote))

# ---- 4. Anche Y_GE15 (15 e oltre, include 65+) ----
d25_ge15 <- raw %>%
  filter(
    sex         == "T",
    age         == "Y_GE15",
    wstatus     == "EMP",
    TIME_PERIOD == 2025,
    !is.na(values)
  ) %>%
  select(geo, frequenc, values) %>%
  pivot_wider(names_from = frequenc, values_from = values) %>%
  mutate(
    age_band   = "15+",
    usually    = USU,
    sometimes  = SMT,
    never      = NVR,
    any_remote = round(USU + SMT, 1)
  ) %>%
  select(geo, age_band, usually, sometimes, never, any_remote) %>%
  arrange(desc(any_remote))

out <- bind_rows(wide_15_64, d25_ge15)

# ---- 5. Salvataggio ----
write_csv(out, file.path(input_dir, "eurostat_lfsa_ehomp_2025.csv"))
write_csv(wide_15_64,
          file.path(output_dir, "50_eurostat_confronto_europeo_2025.csv"))

cat("\n========================================================\n")
cat("Lavoro da remoto 2025 - occupati 15-64, totale\n")
cat("(percentuali, ordinato per quota 'qualunque' = USU+SMT)\n")
cat("========================================================\n")
print(as.data.frame(wide_15_64), row.names = FALSE)

cat("\nPosizione Italia (15-64):\n")
rk <- wide_15_64 %>%
  filter(!geo %in% c("EU27_2020", "EA21", "BA", "MK", "RS", "CH", "NO", "AL", "ME", "TR")) %>%
  mutate(rank = row_number())
it_pos <- rk %>% filter(geo == "IT")
cat(sprintf("  %d-esima su %d paesi UE\n", it_pos$rank, nrow(rk)))

cat("\nFile salvati:\n")
cat(sprintf("  - %s\n", file.path(input_dir, "eurostat_lfsa_ehomp_2025.csv")))
cat(sprintf("  - %s\n", file.path(output_dir, "50_eurostat_confronto_europeo_2025.csv")))
