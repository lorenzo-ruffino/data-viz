# Download dati per l'articolo sulle pensioni in Europa
# Fonti: Eurostat (ESSPROS, demografia, Tabella 29 ESA)
library(eurostat)
library(dplyr)
library(tidyr)
library(readr)

input <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Pensioni in Europa/input"
paesi <- c("IT", "ES", "FR", "DE", "EU27_2020", "EL", "NL", "SE", "DK", "AT", "PT", "BE", "PL")

# 1. Spesa per pensioni in % del PIL (ESSPROS), serie 2000-ultimo
spesa <- get_eurostat("spr_exp_pens", time_format = "num") |>
  filter(unit == "PC_GDP", spdepm == "TOTAL", geo %in% paesi) |>
  select(benefit = spdepb, geo, anno = TIME_PERIOD, valore = values)
write_csv(spesa, file.path(input, "spesa_pensioni_pil.csv"))
cat("spesa_pensioni_pil:", nrow(spesa), "righe,",
    min(spesa$anno), "-", max(spesa$anno), "\n")

# 2. Beneficiari di pensioni (numero di pensionati)
benef <- get_eurostat("spr_pns_ben", time_format = "num") |>
  filter(geo %in% paesi) |>
  select(-any_of(c("freq"))) |>
  rename(anno = TIME_PERIOD, valore = values)
write_csv(benef, file.path(input, "beneficiari_pensioni.csv"))
cat("beneficiari_pensioni:", nrow(benef), "righe\n")

# 3. Popolazione al 1° gennaio per età (per quota pensionati e over 65)
pop <- get_eurostat("demo_pjangroup", time_format = "num") |>
  filter(geo %in% paesi, sex == "T",
         age %in% c("TOTAL", "Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y_GE85")) |>
  select(age, geo, anno = TIME_PERIOD, valore = values)
write_csv(pop, file.path(input, "popolazione_eta.csv"))
cat("popolazione_eta:", nrow(pop), "righe\n")

# 4. Tabella 29 ESA: diritti pensionistici maturati (% PIL) — debito implicito
t29 <- tryCatch(
  get_eurostat("nasa_10_pens1", time_format = "num"),
  error = function(e) NULL)
if (!is.null(t29)) {
  t29f <- t29 |> filter(geo %in% paesi) |> rename(anno = TIME_PERIOD, valore = values)
  write_csv(t29f, file.path(input, "diritti_maturati_t29.csv"))
  cat("diritti_maturati_t29:", nrow(t29f), "righe\n")
} else cat("nasa_10_pens1 non disponibile\n")
