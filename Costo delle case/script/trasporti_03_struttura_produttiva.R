# ============================================================
# Costo delle case - Perché in Italia si lavora poco da remoto
# Struttura produttiva: dimensione delle imprese e settori
# ============================================================
# Fonti:
#   Eurostat sbs_sc_ovw  statistiche strutturali sulle imprese per classe
#                        di addetti (economia d'impresa, NACE B-S escl. O)
#   Eurostat lfsa_egan2  occupati per settore NACE Rev. 2 (forze di lavoro)
# ============================================================

suppressPackageStartupMessages({
  library(eurostat)
  library(dplyr)
  library(tidyr)
  library(readr)
})

base_dir  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Costo delle case"
input_dir <- file.path(base_dir, "input")

paesi <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR",
           "HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO",
           "SE","SI","SK","EU27_2020")

# ---- 1. Quota di occupati per classe dimensionale d'impresa ----
sbs <- get_eurostat("sbs_sc_ovw", time_format = "num")
# Variabile: persone occupate (EMP_NR), economia d'impresa (B-S escl. O)
dim_imprese <- sbs %>%
  filter(geo %in% paesi, indic_sbs == "EMP_NR", nace_r2 == "B-S_X_O_S94",
         size_emp %in% c("TOTAL", "0-9", "10-19", "20-49", "50-249", "GE250")) %>%
  select(geo, anno = TIME_PERIOD, size_emp, values) %>%
  pivot_wider(names_from = size_emp, values_from = values) %>%
  mutate(quota_0_9 = round(100 * `0-9` / TOTAL, 1),
         quota_10_49 = round(100 * (`10-19` + `20-49`) / TOTAL, 1),
         quota_50_249 = round(100 * `50-249` / TOTAL, 1),
         quota_250_e_piu = round(100 * GE250 / TOTAL, 1)) %>%
  select(geo, anno, occupati = TOTAL, starts_with("quota")) %>%
  arrange(desc(anno), desc(quota_0_9))
write_csv(dim_imprese, file.path(input_dir, "trasporti_occupati_dimensione_impresa_ue.csv"))

# ---- 2. Composizione settoriale dell'occupazione ----
settori <- c(C = "manifattura", F = "costruzioni",
             G = "commercio", I = "alloggio e ristorazione",
             J = "informazione e comunicazione", K = "finanza e assicurazioni",
             M = "attività professionali e tecniche", O = "pubblica amministrazione",
             P = "istruzione", Q = "sanità e assistenza sociale")
egan <- get_eurostat("lfsa_egan2", time_format = "num") %>%
  filter(geo %in% paesi, sex == "T", age == "Y15-64",
         nace_r2 %in% c("TOTAL", names(settori)), unit == "THS_PER")
occ_settori <- egan %>%
  select(geo, anno = TIME_PERIOD, nace_r2, values) %>%
  group_by(geo, anno) %>%
  mutate(quota = round(100 * values / values[nace_r2 == "TOTAL"], 1)) %>%
  ungroup() %>%
  filter(nace_r2 != "TOTAL") %>%
  mutate(settore = settori[nace_r2]) %>%
  select(geo, anno, nace_r2, settore, occupati_migliaia = values, quota) %>%
  arrange(geo, anno, nace_r2)
write_csv(occ_settori, file.path(input_dir, "trasporti_occupati_settori_ue.csv"))

ultimo <- max(dim_imprese$anno[dim_imprese$geo == "IT" & !is.na(dim_imprese$quota_0_9)])
print(dim_imprese %>% filter(anno == ultimo, geo %in% c("IT","EU27_2020","DE","FR","ES")))
print(occ_settori %>% filter(anno == max(anno), geo %in% c("IT","EU27_2020")) %>%
        select(geo, settore, quota) %>% pivot_wider(names_from = geo, values_from = quota))
