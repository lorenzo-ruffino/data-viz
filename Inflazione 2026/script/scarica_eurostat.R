# Scarica i dati Eurostat per il confronto europeo sull'inflazione 2026
# Dal 2026 l'HICP è in ECOICOP ver. 2: prc_hicp_minr (indici e tassi mensili),
# prc_hicp_iw (pesi), prc_hicp_fpd (prime pubblicazioni, flash compresa)

library(eurostat)
library(tidyverse)

dir_in <- "Inflazione 2026/input"

ue27 <- c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV",
          "LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")
aggregati <- c("EA","EA21","EA20","EU27_2020")

componenti <- c("TOTAL","NRG","FOOD","IGD_NNRG","SERV","TOT_X_NRG_FOOD",
                "ELC_GAS","FUEL","CP01")

# 1-2. Tassi annui mensili HICP per componente, tutti i paesi UE + aggregati
hicp <- get_eurostat("prc_hicp_minr", time_format = "raw", cache = FALSE,
                     filters = list(unit = c("RCH_A", "I25"),
                                    coicop18 = componenti,
                                    geo = c(aggregati, ue27))) |>
  mutate(time = as.character(time))
hicp |>
  filter(unit == "RCH_A") |>
  select(geo, coicop18, time, values) |>
  arrange(geo, coicop18, time) |>
  write_csv(file.path(dir_in, "eurostat_hicp_tasso_annuo.csv"))

hicp |>
  filter(unit == "I25") |>
  select(geo, coicop18, time, values) |>
  arrange(geo, coicop18, time) |>
  write_csv(file.path(dir_in, "eurostat_hicp_indice_2025.csv"))

# Prime pubblicazioni (flash ed definitive)
fpd <- get_eurostat("prc_hicp_fpd", time_format = "raw", cache = FALSE,
                    filters = list(unit = "RCH_A", coicop18 = componenti,
                                   geo = c(aggregati, ue27),
                                   sinceTimePeriod = "2025-01"))
fpd |>
  select(geo, coicop18, release, time, values) |>
  arrange(geo, coicop18, release, time) |>
  write_csv(file.path(dir_in, "eurostat_hicp_prime_pubblicazioni.csv"))

# 3. Aumento cumulato dei prezzi dal dicembre 2020 e dal 2021 (media annua)
ind <- hicp |> filter(unit == "I25", coicop18 == "TOTAL") |>
  select(geo, time, values)
ultimo <- max(ind$time[!is.na(ind$values) & ind$geo == "IT"])
cumulato <- ind |>
  group_by(geo) |>
  summarise(
    base_dic2020 = values[time == "2020-12"],
    base_media2020 = mean(values[str_starts(time, "2020")]),
    base_media2021 = mean(values[str_starts(time, "2021")]),
    ultimo_mese = ultimo,
    valore_ultimo = values[time == ultimo],
    .groups = "drop") |>
  mutate(var_dic2020 = 100 * (valore_ultimo / base_dic2020 - 1),
         var_media2020 = 100 * (valore_ultimo / base_media2020 - 1),
         var_media2021 = 100 * (valore_ultimo / base_media2021 - 1)) |>
  arrange(desc(var_dic2020))
write_csv(cumulato, file.path(dir_in, "eurostat_hicp_cumulato_2021_2026.csv"))

# Indice medio annuo (per deflazionare i salari)
hicp_annuo <- ind |>
  mutate(anno = as.integer(substr(time, 1, 4))) |>
  group_by(geo, anno) |>
  summarise(mesi = sum(!is.na(values)), hicp = mean(values, na.rm = TRUE),
            .groups = "drop")

# 4. Salari: retribuzione per dipendente (contabilità nazionale) e indice
#    del costo del lavoro, componente salari e stipendi
cpe <- get_eurostat("nama_10_lp_ulc", time_format = "num", cache = FALSE,
                    filters = list(na_item = "D1_SAL_PER", unit = "NAC",
                                   geo = c("EA20", "EU27_2020", ue27)))
cpe |>
  select(geo, anno = time, retrib_per_dip_nac = values) |>
  left_join(hicp_annuo |> select(geo, anno, hicp), by = c("geo", "anno")) |>
  arrange(geo, anno) |>
  write_csv(file.path(dir_in, "eurostat_retribuzione_per_dipendente.csv"))

salari_reali <- cpe |>
  select(geo, anno = time, nom = values) |>
  mutate(geo_hicp = if_else(geo == "EA20", "EA", geo)) |>
  left_join(hicp_annuo |> select(geo_hicp = geo, anno, hicp),
            by = c("geo_hicp", "anno")) |>
  mutate(reale = nom / hicp) |>
  group_by(geo) |>
  summarise(
    ultimo_anno = max(anno[!is.na(reale)]),
    nom_vs2019 = 100 * (nom[anno == ultimo_anno] / nom[anno == 2019] - 1),
    prezzi_vs2019 = 100 * (hicp[anno == ultimo_anno] / hicp[anno == 2019] - 1),
    reale_vs2019 = 100 * (reale[anno == ultimo_anno] / reale[anno == 2019] - 1),
    reale_vs2021 = 100 * (reale[anno == ultimo_anno] / reale[anno == 2021] - 1),
    reale_minimo = 100 * (min(reale[anno >= 2021 & anno <= ultimo_anno]) /
                            reale[anno == 2021] - 1),
    .groups = "drop") |>
  arrange(reale_vs2019)
write_csv(salari_reali, file.path(dir_in, "eurostat_salari_reali_per_dipendente.csv"))

lci <- get_eurostat("lc_lci_r2_q", time_format = "raw", cache = FALSE,
                    filters = list(unit = "I20", s_adj = c("NSA", "CA", "SCA"),
                                   nace_r2 = "B-S", lcstruct = "D11",
                                   geo = c("EA20", "EU27_2020", ue27))) |>
  mutate(time = as.character(time))
hicp_trim <- ind |>
  mutate(tr = paste0(substr(time, 1, 4), "-Q", ceiling(as.integer(substr(time, 6, 7)) / 3))) |>
  group_by(geo, time = tr) |>
  summarise(hicp = mean(values), n = n(), .groups = "drop") |>
  filter(n == 3) |> select(-n)
lci |>
  select(geo, s_adj, time, lci_salari_i20 = values) |>
  left_join(hicp_trim, by = c("geo", "time")) |>
  arrange(geo, s_adj, time) |>
  write_csv(file.path(dir_in, "eurostat_lci_salari_trimestrale.csv"))

# 5. Inflazione percepita: saldo sui prezzi negli ultimi 12 mesi
bsco <- get_eurostat("ei_bsco_m", time_format = "raw", cache = FALSE,
                     filters = list(indic = c("BS-PT-LY", "BS-PT-NY"),
                                    s_adj = "SA",
                                    geo = c("EA", "EA21", "EA20", "EU27_2020", "IT", "DE", "FR", "ES"),
                                    sinceTimePeriod = "2015-01"))
bsco |>
  select(geo, indic, time, saldo = values) |>
  arrange(geo, indic, time) |>
  write_csv(file.path(dir_in, "eurostat_consumatori_prezzi_saldo.csv"))

# 6. Pesi HICP (per mille) 2019-2026
pesi <- get_eurostat("prc_hicp_iw", time_format = "num", cache = FALSE,
                     filters = list(coicop18 = componenti[componenti != "TOTAL"],
                                    geo = c(aggregati, ue27),
                                    sinceTimePeriod = "2019"))
pesi |>
  select(geo, coicop18, anno = time, peso_per_mille = values) |>
  arrange(geo, coicop18, anno) |>
  write_csv(file.path(dir_in, "eurostat_hicp_pesi.csv"))

# 7. Debito pubblico e PIL nominale
debito <- get_eurostat("gov_10dd_edpt1", time_format = "num", cache = FALSE,
                       filters = list(na_item = c("GD", "B1GQ", "D41PAY", "B9"),
                                      sector = c("S13", "S1"),
                                      unit = c("MIO_EUR", "PC_GDP"),
                                      geo = c("IT", "DE", "FR", "ES", "EA20", "EU27_2020"),
                                      sinceTimePeriod = "2015"))
debito |>
  filter((na_item == "B1GQ" & sector == "S1") | (na_item != "B1GQ" & sector == "S13")) |>
  select(geo, na_item, unit, anno = time, values) |>
  arrange(geo, na_item, unit, anno) |>
  write_csv(file.path(dir_in, "eurostat_debito_pil.csv"))

# PIL nominale, reale e deflatore (per scomporre la crescita nominale)
pil <- get_eurostat("nama_10_gdp", time_format = "num", cache = FALSE,
                    filters = list(na_item = "B1GQ",
                                   unit = c("CP_MEUR", "CLV20_MEUR", "PD20_EUR",
                                            "PCH_PRE_CP", "CLV_PCH_PRE"),
                                   geo = c("IT", "DE", "FR", "ES", "EA20", "EU27_2020"),
                                   sinceTimePeriod = "2015"))
pil |>
  select(geo, unit, anno = time, values) |>
  arrange(geo, unit, anno) |>
  write_csv(file.path(dir_in, "eurostat_pil_nominale_reale.csv"))
