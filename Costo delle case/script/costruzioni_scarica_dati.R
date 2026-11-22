# Costo delle case - quanto si costruisce
# Scarica e pulisce:
#   1. permessi di costruire ISTAT (abitazioni in nuovi fabbricati residenziali), Italia 2000-2025
#   2. permessi per classe di comune, regione e provincia, 2010-2025
#   3. confronto europeo: abitazioni autorizzate per 1.000 abitanti (Eurostat sts_cobp_a)
#   4. epoca di costruzione dello stock abitativo (Censimento permanente 2021)
# Fonti:
#   ISTAT SDMX (esploradati.istat.it): 111_111_DF_DCSC_PERM_RAP1_1, 11_111_DF_DCSC_RESID_CONSTR_1,
#   DF_DCSS_HUDW_2_PROV, 22_289 (popolazione)
#   Eurostat: sts_cobp_a (permessi), demo_gind (popolazione al 1 gennaio)
# Nota: la rilevazione ISTAT conta i nuovi fabbricati (anche demoliti e ricostruiti) e gli ampliamenti;
# esclude cambi d'uso, frazionamenti e ristrutturazioni senza aumento di volume.

library(dplyr)
library(readr)
library(tidyr)
library(eurostat)

# da eseguire dalla radice del repo data-viz
dir_input <- "Costo delle case/input"

istat_csv <- function(flow, key, start = NULL) {
  url <- paste0("https://esploradati.istat.it/SDMXWS/rest/data/", flow, "/", key,
                "/ALL/?detail=dataonly&format=csv",
                if (!is.null(start)) paste0("&startPeriod=", start) else "")
  read_csv(url, show_col_types = FALSE, col_types = cols(.default = "c"))
}

# ---- 1. Serie nazionale 2000-2025 ----
# 2000-2009 dalla serie "indicatori" (annuale), 2010-2025 dai dati annuali definitivi
ind <- istat_csv("111_111_DF_DCSC_PERM_RAP1_1", "A...N.", 2000) |>
  filter(BUILDING_TYPE == "NEW_RS_BUIL") |>
  transmute(anno = as.integer(TIME_PERIOD), DATA_TYPE, valore = as.numeric(OBS_VALUE))

annuali <- istat_csv("11_111_DF_DCSC_RESID_CONSTR_1", "A..NUMDW+USFLAR+NRB_NUM+NRB_VOL...........", 2010)

naz <- annuali |>
  filter(REF_AREA == "IT") |>
  transmute(anno = as.integer(TIME_PERIOD), DATA_TYPE, valore = as.numeric(OBS_VALUE)) |>
  bind_rows(ind |> filter(anno < 2010)) |>
  pivot_wider(names_from = DATA_TYPE, values_from = valore) |>
  transmute(anno,
            abitazioni = NUMDW,
            superficie_utile_mq = USFLAR,
            fabbricati = NRB_NUM,
            volume_mc = NRB_VOL) |>
  arrange(anno)

# popolazione al 1 gennaio (Eurostat demo_gind, Italia)
pop_eu <- get_eurostat("demo_gind", filters = list(indic_de = "JAN"), time_format = "num",
                       cache = FALSE) |>
  transmute(geo, anno = as.integer(time), popolazione = values)

naz <- naz |>
  left_join(pop_eu |> filter(geo == "IT") |> select(-geo), by = "anno") |>
  mutate(abitazioni_per_1000_ab = round(abitazioni / popolazione * 1000, 2),
         superficie_media_mq = round(superficie_utile_mq / abitazioni, 1))

write_csv(naz, file.path(dir_input, "costruzioni_permessi_italia_2000_2025.csv"))

# ---- 2. Classi di comune, regioni, province 2010-2025 ----
etichette_classi <- c(
  "INH_GE500001_CAP"     = "Capoluoghi oltre 500.000 ab.",
  "INH250001-500000_CAP" = "Capoluoghi 250.001-500.000 ab.",
  "INH100001-250000_CAP" = "Capoluoghi 100.001-250.000 ab.",
  "INH_UN100000_CAP"     = "Capoluoghi fino a 100.000 ab.",
  "INH_GE50001_NCAP"     = "Non capoluoghi oltre 50.000 ab.",
  "INH20001-50000_NCAP"  = "Non capoluoghi 20.001-50.000 ab.",
  "INH10001-20000_NCAP"  = "Non capoluoghi 10.001-20.000 ab.",
  "INH5001-10000_NCAP"   = "Non capoluoghi 5.001-10.000 ab.",
  "INH2501-5000_NCAP"    = "Non capoluoghi 2.501-5.000 ab.",
  "INH1001-2500_NCAP"    = "Non capoluoghi 1.001-2.500 ab.",
  "INH_UN1000_NCAP"      = "Non capoluoghi fino a 1.000 ab.",
  "PROVCAPM"             = "Tutti i comuni capoluogo",
  "NPROVCAPM"            = "Tutti i comuni non capoluogo",
  "IT"                   = "Italia"
)

classi <- annuali |>
  filter(REF_AREA %in% names(etichette_classi), DATA_TYPE %in% c("NUMDW", "USFLAR")) |>
  transmute(codice = REF_AREA, classe = etichette_classi[REF_AREA],
            anno = as.integer(TIME_PERIOD), DATA_TYPE, valore = as.numeric(OBS_VALUE)) |>
  pivot_wider(names_from = DATA_TYPE, values_from = valore) |>
  rename(abitazioni = NUMDW, superficie_utile_mq = USFLAR) |>
  arrange(codice, anno)

write_csv(classi, file.path(dir_input, "costruzioni_permessi_classi_comune_2010_2025.csv"))

# territori con codice NUTS 2010 usato da ISTAT (es. ITC45 = provincia di Milano, ITE43 = Roma)
territori <- annuali |>
  filter(grepl("^IT", REF_AREA), DATA_TYPE %in% c("NUMDW", "USFLAR")) |>
  transmute(codice = REF_AREA, anno = as.integer(TIME_PERIOD), DATA_TYPE,
            valore = as.numeric(OBS_VALUE)) |>
  pivot_wider(names_from = DATA_TYPE, values_from = valore) |>
  rename(abitazioni = NUMDW, superficie_utile_mq = USFLAR) |>
  mutate(livello = case_when(codice == "IT" ~ "Italia",
                             nchar(codice) == 3 ~ "Ripartizione",
                             nchar(codice) == 4 ~ "Regione",
                             TRUE ~ "Provincia")) |>
  arrange(codice, anno)

write_csv(territori, file.path(dir_input, "costruzioni_permessi_territori_2010_2025.csv"))

# ---- 3. Confronto europeo: abitazioni autorizzate per 1.000 abitanti ----
# Eurostat sts_cobp_a: edifici residenziali esclusi quelli per collettività, migliaia di abitazioni
perm_eu <- get_eurostat("sts_cobp_a",
                        filters = list(indic_bt = "BPRM_DW", cpa2_1 = "CPA_F41001_X_410014",
                                       unit = "THS"),
                        time_format = "num", cache = FALSE) |>
  transmute(geo, anno = as.integer(time), abitazioni_migliaia = values)

eu <- perm_eu |>
  inner_join(pop_eu, by = c("geo", "anno")) |>
  mutate(abitazioni_per_1000_ab = round(abitazioni_migliaia * 1000 / popolazione * 1000, 2)) |>
  filter(anno >= 2005) |>
  arrange(geo, anno)

write_csv(eu, file.path(dir_input, "costruzioni_permessi_europa_per_1000_ab.csv"))

# ---- 4. Epoca di costruzione dello stock abitativo (Censimento 2021) ----
epoca <- istat_csv("DF_DCSS_HUDW_2_PROV", "A.IT+ITC45+ITE43+ITC11+ITF33+ITD55+ITE14.NUM_DW_AV.ALL..ALL.ALL.TOT.ALL.ALL") |>
  transmute(codice = REF_AREA, epoca = PERIOD_CONSTRUCT, stato = OCC_STATUS_CDW,
            abitazioni = as.numeric(OBS_VALUE)) |>
  filter(stato == "ALL") |>
  select(-stato)

# età media approssimata con il punto medio di ogni classe (1900 per "1918 e precedenti",
# 2018 per "2016 e successivi"), riferita al 2021
punto_medio <- c("1918_P" = 1900, "1919-1945" = 1932, "1946-1960" = 1953, "1961-1980" = 1970.5,
                 "1981-2000" = 1990.5, "2001-2010" = 2005.5, "2011-2015" = 2013, "2016_F" = 2018.5)

epoca <- epoca |>
  group_by(codice) |>
  mutate(totale = sum(abitazioni[epoca == "ALL"]),
         quota_pct = round(abitazioni / totale * 100, 1)) |>
  ungroup()

eta_media <- epoca |>
  filter(epoca != "ALL") |>
  group_by(codice) |>
  summarise(eta_media_2021 = round(2021 - weighted.mean(punto_medio[epoca], abitazioni), 1),
            quota_ante_1981 = round(sum(abitazioni[epoca %in% c("1918_P", "1919-1945", "1946-1960", "1961-1980")]) /
                                      sum(abitazioni) * 100, 1),
            quota_dal_2001 = round(sum(abitazioni[epoca %in% c("2001-2010", "2011-2015", "2016_F")]) /
                                     sum(abitazioni) * 100, 1))

write_csv(epoca |> select(-totale), file.path(dir_input, "costruzioni_stock_epoca_costruzione_2021.csv"))
write_csv(eta_media, file.path(dir_input, "costruzioni_stock_eta_media_2021.csv"))
