suppressMessages({ library(eurostat); library(dplyr); library(tidyr) })
options(width = 200)
dir_in  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/input"
dir_out <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/output"
raw <- readRDS(file.path(dir_in, "gov_10a_exp_raw.rds"))

YR <- 2024
geo_keep <- c("IT","FR","DE","ES","EU27_2020")
geo_lab <- c(IT="Italia", FR="Francia", DE="Germania", ES="Spagna", EU27_2020="Media UE")
geo_ord <- c("Italia","Francia","Germania","Spagna","Media UE")

## --- popolazione media annua (demo_gind, indic_de=AVG) ---
pop_full <- get_eurostat("demo_gind", time_format="num", cache=FALSE) %>% as.data.frame()
pop <- pop_full %>%
  filter(geo %in% geo_keep, indic_de=="AVG", TIME_PERIOD==YR) %>%
  select(geo, pop=values)
saveRDS(pop_full, file.path(dir_in,"demo_gind_raw.rds"))
cat("Popolazione media", YR, "(milioni):\n")
print(pop %>% mutate(paese=geo_lab[geo], pop_mln=round(pop/1e6,2)) %>% select(paese,pop_mln), row.names=FALSE)

## --- PPP del PIL (prc_ppp_ind): euro per 1 PPS ---
ppp_full <- get_eurostat("prc_ppp_ind", time_format="num", cache=FALSE) %>% as.data.frame()
cat("\nna_item PPP disponibili:", paste(grep("PPP", unique(ppp_full$na_item), value=TRUE), collapse=", "), "\n")
cat("ppp_cat che contengono GDP:", paste(grep("GDP", unique(ppp_full$ppp_cat), value=TRUE), collapse=", "), "\n")
ppp <- ppp_full %>%
  filter(geo %in% geo_keep, na_item=="PPP_EU27_2020", ppp_cat=="GDP", TIME_PERIOD==YR) %>%
  select(geo, ppp=values)
saveRDS(ppp_full, file.path(dir_in,"prc_ppp_ind_raw.rds"))
cat("\nFattore PPP (euro per 1 PPS), GDP,", YR, ":\n")
print(ppp %>% mutate(paese=geo_lab[geo], ppp=round(ppp,3)) %>% select(paese,ppp), row.names=FALSE)

## --- spesa in MIO_EUR per le voci chiave ---
voci <- c(TOTAL="Spesa totale", GF07="Sanità", GF09="Istruzione",
          GF10="Protezione sociale", GF1002="Pensioni vecchiaia", GF0107="Interessi debito")
spesa <- raw %>%
  filter(TIME_PERIOD==YR, na_item=="TE", cofog99 %in% names(voci), unit=="MIO_EUR") %>%
  select(geo, cofog99, mio_eur=values) %>%
  left_join(pop, by="geo") %>%
  left_join(ppp, by="geo") %>%
  mutate(eur_pc = mio_eur*1e6/pop,
         pps_pc = eur_pc/ppp,
         voce = voci[cofog99],
         paese = geo_lab[geo])

## --- tabella pro-capite in PPS ---
tab_pps <- spesa %>%
  select(voce, cofog99, paese, pps_pc) %>%
  pivot_wider(names_from=paese, values_from=pps_pc) %>%
  arrange(match(cofog99, names(voci)))
cat("\n==================================================================\n")
cat("SPESA PRO-CAPITE in PPS (potere d'acquisto reale,", YR, ")\n")
cat("==================================================================\n")
print(tab_pps %>% mutate(across(where(is.numeric), ~formatC(round(.), big.mark="."))), row.names=FALSE)

## --- Italia in % della media UE (volume reale) ---
idx <- tab_pps %>%
  mutate(`IT in % UE` = round(100*Italia/`Media UE`),
         `IT in % DE` = round(100*Italia/Germania),
         `IT in % FR` = round(100*Italia/Francia)) %>%
  select(voce, `IT in % UE`, `IT in % DE`, `IT in % FR`)
cat("\nItalia in rapporto agli altri (pro-capite reale PPS, 100 = pari):\n")
print(idx, row.names=FALSE)

## --- confronto: stesso quadro in euro nominali pro-capite ---
tab_eur <- spesa %>%
  select(voce, cofog99, paese, eur_pc) %>%
  pivot_wider(names_from=paese, values_from=eur_pc) %>%
  arrange(match(cofog99, names(voci)))
cat("\nSPESA PRO-CAPITE in EURO nominali (", YR, "):\n", sep="")
print(tab_eur %>% mutate(across(where(is.numeric), ~formatC(round(.), big.mark="."))), row.names=FALSE)

write.csv(tab_pps, file.path(dir_out,"06_procapite_pps.csv"), row.names=FALSE)
write.csv(tab_eur, file.path(dir_out,"07_procapite_eur.csv"), row.names=FALSE)
write.csv(idx,     file.path(dir_out,"08_italia_indice_pps.csv"), row.names=FALSE)
cat("\n[ok] salvati\n")
