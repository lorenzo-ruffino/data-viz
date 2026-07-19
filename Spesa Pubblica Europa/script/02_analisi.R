suppressMessages({ library(dplyr); library(tidyr) })
options(width = 200)

dir_in  <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/input"
dir_out <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa Pubblica Europa/output"
raw <- readRDS(file.path(dir_in, "gov_10a_exp_raw.rds"))

YR <- 2024
geo_lab <- c(IT="Italia", FR="Francia", DE="Germania", ES="Spagna", EU27_2020="Media UE")
geo_ord <- c("Italia","Francia","Germania","Spagna","Media UE")

cofog_lab <- c(
  GF01="Servizi generali",
  GF02="Difesa",
  GF03="Ordine pubblico e sicurezza",
  GF04="Affari economici",
  GF05="Protezione ambiente",
  GF06="Abitazioni e assetto territoriale",
  GF07="Sanità",
  GF08="Cultura, sport e religione",
  GF09="Istruzione",
  GF10="Protezione sociale"
)

div10 <- names(cofog_lab)

fmt <- function(x) formatC(x, format="f", digits=1, decimal.mark=",")

## ---------------------------------------------------------------
## 1) SPESA PUBBLICA TOTALE in % del PIL (2024)
## ---------------------------------------------------------------
tot <- raw %>%
  filter(TIME_PERIOD==YR, na_item=="TE", cofog99=="TOTAL", unit=="PC_GDP") %>%
  mutate(paese=geo_lab[geo]) %>%
  select(paese, spesa_tot_pil=values) %>%
  arrange(match(paese, geo_ord))

cat("==================================================================\n")
cat("1) SPESA PUBBLICA TOTALE in % del PIL (", YR, ")\n", sep="")
cat("==================================================================\n")
print(tot %>% mutate(spesa_tot_pil=fmt(spesa_tot_pil)), row.names=FALSE)

## ---------------------------------------------------------------
## 2) COMPOSIZIONE per funzione - in % del PIL
## ---------------------------------------------------------------
comp_pil <- raw %>%
  filter(TIME_PERIOD==YR, na_item=="TE", cofog99 %in% div10, unit=="PC_GDP") %>%
  mutate(paese=geo_lab[geo], funzione=cofog_lab[cofog99]) %>%
  select(funzione, cofog99, paese, values) %>%
  pivot_wider(names_from=paese, values_from=values) %>%
  arrange(cofog99)

cat("\n==================================================================\n")
cat("2) SPESA per FUNZIONE in % del PIL (", YR, ")\n", sep="")
cat("==================================================================\n")
print(comp_pil %>% mutate(across(where(is.numeric), fmt)), row.names=FALSE)

## ---------------------------------------------------------------
## 3) COMPOSIZIONE per funzione - in % della spesa totale
## ---------------------------------------------------------------
comp_tot <- raw %>%
  filter(TIME_PERIOD==YR, na_item=="TE", cofog99 %in% div10, unit=="PC_TOT") %>%
  mutate(paese=geo_lab[geo], funzione=cofog_lab[cofog99]) %>%
  select(funzione, cofog99, paese, values) %>%
  pivot_wider(names_from=paese, values_from=values) %>%
  arrange(cofog99)

cat("\n==================================================================\n")
cat("3) SPESA per FUNZIONE in % della spesa totale (", YR, ")\n", sep="")
cat("==================================================================\n")
print(comp_tot %>% mutate(across(where(is.numeric), fmt)), row.names=FALSE)

## ---------------------------------------------------------------
## 4) ITALIA vs altri: scarti in punti di PIL
## ---------------------------------------------------------------
gap <- comp_pil %>%
  mutate(`IT-UE`   = Italia - `Media UE`,
         `IT-FR`   = Italia - Francia,
         `IT-DE`   = Italia - Germania,
         `IT-ES`   = Italia - Spagna) %>%
  select(funzione, Italia, `Media UE`, `IT-UE`, `IT-FR`, `IT-DE`, `IT-ES`)

cat("\n==================================================================\n")
cat("4) ITALIA vs ALTRI - scarto in punti di PIL (positivo = IT spende piu)\n")
cat("==================================================================\n")
print(gap %>% mutate(across(where(is.numeric), function(x) formatC(x, format="f", digits=1, flag="+", decimal.mark=","))), row.names=FALSE)

## ---------------------------------------------------------------
## 5) SOTTOFUNZIONI CHIAVE in % del PIL
## ---------------------------------------------------------------
sub_lab <- c(
  GF0107="Interessi sul debito",
  GF1002="Pensioni di vecchiaia",
  GF1003="Superstiti (reversibilita)",
  GF1004="Famiglia e infanzia",
  GF1005="Disoccupazione",
  GF1006="Edilizia sociale",
  GF1007="Esclusione sociale",
  GF0701="Prodotti e app. medici (farmaci ecc.)",
  GF0702="Servizi ambulatoriali",
  GF0703="Servizi ospedalieri"
)
sub <- raw %>%
  filter(TIME_PERIOD==YR, na_item=="TE", cofog99 %in% names(sub_lab), unit=="PC_GDP") %>%
  mutate(paese=geo_lab[geo], voce=sub_lab[cofog99]) %>%
  select(voce, cofog99, paese, values) %>%
  pivot_wider(names_from=paese, values_from=values) %>%
  arrange(cofog99)

cat("\n==================================================================\n")
cat("5) SOTTOFUNZIONI CHIAVE in % del PIL (", YR, ")\n", sep="")
cat("==================================================================\n")
print(sub %>% mutate(across(where(is.numeric), fmt)), row.names=FALSE)

## ---------------------------------------------------------------
## 6) CONTROLLO: somma delle 10 funzioni vs TOTALE
## ---------------------------------------------------------------
chk <- comp_pil %>% summarise(across(where(is.numeric), ~round(sum(.),1)))
cat("\n[check] somma 10 funzioni (% PIL):\n"); print(as.data.frame(chk), row.names=FALSE)
cat("[check] totale dichiarato (% PIL):\n"); print(tot, row.names=FALSE)

## salvataggi
write.csv(tot,      file.path(dir_out,"01_spesa_totale_pil.csv"), row.names=FALSE)
write.csv(comp_pil, file.path(dir_out,"02_funzioni_pil.csv"), row.names=FALSE)
write.csv(comp_tot, file.path(dir_out,"03_funzioni_quota.csv"), row.names=FALSE)
write.csv(gap,      file.path(dir_out,"04_italia_vs_altri.csv"), row.names=FALSE)
write.csv(sub,      file.path(dir_out,"05_sottofunzioni_pil.csv"), row.names=FALSE)
cat("\n[ok] CSV salvati in output/\n")
