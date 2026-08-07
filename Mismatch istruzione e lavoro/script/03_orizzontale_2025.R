# ============================================================
# Mismatch ORIZZONTALE - campo di studio vs professione (2025)
# ============================================================
# LIMITE METODOLOGICO IMPORTANTE:
#   nei microdati public-use la professione e' solo a ISCO 1-digit
#   (PROF1). Non si puo' quindi costruire un mismatch orizzontale
#   rigoroso (a 1-digit non si distingue un ingegnere da un medico
#   dentro "professioni intellettuali"). Qui produciamo:
#   (a) la distribuzione dei laureati di ogni campo tra i 9 grandi
#       gruppi ISCO -> dove finiscono i laureati di ogni area;
#   (b) un PROXY di coerenza: quota di laureati che lavora in una
#       professione "alta" (ISCO 1-3), e in particolare in ISCO 2
#       (professioni intellettuali, destinazione tipica della laurea).
#   Per il mismatch orizzontale rigoroso per campo si usano fonti
#   esterne (AlmaLaurea efficacia/utilizzo competenze; Eurostat
#   field-of-study mismatch).
# ============================================================

library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)

micro_dir <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Lavoro da remoto/input"
out_dir   <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Mismatch istruzione e lavoro/output"

files_2025 <- file.path(micro_dir, c(
  "RCFL_Microdati_2025_Primo_trimestre.txt","RCFL_Microdati_2025_Secondo_trimestre.txt",
  "RCFL_Microdati_2025_Terzo_trimestre.txt","RCFL_Microdati_2025_Quarto_trimestre.txt"))

needed <- c("COND3","COEF_CCP","PROF1","HATLEV3MOD","HATFIELD_D")
raw <- map_df(files_2025, ~read_delim(.x, delim="\t", col_select=all_of(needed),
              col_types=cols(.default="c"), show_col_types=FALSE, progress=FALSE))

lab_field <- c("002"="Insegnamento","003"="Arte e design",
  "004"="Letterario-umanistico-linguistico","005"="Scienze sociali e comunicazione",
  "006"="Economico","007"="Giuridico","008"="Scientifico","009"="Informatica/ICT",
  "010"="Ingegneria industriale/informazione","011"="Architettura/Ing. civile",
  "012"="Agrario-forestale-veterinario","013"="Medico-sanitario-farmaceutico","014"="Servizi")
lab_prof <- c("1"="Dirigenti","2"="Intellettuali/scientifiche","3"="Tecniche",
  "4"="Esecutive ufficio","5"="Qualificate commercio/servizi","6"="Artigiani/operai",
  "7"="Conduttori impianti","8"="Non qualificate","9"="Forze armate")

lau <- raw %>% mutate(
    peso=as.numeric(str_trim(COEF_CCP))/10, cond3=as.integer(str_trim(COND3)),
    prof1=as.integer(str_trim(PROF1)), terz=as.integer(str_trim(HATLEV3MOD))==3L,
    field=str_trim(HATFIELD_D)) %>%
  filter(cond3==1L, !is.na(peso), peso>0, terz, !is.na(prof1), field %in% names(lab_field))

# (a) distribuzione ISCO 1-digit per campo (riga = 100%)
dist <- lau %>% group_by(field, prof1) %>% summarise(p=sum(peso), .groups="drop") %>%
  group_by(field) %>% mutate(quota=round(p/sum(p)*100,1)) %>% ungroup() %>%
  mutate(campo=lab_field[field], prof=lab_prof[as.character(prof1)])
dist_wide <- dist %>% select(campo, prof, quota) %>%
  pivot_wider(names_from=prof, values_from=quota, values_fill=0)
write_csv(dist_wide, file.path(out_dir, "h01_distribuzione_isco_per_campo_2025.csv"))
cat("===== Distribuzione ISCO 1-digit per campo (riga=100%) =====\n")
print(as.data.frame(dist_wide), row.names=FALSE)

# (b) proxy coerenza: quota in ISCO 1-3 (alta) e in ISCO 2 (intellettuali)
proxy <- lau %>% group_by(field) %>% summarise(
    base_migliaia=round(sum(peso)/4/1000,0),
    quota_isco123=round(sum(peso[prof1 %in% 1:3])/sum(peso)*100,1),
    quota_isco2  =round(sum(peso[prof1==2])/sum(peso)*100,1),
    .groups="drop") %>%
  mutate(campo=lab_field[field]) %>% select(field, campo, base_migliaia, quota_isco123, quota_isco2) %>%
  arrange(desc(quota_isco2))
write_csv(proxy, file.path(out_dir, "h02_proxy_coerenza_per_campo_2025.csv"))
cat("\n===== Proxy coerenza per campo (quota in professioni alte) =====\n")
print(as.data.frame(proxy), row.names=FALSE)
cat(sprintf("\nOutput salvati in %s\n", out_dir))
