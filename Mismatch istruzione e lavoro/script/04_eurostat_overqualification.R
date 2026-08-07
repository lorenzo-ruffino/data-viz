# ============================================================
# Mismatch VERTICALE - Confronto europeo (Eurostat)
# lfsa_eoqgan  : over-qualification rate (per cittadinanza, eta, sesso)
# lfsa_eoqgan2 : over-qualification rate per settore NACE
# Definizione Eurostat = laureati (ISCED 5-8) occupati in ISCO 4-9.
# ============================================================
suppressMessages({library(eurostat); library(dplyr); library(tidyr); library(readr)})
base_dir <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Mismatch istruzione e lavoro"
in_dir <- file.path(base_dir,"input"); out_dir <- file.path(base_dir,"output")

eu <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT","LV",
        "LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","EU27_2020")

# ---- 1. Confronto UE: totale 25-64, ultimo anno ----
oq <- get_eurostat("lfsa_eoqgan", time_format="num")
yr <- max(oq$TIME_PERIOD, na.rm=TRUE)
conf <- oq %>% filter(citizen=="TOTAL", age=="Y25-64", sex=="T", geo %in% eu, TIME_PERIOD==yr, !is.na(values)) %>%
  select(geo, tasso=values) %>% arrange(desc(tasso))
write_csv(conf, file.path(out_dir, sprintf("e01_overqual_UE_%d.csv", yr)))
cat(sprintf("===== Over-qualification 25-64, %d (top/IT/EU) =====\n", yr))
print(as.data.frame(conf), row.names=FALSE)

# ---- 2. Italia: serie storica + per sesso ----
it <- oq %>% filter(geo=="IT", citizen=="TOTAL", age=="Y25-64", !is.na(values)) %>%
  select(sex, TIME_PERIOD, values) %>% pivot_wider(names_from=sex, values_from=values) %>% arrange(TIME_PERIOD)
write_csv(it, file.path(out_dir, "e02_overqual_IT_serie_sesso.csv"))

# ---- 3. Italia vs EU per cittadinanza, ultimo anno ----
citt <- oq %>% filter(geo %in% c("IT","EU27_2020"), age=="Y25-64", sex=="T", TIME_PERIOD==yr,
                      citizen %in% c("TOTAL","NAT","FOR"), !is.na(values)) %>%
  select(geo, citizen, values) %>% pivot_wider(names_from=citizen, values_from=values)
write_csv(citt, file.path(out_dir, "e03_overqual_cittadinanza.csv"))

# ---- 4. Per settore NACE (IT vs EU), ultimo anno ----
oq2 <- get_eurostat("lfsa_eoqgan2", time_format="num")
yr2 <- max(oq2$TIME_PERIOD, na.rm=TRUE)
nace <- oq2 %>% filter(geo %in% c("IT","EU27_2020"), age=="Y25-64", sex=="T", TIME_PERIOD==yr2, !is.na(values)) %>%
  select(geo, nace_r2, values) %>% pivot_wider(names_from=geo, values_from=values) %>% arrange(desc(IT))
write_csv(nace, file.path(out_dir, sprintf("e04_overqual_settore_nace_%d.csv", yr2)))
cat(sprintf("\n===== Over-qualification per settore NACE %d (IT vs EU) =====\n", yr2))
print(as.data.frame(nace), row.names=FALSE)

cat(sprintf("\nIT %d: tot=%.1f  uomini/donne in e02; per cittadinanza in e03\n", yr,
    conf$tasso[conf$geo=="IT"]))
cat("Output Eurostat salvati.\n")
