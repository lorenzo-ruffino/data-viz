# Approfondimenti sulle previsioni di crescita del governo
# 1. Prima e dopo l'Ufficio parlamentare di bilancio (validazione dal 2014: anni bersaglio dal 2015)
# 2. Scarto rispetto agli enti indipendenti governo per governo
# 3. Sensibilità: con gli anni delle crisi, con l'ultima versione dei conti
# 4. Effetto dichiarato delle politiche (programmatico - tendenziale) e crescita realizzata
# 5. Investimenti per sottoperiodo

library(tidyverse)
library(lubridate)

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Previsioni crescita Italia"
IN   <- file.path(BASE, "input")
OUT  <- file.path(BASE, "output")

# Anni esclusi: crisi finanziaria (2008-2009), crisi del debito sovrano (2012-2013), pandemia e rimbalzo (2020-2021)
ANNI_CON_CRISI <- setdiff(1999:2025, c(2020, 2021))
ANNI <- setdiff(ANNI_CON_CRISI, c(2008, 2009, 2012, 2013))
ENTI <- c("Commissione europea", "FMI", "OCSE")

err_con_crisi <- read_csv(file.path(OUT, "errori_previsioni.csv"), show_col_types = FALSE) |>
  filter(anno %in% ANNI_CON_CRISI)
err <- err_con_crisi |> filter(anno %in% ANNI)

# Previsione del governo meno media degli enti internazionali, sullo stesso orizzonte e anno
scarti <- function(d, var = "pil_reale", vint = "prima_stima", gov = "Governo (programmatico)") {
  d <- d |> filter(variabile == var, vintage == vint)
  g <- d |> filter(serie == gov) |> select(orizzonte, anno, documento, data_pubblicazione, gov = previsione, effettivo)
  e <- d |> filter(serie %in% ENTI) |>
    group_by(orizzonte, anno) |>
    summarise(enti = mean(previsione), n_enti = n(), .groups = "drop")
  g |> inner_join(e, by = c("orizzonte", "anno")) |>
    mutate(scarto = gov - enti, errore_gov = effettivo - gov, errore_enti = effettivo - enti)
}

s <- scarti(err)

# ---- 1. Prima e dopo l'UPB ---------------------------------------------

periodi <- s |>
  mutate(periodo = if_else(anno <= 2014, "1999-2014 (prima dell'UPB)", "2015-2025 (con l'UPB)")) |>
  group_by(orizzonte, periodo) |>
  summarise(n = n(),
            scarto_medio = mean(scarto),
            anni_gov_sopra = sum(scarto > 0),
            errore_gov = mean(errore_gov),
            errore_enti = mean(errore_enti),
            .groups = "drop")

test_periodi <- s |>
  mutate(dopo = anno >= 2015) |>
  group_by(orizzonte) |>
  summarise(p_differenza_scarto = t.test(scarto ~ dopo)$p.value,
            p_wilcoxon = suppressWarnings(wilcox.test(scarto ~ dopo)$p.value),
            .groups = "drop")

write_csv(periodi |> left_join(test_periodi, by = "orizzonte"), file.path(OUT, "upb_prima_dopo.csv"))

# ---- 2. Governo per governo -----------------------------------------------
# Il governo che firma il documento (data di approvazione)

governi <- tribble(
  ~inizio,       ~governo,
  "1998-10-21", "D'Alema I-II",
  "2000-04-25", "Amato II",
  "2001-06-11", "Berlusconi II-III",
  "2006-05-17", "Prodi II",
  "2008-05-08", "Berlusconi IV",
  "2011-11-16", "Monti",
  "2013-04-28", "Letta",
  "2014-02-22", "Renzi",
  "2016-12-12", "Gentiloni",
  "2018-06-01", "Conte I",
  "2019-09-05", "Conte II",
  "2021-02-13", "Draghi",
  "2022-10-22", "Meloni"
) |> mutate(inizio = ymd(inizio))
# I documenti del 1998 precedenti al 21 ottobre sono del governo Prodi I
assegna_governo <- function(data) {
  i <- findInterval(data, governi$inizio)
  if_else(i == 0, "Prodi I", governi$governo[pmax(i, 1)])
}

per_governo <- s |>
  mutate(governo = assegna_governo(data_pubblicazione)) |>
  filter(orizzonte %in% c("t+1, autunno", "t+1, primavera")) |>
  group_by(governo) |>
  summarise(n = n(), primo = min(data_pubblicazione),
            scarto_medio = mean(scarto), errore_gov = mean(errore_gov), errore_enti = mean(errore_enti),
            .groups = "drop") |>
  arrange(primo)
write_csv(per_governo, file.path(OUT, "scarto_per_governo.csv"))

# ---- 3. Sensibilità -------------------------------------------------------

sens <- bind_rows(
  scarti(err) |> mutate(caso = "base (prima stima, senza crisi e pandemia)"),
  scarti(err_con_crisi) |> mutate(caso = "con le crisi 2008-09 e 2012-13"),
  scarti(err, vint = "ultima") |> mutate(caso = "ultima versione dei conti"),
  scarti(err, gov = "Governo (tendenziale)") |> mutate(caso = "governo tendenziale")
) |>
  group_by(caso, orizzonte) |>
  summarise(n = n(), scarto_medio = mean(scarto),
            # prima le misure che usano gli errori anno per anno, poi le medie che li sostituiscono
            mae_gov = mean(abs(errore_gov)), mae_enti = mean(abs(errore_enti)),
            p_dm_assoluta = tryCatch(t.test(abs(errore_gov) - abs(errore_enti))$p.value, error = function(e) NA_real_),
            errore_gov = mean(errore_gov), errore_enti = mean(errore_enti),
            .groups = "drop")
write_csv(sens, file.path(OUT, "sensibilita.csv"))

# ---- 4. Effetto dichiarato delle politiche --------------------------------

prev_gov <- list.files(IN, pattern = "^previsioni_governo_.*\\.csv$", full.names = TRUE) |>
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c"))) |>
  filter(variabile == "pil_reale", scenario %in% c("tendenziale", "programmatico"),
         !str_detect(replace_na(note, ""), "EXTRA")) |>
  mutate(anno = as.integer(anno), valore = as.numeric(valore), data_pubblicazione = ymd(data_pubblicazione)) |>
  distinct(documento, data_pubblicazione, anno, scenario, .keep_all = TRUE) |>
  select(documento, data_pubblicazione, anno, scenario, valore) |>
  pivot_wider(names_from = scenario, values_from = valore) |>
  filter(!is.na(tendenziale), !is.na(programmatico)) |>
  mutate(distanza = anno - year(data_pubblicazione), effetto = programmatico - tendenziale)

eff <- c("effettivi.csv", "effettivi_1995_2007.csv") |>
  map_dfr(~ read_csv(file.path(IN, .x), show_col_types = FALSE)) |>
  filter(variabile == "pil_reale", vintage %in% c("prima_stima", "ultima")) |>
  select(anno, vintage, valore) |>
  pivot_wider(names_from = vintage, values_from = valore)

effetto <- prev_gov |>
  filter(distanza == 1, anno %in% ANNI) |>
  left_join(eff, by = "anno") |>
  left_join(s |> filter(str_detect(orizzonte, "t\\+1")) |> select(documento, anno, enti), by = c("documento", "anno")) |>
  mutate(realizzato_prima = prima_stima - tendenziale,
         realizzato_ultima = ultima - tendenziale,
         enti_su_tend = enti - tendenziale)
write_csv(effetto, file.path(OUT, "effetto_politiche_anno_dopo.csv"))

# ---- 5. Investimenti per sottoperiodo -------------------------------------

inv <- err |>
  filter(variabile == "investimenti_fissi", vintage == "prima_stima", orizzonte == "t+1, autunno",
         serie %in% c("Governo (programmatico)", ENTI)) |>
  mutate(periodo = case_when(anno <= 2011 ~ "1999-2011",
                             anno <= 2014 ~ "2012-2014",
                             anno <= 2019 ~ "2015-2019",
                             TRUE ~ "2022-2025")) |>
  group_by(periodo, serie) |>
  summarise(n = n(), errore_medio = mean(errore), .groups = "drop")
write_csv(inv, file.path(OUT, "investimenti_per_periodo.csv"))
