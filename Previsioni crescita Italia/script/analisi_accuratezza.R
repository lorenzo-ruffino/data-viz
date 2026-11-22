# Accuratezza delle previsioni di crescita sull'Italia
# Governo (DEF/NADEF/DFP/DPFP), Commissione europea, FMI, OCSE, Banca d'Italia, Istat, UPB
# Errore = valore effettivo - previsione (negativo = previsione troppo ottimista)

library(tidyverse)
library(lubridate)

BASE <- "/Users/lorenzoruffino/Documents/Progetti/data-viz/Previsioni crescita Italia"
IN   <- file.path(BASE, "input")
OUT  <- file.path(BASE, "output")

# ---- Previsioni ---------------------------------------------------------

file_prev <- list.files(IN, pattern = "^previsioni_.*\\.csv$", full.names = TRUE)

prev <- map_dfr(file_prev, ~ read_csv(.x, col_types = cols(.default = "c"))) |>
  mutate(
    data_pubblicazione = ymd(data_pubblicazione),
    anno = as.integer(anno),
    valore = as.numeric(valore),
    # l'FMI pubblica le previsioni a un decimale, il database ne ha di più
    valore = if_else(fonte == "fmi", round(valore, 1), valore),
    anno_pub = year(data_pubblicazione),
    mese_pub = month(data_pubblicazione),
    note = replace_na(note, "")
  ) |>
  filter(!is.na(valore), !str_detect(note, "consuntivo/stima"))

# Edizioni intermedie o fuori ciclo, escluse dal confronto principale
prev <- prev |>
  mutate(intermedia = case_when(
    fonte == "ce"   & str_detect(documento, "Interim|Winter|Summer") ~ TRUE,
    fonte == "ocse" & str_detect(documento, "Interim") ~ TRUE,
    fonte == "governo" & str_detect(note, "EXTRA") ~ TRUE,
    fonte == "governo" & str_detect(documento, "Aggiornamento QM") ~ TRUE,
    TRUE ~ FALSE
  ))

# Serie del governo: tendenziale e programmatico (se il programmatico manca si usa il tendenziale,
# come nei documenti con quadro unico fino al 2014 e in quelli solo tendenziali)
gov <- prev |> filter(fonte == "governo")
gov_tend <- gov |> filter(scenario == "tendenziale") |> mutate(serie = "Governo (tendenziale)")
gov_prog <- gov |>
  group_by(documento, variabile, anno) |>
  arrange(desc(scenario == "programmatico"), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  mutate(serie = "Governo (programmatico)")

etichette <- c(ce = "Commissione europea", fmi = "FMI", ocse = "OCSE",
               bdi = "Banca d'Italia", istat = "Istat", upb = "UPB")

# UPB: righe distinte per confronto col quadro tendenziale o programmatico, si preferisce il programmatico
altri <- prev |>
  filter(fonte != "governo") |>
  mutate(pref = if_else(fonte == "upb" & str_detect(note, "confronto con quadro tendenziale"), 2L, 1L)) |>
  group_by(fonte, documento, variabile, anno) |>
  arrange(pref, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  mutate(serie = etichette[fonte])

tutte <- bind_rows(gov_tend, gov_prog, altri) |> filter(!intermedia)

# ---- Orizzonte ----------------------------------------------------------
# Primavera = edizioni aprile-luglio, autunno = settembre-dicembre.
# Per gli enti esterni si prende l'edizione più vicina al documento del governo
# (la prima della finestra); per il governo l'ultima (NADEF 2022 rivista dal governo Meloni).

tutte <- tutte |>
  mutate(
    finestra = case_when(mese_pub %in% 4:7 ~ "primavera",
                         mese_pub %in% 9:12 ~ "autunno",
                         TRUE ~ NA_character_),
    distanza = anno - anno_pub,
    orizzonte = case_when(
      distanza == 0 & finestra == "primavera" ~ "t, primavera",
      distanza == 0 & finestra == "autunno"   ~ "t, autunno",
      distanza == 1 & finestra == "primavera" ~ "t+1, primavera",
      distanza == 1 & finestra == "autunno"   ~ "t+1, autunno",
      distanza == 2 & finestra == "primavera" ~ "t+2, primavera",
      distanza == 2 & finestra == "autunno"   ~ "t+2, autunno",
      distanza == 3 & finestra == "primavera" ~ "t+3, primavera",
      distanza == 3 & finestra == "autunno"   ~ "t+3, autunno",
      TRUE ~ NA_character_)
  ) |>
  filter(!is.na(orizzonte))

scelte <- tutte |>
  group_by(serie, variabile, anno, orizzonte) |>
  arrange(data_pubblicazione, .by_group = TRUE) |>
  filter(if (first(fonte) == "governo") row_number() == n() else row_number() == 1) |>
  ungroup()

# ---- Effettivi ----------------------------------------------------------

eff <- c("effettivi.csv", "effettivi_1995_2007.csv") |>
  map_dfr(~ read_csv(file.path(IN, .x), col_types = cols(.default = "c"))) |>
  transmute(variabile, anno = as.integer(anno), vintage, effettivo = as.numeric(valore)) |>
  filter(vintage %in% c("prima_stima", "seconda_stima", "ultima")) |>
  distinct(variabile, anno, vintage, .keep_all = TRUE)

errori <- scelte |>
  inner_join(eff, by = c("variabile", "anno"), relationship = "many-to-many") |>
  mutate(errore = effettivo - valore)

write_csv(
  errori |> select(serie, documento, data_pubblicazione, orizzonte, variabile, anno,
                   previsione = valore, vintage, effettivo, errore, url),
  file.path(OUT, "errori_previsioni.csv"))

# ---- Metriche -----------------------------------------------------------

metriche <- function(d) {
  d |> summarise(
    n = n(),
    anni = paste(min(anno), max(anno), sep = "-"),
    errore_medio = mean(errore),
    errore_assoluto_medio = mean(abs(errore)),
    rmse = sqrt(mean(errore^2)),
    quota_ottimiste = mean(errore < 0),
    .groups = "drop")
}

ANNI <- 1999:2025
# Anni esclusi: le due grandi crisi (2008-2009, debito sovrano 2012-2013) e la pandemia con il rimbalzo (2020-2021)
ESCLUSI <- c(2008, 2009, 2012, 2013, 2020, 2021)

campioni <- list(
  tutti = function(d) d |> filter(anno %in% ANNI),
  senza_crisi = function(d) d |> filter(anno %in% setdiff(ANNI, ESCLUSI))
)

met <- imap_dfr(campioni, function(f, nome) {
  errori |> f() |>
    group_by(vintage, variabile, orizzonte, serie) |>
    metriche() |>
    mutate(campione = nome)
})
write_csv(met, file.path(OUT, "metriche_per_ente.csv"))

# Campione comune: solo gli anni in cui tutti gli enti confrontati hanno una previsione
confronto_comune <- function(enti, var = "pil_reale", vint = "prima_stima", escludi = ESCLUSI) {
  d <- errori |>
    filter(variabile == var, vintage == vint, serie %in% enti,
           anno %in% setdiff(ANNI, escludi))
  anni_ok <- d |> group_by(orizzonte, anno) |>
    summarise(k = n_distinct(serie), .groups = "drop") |>
    filter(k == length(enti))
  d |> semi_join(anni_ok, by = c("orizzonte", "anno")) |>
    group_by(orizzonte, serie) |> metriche()
}

enti_base <- c("Governo (programmatico)", "Governo (tendenziale)",
               "Commissione europea", "FMI", "OCSE")
comune <- bind_rows(
  confronto_comune(enti_base) |> mutate(campione = "senza_crisi"),
  confronto_comune(enti_base, escludi = integer(0)) |> mutate(campione = "tutti")
)
write_csv(comune, file.path(OUT, "metriche_campione_comune.csv"))

# Testa a testa anno per anno: governo contro ciascun ente sullo stesso orizzonte
testa <- errori |>
  filter(variabile == "pil_reale", vintage == "prima_stima", anno %in% ANNI) |>
  select(serie, orizzonte, anno, errore) |>
  pivot_wider(names_from = serie, values_from = errore)
write_csv(testa, file.path(OUT, "errori_pil_per_anno.csv"))

# ---- Confronti aggiuntivi -----------------------------------------------

# Campione comune con gli enti italiani (coperture più corte)
enti_it <- c("Governo (programmatico)", "Governo (tendenziale)", "UPB", "Istat", "FMI")
comune_it <- bind_rows(
  confronto_comune(enti_it) |> mutate(enti = "UPB, Istat"),
  confronto_comune(c(enti_base, "Banca d'Italia")) |> mutate(enti = "Banca d'Italia")
)
write_csv(comune_it, file.path(OUT, "metriche_campione_comune_italiani.csv"))

# Robustezza: valori effettivi dell'ultima versione dei conti
comune_ultima <- confronto_comune(enti_base, vint = "ultima") |> mutate(campione = "senza_crisi")
write_csv(comune_ultima, file.path(OUT, "metriche_campione_comune_ultima_stima.csv"))

# Governo contro ciascun ente: in quanti anni il governo è stato più ottimista / più preciso
h2h <- errori |>
  filter(variabile == "pil_reale", vintage == "prima_stima", anno %in% ANNI) |>
  select(serie, orizzonte, anno, previsione = valore, effettivo, errore)
h2h <- h2h |>
  filter(serie == "Governo (programmatico)") |>
  inner_join(h2h |> filter(!str_detect(serie, "Governo")), by = c("orizzonte", "anno", "effettivo"),
             suffix = c("_gov", "_altro")) |>
  group_by(orizzonte, ente = serie_altro) |>
  summarise(n = n(),
            gov_piu_ottimista = sum(previsione_gov > previsione_altro),
            uguale = sum(previsione_gov == previsione_altro),
            gov_piu_preciso = sum(abs(errore_gov) < abs(errore_altro)),
            scarto_medio = mean(previsione_gov - previsione_altro),
            .groups = "drop")
write_csv(h2h, file.path(OUT, "governo_contro_enti.csv"))

# Effetto delle politiche: programmatico meno tendenziale nello stesso documento
effetto_politiche <- gov |>
  filter(variabile == "pil_reale", scenario %in% c("tendenziale", "programmatico")) |>
  select(documento, data_pubblicazione, anno, scenario, valore) |>
  pivot_wider(names_from = scenario, values_from = valore) |>
  filter(!is.na(programmatico), !is.na(tendenziale)) |>
  mutate(distanza = anno - year(data_pubblicazione), effetto = programmatico - tendenziale)
write_csv(effetto_politiche, file.path(OUT, "effetto_politiche_pil.csv"))

# Componenti della domanda, previsione d'autunno per l'anno dopo
componenti <- errori |>
  filter(vintage == "prima_stima", orizzonte %in% c("t+1, autunno", "t, primavera"),
         variabile %in% c("pil_reale", "consumi_famiglie", "consumi_pa", "investimenti_fissi",
                          "esportazioni", "importazioni", "deflatore_pil", "pil_nominale",
                          "indebitamento_netto", "debito_pil"),
         anno %in% setdiff(ANNI, ESCLUSI)) |>
  group_by(orizzonte, variabile, serie) |>
  metriche()
write_csv(componenti, file.path(OUT, "metriche_componenti.csv"))
