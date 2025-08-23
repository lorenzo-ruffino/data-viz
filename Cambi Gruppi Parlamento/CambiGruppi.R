library(tidyverse)

## Leggi i dati e crea i dataframe

leggi_file = function(path) {
  nome_file = basename(path) %>% str_remove("\\.csv$")
  read_csv(path, col_types = cols(.default = "c")) %>%
    mutate(file = nome_file)
}

df_camera = map_dfr(
  list.files("Camera", pattern = "\\.csv$", full.names = TRUE),
  leggi_file
)

df_senato = map_dfr(
  list.files("Senato", pattern = "\\.csv$", full.names = TRUE),
  leggi_file
)

camera = df_camera %>%
  mutate(
    inizio = as.Date(inizio, format = "%Y%m%d"),
    fine = as.Date(fine, format = "%Y%m%d"),
    parlamentare = paste0(nome, " ", cognome)
  ) %>%
  select(parlamentare, nomeGruppo, inizio, fine, file) %>%
  separate(file, into = c("ramo", "tmp", "legislatura"), sep = "_") %>%
  select(-tmp) %>%
  mutate(legislatura = as.integer(legislatura))


senato = df_senato %>%
  mutate(
    inizio = as.Date(inizio),
    fine = coalesce(as.Date(fine), as.Date("2025-08-23")),
    parlamentare = paste0(nome, " ", cognome)
  ) %>%
  group_by(parlamentare, nomeGruppo, file) %>%
  select(parlamentare, nomeGruppo, inizio, fine, file) %>%
  arrange(file, parlamentare, inizio) %>%
  separate(file, into = c("ramo", "tmp", "legislatura"), sep = "_") %>%
  select(-tmp) %>%
  mutate(legislatura = as.integer(legislatura)) %>%
  mutate(
    nomeGruppo = case_when(
      ## Bisogna correggere alcuni gruppi che hanno cambiato nome nel corso della legislatura
      ramo == "senato" &
        legislatura == 14 &
        nomeGruppo %in% c("Margherita", "Margherita - DL - L'Ulivo") ~
        "Margherita",
      ramo == "senato" &
        legislatura == 14 &
        nomeGruppo %in% c("Verdi - l'Ulivo", "Verdi - l'Unione") ~
        "Verdi - l'Ulivo",
      ramo == "senato" &
        legislatura == 15 &
        nomeGruppo %in%
          c(
            "Democrazia Cristiana-P. repubblicano ital-Indip.-Mov. per l'Autonomia",
            "Democrazia Cristiana - Indipendenti - Movimento per l'Autonomia"
          ) ~
        "Democrazia Cristiana",
      ramo == "senato" &
        legislatura == 16 &
        nomeGruppo %in%
          c(
            "Coesione Nazionale",
            "Coesione Nazionale-Io Sud-Forza del Sud",
            "Coesione Nazionale - Io Sud"
          ) ~
        "Coesione Nazionale",
      ramo == "senato" &
        legislatura == 16 &
        nomeGruppo %in%
          c("UDC, SVP e Autonomie", "UDC,SVP,Io Sud e Autonomie") ~
        "UDC, SVP e Autonomie",
      ramo == "senato" &
        legislatura == 17 &
        nomeGruppo %in%
          c("ALA - Alleanza Liberalpopolare Autonomie", "ALA  - PRI") ~
        "ALA",
      ramo == "senato" &
        legislatura == 17 &
        nomeGruppo %in%
          c(
            "Grandi Autonomie e Libertà",
            "Grandi Autonomie e Libertà - Unione dei Democratici Cristiani e Democratici di Centro"
          ) ~
        "Grandi Autonomie e Libertà",
      ramo == "senato" &
        legislatura == 17 &
        nomeGruppo %in%
          c("Per le Autonomie  - PSI", "Per le Autonomie -PSI-MAIE") ~
        "Per le Autonomie",
      ramo == "senato" &
        legislatura == 18 &
        nomeGruppo %in% c("C.A.L. -Pc-Idv", "C.A.L. -Alternativa-P.C.-I.d.V.") ~
        "C.A.L.",
      ramo == "senato" &
        legislatura == 18 &
        nomeGruppo %in%
          c(
            "Lega-Salvini Premier",
            "Lega-Salvini Premier-Partito Sardo d'Azione"
          ) ~
        "Lega - Salvini Premier",
      ramo == "senato" &
        legislatura == 19 &
        nomeGruppo %in%
          c(
            "Civici d'Italia - Noi Moderati  - MAIE",
            "Civici d'Italia - Noi Moderati - Coraggio Italia - MAIE"
          ) ~
        "Civici d'Italia",
      TRUE ~ nomeGruppo
    )
  ) %>%
  group_by(parlamentare, nomeGruppo, ramo, legislatura) %>%
  summarise(inizio = min(inizio), fine = max(fine))


rm_trailing_dates <- function(x) {
  # rimuove pattern tipo " (dd.mm.yyyy)" o " (dd.mm.yyyy-dd.mm.yyyy)" alla fine stringa, ) opzionale
  x <- str_replace(
    x,
    "\\s*\\((\\d{1,2}\\.\\d{1,2}\\.\\d{4})(?:-\\d{1,2}\\.\\d{1,2}\\.\\d{4})?\\)?\\s*$",
    ""
  )
  x
}

parlamentari = bind_rows(camera, senato) %>%
  mutate(fine = coalesce(fine, Sys.Date())) %>%
  group_by(ramo, legislatura) %>%
  mutate(
    inizio_leg = min(inizio, na.rm = TRUE),
    giorni_da_inizio_leg = as.integer(fine - inizio_leg),
    flg = inizio_leg
  ) %>%
  ungroup() %>%
  mutate(
    nomeGruppo = nomeGruppo %>%
      rm_trailing_dates() %>%
      str_to_upper(locale = "it") %>%
      str_squish()
  )

### Numero di parlamentari che hanno cambiato gruppo

parlamentari_con_cambio = parlamentari %>%
  group_by(ramo, legislatura, parlamentare) %>%
  summarise(n_gruppi = n_distinct(nomeGruppo), .groups = "drop") %>%
  filter(n_gruppi > 1) %>%
  count(ramo, legislatura, name = "n_parlamentari_con_cambio")


write.csv(
  parlamentari_con_cambio,
  file = "parlamentari_con_cambio.csv",
  row.names = F
)


dettaglio_cambi <- parlamentari %>%
  arrange(ramo, legislatura, parlamentare, inizio) %>%
  group_by(ramo, legislatura, parlamentare) %>%
  summarise(
    n_gruppi = n_distinct(nomeGruppo),
    gruppo_inizio = first(nomeGruppo),
    gruppo_fine = last(nomeGruppo),
    .groups = "drop"
  ) %>%
  filter(n_gruppi > 1)


### Cambri cumulativi per grafico

cambi_per_giorno_leg <- parlamentari %>%
  arrange(legislatura, ramo, parlamentare, inizio) %>%
  group_by(legislatura, parlamentare, ramo) %>%
  mutate(
    gruppo_prec = lag(nomeGruppo),
    cambio = !is.na(gruppo_prec) & nomeGruppo != gruppo_prec
  ) %>%
  filter(cambio) %>%
  ungroup() %>%
  count(legislatura, inizio, name = "n_cambi") %>%
  rename(giorno = inizio)


# Estremi temporali di ciascuna legislatura
bounds_leg <- parlamentari %>%
  group_by(legislatura) %>%
  summarise(
    start = min(inizio_leg, na.rm = TRUE),
    end = max(fine, na.rm = TRUE),
    .groups = "drop"
  )
cal_leg <- bounds_leg %>%
  rowwise() %>%
  mutate(giorno = list(seq.Date(start, end, by = "day"))) %>%
  unnest(giorno) %>%
  select(legislatura, giorno)

# Join + riempimento zeri + cumulativo
cambi_cumulativi_leg <- cal_leg %>%
  left_join(cambi_per_giorno_leg, by = c("legislatura", "giorno")) %>%
  mutate(n_cambi = coalesce(n_cambi, 0L)) %>%
  group_by(legislatura) %>%
  arrange(giorno, .by_group = TRUE) %>%
  mutate(cambi_cum = cumsum(n_cambi)) %>%
  ungroup()

cambi_cumulativi_leg_num <- cambi_cumulativi_leg %>%
  group_by(legislatura) %>%
  arrange(giorno, .by_group = TRUE) %>%
  mutate(giorno_leg = row_number()) %>%
  ungroup()

# Formato wide
cumulativi_long <- cambi_cumulativi_leg_num %>%
  select(legislatura, giorno_leg, cambi_cum) %>%
  pivot_wider(
    names_from = legislatura,
    values_from = cambi_cum,
    names_prefix = "leg_"
  ) %>%
  arrange(giorno_leg)


write.csv(cumulativi_wide, "cumulativi_wide.csv", row.names = F)


### Inizio e attuale della 19° legislatura con combinazioni

start_end_19 <- parlamentari %>%
  filter(legislatura == 19) %>%
  arrange(ramo, parlamentare, inizio) %>%
  group_by(ramo, parlamentare) %>%
  summarise(
    gruppi_inizio = first(nomeGruppo),
    gruppi_fine = last(nomeGruppo),
    n_gruppi = n_distinct(nomeGruppo),
    fine_max = max(fine, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!(n_gruppi == 1 & fine_max < Sys.Date())) %>% # escludi: 1 solo gruppo e fine < oggi (morti/dimessi)
  select(ramo, parlamentare, gruppi_inizio, gruppi_fine)

combinazioni_leg19 <- start_end_19 %>%
  count(ramo, gruppi_inizio, gruppi_fine, name = "n_parlamentari") %>%
  arrange(desc(n_parlamentari))


write.csv(combinazioni_leg19, "combinazioni_19.csv", row.names = F)
