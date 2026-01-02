library(eurostat)
library(tidyverse)

setwd("~/Documents/Progetti/data-viz/Presidenti_Regione_1995_2025")

df = read.csv("presidenti_regione_2025_12_06.csv")

arrotonda_mese = function(data_str) {
  if (is.na(data_str) || data_str == "") {
    return(NA)
  }
  data = as.Date(data_str)
  giorno = as.integer(format(data, "%d"))
  
  if (giorno < 15) {
    as.Date(paste0(format(data, "%Y-%m"), "-01"))
  } else {
    anno = as.integer(format(data, "%Y"))
    mese = as.integer(format(data, "%m"))
    mese = mese + 1
    if (mese > 12) {
      mese = 1
      anno = anno + 1
    }
    as.Date(sprintf("%04d-%02d-01", anno, mese))
  }
}

df$data_inizio_orig = df$data_inizio
df$data_inizio = as.Date(df$data_inizio)
df$data_fine = as.Date(df$data_fine)
df$data_inizio_arrotondata = sapply(df$data_inizio_orig, arrotonda_mese)
df$data_inizio_arrotondata = as.Date(df$data_inizio_arrotondata, origin = "1970-01-01")
df = df[order(df$regione, df$data_inizio), ]

df$data_fine_nuova = as.Date(NA)
df$ultimo_presidente = FALSE
for (i in 1:nrow(df)) {
  regione_corrente = df$regione[i]
  prossima_riga = which(df$regione == regione_corrente & df$data_inizio > df$data_inizio[i])
  if (length(prossima_riga) > 0) {
    df$data_fine_nuova[i] = df$data_inizio_arrotondata[prossima_riga[1]]
    df$ultimo_presidente[i] = FALSE
  } else {
    df$data_fine_nuova[i] = as.Date("2025-12-31")
    df$ultimo_presidente[i] = TRUE
  }
}

result_list = list()
for (i in 1:nrow(df)) {
  mesi = seq.Date(from = df$data_inizio_arrotondata[i], to = df$data_fine_nuova[i], by = "month")
  
  if (!df$ultimo_presidente[i] && length(mesi) > 1) {
    mesi = mesi[-length(mesi)]
  }
  
  n_mesi = length(mesi)
  expanded_df = data.frame(
    regione = rep(df$regione[i], n_mesi),
    nome_cognome = rep(df$nome_cognome[i], n_mesi),
    genere = rep(df$genere[i], n_mesi),
    partito = rep(df$partito[i], n_mesi),
    coalizione = rep(df$coalizione[i], n_mesi),
    data_inizio = rep(df$data_inizio_orig[i], n_mesi),
    data_fine = rep(as.character(df$data_fine[i]), n_mesi),
    data_inizio_arrotondata = rep(as.character(df$data_inizio_arrotondata[i]), n_mesi),
    data_fine_nuova = rep(as.character(df$data_fine_nuova[i]), n_mesi),
    mese = as.character(mesi),
    stringsAsFactors = FALSE
  )
  result_list[[i]] = expanded_df
}

df_espanso = do.call(rbind, result_list)

data = get_eurostat("demo_r_d2jan")
data = data[data$age == "TOTAL" & data$sex == "T" & data$unit == "NR" & data$TIME_PERIOD >= as.Date("1995-01-01"), ]
data_ita = data[substr(data$geo, 1, 2) == "IT", ]

mappatura_nuts2 = data.frame(
  regione = c("Piemonte", "Liguria", "Lombardia", "Veneto", 
              "Friuli-Venezia Giulia", "Emilia-Romagna", "Toscana", 
              "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", 
              "Campania", "Puglia", "Basilicata", "Calabria", 
              "Sicilia", "Sardegna"),
  nuts2 = c("ITC1", "ITC3", "ITC4", "ITH3", "ITH4", "ITH5", 
            "ITI1", "ITI2", "ITI3", "ITI4", "ITF1", "ITF2", 
            "ITF3", "ITF4", "ITF5", "ITF6", "ITG1", "ITG2"),
  stringsAsFactors = FALSE
)

df_espanso = merge(df_espanso, mappatura_nuts2, by = "regione", all.x = TRUE)
df_espanso$anno = as.integer(format(as.Date(df_espanso$mese), "%Y"))
data_ita$anno = as.integer(format(data_ita$TIME_PERIOD, "%Y"))

data_ita_join = data.frame(
  nuts2 = data_ita$geo,
  anno = data_ita$anno,
  popolazione = data_ita$values,
  stringsAsFactors = FALSE
)

data_ita_join = data_ita_join[!duplicated(data_ita_join[c("nuts2", "anno")]), ]

anni_mancanti = expand.grid(
  nuts2 = unique(mappatura_nuts2$nuts2),
  anno = 1995:2025,
  stringsAsFactors = FALSE
)

data_completa = merge(anni_mancanti, data_ita_join, by = c("nuts2", "anno"), all.x = TRUE)

data_completa = data_completa %>%
  group_by(nuts2) %>%
  arrange(anno) %>%
  fill(popolazione, .direction = "up") %>%
  ungroup()

data_2024 = data_completa %>% filter(anno == 2024)
data_2024$anno = 2025
data_completa = bind_rows(data_completa %>% filter(anno != 2025), data_2024)

df_finale = merge(df_espanso, data_completa, by = c("nuts2", "anno"), all.x = TRUE)

df_finale = df_finale[, c("regione", "nuts2", "nome_cognome", "genere", 
                          "partito", "coalizione", "data_inizio", "data_fine",
                          "data_inizio_arrotondata", "data_fine_nuova", 
                          "mese", "anno", "popolazione")]

df_finale = df_finale[order(df_finale$regione, df_finale$mese), ]

write.csv(df_finale, "presidenti_con_popolazione.csv", row.names = FALSE)

summary_mese = df_finale %>%
  group_by(mese, coalizione) %>%
  summarise(
    n_regioni = n_distinct(regione),
    popolazione = sum(popolazione, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = coalizione,
    values_from = c(n_regioni, popolazione),
    values_fill = 0
  ) %>%
  mutate(
    totale_regioni = n_regioni_CDX + n_regioni_CSX,
    totale_popolazione = popolazione_CDX + popolazione_CSX,
    perc_regioni_CDX = (n_regioni_CDX / totale_regioni) * 100,
    perc_regioni_CSX = (n_regioni_CSX / totale_regioni) * 100,
    perc_popolazione_CDX = (popolazione_CDX / totale_popolazione) * 100,
    perc_popolazione_CSX = (popolazione_CSX / totale_popolazione) * 100
  )

write.csv(summary_mese, "summary_per_mese.csv", row.names = FALSE)


summary_regione_coalizione = df_finale %>%
  group_by(regione, coalizione) %>%
  summarise(
    n_mesi = n(),
    n_presidenti = n_distinct(nome_cognome),
    .groups = "drop"
  ) %>%
  group_by(regione) %>%
  mutate(
    totale_mesi = sum(n_mesi),
    perc_mesi = (n_mesi / totale_mesi) * 100
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = coalizione,
    values_from = c(n_mesi, n_presidenti, perc_mesi),
    values_fill = 0
  )

write.csv(summary_regione_coalizione, "summary_regione_coalizione.csv", row.names = FALSE)


alternanza_regione = df_finale %>%
  group_by(regione, nome_cognome, coalizione, data_inizio) %>%
  summarise(min_mese = min(mese), .groups = "drop") %>%
  arrange(regione, min_mese) %>%
  group_by(regione) %>%
  mutate(
    cambio_coalizione = coalizione != lag(coalizione, default = first(coalizione))
  ) %>%
  summarise(
    n_alternanze = sum(cambio_coalizione),
    n_presidenti_CDX = sum(coalizione == "CDX"),
    n_presidenti_CSX = sum(coalizione == "CSX"),
    .groups = "drop"
  ) %>%
  mutate(
    categoria = case_when(
      n_presidenti_CDX == 0 ~ "Solo centrosinistra",
      n_presidenti_CSX == 0 ~ "Solo centrodestra",
      n_alternanze <= 3 ~ "Media alternanza",
      TRUE ~ "Alta alternanza"
    )
  )

write.csv(alternanza_regione, "alternanza_regione.csv", row.names = FALSE)

summary_alternanza = alternanza_regione %>%
  group_by(categoria) %>%
  summarise(
    n_regioni = n(),
    .groups = "drop"
  )

write.csv(summary_alternanza, "summary_alternanza.csv", row.names = FALSE)


summary_complessivo = df_finale %>%
  group_by(coalizione) %>%
  summarise(
    n_regioni_governate = n_distinct(regione),
    n_mesi_totali = n(),
    popolazione_media_amministrata = mean(popolazione, na.rm = TRUE),
    popolazione_totale_mesi = sum(popolazione, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    perc_mesi = (n_mesi_totali / sum(n_mesi_totali)) * 100,
    perc_popolazione = (popolazione_totale_mesi / sum(popolazione_totale_mesi)) * 100
  )

write.csv(summary_complessivo, "summary_complessivo.csv", row.names = FALSE)