library(data.table)
library(tidyverse)

data <- fread("ESS1e06_7-ESS6e02_6-ESS8e02_3-ESS9e03_2-ESS10-ESS11-subset.csv")

# Aggiunta colonna anno
data <- data %>%
  mutate(
    year = case_when(
      essround == 1 ~ 2002,
      essround == 6 ~ 2012,
      essround == 8 ~ 2016,
      essround == 9 ~ 2018,
      essround == 10 ~ 2020,
      essround == 11 ~ 2022,
      TRUE ~ NA_real_
    )
  )

# Etichette e scale massime
vars_labels <- c(
  stfdem   = "Soddisfazione per la democrazia",
 # stfgov   = "Soddisfazione per il governo",
 # stfeco   = "Soddisfazione per l'economia",
 # stfedu   = "Soddisfazione per l'istruzione",
 # stfhlth  = "Soddisfazione per la sanità",
 # stflife  = "Soddisfazione per la vita",
  dmcntov  = "Quanto è democratica la nazione",
  implvdm  = "Importanza di vivere in una democrazia",
  psppipla = "Il sistema politico permette influenza",
  psppsgva = "Il sistema politico permette voce",
  trstplt  = "Fiducia nei politici",
  trstprt  = "Fiducia nei partiti politici",
  trstprl  = "Fiducia nel parlamento",
  trstep   = "Fiducia nel parlamento europeo",
  pltcare  = "I politici si preoccupano delle persone",
  pltinvt  = "I politici vogliono solo voti",
  polintr  = "Interesse per la politica",
  cptppola = "Fiducia nella propria capacità politica",
  actrolga = "Capacità di ruolo attivo in gruppo politico",
  polactiv = "Disponibilità a partecipare a gruppo politico"
)

valid_ranges <- list(
  stfdem    = 0:10, 
  #stfgov   = 0:10, stfeco   = 0:10, stfedu   = 0:10,
  #stfhlth   = 0:10, stflife  = 0:10,
  dmcntov  = 0:10, implvdm  = 0:10,
  psppipla  = 0:5,  psppsgva = 0:5,  trstplt  = 0:10, trstprt  = 0:10,
  trstprl   = 0:10, trstep   = 0:10, pltcare  = 0:4,  pltinvt  = 0:4,
  polintr   = 0:4,  cptppola = 0:5, actrolga = 0:5, polactiv = 0:10
)

# Trasformiamo il dizionario dei range in un dataframe d'appoggio
meta_df <- tibble(
  variable = names(vars_labels),
  label = unname(vars_labels),
  scale_max = sapply(valid_ranges, max)
)

# Calcolo per il solo 2022, con join dei metadati
results_2022 <- data %>%
 # filter(year == 2022) %>%
  pivot_longer(cols = all_of(meta_df$variable), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value), !is.na(anweight)) %>%
  rowwise() %>%
  filter(value %in% valid_ranges[[variable]]) %>%
  ungroup() %>%
  group_by(year, variable) %>%
  summarise(
    weighted_mean = weighted.mean(value, anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  left_join(meta_df, by = "variable") %>%
  select(year, variable, label, weighted_mean, n, scale_max)%>%
  distinct()

# Visualizza
print(results_2022)
