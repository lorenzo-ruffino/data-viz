library(data.table)
library(tidyverse)

cgsse_2022_2024 = fread("https://raw.githubusercontent.com/ondata/liberiamoli-tutti/refs/heads/main/scioperi/data/cgsse/cgsse_data_2022-01-01_2025-07-28.csv")%>%
  filter(data_dal_iso < as.Date('2025-01-01'))


cgsse_2025 = fread("https://raw.githubusercontent.com/ondata/liberiamoli-tutti/refs/heads/main/scioperi/data/cgsse/cgsse_data.csv")

Sys.setlocale("LC_TIME", "it_IT.UTF-8")

data = bind_rows(cgsse_2022_2024, cgsse_2025)%>%
  filter(revocato == FALSE)%>%
  mutate(
    anno   = year(data_dal_iso),
    mese   = month(data_dal_iso, label = TRUE, abbr = FALSE),
    mese_num   = month(data_dal_iso),
    giorno = wday(data_dal_iso, label = TRUE, abbr = FALSE, week_start = 1) 
  )

mappa_settori <- c(
  # Trasporti
  "Trasporto pubblico locale"                                                    = "Trasporti",
  "Trasporto pubblico locale, Pulizie e multiservizi"                            = "Trasporti",
  "Trasporto pubblico locale, Regioni e autonomie Locali"                        = "Trasporti",
  "Trasporto merci su rotaia"                                                    = "Trasporti",
  "Trasporto merci su gomma"                                                     = "Trasporti",
  "Trasporto ferroviario"                                                        = "Trasporti",
  "Appalti ferroviari"                                                           = "Trasporti",
  "Trasporto merci su rotaia, Trasporto ferroviario"                             = "Trasporti",
  "Trasporto merci su rotaia, Trasporto ferroviario, Appalti ferroviari"         = "Trasporti",
  "Trasporto merci su rotaia, Trasporto pubblico locale, Trasporto ferroviario, Plurisettoriale nazionale" = "Trasporti",
  "Trasporto merci su rotaia, Trasporto pubblico locale, Trasporto ferroviario, PLURISETTORIALE TRASPORTI" = "Trasporti",
  "Trasporto merci su rotaia, Trasporto ferroviario, PLURISETTORIALE TRASPORTI"  = "Trasporti",
  "Trasporto merci su rotaia, Trasporto ferroviario, Plurisettoriale nazionale"  = "Trasporti",
  "Trasporto merci su rotaia, Trasporto merci su gomma, Trasporto ferroviario, Plurisettoriale nazionale" = "Trasporti",
  "Trasporto merci su gomma, Regioni e autonomie Locali"                         = "Trasporti",
  "Trasporto aereo"                                                              = "Trasporti",
  "Trasporto aereo, Istituti di vigilanza"                                       = "Trasporti",
  "Trasporto marittimo"                                                          = "Trasporti",
  "Elicotteri"                                                                   = "Trasporti",
  "Taxi"                                                                         = "Trasporti",
  "Noleggio con conducente"                                                      = "Trasporti",
  
  # Energia e utilities
  "Elettricità"                                                                  = "Energia e utilities",
  "Gas"                                                                          = "Energia e utilities",
  "Acqua"                                                                        = "Energia e utilities",
  "Carburanti"                                                                   = "Energia e utilities",
  "Energia e petrolio"                                                           = "Energia e utilities",
  "Elettricità, Gas"                                                             = "Energia e utilities",
  "Gas/Acqua"                                                                    = "Energia e utilities",
  "Gas, Acqua"                                                                   = "Energia e utilities",
  
  # Pubblica amministrazione
  "Ministeri"                                                                    = "Pubblica amministrazione",
  "Regioni e autonomie Locali"                                                   = "Pubblica amministrazione",
  "Enti pubblici non economici"                                                  = "Pubblica amministrazione",
  "Camere di commercio"                                                          = "Pubblica amministrazione",
  "Agenzie fiscali"                                                              = "Pubblica amministrazione",
  "Consorzi di bonifica"                                                         = "Pubblica amministrazione",
  "Vigili del fuoco"                                                             = "Pubblica amministrazione",
  
  # Istruzione e ricerca
  "Scuola"                                                                       = "Istruzione e ricerca",
  "Università"                                                                   = "Istruzione e ricerca",
  "Ricerca"                                                                      = "Istruzione e ricerca",
  "Istruzione e ricerca"                                                         = "Istruzione e ricerca",
  "Scuola, Regioni e autonomie Locali"                                           = "Istruzione e ricerca",
  "Ricerca, Università"                                                          = "Istruzione e ricerca",
  "Plurisettoriale nazionale, Università"                                        = "Istruzione e ricerca",
  
  # Sanità
  "Servizio sanitario nazionale"                                                 = "Sanità",
  "Sanità privata"                                                               = "Sanità",
  "Sanità privata, Servizio sanitario nazionale"                                 = "Sanità",
  "Servizio sanitario nazionale, Pulizie e multiservizi"                         = "Sanità",
  "Servizio sanitario nazionale, Plurisettoriale provinciale"                    = "Sanità",
  "Servizio sanitario nazionale, Metalmeccanici"                                 = "Sanità",
  "Servizio sanitario nazionale, Regioni e autonomie Locali"                     = "Sanità",
  
  # Servizi
  "Pulizie e multiservizi"                                                       = "Servizi",
  "Igiene ambientale"                                                            = "Servizi",
  "Igiene ambientale, Pulizie e multiservizi"                                    = "Servizi",
  "Farmacie"                                                                     = "Servizi",
  "Distribuzione farmaci e logistica farmaceutica"                               = "Servizi",
  "Poste"                                                                        = "Servizi",
  "Funerario"                                                                    = "Servizi",
  "Radio e TV"                                                                   = "Servizi",
  "Telecomunicazioni"                                                            = "Servizi",
  "Istituti di vigilanza"                                                        = "Servizi",
  "Credito"                                                                      = "Servizi",
  
  # Professioni
  "Avvocati"                                                                     = "Professioni",
  "MAGISTRATI TOGATI E ONORARI"                                                  = "Professioni",
  "Liberi professionisti"                                                        = "Professioni",
  
  # Industria
  "Metalmeccanici"                                                               = "Industria",
  "Metalmeccanici, Plurisettoriale nazionale"                                    = "Industria",
  
  # Plurisettoriali e generali
  "Generale nazionale"                                                           = "Plurisettoriale",
  "Generale territoriale"                                                        = "Plurisettoriale",
  "Generale regionale"                                                           = "Plurisettoriale",
  "Generale provinciale"                                                         = "Plurisettoriale",
  "Plurisettoriale territoriale"                                                 = "Plurisettoriale",
  "Plurisettoriale nazionale"                                                    = "Plurisettoriale",
  "Plurisettoriale provinciale"                                                  = "Plurisettoriale",
  "Plurisettoriale regionale"                                                    = "Plurisettoriale"
)

# Applicazione del mapping
data <- data %>%
  mutate(settore_mappato = recode(settore, !!!mappa_settori, .default = settore))



write.csv(data, file="scioperi.csv", row.names = F)



scioperi_giorni = data %>%
  group_by(giorno)%>%
  count()%>%
  ungroup()%>%
  mutate(n = 100 * (n/sum(n)))

write.csv(scioperi_giorni, file="scioperi_giorni.csv", row.names = F)


scioperi_giorni_nazionale = data %>%
  filter(ambito_geografico == 'NAZIONALE')%>%
  group_by(giorno)%>%
  count()%>%
  ungroup()%>%
  mutate(n = 100 * (n/sum(n)))

write.csv(scioperi_giorni_nazionale, file="scioperi_giorni_nazionale.csv", row.names = F)


scioperi_giorni_settore = data %>%
  group_by(giorno, settore_mappato)%>%
  count()%>%
  group_by(settore_mappato)%>%
  mutate(n = 100 * (n/sum(n)))%>%
  spread(giorno, n)

write.csv(scioperi_giorni_settore, file="scioperi_giorni_settore.csv", row.names = F)





