library(httr)
library(readxl)
library(data.table)
library(tidyverse)

temp_file <- tempfile(fileext = ".xlsx")

download.file(url = "https://github.com/UnioneAteiAgnosticiRazionalisti/dati-no-irc/raw/main/dati/mim/Alunni%20che%20frequentano%20Rel%20Catt_2023-24.xlsx", destfile = temp_file, mode = "wb")

ora_religione <- read_excel(temp_file, skip = 2, col_names = c(
  "sigla_provincia",
  "codice_sede",
  "n_alunni_totale",
  "n_alunni_religione"
))

anagrafica_0 = fread("https://raw.githubusercontent.com/UnioneAteiAgnosticiRazionalisti/dati-no-irc/refs/heads/main/dati/mim/SCUANAGRAFESTAT20222320220901.csv")

anagrafica_1 = fread("https://raw.githubusercontent.com/UnioneAteiAgnosticiRazionalisti/dati-no-irc/refs/heads/main/dati/mim/SCUANAGRAFESTAT20232420230901.csv")

anagrafica_2 = fread("https://raw.githubusercontent.com/UnioneAteiAgnosticiRazionalisti/dati-no-irc/refs/heads/main/dati/mim/SCUANAAUTSTAT20222320220901.csv")

anagrafica_3 = fread("https://raw.githubusercontent.com/UnioneAteiAgnosticiRazionalisti/dati-no-irc/refs/heads/main/dati/mim/SCUANAGRAFEPAR20222320220901.csv")

anagrafica_4 = fread("https://raw.githubusercontent.com/UnioneAteiAgnosticiRazionalisti/dati-no-irc/refs/heads/main/dati/mim/SCUANAAUTPAR20222320220901.csv")

anagrafica_1$CAPSCUOLA <- as.character(anagrafica_1$CAPSCUOLA)
anagrafica_2$CAPSCUOLA <- as.character(anagrafica_2$CAPSCUOLA)
anagrafica_3$CAPSCUOLA <- as.character(anagrafica_3$CAPSCUOLA)
anagrafica_4$CAPSCUOLA <- as.character(anagrafica_4$CAPSCUOLA)

anagrafica_tot = bind_rows(anagrafica_1, anagrafica_2, anagrafica_3, anagrafica_4)%>%
  distinct()%>%
  select(AREAGEOGRAFICA, REGIONE, PROVINCIA, CODICESCUOLA, DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA)

data = left_join(ora_religione, anagrafica_tot, by = c("codice_sede" = "CODICESCUOLA"))%>%
  mutate(n_alunni_totale = ifelse(n_alunni_totale == '<=3', 3, as.numeric(n_alunni_totale)),
         n_alunni_religione = ifelse(n_alunni_religione == '<=3', 3, as.numeric(n_alunni_religione)))%>%
  filter(!is.na(REGIONE))%>%
  mutate(livello_scolastico = case_when(
    grepl("INFANZIA", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Infanzia",
    grepl("PRIMARIA", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Primaria",
    grepl("PRIMO GRADO", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Medie",
    grepl("SECONDO GRADO|LICEO|ISTITUTO|IST PROF|IST TEC|MAGISTRALE|ARTE|ARTISTICO", 
          DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Superiori",
    TRUE ~ "Altro"
  ))%>%
  mutate(tipo_superiore = case_when(
    grepl("LICEO|ARTISTICO|MAGISTRALE", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Licei",
    grepl("ISTITUTO TECNICO|IST TECNICO|IST TEC", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Istituti Tecnici",
    grepl("IST PROF", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Istituti Professionali",
    TRUE ~ "Non applicabile"
  ))%>%
  mutate(tipo_liceo = case_when(
    grepl("CLASSICO", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Liceo Classico",
    grepl("SCIENTIFICO", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Liceo Scientifico",
    grepl("LINGUISTICO", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Liceo Linguistico",
    grepl("ARTISTICO", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Liceo Artistico",
    grepl("MAGISTRALE", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Istituto Magistrale",
    grepl("LICEO", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) & 
      !grepl("CLASSICO|SCIENTIFICO|LINGUISTICO|ARTISTICO", DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA) ~ "Altri Licei",
    TRUE ~ "Non Liceo"
  ))


write.csv(

data %>%
  group_by(livello_scolastico)%>%
  summarise(n_alunni_totale = sum(n_alunni_totale, na.rm=T),
            n_alunni_religione = sum(n_alunni_religione, na.rm=T))%>%
  mutate(no_religione = n_alunni_totale - n_alunni_religione,
        rate = 100 * (1- (n_alunni_religione / n_alunni_totale)))%>%
  arrange(desc(rate))

, "livello_scolastico.csv", row.names = F)


