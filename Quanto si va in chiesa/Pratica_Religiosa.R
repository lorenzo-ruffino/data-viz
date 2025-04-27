library(istat)


search_istatdata("religiosa", lang = "ita")


agencyId id                              version name                                                          
[1,] "IT1"    "83_63_DF_DCCV_AVQ_PERSONE_133" "1.0"   "Pratica religiosa - età dettaglio"                           
[2,] "IT1"    "83_63_DF_DCCV_AVQ_PERSONE_134" "1.0"   "Pratica religiosa - età, titolo di studio"                   
[3,] "IT1"    "83_63_DF_DCCV_AVQ_PERSONE_135" "1.0"   "Pratica religiosa - condizione e posizione nella professione"
[4,] "IT1"    "83_63_DF_DCCV_AVQ_PERSONE_136" "1.0"   "Pratica religiosa - regioni e tipo di comune"     


dati = get_istatdata(agencyId = "IT1",
                     dataset_id = "83_63_DF_DCCV_AVQ_PERSONE_134",
                     version = "1.0",
                     csv = FALSE,
                     xlsx = FALSE)




data = dati%>%
  filter(MEASURE == 'HSC'
         & AGE == 'Y_GE6'
         & SEX == 9)%>%
  mutate(DATA_TYPE = ifelse(DATA_TYPE == '6_NEVER_RELIG', 'mai', "una_volta_settimana"))%>%
  spread(DATA_TYPE, obsValue)%>%
  select(obsTime, mai, una_volta_settimana)


write.csv(data, file="frequenza_chiesa_anno.csv", row.names = F)




data = dati%>%
  filter(MEASURE == 'HSC'
         & DATA_TYPE == '6_NEVER_RELIG'
         & SEX == 9)%>%
  spread(AGE, obsValue)%>%
  select(obsTime, `Y6-13`,`Y14-17` ,`Y18-19` ,`Y20-24`, `Y25-34`, `Y35-44`,`Y45-54`, `Y55-59`, `Y60-64` ,`Y65-74`,  `Y_GE75`)
        

write.csv(data, file="frequenza_chiesa_mai_eta.csv", row.names = F)




data = dati%>%
  filter(MEASURE == 'HSC'
         & DATA_TYPE == '6_NEVER_RELIG'
         & SEX == 9
         & obsTime %in% c(2001, 2023))%>%
  group_by(AGE)%>%
  mutate(prec = lag(obsValue),
         var = 100 * (obsValue / lag(obsValue)-1))%>%
  filter(obsTime == 2023)




dati = get_istatdata(agencyId = "IT1",
                     dataset_id = "83_63_DF_DCCV_AVQ_PERSONE_136",
                     version = "1.0",
                     csv = FALSE,
                     xlsx = FALSE)

data = dati%>%
  filter(MEASURE == 'HSC'
         & DATA_TYPE == '6_NEVER_RELIG'
         & SEX == 9
         & REF_AREA %in% c("ITC","ITD", "ITE", "ITF", "ITG"))%>%
  mutate(REF_AREA = case_when(
         REF_AREA == "ITC" ~ "Nord-Ovest",
         REF_AREA == "ITD" ~ "Nord-Est",
         REF_AREA == "ITE" ~ "Centro",
         REF_AREA == "ITF" ~ "Sud",
         REF_AREA == "ITG" ~ "Isole"))%>%
  spread(REF_AREA, obsValue)%>%
  select(obsTime, `Nord-Ovest`, `Nord-Est`, `Centro`, `Sud`, `Isole`)


write.csv(data, file="frequenza_chiesa_mai_area.csv", row.names = F)

