library(eurostat)
library(tidyverse)

setwd("~/Documents/Progetti/data-viz/Categorie corsi di laurea")

data_eurostat = get_eurostat("educ_uoe_enrt03")


data = data_eurostat%>%
  filter(isced11 %in% c("ED6", "ED7")
         #isced11 == "ED5-8"
         & TIME_PERIOD == as.Date("2023-01-01")
         & iscedf13 %in% c("F00", "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10"))%>%
  label_eurostat(lang = "en") %>%
  mutate(field_group = case_when(
    iscedf13 %in% c("Natural sciences, mathematics and statistics",
                    "Information and communication technologies",
                    "Engineering, manufacturing and construction") ~ "STEM",
    iscedf13 == "Health and welfare" ~ "Salute",
    iscedf13 %in% c("Arts and humanities",
                    "Social sciences, journalism and information") ~ "Scienze Sociali & Umanistiche",
    iscedf13 == "Business, administration and law" ~ "Economia e Legge",
    iscedf13 %in% c("Education",
                    "Generic programmes and qualifications",
                    "Services",
                    "Agriculture, forestry, fisheries and veterinary") ~ "Altro"
  ))%>%
  group_by(geo, field_group)%>%
  summarise(values = sum(values, na.rm = T))%>%
  group_by(geo)%>%
  mutate(perc = 100 * (values / sum(values)))%>%
  select(geo, field_group, perc)%>%
spread(field_group, perc)

write.csv(data, file="categorie_corsi_laurea.csv", row.names = F)

