library(readxl)
library(tidyverse)
library(zoo)

file_url <- "https://www.trade.gov//sites/default/files/2024-06/Monthly%20Arrivals%202000%20to%20Present%20%E2%80%93%20Country%20of%20Residence%20%28COR%29_1.xlsx"

temp_file <- tempfile(fileext = ".xlsx")
download.file(file_url, temp_file, mode = "wb")


data <- read_excel(temp_file, sheet = 1) %>%
 mutate(across(everything(), as.character))%>%
  pivot_longer(
    cols = -c(1, 2),  # Escludi le prime due colonne
    names_to = "Data",  # Nome della nuova colonna per le date
    values_to = "Valore"  # Nome della nuova colonna per i valori
  )%>%
  rename(Country = `International Visitors--\r\n   1) Country of Residence\r\n   2) 1+ nights in the USA\r\n   3)  Among qualified visa types`) %>%
  filter(Country == "Italy" & Data != "World  Region" & Valore != "Western Europe")%>%
  select(-`1`)%>%
  mutate(Data_corretta = case_when(
    grepl("^\\d{5}$", Data) ~ as.character(as.Date(as.numeric(Data), origin = "1899-12-30")),
    
    grepl("^\\d{4}-\\d{1,2}$", Data) ~ as.character(ymd(paste0(Data, "-01"))),
    
    grepl("^\\d{4}-\\d{1,2}\\r\\nPreliminary$", Data) ~ {
      date_part <- sub("\\r\\nPreliminary$", "", Data)
      as.character(ymd(paste0(date_part, "-01")))
    },
    
    grepl("^\\d{4}-\\d{1}$", Data) ~ {
      parts <- strsplit(Data, "-")[[1]]
      as.character(ymd(paste0(parts[1], "-0", parts[2], "-01")))
    },
    
    TRUE ~ Data
  )) %>%
  mutate(
    Data_corretta = as.Date(Data_corretta),
    Valore = as.numeric(Valore)
  )%>%
  arrange(Data_corretta)%>%
  mutate(Media_Mobile = rollmean(Valore, k = 12, fill = NA, align = "center"),
         YoY = 100*(Valore / lag(Valore, 12) -1),
         mese = lubridate::month(Data_corretta))


ggplot(data) +
  geom_bar(aes(x = (Data_corretta), y = Valore), 
           stat = "identity", fill = "steelblue", width = 32) +
  geom_line(aes(x = (Data_corretta), y = Media_Mobile), 
            color = "red", size = 1) +
  labs(
    title = "Arrivi in USA dall'Italia",
    x = "Data",
    y = "Numero di Visitatori"
  ) +
  scale_y_continuous(
    breaks = seq(25000, 300000, by = 25000),
    labels = function(x) paste0(x/1000, " mila"),
    expand = c(0, 0)
  )+
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = c(0.02, 0.02))


unlink(temp_file)