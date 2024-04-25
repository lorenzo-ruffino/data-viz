library(tidyverse)
library(readxl)
library(showtext)
library(lubridate)
library(zoo)

# Scarica tutti i file dal 2012 al 2024

mesi = seq(1, 12, by=1)
anni = seq(2012, 2024, by=1)


if (!file.exists("downloaded_files")) {
  dir.create("downloaded_files")
}


for (anno in anni) {
  for (mese in mesi) {
    
    link <- paste0("https://www.adsnotizie.it/Dati/downloadDMS?mese=", mese, "&anno=", anno, "&periodicita=1&nomeReport=DMSFilePubblicato_3")
    
    nome_file <- paste0("DMS_", anno, "_", sprintf("%02d", mese), ".xls")
    
    percorso_file <- file.path("downloaded_files", nome_file)
    
    download.file(link, destfile = percorso_file, mode = "wb")
    
    Sys.sleep(2)
  }
}


# Leggi tutti i file

df = data.frame()

for (anno in anni) {
  for (mese in mesi) {

    nome_file = paste0("DMS_", anno, "_", sprintf("%02d", mese), ".xls")
    
    percorso_file = file.path("downloaded_files", nome_file)
    
    dati = read_excel(percorso_file)
    
    df = rbind(df, dati)
  }
}


# Filtra i dati
  
data = df%>%
  filter(...1 %in% c("CORRIERE DELLA SERA", "REPUBBLICA (LA)"))%>%
  rename(testata = ...1,
         anno = ...6,
         mese = ...7)%>%
  mutate(diffusione = as.numeric(`Diffusione media Italia cartacea e digitale`)+as.numeric(...12)+as.numeric(...13)+as.numeric(...14))%>%
  select(testata, anno, mese, diffusione)%>%
  mutate(testata = ifelse(testata == "CORRIERE DELLA SERA", "Corriere", "Repubblica"),
         diffusione = as.numeric(diffusione),
         date = as.Date(paste(anno, mese, "01", sep = "-"), format = "%Y-%m-%d"))%>%
  group_by(testata)%>%
  mutate(media = rollmean(diffusione, k = 6, align = "right", fill = NA))




# Tema e grafico

font_add_google("Source Sans Pro")
showtext_auto()

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_line(linewidth = 1),
      axis.text = element_text(size = 50,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.title = element_text(size = 50,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0.5, unit = "cm")),
      axis.ticks = element_blank(),
      panel.background = element_blank() ,
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title=element_blank(),
      plot.margin = unit(c(b = 0.8, l = 0.8, t = 0.8, r=0.8), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 55,  color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 0, t = 0.1, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 1, r=0, unit = "cm")),
      ...
    )
}


# Grafico Repubblica e Corriere

png("Diffusione_repubblica_corriere.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=date, y=media, group=testata, colour=testata)) +
  theme_linechart()+
  geom_line(linewidth = 1.5, show.legend = F)+
  scale_color_manual(values =c("Repubblica"="#F12938", "Corriere"="#0478EA"))+
  scale_fill_manual(values =c("Repubblica"="#F12938", "Corriere"="#0478EA"))+
  geom_text(data%>%filter(date == max(date)),
            mapping= aes(x=date, y=media-15000, group=testata, colour=testata, label=testata), size=20, hjust=1, fontface="bold", show.legend = FALSE, family="Source Sans Pro")+
  scale_y_continuous(limits=c(0,400000), breaks=seq(0, 500000, by=100000), labels = function(x) paste0(x/1000, " mila"), expand = c(0.01, 0))+
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2024-3-01")),
               date_breaks = "1 year",
               date_labels = "%Y",
               expand = c(0.01, 0))+
  labs(x = NULL,
       y = NULL,
       title = "Quanto vendono Repubblica e Corriere", 
       subtitle = "Media mobile a sei mesi del totale delle vendite individuali di Repubblica e Corriere",  
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Accertamenti Diffusione Stampa"))
dev.off()


data %>%
  filter(date %in% c(as.Date("2014-02-01"), as.Date("2024-02-01")))%>%
  group_by(testata)%>%
  mutate(variazione = ((media / lag(media, 1)) -1 )*100)

write.csv(data%>%select(testata, date, diffusione, media), file="Diffusione_repubblica_corriere.csv")


# Grafico Verità, Giornale e Libero


data = df%>%
  filter(...1 %in% c("VERITA'(LA)" , "GIORNALE (IL)", "LIBERO"))%>%
  rename(testata = ...1,
         anno = ...6,
         mese = ...7)%>%
  mutate(diffusione = as.numeric(`Diffusione media Italia cartacea e digitale`)+as.numeric(...12)+as.numeric(...13)+as.numeric(...14))%>%
  select(testata, anno, mese, diffusione)%>%
  mutate(testata = case_when(testata == "VERITA'(LA)" ~ "La Verità",
                             testata == "GIORNALE (IL)" ~ "Il Giornale",
                             testata == "LIBERO" ~ "Libero"),
         diffusione = as.numeric(diffusione),
         date = as.Date(paste(anno, mese, "01", sep = "-"), format = "%Y-%m-%d"))%>%
  group_by(testata)%>%
  mutate(media = rollmean(diffusione, k = 6, align = "right", fill = NA))


png("Diffusione_Giornali_Destra.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=date, y=media, group=testata, colour=testata)) +
  theme_linechart()+
  geom_line(linewidth = 1.5, show.legend = F)+
  scale_color_manual(values =c("La Verità"="#000525", "Il Giornale"="#0478EA", "Libero"="#C904C6"))+
  scale_fill_manual(values =c("La Verità"="#000525", "Il Giornale"="#0478EA", "Libero"="#C904C6"))+
  geom_text(data%>%filter(date == max(date)),
            mapping= aes(x=date, y=media-ifelse(testata == "Il Giornale", 4000, 1800), group=testata, colour=testata, label=testata), size=19, hjust=1, fontface="bold", show.legend = FALSE, family="Source Sans Pro")+
  scale_y_continuous(limits=c(0,110000), breaks=seq(0, 125000, by=25000), labels = function(x) paste0(x/1000, " mila"), expand = c(0.01, 0))+
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2024-3-01")),
               date_breaks = "1 year",
               date_labels = "%Y",
               expand = c(0.01, 0))+
  labs(x = NULL,
       y = NULL,
       title = "Quanto vendono il Giornale, Libero e la Verità", 
       subtitle = "Media mobile a sei mesi del totale delle vendite individuali (cartacee + digitali)",  
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Accertamenti Diffusione Stampa"))
dev.off()

data2 = data %>%
  group_by(testata)%>%
  mutate(variazione = ((media / lag(media, 1)) -1 )*100)


# Grafico quotidiano

data = df%>%
  filter(...1 %in% c("FATTO QUOTIDIANO (IL)"))%>%
  rename(testata = ...1,
         anno = ...6,
         mese = ...7)%>%
  mutate(diffusione = as.numeric(`Diffusione media Italia cartacea e digitale`)+as.numeric(...12)+as.numeric(...13)+as.numeric(...14))%>%
  select(testata, anno, mese, diffusione)%>%
  mutate(testata = case_when(testata == "FATTO QUOTIDIANO (IL)" ~ "Fatto Quotidiano",
                             testata == "GIORNALE (IL)" ~ "Il Giornale",
                             testata == "LIBERO" ~ "Libero"),
         diffusione = as.numeric(diffusione),
         date = as.Date(paste(anno, mese, "01", sep = "-"), format = "%Y-%m-%d"))%>%
  group_by(testata)%>%
  mutate(media = rollmean(diffusione, k = 6, align = "right", fill = NA))



png("Diffusione_FQ.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=date, y=media, group=testata, colour=testata)) +
  theme_linechart()+
  geom_line(linewidth = 1.5, show.legend = F)+
  scale_color_manual(values =c("Fatto Quotidiano"="#C904C6"))+
  scale_fill_manual(values =c("Fatto Quotidiano"="#C904C6"))+
  geom_text(data%>%filter(date == max(date)),
            mapping= aes(x=date, y=media-5000, group=testata, colour=testata, label=testata), size=19, hjust=1, fontface="bold", show.legend = FALSE, family="Source Sans Pro")+
  scale_y_continuous(limits=c(0,75000), breaks=seq(0, 125000, by=25000), labels = function(x) paste0(x/1000, " mila"), expand = c(0.01, 0))+
  scale_x_date(limits = c(as.Date("2014-01-01"), as.Date("2024-3-01")),
               date_breaks = "1 year",
               date_labels = "%Y",
               expand = c(0.01, 0))+
  labs(x = NULL,
       y = NULL,
       title = "Quanto vende il Fatto Quotidiano", 
       subtitle = "Media mobile a sei mesi del totale delle vendite individuali (cartacee + digitali)",  
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Accertamenti Diffusione Stampa"))
dev.off()


# Principali giornali 2014 vs 2024


proper_case <- function(string) {
  result <- gsub("\\s*\\(.*\\)", "", string) # Rimuove il testo tra parentesi
  
  # Divide la stringa in parole
  words <- strsplit(result, "\\s+")[[1]]
  
  # Converte in Proper Case o minuscolo in base alle condizioni
  modified_words <- sapply(words, function(word) {
    if (grepl("\\s+D", paste0(ifelse(word=="DOLOMITEN", ""," "), word))) {
      tolower(word)
    } else if (grepl("^d", word)) {
      gsub("^d", "D", word)
    } else {
      gsub("\\b([a-z])", "\\U\\1", tolower(word), perl = TRUE)
    }
  })
  
  # Ricompone la stringa modificata
  result <- paste(modified_words, collapse = " ")
  
  return(result)
}


data = df%>%
  rename(testata = ...1,
         anno = ...6,
         mese = ...7)%>%
  mutate(diffusione = as.numeric(`Diffusione media Italia cartacea e digitale`)+as.numeric(...12)+as.numeric(...13)+as.numeric(...14))%>%
  select(testata, anno, mese, diffusione)%>%
  mutate(date = as.Date(paste(anno, mese, "01", sep = "-"), format = "%Y-%m-%d"))%>%
  group_by(testata)%>%
  mutate(media = rollmean(diffusione, k = 6, align = "right", fill = NA))%>%
  filter(date %in% c(as.Date("2014-02-01"), as.Date("2024-02-01")))%>%
  select(testata, date, media)%>%
  spread(date, media)%>%
  clean_names()%>%
  ungroup()%>%
  arrange(desc(x2024_02_01))%>%
  filter(!testata %in% c("VERITA'(LA)",
                         "GAZZETTA SPORT-LUNEDI (LA)", "GAZZETTA SPORT (LA)", "CORRIERE SPORT-STADIO LUN.", "TUTTOSPORT LUNEDI'",
                         "CORRIERE SPORT - STADIO", "TUTTOSPORT"))%>%
  slice(1:20)%>%
  mutate(var = ((x2024_02_01 / x2014_02_01) -1 )*100)%>%
  ungroup()%>%
  mutate(
         testata = case_when(testata == 'CORRIERE DELLA SERA' ~ 'Corriere della Sera',
                             testata == 'REPUBBLICA (LA)' ~ 'La Repubblica',
                             testata == 'SOLE 24 ORE (IL)' ~ 'Il Sole 24 Ore',
                             testata == 'STAMPA (LA)' ~ 'La Stampa',
                             testata == 'QN-Il Resto del Carlino' ~ 'Il Resto del Carlino',
                             testata == 'FATTO QUOTIDIANO (IL)' ~ 'Il Fatto Quotidiano',
                             testata == 'MESSAGGERO (IL)' ~ 'Il Messaggero',
                             testata == 'GAZZETTINO (IL)' ~ 'Il Gazzettino',
                             testata == 'QN-La Nazione' ~ 'La Nazione',
                             testata == 'DOLOMITEN' ~ 'Dolomiten',
                             testata == 'GIORNALE (IL)' ~ 'Il Giornale',
                             testata == "ECO DI BERGAMO (L')" ~ "L'Eco di Bergamo",
                             testata == 'MESSAGGERO VENETO' ~ 'Messaggero Veneto',
                             testata == "UNIONE SARDA (L')" ~ "L'Unione Sarda",
                              testata == 'SECOLO XIX (IL)' ~ 'Il Secolo XIX',
                              testata == 'GIORNALE DI BRESCIA' ~ 'Giornale di Brescia',
                              testata == 'LIBERO' ~ 'Libero',
                              testata == 'GAZZETTA DI PARMA' ~ 'Gazzetta di Parma',
                              testata == 'NUOVA SARDEGNA (LA)' ~ 'La Nuova Sardegna',
                              testata == "ADIGE (L')" ~ "L'Adige"))



data$testata <- reorder(data$testata, data$var)



#font_add_google("Source Sans Pro")
showtext_auto()


theme_barchart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.y = element_text(size = 50,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank() ,
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.border = element_blank(),
      legend.title=element_blank(),
      plot.margin = unit(c(b = 1, l = 1, t = 1, r=1), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0.5, l = 0, t = 0.25, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 0.5, r=0, unit = "cm")),
      plot.caption.position =  "plot",
      ...
    )
}

media = (sum(data$x2024_02_01) / sum(data$x2014_02_01) -1 )*100


png("Vendite_Quotidiani_Dieci_Anni.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=var, y=testata, label = paste0(round(var, 0), "%"))) +
  theme_barchart()+
 # geom_vline(xintercept = media, linewidth=0.5)+
  geom_col(fill = "#F12938", show.legend = F)+
 # scale_fill_manual(values =c("TRUE"="#F12938", "FALSE"="#0478EA"))+
  geom_text(size=17, hjust=-0.2, show.legend = FALSE, family="Source Sans Pro", colour="white")+
  scale_x_continuous(limits=c(NA,0), expand = c(0.01, 0))+
  scale_y_discrete(position = "right")+
  labs(x = NULL,
       y = NULL,
       title = "Le vendite dei quotidiani in 10 anni", 
       subtitle = "Totale delle vendite individuali (cartacee + digitali) nel 2014 e 2024 dei primi 20 giornali",  
       caption = paste("Elaborazione di Lorenzo Ruffino | Fonte dati: Accertamenti Diffusione Stampa"))
dev.off()
