library(rvest)
library(httr)
library(tidyverse)
library(purrr)
library(stringr)
library(readr)
library(showtext)
library(ggtext)

# Funzione per estrarre dati di un mese specifico
estrai_mese <- function(anno, mese) {
  url <- paste0("https://www.meteo.dfg.unito.it/mese-", mese, "-", anno)
  
  pagina <- tryCatch({
    GET(url,
        config(ssl_verifypeer = 0),
        user_agent("Mozilla/5.0")
    ) %>%
      content(as = "text", encoding = "ISO-8859-1") %>%
      read_html()
  }, error = function(e) {
    message("Errore ", anno, " mese ", mese, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(pagina)) return(NULL)
  
  tabella <- pagina %>% html_element(".divTable.mese")
  if (is.na(tabella)) {
    message("Tabella non trovata ", anno, " mese ", mese)
    return(NULL)
  }
  
  righe <- tabella %>% html_elements(".divTableRow")
  intestazioni <- righe[[1]] %>%
    html_elements(".divTableHead") %>%
    html_text(trim = TRUE) %>%
    str_replace_all("[\\r\\n]", " ") %>%
    str_squish()
  
  dati <- righe[-1] %>%
    map(function(riga) {
      celle <- riga %>% html_elements(".divTableCell")
      valori <- celle %>%
        map_chr(~ .x %>% html_text(trim = TRUE) %>% str_split("\n") %>% .[[1]] %>% .[1])
      if (length(valori) == 0 || !str_detect(valori[1], "^[0-9]+$")) return(NULL)
      return(valori)
    }) %>%
    compact() %>%
    map_df(~ set_names(as.list(.x), intestazioni)) %>%
    mutate(anno = anno, mese = mese)
  
  return(dati)
}

# Funzione per estrarre tutti i mesi estivi di un anno
estrai_estate <- function(anno) {
  mesi_estivi <- c(6, 7, 8)  # Giugno, Luglio, Agosto
  
  risultati <- map_df(mesi_estivi, function(mese) {
    estrai_mese(anno, mese)
  })
  
  return(risultati)
}

# Estrai dati per tutti gli anni
tutti_i_dati_estivi <- 2005:2025 %>%
  map_df(estrai_estate)

# Pulisci i dati
tutti_i_dati_estivi_pulito <- tutti_i_dati_estivi %>%
  mutate(
    Tmax = str_extract(`Tmax[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
    Tmin = str_extract(`Tmin[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
    Tmed = str_extract(`Tmed[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
    giorno = as.numeric(giorno)
  ) %>%
  select(anno, mese, giorno, Tmax, Tmin, Tmed) %>%
  # Crea date reali
  mutate(
    data = as.Date(paste(anno, mese, giorno, sep = "-")),
    # Crea una data "generica" per allineare tutti gli anni (usando 2024 come anno di riferimento)
    data_generica = as.Date(paste(2024, mese, giorno, sep = "-"))
  )

# Calcola la media 2005-2024
media_2005_2024 <- tutti_i_dati_estivi_pulito %>%
  filter(anno >= 2005, anno <= 2024) %>%
  group_by(data_generica) %>%
  summarise(Tmed = mean(Tmed, na.rm = TRUE), .groups = "drop")

# Tema e grafico
font_add_google("Source Sans Pro")
showtext_auto()

theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 50,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
      axis.title = element_text(size = 45,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0.5, unit = "cm")),
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
      plot.margin = unit(c(b = .75, l = .57, t = .75, r=.75), "cm"),
      plot.title.position = "plot",
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = ggtext::element_markdown(size = 55,  color = "#1C1C1C", hjust = 0, 
                                               margin = margin(b = 0.5, l = 0, t = 0.2, r = 0, unit = "cm")),
      plot.caption = element_text(size = 50, color = "#1C1C1C", hjust = 1, lineheight = 0.35,
                                  margin = margin(b = 0, l = 0, t = 0.5, r = 0, unit = "cm")),
      ...
    )
}

png("Temperature_Estate_Torino.png", width = 12, height = 10, units="in", res=300)
ggplot(tutti_i_dati_estivi_pulito, aes(x = data_generica, y = Tmed)) +
  # Linee per tutti gli anni tranne 2025
  geom_line(data = filter(tutti_i_dati_estivi_pulito, anno < 2025),
            aes(group = anno),
            color = "grey80", alpha = 0.6, size = 0.3) +
  # Media 2005-2024
  geom_line(data = media_2005_2024,
            aes(x = data_generica, y = Tmed),
            color = "#0478EA", size = 1) +
  # Anno 2025 in evidenza
  geom_line(data = filter(tutti_i_dati_estivi_pulito, anno == 2025),
            aes(group = anno),
            color = "#F12938", size = 1.2) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d %b",
    limits = as.Date(c("2024-06-01", "2024-08-31")),
    expand = c(0,0)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "°")) +
  theme_linechart() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Data", 
       y = "Temperatura media",
  scale_y_continuous(labels = function(x) paste0(x, "°")),
  expand = c(0,0)) +
  theme_linechart() +
  geom_vline(xintercept = as.Date("2024-07-01"), color = "grey60", linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = as.Date("2024-08-01"), color = "grey60", linetype = "dashed", alpha = 0.7) +
 labs(x = "", 
              y = "Temperatura media", 
              title = "Quanto caldo fa a Torino",
              subtitle = "Temperature medie giornaliere: <span style='color:#F12938;'>2025</span>, <span style='color:#0478EA;'>media 2005–2024</span> e <span style='color:grey40;'>singoli anni</span>",
              caption = "Elaborazione di Lorenzo Ruffino \nFonte: Osservatorio Meteorologico dell'Università di Torino, Dipartimento di Fisica")
   dev.off()
       
       # Statistiche riassuntive per confronto
       tutti_i_dati_estivi_pulito %>%
         group_by(anno < 2025) %>%
         summarise(
           Tmax = mean(Tmax, na.rm = TRUE),
           Tmed = mean(Tmed, na.rm = TRUE),
           Tmin = mean(Tmin, na.rm = TRUE),
           .groups = "drop"
         )
       
       # Statistiche per singolo mese
      medie =  tutti_i_dati_estivi_pulito %>%
         mutate(nome_mese = case_when(
           mese == 6 ~ "Giugno",
           mese == 7 ~ "Luglio", 
           mese == 8 ~ "Agosto" 
         )) %>%
         filter(giorno <=10)%>%
         group_by(nome_mese, giorno, anno < 2025) %>%
         summarise(
           Tmax = mean(Tmax, na.rm = TRUE),
           Tmed = mean(Tmed, na.rm = TRUE),
           Tmin = mean(Tmin, na.rm = TRUE),
           .groups = "drop"
         )