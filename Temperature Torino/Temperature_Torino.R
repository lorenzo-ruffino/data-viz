library(rvest)
library(httr)
library(tidyverse)
library(purrr)
library(stringr)
library(readr)
library(showtext)
library(ggtext)

estrai_giugno <- function(anno) {
  url <- paste0("https://www.meteo.dfg.unito.it/mese-6-", anno)
  
  pagina <- tryCatch({
    GET(url,
        config(ssl_verifypeer = 0),
        user_agent("Mozilla/5.0")
    ) %>%
      content(as = "text", encoding = "ISO-8859-1") %>%
      read_html()
  }, error = function(e) {
    message("Errore", anno, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(pagina)) return(NULL)
  
  tabella <- pagina %>% html_element(".divTable.mese")
  if (is.na(tabella)) {
    message("Tabella non trovata", anno)
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
    mutate(anno = anno, mese = 6)
  
  return(dati)
}

tutti_i_giugno <- 2005:2025 %>%
  map_df(estrai_giugno)


tutti_i_giugno_pulito <- tutti_i_giugno %>%
  mutate(
    Tmax = str_extract(`Tmax[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
    Tmin = str_extract(`Tmin[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
    Tmed = str_extract(`Tmed[°C]`, "^[0-9]+\\.?[0-9]*") %>% as.numeric(),
    giorno = as.numeric(giorno)
  )%>%
  select(anno, mese, giorno, Tmax, Tmin, Tmed)


media_2005_2024 <- tutti_i_giugno_pulito %>%
  filter(anno >= 2005, anno <= 2024) %>%
  mutate(
    giorno = as.integer(giorno),
    Tmed = as.numeric(Tmed)
  ) %>%
  group_by(giorno) %>%
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

png("Temperatura_Giugno_Torino.png", width = 9.5, height = 10, units="in", res=300)
ggplot(tutti_i_giugno_pulito, aes(x = giorno, y = Tmed)) +
  geom_line(data = filter(tutti_i_giugno_pulito, anno < 2025),
            aes(group = anno),
            color = "grey80", alpha = 0.6) +
  geom_line(data = filter(tutti_i_giugno_pulito, anno == 2025),
            aes(group = anno),
            color = "#F12938", size = 1.2) +
  geom_line(data = media_2005_2024,
            aes(x = giorno, y = Tmed),
            color = "#0478EA", size = 1) +
  scale_x_continuous(breaks = seq(1, 30, by = 3)) +
  scale_y_continuous(labels = function(x) paste0(x, "°")) +
  theme_linechart()+
  labs(x = "Giorno del mese di giugno", 
       y = "Temperature media", 
       title = "Quanto caldo fa a Torino",
       subtitle = "Temperature medie giornaliere a giugno: <span style='color:#F12938;'>2025</span>, <span style='color:#0478EA;'>media 2005–2024</span> e <span style='color:grey40;'>singoli anni</span>",
       caption = "Elaborazione di Lorenzo Ruffino \nFonte: Osservatorio Meteorologico dell'Università di Torino, Dipartimento di Fisica")
dev.off()


tutti_i_giugno_pulito %>%
  filter(giorno <= 27)%>%
  group_by(anno < 2025)%>%
  summarise(Tmax = mean(Tmax, na.rm=T),
            Tmed = mean(Tmed, na.rm=T),
            Tmin = mean(Tmin, na.rm=T))
  