library(tidyverse)
library(ggtext)
library(data.table)
library(showtext)

# Dati

dati = data.frame(fread("https://docs.google.com/spreadsheets/d/e/2PACX-1vQnmtInnyQjQAoOMtiAUkx4bkuUJjo3a4aooxxQMBJWkg-6IRdAzCEwKPnC6k_WRem841nljBdBr4vF/pub?gid=0&single=true&output=csv"))%>%
  mutate(Data = as.Date(Data)) # File che mantengo io

# Inizio e fine dei governi

inizio = c("2022-10-22", "2021-02-13", "2019-09-05", "2018-06-01")
fine = c(Sys.Date(), "2022-07-21", "2021-01-26", "2019-08-20")
governi = c("Meloni", "Draghi", "Conte II", "Conte I")


gov = data.frame(governi, inizio, fine)%>%
  mutate(inizio = as.Date(inizio),
         fine = as.Date(fine))

# Righe dummy per dopo

righe = gov %>%
  rename(Data = fine)%>%
  select(Data, governi, inizio)%>%
  filter(fine != max(dati$Data))


# Metti insieme tutto

data = dati %>%
  cross_join(gov) %>%
  filter(Data >= inizio & Data <= fine)%>%
  bind_rows(righe)%>%
  group_by(governi)%>%
  arrange(Data)%>%
  mutate(numero = ifelse(is.na(fine), 0, 1),
         conteggio = cumsum(numero))%>%
  arrange(Data)%>%
  group_by(governi)%>%
  mutate(precedente = lag(Data, 1),
         precedente = fifelse(is.na(precedente), inizio, precedente))%>% # Ifelse converte in numero
  group_by(governi) %>%
  group_modify(~ {
    .x %>%
      complete(Data = seq.Date(min(.x$precedente), max(.x$Data), by = "day")) %>%
      fill(inizio, conteggio, .direction = "down")
  }) %>%
  ungroup()%>%
  mutate(inizio = as.Date((case_when(governi == "Meloni" ~ "2022-10-22",
                            governi == "Draghi" ~ "2021-02-13",
                            governi == "Conte II" ~ "2019-09-05",
                            governi == "Conte I" ~ "2018-06-01"))),
         conteggio = ifelse(is.na(conteggio), 0, conteggio),
         giorni = Data - inizio)%>%
  select(Data, giorni, governi, conteggio)%>%
  distinct()


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
      plot.margin = unit(c(b = 1, l = 1.5, t = 1, r=1.5), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0.25, t = 0, r=0, unit = "cm")),
      plot.subtitle =  element_text(size = 60,  color = "#1C1C1C", hjust = 0, margin = margin(b = 1, l = 1, t = 0.2, r=0, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = 1, r=0, unit = "cm")),
      ...
    )
}


png("Voti_Fiducia.png", width = 9.5, height = 10, units="in", res=300)
ggplot(data, aes(x=giorni, y=conteggio, group=governi, colour=governi)) +
  theme_linechart()+
  geom_line(linewidth = 1.5, show.legend = F)+
  scale_color_manual(values =c("Meloni"="#F12938", "Draghi"="#0478EA", "Conte I"="#C46552",  "Conte II"="#565679" ))+
  scale_fill_manual(values =c("Meloni"="#F12938", "Draghi"="#0478EA", "Conte I"="#C46552",  "Conte II"="#565679" ))+
  geom_text(data%>%group_by(governi)%>%filter(Data == max(Data))%>%slice(1),
            mapping= aes(x=giorni+5, y=conteggio, group=governi, colour=governi, label=governi), size=20, hjust=0, fontface="bold", show.legend = FALSE, family="Source Sans Pro")+
  scale_x_continuous(limits=c(0,630), breaks=seq(0, 3000, by=100), labels = function(x) paste0(x, ""), expand = c(0.01, 0))+
  scale_y_continuous(limits=c(0,max(data$conteggio)*1.1), breaks=seq(0, 300, by=5), labels = function(x) paste0(x, ""), expand = c(0.01, 0))+
  labs(x = "Giorni di governo", 
       y = "Numero di fiducie richieste", 
       title = "I voti di fiducia richiesti dai governi",
       subtitle = "Voti di fiducia richiesti a Camera e Senato nella ultime due legislature", 
       caption = paste0("Elaborazione di Lorenzo Ruffino | Fonte dati: Camera e Senato | Data: ", format(Sys.Date(), "%d/%m/%Y")))
dev.off()