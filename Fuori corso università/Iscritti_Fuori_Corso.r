library(data.table)
library(tidyverse)
library(showtext)
library(ggrepel)

iscritti = fread("https://dati-ustat.mur.gov.it/datastore/dump/f62e0f3b-5ee1-489a-a1d1-6794943a6408?bom=True")

iscritti[, CorsoTIPO := fcase(
  grepl("vecchio ordinamento|Diploma Universitario|Scuola diretta", CorsoTIPO), "Vecchio Ordinamento",
  grepl("Laurea  - dm 509/99|Laurea - dm 270/04", CorsoTIPO), "Laurea",
  grepl("Laurea Magistrale - dm 270/04|Laurea Specialistica - dm 509/99", CorsoTIPO), "Laurea Magistrale",
  grepl("Ciclo Unico", CorsoTIPO), "Laurea Ciclo Unico",
  default = NA_character_
)]


iscritti = iscritti %>%
  group_by(CorsoTIPO, AnnoA)%>%
  summarise(Isc = sum(Isc))

fuori_corso = fread("https://dati-ustat.mur.gov.it/datastore/dump/876d093a-f939-4193-bf21-5eaa7b340db5?bom=True")%>%
  mutate(CorsoTIPO = ifelse(CorsoTIPO == 'Laurea Magistrale Ciclo Unico', "Laurea Ciclo Unico", CorsoTIPO))%>%
  group_by(CorsoTIPO, AnnoA)%>%
  summarise(Isc_FC = sum(Isc_FC))

data = left_join(fuori_corso, iscritti, by = c("CorsoTIPO", "AnnoA"))%>%
  mutate(fc_perc = 100 * (Isc_FC / Isc))%>%
  mutate(Anno = sub("/.*", "", AnnoA)) %>%   
  mutate(Anno = as.integer(Anno))    %>%
  filter(CorsoTIPO != 'Vecchio Ordinamento')

data %>%
  ungroup()%>%
  filter(Anno == max(Anno))%>%
  summarise(Isc_FC = sum(Isc_FC),
            Isc = sum(Isc))%>%
  mutate(fc_perc = 100 * (Isc_FC / Isc))



font_add_google("Source Sans Pro")
showtext_auto()


theme_linechart <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_line(linewidth = 0.3),
      axis.text = element_text(size = 10,  color = "#1C1C1C", hjust = 0.5,  margin = margin(b = 0, t = 0, l = 0, unit = "cm")),
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
      plot.margin = unit(c(b = 0.4, l = 0.4, t = 0.4, r=0.4), "cm"),
      plot.title.position = "plot",
      legend.text =    element_text(size = 15,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 18,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, l = 0, t = 0, r=0, unit = "cm")),
      plot.subtitle = element_text(size = 12, color = "#1C1C1C", hjust = 0, lineheight = 0.3, margin = margin(b = 1, l = 0, t = 0.15, r = 0, unit = "cm")),
      plot.caption =   element_text(size = 11,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0, l = 0, t = .5, r=0, unit = "cm")),
      ...
    )
}

labels_data = data %>%
  group_by(CorsoTIPO) %>%
  filter(Anno == max(Anno)) %>%
  ungroup()%>%
  mutate(y_pos = case_when(
    CorsoTIPO == "Laurea Ciclo Unico" ~ fc_perc - 0.5,  
    TRUE ~ fc_perc + 0.5                            
  ))


png("Iscritti_Fuori_Corso.png", width = 8, height = 7, units="in", res=300)

ggplot(data, aes(x = Anno, y = fc_perc, colour = CorsoTIPO, group = CorsoTIPO)) +
  theme_minimal(base_family = "Source Sans Pro") +
  geom_line(linewidth = 1, show.legend = F) +
  geom_point(size = 2, show.legend = F) +
  scale_color_manual(
    values = c(
      "Laurea" = "#F12938",
      "Laurea Magistrale" = "#0478EA",
      "Laurea Ciclo Unico" = "#02B875",
      "Vecchio Ordinamento" = "#9B59B6"
    )
  ) +
  scale_y_continuous(
    limits = c(10, 40),
    breaks = seq(0, 100, by = 5),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(2011, 2023, by = 2),
    limits = c(2010.5, 2023.5),
    expand = c(0, 0)
  ) +
  geom_text_repel(
    data = labels_data,
    aes(x = Anno, y = y_pos, label = CorsoTIPO, colour = CorsoTIPO),
    size = 4,
    direction = "y",
    show.legend = FALSE,
    family = "Source Sans Pro"
  )+
  labs(
    x = NULL,
    y = NULL,
    title = "Quanti sono gli iscritti fuori corso all'università",
    subtitle = "Percentuale di iscritti fuori corso per tipo di corso di laurea (2011–2023)",
    caption = "Elaborazione di Lorenzo Ruffino | Fonte dati: Portale dei dati dell'istruzione superiore"
  ) +
  theme_linechart()

dev.off()
