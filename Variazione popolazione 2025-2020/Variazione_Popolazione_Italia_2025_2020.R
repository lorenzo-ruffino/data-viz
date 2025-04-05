library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(scales)
library(showtext)

# Sources

# POSAS_2025_it_Comuni.csv: https://demo.istat.it/data/posas/POSAS_2025_it_Comuni.zip
# POSAS_2020_it_Comuni.csv: https://demo.istat.it/data/posas/POSAS_2020_it_Comuni.zip
# Elenco comuni soppressi.csv: https://www.istat.it/wp-content/uploads/2024/09/Elenco-comuni-soppressi.zip


comuni_2025 = as.data.frame(fread("POSAS_2025_it_Comuni.csv"))%>%
  clean_names()%>%
  filter(eta == 999)%>%
  select(codice_comune, comune, totale)%>%
  rename(pop_2025 = totale)

comuni_2020 = as.data.frame(fread("POSAS_2020_it_Comuni.csv"))%>%
  clean_names()%>%
  filter(eta == 999)%>%
  select(codice_comune, totale)%>%
  rename(pop_2020 = totale)

comuni_soppressi = as.data.frame(fread("Elenco comuni soppressi.csv"))%>%
  clean_names()%>%
  select(codice_comune, codice_del_comune_associato_alla_variazione)

comuni_2020_all = left_join(comuni_2020, comuni_soppressi, by="codice_comune")%>%
  mutate(nuovo_codice = ifelse(!is.na(codice_del_comune_associato_alla_variazione), codice_del_comune_associato_alla_variazione, codice_comune))%>%
  group_by(nuovo_codice)%>%
  summarise(pop_2020 = sum(pop_2020))%>%
  rename(codice_comune = nuovo_codice)

sum(comuni_2020_all$pop_2020)  == sum(comuni_2020$pop_2020)

pop_all_years = left_join(comuni_2025, comuni_2020_all, by="codice_comune")%>%
  mutate(abs_var = pop_2025 - pop_2020,
         rel_var = ((pop_2025 / pop_2020) -1)*100)


sum(pop_all_years$pop_2020, na.rm = T) - sum(comuni_2020_all$pop_2020) 


comuni_geo = read_sf("Com01012025_g_WGS84.json")

pop_all_years_geo = left_join(comuni_geo, pop_all_years, by=c("PRO_COM" = "codice_comune"))

sf_use_s2(FALSE)

regioni_geo = st_make_valid(comuni_geo) %>%
  group_by(COD_REG) %>%
  summarise(
    NOME_REGIONE = first(COD_REG),
    .groups = "drop"
  )


province_geo = st_make_valid(comuni_geo) %>%
  group_by(COD_PROV) %>%
  summarise(
    NOME_REGIONE = first(COD_PROV),
    .groups = "drop"
  )

font_add_google("Source Sans Pro")
showtext_auto()

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="Source Sans Pro"),
      legend.position="right",
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
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
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.key.height = unit(2.2, 'cm'), 
      legend.key.width = unit(0.8, 'cm'),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      legend.text =    element_text(size = 45,  color = "#1C1C1C", hjust = 0,  margin = margin(b = 0, t = 0, l = 0.2, unit = "cm")),
      plot.caption =   element_text(size = 50,  color = "#1C1C1C", hjust = 1, margin = margin(b = 0.5, t = -1, l = 0, r=0, unit = "cm")),
      plot.title =     element_text(size = 90,  color = "#1C1C1C", hjust = 0, margin = margin(b = 0, t = 1, l = 0, r = 1, unit = "cm")),
      plot.subtitle =  element_text(size = 50,  color = "#1C1C1C", hjust = 0, margin = margin(b = -0.2, t = 0.25, l = 0, unit = "cm")),
      ...
    )
}



### Variazione Popolazione nel 2023

min = min(pop_all_years_geo$rel_var, na.rm=T)
max = max(pop_all_years_geo$rel_var, na.rm=T)

min
max


png("Variazione_Popolazione_Italia_2025_2020.png", width = 9.5, height = 10, units="in", res=300)
ggplot()  +
  theme_map()+
  geom_sf(data = pop_all_years_geo, mapping=aes(geometry = geometry, fill=rel_var), color = "#a6a6a6",  lwd = 0)+
  geom_sf(data = province_geo, mapping=aes(geometry = geometry), color = "#1C1C1C",  lwd = 0.2, fill="white", alpha=0)+
  geom_sf(data = regioni_geo, mapping=aes(geometry = geometry), color = "#1C1C1C",  lwd = 0.5, fill="white", alpha=0)+
  scale_fill_stepsn(
    breaks= seq(-10, 10, by=2),
    values=rescale(c(-10, 0, 10)),
    limits = c(-10, 10),
    colours = c("#F12938", "white", "#0478EA"),
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    oob = scales::squish,
    labels = function(x) {
      ifelse(x == 10, ">10%",
             ifelse(x == -10, "<-10%",
                    paste0(" ", round(x, 1), "%")))
    },
    guide_colourbar( label.position = "left", direction = "vertical", barheight = 100, barwidth = 40, frame.colour = "black", ticks.colour = "black"))+
  labs(x = NULL, 
       y = NULL, 
       title = "Com'è cambiata la popolazione in cinque anni", 
       subtitle = "Variazione percentuale della popolazione tra il 1 gennaio 2025 e il 1 gennaio 2020", 
       caption =  "Elaborazione di Lorenzo Ruffino | Fonte dati: Istat")
dev.off()




pop_all_years
